/*!
A library for reading and writing Unreal Engine save files (commonly referred to
as GVAS).

It has been tested on an extensive set of object structures and can fully read
and write Deep Rock Galactic save files (and likely a lot more).

There is a small binary utility to quickly convert saves to and from a plain
text JSON format which can be used for manual save editing.

# Example

```
use std::fs::File;

use uesave::{Property, Save};

let save = Save::read(&mut File::open("drg-save-test.sav")?)?;
match save.root.properties["NumberOfGamesPlayed"] {
    Property::Int { value, .. } => {
        assert_eq!(2173, value);
    }
    _ => {}
}
# Ok::<(), Box<dyn std::error::Error>>(())

```
*/

mod error;

pub use error::Error;

use byteorder::{ReadBytesExt, WriteBytesExt, LE};
use error::ParseError;
use std::io::{Cursor, Read, Seek, Write};

use serde::{Deserialize, Serialize};

type TResult<T> = Result<T, Error>;

trait Readable<R: Read + Seek> {
    fn read(reader: &mut Context<R>) -> TResult<Self>
    where
        Self: Sized;
}
trait Writable<W> {
    fn write(&self, writer: &mut Context<W>) -> TResult<()>;
}

struct SeekReader<R: Read> {
    reader: R,
    read_bytes: usize,
}

impl<R: Read> SeekReader<R> {
    fn new(reader: R) -> Self {
        Self {
            reader,
            read_bytes: 0,
        }
    }
}
impl<R: Read> Seek for SeekReader<R> {
    fn seek(&mut self, pos: std::io::SeekFrom) -> std::io::Result<u64> {
        match pos {
            std::io::SeekFrom::Current(0) => Ok(self.read_bytes as u64),
            _ => unimplemented!(),
        }
    }
}
impl<R: Read> Read for SeekReader<R> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        self.reader.read(buf).map(|s| {
            self.read_bytes += s;
            s
        })
    }
}

fn read_optional_uuid<R: Read + Seek>(reader: &mut Context<R>) -> TResult<Option<uuid::Uuid>> {
    Ok(if reader.read_u8()? > 0 {
        Some(uuid::Uuid::read(reader)?)
    } else {
        None
    })
}
fn write_optional_uuid<W: Write>(writer: &mut Context<W>, id: Option<uuid::Uuid>) -> TResult<()> {
    if let Some(id) = id {
        writer.write_u8(1)?;
        id.write(writer)?;
    } else {
        writer.write_u8(0)?;
    }
    Ok(())
}

fn read_string<R: Read + Seek>(reader: &mut Context<R>) -> TResult<String> {
    let len = reader.read_i32::<LE>()?;
    if len < 0 {
        let chars = read_array((-len) as u32, reader, |r| Ok(r.read_u16::<LE>()?))?;
        let length = chars.iter().position(|&c| c == 0).unwrap_or(chars.len());
        Ok(String::from_utf16(&chars[..length]).unwrap())
    } else {
        let mut chars = vec![0; len as usize];
        reader.read_exact(&mut chars)?;
        let length = chars.iter().position(|&c| c == 0).unwrap_or(chars.len());
        Ok(String::from_utf8_lossy(&chars[..length]).into_owned())
    }
}
fn write_string<W: Write>(writer: &mut Context<W>, string: &str) -> TResult<()> {
    if string.is_empty() {
        writer.write_u32::<LE>(0)?;
    } else {
        write_string_always_trailing(writer, string)?;
    }
    Ok(())
}

fn write_string_always_trailing<W: Write>(writer: &mut Context<W>, string: &str) -> TResult<()> {
    if string.is_empty() || string.is_ascii() {
        writer.write_u32::<LE>(string.as_bytes().len() as u32 + 1)?;
        writer.write_all(string.as_bytes())?;
        writer.write_u8(0)?;
    } else {
        let chars: Vec<u16> = string.encode_utf16().collect();
        writer.write_i32::<LE>(-(chars.len() as i32 + 1))?;
        for c in chars {
            writer.write_u16::<LE>(c)?;
        }
        writer.write_u16::<LE>(0)?;
    }
    Ok(())
}

type Properties = indexmap::IndexMap<String, Property>;
fn read_properties_until_none<R: Read + Seek>(reader: &mut Context<R>) -> TResult<Properties> {
    let mut properties = Properties::new();
    while let Some((name, prop)) = read_property(reader)? {
        let mut is_parsed = false;
        #[cfg(feature = "parse_raw_data")]
        if name == "RawData" {
            match &prop {
                Property {
                    id,
                    inner: PropertyInner::Array { value, .. },
                } => match value {
                    ValueArray::Base(ValueVec::Byte(ByteArray::Byte(v))) => {
                        let buf = std::io::Cursor::new(v.as_slice());
                        let mut temp_buf = std::io::BufReader::new(buf);
                        let mut temp_reader =
                            Context::<'_, '_, '_, '_, std::io::BufReader<Cursor<&[u8]>>> {
                                stream: &mut temp_buf,
                                header: reader.header,
                                types: reader.types,
                                scope: reader.scope,
                            };
                        if let Ok(inner_props) = read_properties_until_none(&mut temp_reader) {
                            temp_reader.read_u32::<LE>()?;
                            let struct_id = uuid::Uuid::read(&mut temp_reader)?;
                            let replacement = Property {
                                id: *id,
                                inner: PropertyInner::RawData {
                                    properties: inner_props,
                                    struct_id,
                                },
                            };
                            properties.insert(name.clone(), replacement);
                            is_parsed = true;
                        }
                    }
                    _ => {}
                },
                _ => {}
            }
        }
        if !is_parsed {
            properties.insert(name, prop);
        }
    }
    Ok(properties)
}
fn write_properties_none_terminated<W: Write>(
    writer: &mut Context<W>,
    properties: &Properties,
) -> TResult<()> {
    for p in properties {
        write_property(p, writer)?;
    }
    write_string(writer, "None")?;
    Ok(())
}

fn read_property<R: Read + Seek>(reader: &mut Context<R>) -> TResult<Option<(String, Property)>> {
    let name = read_string(reader)?;
    if name == "None" {
        Ok(None)
    } else {
        reader.scope(&name, |reader| {
            let t = PropertyType::read(reader)?;
            let size = reader.read_u64::<LE>()?;
            let value = Property::read(reader, t, size)?;
            Ok(Some((name.clone(), value)))
        })
    }
}
fn write_property<W: Write>(prop: (&String, &Property), writer: &mut Context<W>) -> TResult<()> {
    write_string(writer, prop.0)?;
    prop.1.get_type().write(writer)?;

    let mut buf = vec![];
    let size = writer.stream(&mut buf, |writer| prop.1.write(writer))?;

    writer.write_u64::<LE>(size as u64)?;
    writer.write_all(&buf[..])?;
    Ok(())
}

fn read_array<T, F, R: Read + Seek>(length: u32, reader: &mut Context<R>, f: F) -> TResult<Vec<T>>
where
    F: Fn(&mut Context<R>) -> TResult<T>,
{
    (0..length).map(|_| f(reader)).collect()
}

#[rustfmt::skip]
impl<R: Read + Seek> Readable<R> for uuid::Uuid {
    fn read(reader: &mut Context<R>) -> TResult<uuid::Uuid> {
        let mut b = [0; 16];
        reader.read_exact(&mut b)?;
        Ok(uuid::Uuid::from_bytes([
            b[0x3], b[0x2], b[0x1], b[0x0],
            b[0x7], b[0x6], b[0x5], b[0x4],
            b[0xb], b[0xa], b[0x9], b[0x8],
            b[0xf], b[0xe], b[0xd], b[0xc],
        ]))
    }
}
#[rustfmt::skip]
impl<W: Write> Writable<W> for uuid::Uuid {
    fn write(&self, writer: &mut Context<W>) -> TResult<()> {
        let b = self.as_bytes();
        writer.write_all(&[
            b[0x3], b[0x2], b[0x1], b[0x0],
            b[0x7], b[0x6], b[0x5], b[0x4],
            b[0xb], b[0xa], b[0x9], b[0x8],
            b[0xf], b[0xe], b[0xd], b[0xc],
        ])?;
        Ok(())
    }
}

/// Used to disambiguate types within a [`Property::Set`] or [`Property::Map`] during parsing.
#[derive(Debug, Default, Clone)]
pub struct Types {
    types: std::collections::HashMap<String, StructType>,
}
impl Types {
    /// Create an empty [`Types`] specification
    pub fn new() -> Self {
        Self::default()
    }
    /// Add a new type at the given path
    pub fn add(&mut self, path: String, t: StructType) {
        // TODO: Handle escaping of '.' in property names
        // probably should store keys as Vec<String>
        self.types.insert(path, t);
    }
}

#[derive(Debug)]
enum Scope<'p, 'n> {
    Root,
    Node {
        parent: &'p Scope<'p, 'p>,
        name: &'n str,
    },
}

impl<'p, 'n> Scope<'p, 'n> {
    fn path(&self) -> String {
        match self {
            Self::Root => "".into(),
            Self::Node { parent, name } => {
                format!("{}.{}", parent.path(), name)
            }
        }
    }
}

#[derive(Debug)]
struct Context<'stream, 'header, 'types, 'scope, S> {
    stream: &'stream mut S,
    header: Option<&'header Header>,
    types: &'types Types,
    scope: &'scope Scope<'scope, 'scope>,
}
impl<R: Read> Read for Context<'_, '_, '_, '_, R> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        self.stream.read(buf)
    }
}
impl<S: Seek> Seek for Context<'_, '_, '_, '_, S> {
    fn seek(&mut self, pos: std::io::SeekFrom) -> std::io::Result<u64> {
        self.stream.seek(pos)
    }
}
impl<W: Write> Write for Context<'_, '_, '_, '_, W> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.stream.write(buf)
    }
    fn flush(&mut self) -> std::io::Result<()> {
        self.stream.flush()
    }
}

impl<'stream, 'header, 'types, 'scope, S> Context<'stream, 'header, 'types, 'scope, S> {
    fn run<F, T>(stream: &'stream mut S, f: F) -> T
    where
        F: FnOnce(&mut Context<'stream, '_, '_, 'scope, S>) -> T,
    {
        f(&mut Context::<'stream, '_, '_, 'scope> {
            stream,
            header: None,
            types: &Types::new(),
            scope: &Scope::Root,
        })
    }
    fn run_with_types<F, T>(types: &'types Types, stream: &'stream mut S, f: F) -> T
    where
        F: FnOnce(&mut Context<'stream, '_, 'types, 'scope, S>) -> T,
    {
        f(&mut Context::<'stream, '_, 'types, 'scope> {
            stream,
            header: None,
            types,
            scope: &Scope::Root,
        })
    }
    fn scope<'name, F, T>(&mut self, name: &'name str, f: F) -> T
    where
        F: FnOnce(&mut Context<'_, '_, 'types, '_, S>) -> T,
    {
        f(&mut Context {
            stream: self.stream,
            header: self.header,
            types: self.types,
            scope: &Scope::Node {
                name,
                parent: self.scope,
            },
        })
    }
    fn header<'h, F, T>(&mut self, header: &'h Header, f: F) -> T
    where
        F: FnOnce(&mut Context<'_, '_, 'types, '_, S>) -> T,
    {
        f(&mut Context {
            stream: self.stream,
            header: Some(header),
            types: self.types,
            scope: self.scope,
        })
    }
    fn stream<'s, F, T, S2>(&mut self, stream: &'s mut S2, f: F) -> T
    where
        F: FnOnce(&mut Context<'_, '_, 'types, '_, S2>) -> T,
    {
        f(&mut Context {
            stream,
            header: self.header,
            types: self.types,
            scope: self.scope,
        })
    }
    fn path(&self) -> String {
        self.scope.path()
    }
    fn get_type(&self) -> Option<&'types StructType> {
        self.types.types.get(&self.path())
    }
}
impl<'stream, 'header, 'types, 'scope, R: Read + Seek>
    Context<'stream, 'header, 'types, 'scope, R>
{
    fn get_type_or<'t>(&mut self, t: &'t StructType) -> TResult<&'t StructType>
    where
        'types: 't,
    {
        let offset = self.stream.stream_position()?;
        Ok(self.get_type().unwrap_or_else(|| t))
    }
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum PropertyType {
    IntProperty,
    Int8Property,
    Int16Property,
    Int64Property,
    UInt8Property,
    UInt16Property,
    UInt32Property,
    UInt64Property,
    FloatProperty,
    DoubleProperty,
    BoolProperty,
    ByteProperty,
    EnumProperty,
    ArrayProperty,
    ObjectProperty,
    StrProperty,
    FieldPathProperty,
    SoftObjectProperty,
    NameProperty,
    TextProperty,
    DelegateProperty,
    MulticastDelegateProperty,
    MulticastInlineDelegateProperty,
    MulticastSparseDelegateProperty,
    SetProperty,
    MapProperty,
    StructProperty,
}
impl PropertyType {
    fn get_name(&self) -> &str {
        match &self {
            PropertyType::Int8Property => "Int8Property",
            PropertyType::Int16Property => "Int16Property",
            PropertyType::IntProperty => "IntProperty",
            PropertyType::Int64Property => "Int64Property",
            PropertyType::UInt8Property => "UInt8Property",
            PropertyType::UInt16Property => "UInt16Property",
            PropertyType::UInt32Property => "UInt32Property",
            PropertyType::UInt64Property => "UInt64Property",
            PropertyType::FloatProperty => "FloatProperty",
            PropertyType::DoubleProperty => "DoubleProperty",
            PropertyType::BoolProperty => "BoolProperty",
            PropertyType::ByteProperty => "ByteProperty",
            PropertyType::EnumProperty => "EnumProperty",
            PropertyType::ArrayProperty => "ArrayProperty",
            PropertyType::ObjectProperty => "ObjectProperty",
            PropertyType::StrProperty => "StrProperty",
            PropertyType::FieldPathProperty => "FieldPathProperty",
            PropertyType::SoftObjectProperty => "SoftObjectProperty",
            PropertyType::NameProperty => "NameProperty",
            PropertyType::TextProperty => "TextProperty",
            PropertyType::DelegateProperty => "DelegateProperty",
            PropertyType::MulticastDelegateProperty => "MulticastDelegateProperty",
            PropertyType::MulticastInlineDelegateProperty => "MulticastInlineDelegateProperty",
            PropertyType::MulticastSparseDelegateProperty => "MulticastSparseDelegateProperty",
            PropertyType::SetProperty => "SetProperty",
            PropertyType::MapProperty => "MapProperty",
            PropertyType::StructProperty => "StructProperty",
        }
    }
    fn read<R: Read + Seek>(reader: &mut Context<R>) -> TResult<Self> {
        let t = read_string(reader)?;
        match t.as_str() {
            "Int8Property" => Ok(PropertyType::Int8Property),
            "Int16Property" => Ok(PropertyType::Int16Property),
            "IntProperty" => Ok(PropertyType::IntProperty),
            "Int64Property" => Ok(PropertyType::Int64Property),
            "UInt8Property" => Ok(PropertyType::UInt8Property),
            "UInt16Property" => Ok(PropertyType::UInt16Property),
            "UInt32Property" => Ok(PropertyType::UInt32Property),
            "UInt64Property" => Ok(PropertyType::UInt64Property),
            "FloatProperty" => Ok(PropertyType::FloatProperty),
            "DoubleProperty" => Ok(PropertyType::DoubleProperty),
            "BoolProperty" => Ok(PropertyType::BoolProperty),
            "ByteProperty" => Ok(PropertyType::ByteProperty),
            "EnumProperty" => Ok(PropertyType::EnumProperty),
            "ArrayProperty" => Ok(PropertyType::ArrayProperty),
            "ObjectProperty" => Ok(PropertyType::ObjectProperty),
            "StrProperty" => Ok(PropertyType::StrProperty),
            "FieldPathProperty" => Ok(PropertyType::FieldPathProperty),
            "SoftObjectProperty" => Ok(PropertyType::SoftObjectProperty),
            "NameProperty" => Ok(PropertyType::NameProperty),
            "TextProperty" => Ok(PropertyType::TextProperty),
            "DelegateProperty" => Ok(PropertyType::DelegateProperty),
            "MulticastDelegateProperty" => Ok(PropertyType::MulticastDelegateProperty),
            "MulticastInlineDelegateProperty" => Ok(PropertyType::MulticastInlineDelegateProperty),
            "MulticastSparseDelegateProperty" => Ok(PropertyType::MulticastSparseDelegateProperty),
            "SetProperty" => Ok(PropertyType::SetProperty),
            "MapProperty" => Ok(PropertyType::MapProperty),
            "StructProperty" => Ok(PropertyType::StructProperty),
            _ => Err(Error::UnknownPropertyType(format!("{t:?}"))),
        }
    }
    fn write<W: Write>(&self, writer: &mut Context<W>) -> TResult<()> {
        write_string(writer, self.get_name())?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum StructType {
    Guid,
    DateTime,
    Timespan,
    Vector2D,
    Vector,
    Box,
    IntPoint,
    Quat,
    Rotator,
    LinearColor,
    Color,
    SoftObjectPath,
    GameplayTagContainer,
    Struct(Option<String>),
}
impl From<&str> for StructType {
    fn from(t: &str) -> Self {
        match t {
            "Guid" => StructType::Guid,
            "DateTime" => StructType::DateTime,
            "Timespan" => StructType::Timespan,
            "Vector2D" => StructType::Vector2D,
            "Vector" => StructType::Vector,
            "Box" => StructType::Box,
            "IntPoint" => StructType::IntPoint,
            "Quat" => StructType::Quat,
            "Rotator" => StructType::Rotator,
            "LinearColor" => StructType::LinearColor,
            "Color" => StructType::Color,
            "SoftObjectPath" => StructType::SoftObjectPath,
            "GameplayTagContainer" => StructType::GameplayTagContainer,
            "Struct" => StructType::Struct(None),
            _ => StructType::Struct(Some(t.to_owned())),
        }
    }
}
impl From<String> for StructType {
    fn from(t: String) -> Self {
        match t.as_str() {
            "Guid" => StructType::Guid,
            "DateTime" => StructType::DateTime,
            "Timespan" => StructType::Timespan,
            "Vector2D" => StructType::Vector2D,
            "Vector" => StructType::Vector,
            "Box" => StructType::Box,
            "IntPoint" => StructType::IntPoint,
            "Quat" => StructType::Quat,
            "Rotator" => StructType::Rotator,
            "LinearColor" => StructType::LinearColor,
            "Color" => StructType::Color,
            "SoftObjectPath" => StructType::SoftObjectPath,
            "GameplayTagContainer" => StructType::GameplayTagContainer,
            "Struct" => StructType::Struct(None),
            _ => StructType::Struct(Some(t)),
        }
    }
}
impl StructType {
    fn read<R: Read + Seek>(reader: &mut Context<R>) -> TResult<Self> {
        Ok(read_string(reader)?.into())
    }
    fn write<W: Write>(&self, writer: &mut Context<W>) -> TResult<()> {
        write_string(
            writer,
            match &self {
                StructType::Guid => "Guid",
                StructType::DateTime => "DateTime",
                StructType::Timespan => "Timespan",
                StructType::Vector2D => "Vector2D",
                StructType::Vector => "Vector",
                StructType::Box => "Box",
                StructType::IntPoint => "IntPoint",
                StructType::Quat => "Quat",
                StructType::Rotator => "Rotator",
                StructType::LinearColor => "LinearColor",
                StructType::Color => "Color",
                StructType::SoftObjectPath => "SoftObjectPath",
                StructType::GameplayTagContainer => "GameplayTagContainer",
                StructType::Struct(Some(t)) => t,
                _ => unreachable!(),
            },
        )?;
        Ok(())
    }
}

type DateTime = u64;
type Timespan = i64;
type Int8 = i8;
type Int16 = i16;
type Int = i32;
type Int64 = i64;
type UInt8 = u8;
type UInt16 = u16;
type UInt32 = u32;
type UInt64 = u64;
type Float = f32;
type Double = f64;
type Bool = bool;
type Enum = String;

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct MapEntry {
    pub key: PropertyValue,
    pub value: PropertyValue,
}
impl MapEntry {
    fn read<R: Read + Seek>(
        reader: &mut Context<R>,
        key_type: &PropertyType,
        key_struct_type: Option<&StructType>,
        value_type: &PropertyType,
        value_struct_type: Option<&StructType>,
    ) -> TResult<MapEntry> {
        let key = PropertyValue::read(reader, key_type, key_struct_type)?;
        let value = PropertyValue::read(reader, value_type, value_struct_type)?;
        Ok(Self { key, value })
    }
    fn write<W: Write>(&self, writer: &mut Context<W>) -> TResult<()> {
        self.key.write(writer)?;
        self.value.write(writer)?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct FieldPath {
    path: Vec<String>,
    owner: String,
}
impl FieldPath {
    fn read<R: Read + Seek>(reader: &mut Context<R>) -> TResult<Self> {
        Ok(Self {
            path: read_array(reader.read_u32::<LE>()?, reader, read_string)?,
            owner: read_string(reader)?,
        })
    }
    fn write<W: Write>(&self, writer: &mut Context<W>) -> TResult<()> {
        writer.write_u32::<LE>(self.path.len() as u32)?;
        for p in &self.path {
            write_string(writer, p)?;
        }
        write_string(writer, &self.owner)?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Delegate {
    name: String,
    path: String,
}
impl Delegate {
    fn read<R: Read + Seek>(reader: &mut Context<R>) -> TResult<Self> {
        Ok(Self {
            name: read_string(reader)?,
            path: read_string(reader)?,
        })
    }
    fn write<W: Write>(&self, writer: &mut Context<W>) -> TResult<()> {
        write_string(writer, &self.name)?;
        write_string(writer, &self.path)?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct MulticastDelegate(Vec<Delegate>);
impl MulticastDelegate {
    fn read<R: Read + Seek>(reader: &mut Context<R>) -> TResult<Self> {
        Ok(Self(read_array(
            reader.read_u32::<LE>()?,
            reader,
            Delegate::read,
        )?))
    }
    fn write<W: Write>(&self, writer: &mut Context<W>) -> TResult<()> {
        writer.write_u32::<LE>(self.0.len() as u32)?;
        for entry in &self.0 {
            entry.write(writer)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct MulticastInlineDelegate(Vec<Delegate>);
impl MulticastInlineDelegate {
    fn read<R: Read + Seek>(reader: &mut Context<R>) -> TResult<Self> {
        Ok(Self(read_array(
            reader.read_u32::<LE>()?,
            reader,
            Delegate::read,
        )?))
    }
    fn write<W: Write>(&self, writer: &mut Context<W>) -> TResult<()> {
        writer.write_u32::<LE>(self.0.len() as u32)?;
        for entry in &self.0 {
            entry.write(writer)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct MulticastSparseDelegate(Vec<Delegate>);
impl MulticastSparseDelegate {
    fn read<R: Read + Seek>(reader: &mut Context<R>) -> TResult<Self> {
        Ok(Self(read_array(
            reader.read_u32::<LE>()?,
            reader,
            Delegate::read,
        )?))
    }
    fn write<W: Write>(&self, writer: &mut Context<W>) -> TResult<()> {
        writer.write_u32::<LE>(self.0.len() as u32)?;
        for entry in &self.0 {
            entry.write(writer)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct LinearColor {
    pub r: f32,
    pub g: f32,
    pub b: f32,
    pub a: f32,
}
impl LinearColor {
    fn read<R: Read + Seek>(reader: &mut Context<R>) -> TResult<Self> {
        Ok(Self {
            r: reader.read_f32::<LE>()?,
            g: reader.read_f32::<LE>()?,
            b: reader.read_f32::<LE>()?,
            a: reader.read_f32::<LE>()?,
        })
    }
    fn write<W: Write>(&self, writer: &mut Context<W>) -> TResult<()> {
        writer.write_f32::<LE>(self.r)?;
        writer.write_f32::<LE>(self.g)?;
        writer.write_f32::<LE>(self.b)?;
        writer.write_f32::<LE>(self.a)?;
        Ok(())
    }
}
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Quat {
    pub x: f64,
    pub y: f64,
    pub z: f64,
    pub w: f64,
}
impl Quat {
    fn read<R: Read + Seek>(reader: &mut Context<R>) -> TResult<Self> {
        if reader.header.as_ref().unwrap().large_world_coordinates() {
            Ok(Self {
                x: reader.read_f64::<LE>()?,
                y: reader.read_f64::<LE>()?,
                z: reader.read_f64::<LE>()?,
                w: reader.read_f64::<LE>()?,
            })
        } else {
            Ok(Self {
                x: reader.read_f32::<LE>()? as f64,
                y: reader.read_f32::<LE>()? as f64,
                z: reader.read_f32::<LE>()? as f64,
                w: reader.read_f32::<LE>()? as f64,
            })
        }
    }
    fn write<W: Write>(&self, writer: &mut Context<W>) -> TResult<()> {
        if writer.header.as_ref().unwrap().large_world_coordinates() {
            writer.write_f64::<LE>(self.x)?;
            writer.write_f64::<LE>(self.y)?;
            writer.write_f64::<LE>(self.z)?;
            writer.write_f64::<LE>(self.w)?;
        } else {
            writer.write_f32::<LE>(self.x as f32)?;
            writer.write_f32::<LE>(self.y as f32)?;
            writer.write_f32::<LE>(self.z as f32)?;
            writer.write_f32::<LE>(self.w as f32)?;
        }
        Ok(())
    }
}
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Rotator {
    pub x: f64,
    pub y: f64,
    pub z: f64,
}
impl Rotator {
    fn read<R: Read + Seek>(reader: &mut Context<R>) -> TResult<Self> {
        if reader.header.as_ref().unwrap().large_world_coordinates() {
            Ok(Self {
                x: reader.read_f64::<LE>()?,
                y: reader.read_f64::<LE>()?,
                z: reader.read_f64::<LE>()?,
            })
        } else {
            Ok(Self {
                x: reader.read_f32::<LE>()? as f64,
                y: reader.read_f32::<LE>()? as f64,
                z: reader.read_f32::<LE>()? as f64,
            })
        }
    }
    fn write<W: Write>(&self, writer: &mut Context<W>) -> TResult<()> {
        if writer.header.as_ref().unwrap().large_world_coordinates() {
            writer.write_f64::<LE>(self.x)?;
            writer.write_f64::<LE>(self.y)?;
            writer.write_f64::<LE>(self.z)?;
        } else {
            writer.write_f32::<LE>(self.x as f32)?;
            writer.write_f32::<LE>(self.y as f32)?;
            writer.write_f32::<LE>(self.z as f32)?;
        }
        Ok(())
    }
}
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Color {
    pub r: u8,
    pub g: u8,
    pub b: u8,
    pub a: u8,
}
impl Color {
    fn read<R: Read + Seek>(reader: &mut Context<R>) -> TResult<Self> {
        Ok(Self {
            r: reader.read_u8()?,
            g: reader.read_u8()?,
            b: reader.read_u8()?,
            a: reader.read_u8()?,
        })
    }
    fn write<W: Write>(&self, writer: &mut Context<W>) -> TResult<()> {
        writer.write_u8(self.r)?;
        writer.write_u8(self.g)?;
        writer.write_u8(self.b)?;
        writer.write_u8(self.a)?;
        Ok(())
    }
}
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Vector {
    pub x: f64,
    pub y: f64,
    pub z: f64,
}
impl Vector {
    fn read<R: Read + Seek>(reader: &mut Context<R>) -> TResult<Self> {
        if reader.header.as_ref().unwrap().large_world_coordinates() {
            Ok(Self {
                x: reader.read_f64::<LE>()?,
                y: reader.read_f64::<LE>()?,
                z: reader.read_f64::<LE>()?,
            })
        } else {
            Ok(Self {
                x: reader.read_f32::<LE>()? as f64,
                y: reader.read_f32::<LE>()? as f64,
                z: reader.read_f32::<LE>()? as f64,
            })
        }
    }
    fn write<W: Write>(&self, writer: &mut Context<W>) -> TResult<()> {
        if writer.header.as_ref().unwrap().large_world_coordinates() {
            writer.write_f64::<LE>(self.x)?;
            writer.write_f64::<LE>(self.y)?;
            writer.write_f64::<LE>(self.z)?;
        } else {
            writer.write_f32::<LE>(self.x as f32)?;
            writer.write_f32::<LE>(self.y as f32)?;
            writer.write_f32::<LE>(self.z as f32)?;
        }
        Ok(())
    }
}
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Vector2D {
    pub x: f32,
    pub y: f32,
}
impl Vector2D {
    fn read<R: Read + Seek>(reader: &mut Context<R>) -> TResult<Self> {
        Ok(Self {
            x: reader.read_f32::<LE>()?,
            y: reader.read_f32::<LE>()?,
        })
    }
    fn write<W: Write>(&self, writer: &mut Context<W>) -> TResult<()> {
        writer.write_f32::<LE>(self.x)?;
        writer.write_f32::<LE>(self.y)?;
        Ok(())
    }
}
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Box {
    pub a: Vector,
    pub b: Vector,
}
impl Box {
    fn read<R: Read + Seek>(reader: &mut Context<R>) -> TResult<Self> {
        let a = Vector::read(reader)?;
        let b = Vector::read(reader)?;
        reader.read_u8()?;
        Ok(Self { a, b })
    }
    fn write<W: Write>(&self, writer: &mut Context<W>) -> TResult<()> {
        self.a.write(writer)?;
        self.b.write(writer)?;
        Ok(())
    }
}
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct IntPoint {
    pub x: i32,
    pub y: i32,
}
impl IntPoint {
    fn read<R: Read + Seek>(reader: &mut Context<R>) -> TResult<Self> {
        Ok(Self {
            x: reader.read_i32::<LE>()?,
            y: reader.read_i32::<LE>()?,
        })
    }
    fn write<W: Write>(&self, writer: &mut Context<W>) -> TResult<()> {
        writer.write_i32::<LE>(self.x)?;
        writer.write_i32::<LE>(self.y)?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct GameplayTag {
    pub name: String,
}
impl GameplayTag {
    fn read<R: Read + Seek>(reader: &mut Context<R>) -> TResult<Self> {
        Ok(Self {
            name: read_string(reader)?,
        })
    }
    fn write<W: Write>(&self, writer: &mut Context<W>) -> TResult<()> {
        write_string(writer, &self.name)?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct GameplayTagContainer {
    pub gameplay_tags: Vec<GameplayTag>,
}
impl GameplayTagContainer {
    fn read<R: Read + Seek>(reader: &mut Context<R>) -> TResult<Self> {
        Ok(Self {
            gameplay_tags: read_array(reader.read_u32::<LE>()?, reader, GameplayTag::read)?,
        })
    }
    fn write<W: Write>(&self, writer: &mut Context<W>) -> TResult<()> {
        writer.write_u32::<LE>(self.gameplay_tags.len() as u32)?;
        for entry in &self.gameplay_tags {
            entry.write(writer)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct FFormatArgumentData {
    name: String,
    value: FFormatArgumentDataValue,
}
impl<R: Read + Seek> Readable<R> for FFormatArgumentData {
    fn read(reader: &mut Context<R>) -> TResult<Self> {
        Ok(Self {
            name: read_string(reader)?,
            value: FFormatArgumentDataValue::read(reader)?,
        })
    }
}
impl<W: Write> Writable<W> for FFormatArgumentData {
    fn write(&self, writer: &mut Context<W>) -> TResult<()> {
        write_string(writer, &self.name)?;
        self.value.write(writer)?;
        Ok(())
    }
}
// very similar to FFormatArgumentValue but serializes ints as 32 bits (TODO changes to 64 bit
// again at some later UE version)
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum FFormatArgumentDataValue {
    Int(i32),
    UInt(u32),
    Float(f32),
    Double(f64),
    Text(std::boxed::Box<Text>),
    Gender(u64),
}
impl<R: Read + Seek> Readable<R> for FFormatArgumentDataValue {
    fn read(reader: &mut Context<R>) -> TResult<Self> {
        let type_ = reader.read_u8()?;
        match type_ {
            0 => Ok(Self::Int(reader.read_i32::<LE>()?)),
            1 => Ok(Self::UInt(reader.read_u32::<LE>()?)),
            2 => Ok(Self::Float(reader.read_f32::<LE>()?)),
            3 => Ok(Self::Double(reader.read_f64::<LE>()?)),
            4 => Ok(Self::Text(std::boxed::Box::new(Text::read(reader)?))),
            5 => Ok(Self::Gender(reader.read_u64::<LE>()?)),
            _ => Err(Error::Other(format!(
                "unimplemented variant for FFormatArgumentDataValue 0x{type_:x}"
            ))),
        }
    }
}
impl<W: Write> Writable<W> for FFormatArgumentDataValue {
    fn write(&self, writer: &mut Context<W>) -> TResult<()> {
        match self {
            Self::Int(value) => {
                writer.write_u8(0)?;
                writer.write_i32::<LE>(*value)?;
            }
            Self::UInt(value) => {
                writer.write_u8(1)?;
                writer.write_u32::<LE>(*value)?;
            }
            Self::Float(value) => {
                writer.write_u8(2)?;
                writer.write_f32::<LE>(*value)?;
            }
            Self::Double(value) => {
                writer.write_u8(3)?;
                writer.write_f64::<LE>(*value)?;
            }
            Self::Text(value) => {
                writer.write_u8(4)?;
                value.write(writer)?;
            }
            Self::Gender(value) => {
                writer.write_u8(5)?;
                writer.write_u64::<LE>(*value)?;
            }
        };
        Ok(())
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum FFormatArgumentValue {
    Int(i64),
    UInt(u64),
    Float(f32),
    Double(f64),
    Text(std::boxed::Box<Text>),
    Gender(u64),
}

impl<R: Read + Seek> Readable<R> for FFormatArgumentValue {
    fn read(reader: &mut Context<R>) -> TResult<Self> {
        let type_ = reader.read_u8()?;
        match type_ {
            0 => Ok(Self::Int(reader.read_i64::<LE>()?)),
            1 => Ok(Self::UInt(reader.read_u64::<LE>()?)),
            2 => Ok(Self::Float(reader.read_f32::<LE>()?)),
            3 => Ok(Self::Double(reader.read_f64::<LE>()?)),
            4 => Ok(Self::Text(std::boxed::Box::new(Text::read(reader)?))),
            5 => Ok(Self::Gender(reader.read_u64::<LE>()?)),
            _ => Err(Error::Other(format!(
                "unimplemented variant for FFormatArgumentValue 0x{type_:x}"
            ))),
        }
    }
}
impl<W: Write> Writable<W> for FFormatArgumentValue {
    fn write(&self, writer: &mut Context<W>) -> TResult<()> {
        match self {
            Self::Int(value) => {
                writer.write_u8(0)?;
                writer.write_i64::<LE>(*value)?;
            }
            Self::UInt(value) => {
                writer.write_u8(1)?;
                writer.write_u64::<LE>(*value)?;
            }
            Self::Float(value) => {
                writer.write_u8(2)?;
                writer.write_f32::<LE>(*value)?;
            }
            Self::Double(value) => {
                writer.write_u8(3)?;
                writer.write_f64::<LE>(*value)?;
            }
            Self::Text(value) => {
                writer.write_u8(4)?;
                value.write(writer)?;
            }
            Self::Gender(value) => {
                writer.write_u8(5)?;
                writer.write_u64::<LE>(*value)?;
            }
        };
        Ok(())
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct FNumberFormattingOptions {
    always_sign: bool,
    use_grouping: bool,
    rounding_mode: i8, // TODO enum ERoundingMode
    minimum_integral_digits: i32,
    maximum_integral_digits: i32,
    minimum_fractional_digits: i32,
    maximum_fractional_digits: i32,
}
impl<R: Read + Seek> Readable<R> for FNumberFormattingOptions {
    fn read(reader: &mut Context<R>) -> TResult<Self> {
        Ok(Self {
            always_sign: reader.read_u32::<LE>()? != 0,
            use_grouping: reader.read_u32::<LE>()? != 0,
            rounding_mode: reader.read_i8()?,
            minimum_integral_digits: reader.read_i32::<LE>()?,
            maximum_integral_digits: reader.read_i32::<LE>()?,
            minimum_fractional_digits: reader.read_i32::<LE>()?,
            maximum_fractional_digits: reader.read_i32::<LE>()?,
        })
    }
}
impl<W: Write> Writable<W> for FNumberFormattingOptions {
    fn write(&self, writer: &mut Context<W>) -> TResult<()> {
        writer.write_u32::<LE>(self.always_sign as u32)?;
        writer.write_u32::<LE>(self.use_grouping as u32)?;
        writer.write_i8(self.rounding_mode)?;
        writer.write_i32::<LE>(self.minimum_integral_digits)?;
        writer.write_i32::<LE>(self.maximum_integral_digits)?;
        writer.write_i32::<LE>(self.minimum_fractional_digits)?;
        writer.write_i32::<LE>(self.maximum_fractional_digits)?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Text {
    flags: u32,
    variant: TextVariant,
}
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum TextVariant {
    // -0x1
    None {
        culture_invariant: Option<String>,
    },
    // 0x0
    Base {
        namespace: String,
        key: String,
        source_string: String,
    },
    // 0x3
    ArgumentFormat {
        // aka ArgumentDataFormat
        format_text: std::boxed::Box<Text>,
        arguments: Vec<FFormatArgumentData>,
    },
    // 0x4
    AsNumber {
        source_value: FFormatArgumentValue,
        format_options: Option<FNumberFormattingOptions>,
        culture_name: String,
    },
    // 0x7
    AsDate {
        source_date_time: DateTime,
        date_style: i8, // TODO EDateTimeStyle::Type
        time_zone: String,
        culture_name: String,
    },
    StringTableEntry {
        // 0xb
        table: String,
        key: String,
    },
}

impl<R: Read + Seek> Readable<R> for Text {
    fn read(reader: &mut Context<R>) -> TResult<Self> {
        let flags = reader.read_u32::<LE>()?;
        let text_history_type = reader.read_i8()?;
        let variant = match text_history_type {
            -0x1 => Ok(TextVariant::None {
                culture_invariant: (reader.read_u32::<LE>()? != 0) // bHasCultureInvariantString
                    .then(|| read_string(reader))
                    .transpose()?,
            }),
            0x0 => Ok(TextVariant::Base {
                namespace: read_string(reader)?,
                key: read_string(reader)?,
                source_string: read_string(reader)?,
            }),
            0x3 => Ok(TextVariant::ArgumentFormat {
                format_text: std::boxed::Box::new(Text::read(reader)?),
                arguments: read_array(reader.read_u32::<LE>()?, reader, FFormatArgumentData::read)?,
            }),
            0x4 => Ok(TextVariant::AsNumber {
                source_value: FFormatArgumentValue::read(reader)?,
                format_options:
                    (reader.read_u32::<LE>()? != 0) // bHasFormatOptions
                        .then(|| FNumberFormattingOptions::read(reader))
                        .transpose()?,
                culture_name: read_string(reader)?,
            }),
            0x7 => Ok(TextVariant::AsDate {
                source_date_time: reader.read_u64::<LE>()?,
                date_style: reader.read_i8()?,
                time_zone: read_string(reader)?,
                culture_name: read_string(reader)?,
            }),
            0xb => Ok({
                TextVariant::StringTableEntry {
                    table: read_string(reader)?,
                    key: read_string(reader)?,
                }
            }),
            _ => Err(Error::Other(format!(
                "unimplemented variant for FTextHistory 0x{text_history_type:x}"
            ))),
        }?;
        Ok(Self { flags, variant })
    }
}
impl<W: Write> Writable<W> for Text {
    fn write(&self, writer: &mut Context<W>) -> TResult<()> {
        writer.write_u32::<LE>(self.flags)?;
        match &self.variant {
            TextVariant::None { culture_invariant } => {
                writer.write_i8(-0x1)?;
                writer.write_u32::<LE>(culture_invariant.is_some() as u32)?;
                if let Some(culture_invariant) = culture_invariant {
                    write_string(writer, culture_invariant)?;
                }
            }
            TextVariant::Base {
                namespace,
                key,
                source_string,
            } => {
                writer.write_i8(0x0)?;
                // TODO sometimes trailing sometimes not?
                write_string_always_trailing(writer, namespace)?;
                write_string(writer, key)?;
                write_string(writer, source_string)?;
            }
            TextVariant::ArgumentFormat {
                format_text,
                arguments,
            } => {
                writer.write_i8(0x3)?;
                format_text.write(writer)?;
                writer.write_u32::<LE>(arguments.len() as u32)?;
                for a in arguments {
                    a.write(writer)?;
                }
            }
            TextVariant::AsNumber {
                source_value,
                format_options,
                culture_name,
            } => {
                writer.write_i8(0x4)?;
                source_value.write(writer)?;
                writer.write_u32::<LE>(format_options.is_some() as u32)?;
                if let Some(format_options) = format_options {
                    format_options.write(writer)?;
                }
                write_string(writer, culture_name)?;
            }
            TextVariant::AsDate {
                source_date_time,
                date_style,
                time_zone,
                culture_name,
            } => {
                writer.write_i8(0x7)?;
                writer.write_u64::<LE>(*source_date_time)?;
                writer.write_i8(*date_style)?;
                write_string(writer, time_zone)?;
                write_string(writer, culture_name)?;
            }
            TextVariant::StringTableEntry { table, key } => {
                writer.write_i8(0xb)?;
                write_string(writer, table)?;
                write_string(writer, key)?;
            }
        }
        Ok(())
    }
}

/// Just a plain byte, or an enum in which case the variant will be a String
#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Byte {
    Byte(u8),
    Label(String),
}
/// Vectorized [`Byte`]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum ByteArray {
    Byte(Vec<u8>),
    Label(Vec<String>),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum PropertyValue {
    Base(PropertyValueBase),
    Struct(StructValue),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum PropertyValueBase {
    Int(Int),
    Int8(Int8),
    Int16(Int16),
    Int64(Int64),
    UInt16(UInt16),
    UInt32(UInt32),
    Float(Float),
    Double(Double),
    Bool(Bool),
    Byte(Byte),
    Enum(Enum),
    Name(String),
    Str(String),
    SoftObject(String, String),
    SoftObjectPath(String, String),
    Object(String),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum StructValue {
    Guid(uuid::Uuid),
    DateTime(DateTime),
    Timespan(Timespan),
    Vector2D(Vector2D),
    Vector(Vector),
    Box(Box),
    IntPoint(IntPoint),
    Quat(Quat),
    LinearColor(LinearColor),
    Color(Color),
    Rotator(Rotator),
    SoftObjectPath(String, String),
    GameplayTagContainer(GameplayTagContainer),
    /// User defined struct which is simply a list of properties
    Struct(Properties),
}

/// Vectorized properties to avoid storing the variant with each value
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum ValueVec {
    Int8(Vec<Int8>),
    Int16(Vec<Int16>),
    Int(Vec<Int>),
    Int64(Vec<Int64>),
    UInt8(Vec<UInt8>),
    UInt16(Vec<UInt16>),
    UInt32(Vec<UInt32>),
    UInt64(Vec<UInt64>),
    Float(Vec<Float>),
    Double(Vec<Double>),
    Bool(Vec<bool>),
    Byte(ByteArray),
    Enum(Vec<Enum>),
    Str(Vec<String>),
    Text(Vec<Text>),
    SoftObject(Vec<(String, String)>),
    Name(Vec<String>),
    Object(Vec<String>),
    Box(Vec<Box>),
}

/// Encapsulates [`ValueVec`] with a special handling of structs. See also: [`ValueSet`]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum ValueArray {
    Base(ValueVec),
    Struct(ValueArrayStruct),
}
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum ValueArrayStruct {
    Struct {
        _type: String,
        name: String,
        struct_type: StructType,
        id: uuid::Uuid,
        value: Vec<StructValue>,
    },
}
/// Encapsulates [`ValueVec`] with a special handling of structs. See also: [`ValueArray`]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum ValueSet {
    Base(ValueVec),
    Struct(Vec<StructValue>),
}

impl PropertyValue {
    fn read<R: Read + Seek>(
        reader: &mut Context<R>,
        t: &PropertyType,
        st: Option<&StructType>,
    ) -> TResult<PropertyValue> {
        Ok(match t {
            PropertyType::IntProperty => {
                PropertyValue::Base(PropertyValueBase::Int(reader.read_i32::<LE>()?))
            }
            PropertyType::Int8Property => {
                PropertyValue::Base(PropertyValueBase::Int8(reader.read_i8()?))
            }
            PropertyType::Int16Property => {
                PropertyValue::Base(PropertyValueBase::Int16(reader.read_i16::<LE>()?))
            }
            PropertyType::Int64Property => {
                PropertyValue::Base(PropertyValueBase::Int64(reader.read_i64::<LE>()?))
            }
            PropertyType::UInt16Property => {
                PropertyValue::Base(PropertyValueBase::UInt16(reader.read_u16::<LE>()?))
            }
            PropertyType::UInt32Property => {
                PropertyValue::Base(PropertyValueBase::UInt32(reader.read_u32::<LE>()?))
            }
            PropertyType::FloatProperty => {
                PropertyValue::Base(PropertyValueBase::Float(reader.read_f32::<LE>()?))
            }
            PropertyType::DoubleProperty => {
                PropertyValue::Base(PropertyValueBase::Double(reader.read_f64::<LE>()?))
            }
            PropertyType::BoolProperty => {
                PropertyValue::Base(PropertyValueBase::Bool(reader.read_u8()? > 0))
            }
            PropertyType::NameProperty => {
                PropertyValue::Base(PropertyValueBase::Name(read_string(reader)?))
            }
            PropertyType::StrProperty => {
                PropertyValue::Base(PropertyValueBase::Str(read_string(reader)?))
            }
            PropertyType::SoftObjectProperty => PropertyValue::Base(PropertyValueBase::SoftObject(
                read_string(reader)?,
                read_string(reader)?,
            )),
            PropertyType::ObjectProperty => {
                PropertyValue::Base(PropertyValueBase::Object(read_string(reader)?))
            }
            PropertyType::ByteProperty => {
                PropertyValue::Base(PropertyValueBase::Byte(Byte::Label(read_string(reader)?)))
            }
            PropertyType::EnumProperty => {
                PropertyValue::Base(PropertyValueBase::Enum(read_string(reader)?))
            }
            PropertyType::StructProperty => {
                PropertyValue::Struct(StructValue::read(reader, st.as_ref().unwrap())?)
            }
            _ => return Err(Error::Other(format!("unimplemented property {t:?}"))),
        })
    }
    fn write<W: Write>(&self, writer: &mut Context<W>) -> TResult<()> {
        match &self {
            PropertyValue::Base(PropertyValueBase::Int(v)) => writer.write_i32::<LE>(*v)?,
            PropertyValue::Base(PropertyValueBase::Int8(v)) => writer.write_i8(*v)?,
            PropertyValue::Base(PropertyValueBase::Int16(v)) => writer.write_i16::<LE>(*v)?,
            PropertyValue::Base(PropertyValueBase::Int64(v)) => writer.write_i64::<LE>(*v)?,
            PropertyValue::Base(PropertyValueBase::UInt16(v)) => writer.write_u16::<LE>(*v)?,
            PropertyValue::Base(PropertyValueBase::UInt32(v)) => writer.write_u32::<LE>(*v)?,
            PropertyValue::Base(PropertyValueBase::Float(v)) => writer.write_f32::<LE>(*v)?,
            PropertyValue::Base(PropertyValueBase::Double(v)) => writer.write_f64::<LE>(*v)?,
            PropertyValue::Base(PropertyValueBase::Bool(v)) => writer.write_u8(u8::from(*v))?,
            PropertyValue::Base(PropertyValueBase::Name(v)) => write_string(writer, v)?,
            PropertyValue::Base(PropertyValueBase::Str(v)) => write_string(writer, v)?,
            PropertyValue::Base(PropertyValueBase::SoftObject(a, b)) => {
                write_string(writer, a)?;
                write_string(writer, b)?;
            }
            PropertyValue::Base(PropertyValueBase::SoftObjectPath(a, b)) => {
                write_string(writer, a)?;
                write_string(writer, b)?;
            }
            PropertyValue::Base(PropertyValueBase::Object(v)) => write_string(writer, v)?,
            PropertyValue::Base(PropertyValueBase::Byte(v)) => match v {
                Byte::Byte(b) => writer.write_u8(*b)?,
                Byte::Label(l) => write_string(writer, l)?,
            },
            PropertyValue::Base(PropertyValueBase::Enum(v)) => write_string(writer, v)?,
            PropertyValue::Struct(v) => v.write(writer)?,
        };
        Ok(())
    }
}
impl StructValue {
    fn read<R: Read + Seek>(reader: &mut Context<R>, t: &StructType) -> TResult<StructValue> {
        Ok(match t {
            StructType::Guid => StructValue::Guid(uuid::Uuid::read(reader)?),
            StructType::DateTime => StructValue::DateTime(reader.read_u64::<LE>()?),
            StructType::Timespan => StructValue::Timespan(reader.read_i64::<LE>()?),
            StructType::Vector2D => StructValue::Vector2D(Vector2D::read(reader)?),
            StructType::Vector => StructValue::Vector(Vector::read(reader)?),
            StructType::Box => StructValue::Box(Box::read(reader)?),
            StructType::IntPoint => StructValue::IntPoint(IntPoint::read(reader)?),
            StructType::Quat => StructValue::Quat(Quat::read(reader)?),
            StructType::LinearColor => StructValue::LinearColor(LinearColor::read(reader)?),
            StructType::Color => StructValue::Color(Color::read(reader)?),
            StructType::Rotator => StructValue::Rotator(Rotator::read(reader)?),
            StructType::SoftObjectPath => {
                StructValue::SoftObjectPath(read_string(reader)?, read_string(reader)?)
            }
            StructType::GameplayTagContainer => {
                StructValue::GameplayTagContainer(GameplayTagContainer::read(reader)?)
            }

            StructType::Struct(_) => StructValue::Struct(read_properties_until_none(reader)?),
        })
    }
    fn write<W: Write>(&self, writer: &mut Context<W>) -> TResult<()> {
        match self {
            StructValue::Guid(v) => v.write(writer)?,
            StructValue::DateTime(v) => writer.write_u64::<LE>(*v)?,
            StructValue::Timespan(v) => writer.write_i64::<LE>(*v)?,
            StructValue::Vector2D(v) => v.write(writer)?,
            StructValue::Vector(v) => v.write(writer)?,
            StructValue::Box(v) => v.write(writer)?,
            StructValue::IntPoint(v) => v.write(writer)?,
            StructValue::Quat(v) => v.write(writer)?,
            StructValue::LinearColor(v) => v.write(writer)?,
            StructValue::Color(v) => v.write(writer)?,
            StructValue::Rotator(v) => v.write(writer)?,
            StructValue::SoftObjectPath(a, b) => {
                write_string(writer, a)?;
                write_string(writer, b)?;
            }
            StructValue::GameplayTagContainer(v) => v.write(writer)?,
            StructValue::Struct(v) => write_properties_none_terminated(writer, v)?,
        }
        Ok(())
    }
}
impl ValueVec {
    fn read<R: Read + Seek>(
        reader: &mut Context<R>,
        t: &PropertyType,
        size: u64,
        count: u32,
    ) -> TResult<ValueVec> {
        Ok(match t {
            PropertyType::IntProperty => {
                ValueVec::Int(read_array(count, reader, |r| Ok(r.read_i32::<LE>()?))?)
            }
            PropertyType::Int16Property => {
                ValueVec::Int16(read_array(count, reader, |r| Ok(r.read_i16::<LE>()?))?)
            }
            PropertyType::Int64Property => {
                ValueVec::Int64(read_array(count, reader, |r| Ok(r.read_i64::<LE>()?))?)
            }
            PropertyType::UInt16Property => {
                ValueVec::UInt16(read_array(count, reader, |r| Ok(r.read_u16::<LE>()?))?)
            }
            PropertyType::UInt32Property => {
                ValueVec::UInt32(read_array(count, reader, |r| Ok(r.read_u32::<LE>()?))?)
            }
            PropertyType::FloatProperty => {
                ValueVec::Float(read_array(count, reader, |r| Ok(r.read_f32::<LE>()?))?)
            }
            PropertyType::DoubleProperty => {
                ValueVec::Double(read_array(count, reader, |r| Ok(r.read_f64::<LE>()?))?)
            }
            PropertyType::BoolProperty => {
                ValueVec::Bool(read_array(count, reader, |r| Ok(r.read_u8()? > 0))?)
            }
            PropertyType::ByteProperty => {
                if size == count.into() {
                    ValueVec::Byte(ByteArray::Byte(read_array(count, reader, |r| {
                        Ok(r.read_u8()?)
                    })?))
                } else {
                    ValueVec::Byte(ByteArray::Label(read_array(count, reader, |r| {
                        read_string(r)
                    })?))
                }
            }
            PropertyType::EnumProperty => {
                ValueVec::Enum(read_array(count, reader, |r| read_string(r))?)
            }
            PropertyType::StrProperty => ValueVec::Str(read_array(count, reader, read_string)?),
            PropertyType::TextProperty => ValueVec::Text(read_array(count, reader, Text::read)?),
            PropertyType::SoftObjectProperty => {
                ValueVec::SoftObject(read_array(count, reader, |r| {
                    Ok((read_string(r)?, read_string(r)?))
                })?)
            }
            PropertyType::NameProperty => ValueVec::Name(read_array(count, reader, read_string)?),
            PropertyType::ObjectProperty => {
                ValueVec::Object(read_array(count, reader, read_string)?)
            }
            _ => return Err(Error::UnknownVecType(format!("{t:?}"))),
        })
    }
    fn write<W: Write>(&self, writer: &mut Context<W>) -> TResult<()> {
        match &self {
            ValueVec::Int8(v) => {
                writer.write_u32::<LE>(v.len() as u32)?;
                for i in v {
                    writer.write_i8(*i)?;
                }
            }
            ValueVec::Int16(v) => {
                writer.write_u32::<LE>(v.len() as u32)?;
                for i in v {
                    writer.write_i16::<LE>(*i)?;
                }
            }
            ValueVec::Int(v) => {
                writer.write_u32::<LE>(v.len() as u32)?;
                for i in v {
                    writer.write_i32::<LE>(*i)?;
                }
            }
            ValueVec::Int64(v) => {
                writer.write_u32::<LE>(v.len() as u32)?;
                for i in v {
                    writer.write_i64::<LE>(*i)?;
                }
            }
            ValueVec::UInt8(v) => {
                writer.write_u32::<LE>(v.len() as u32)?;
                for i in v {
                    writer.write_u8(*i)?;
                }
            }
            ValueVec::UInt16(v) => {
                writer.write_u32::<LE>(v.len() as u32)?;
                for i in v {
                    writer.write_u16::<LE>(*i)?;
                }
            }
            ValueVec::UInt32(v) => {
                writer.write_u32::<LE>(v.len() as u32)?;
                for i in v {
                    writer.write_u32::<LE>(*i)?;
                }
            }
            ValueVec::UInt64(v) => {
                writer.write_u32::<LE>(v.len() as u32)?;
                for i in v {
                    writer.write_u64::<LE>(*i)?;
                }
            }
            ValueVec::Float(v) => {
                writer.write_u32::<LE>(v.len() as u32)?;
                for i in v {
                    writer.write_f32::<LE>(*i)?;
                }
            }
            ValueVec::Double(v) => {
                writer.write_u32::<LE>(v.len() as u32)?;
                for i in v {
                    writer.write_f64::<LE>(*i)?;
                }
            }
            ValueVec::Bool(v) => {
                writer.write_u32::<LE>(v.len() as u32)?;
                for b in v {
                    writer.write_u8(*b as u8)?;
                }
            }
            ValueVec::Byte(v) => match v {
                ByteArray::Byte(b) => {
                    writer.write_u32::<LE>(b.len() as u32)?;
                    for b in b {
                        writer.write_u8(*b)?;
                    }
                }
                ByteArray::Label(l) => {
                    writer.write_u32::<LE>(l.len() as u32)?;
                    for l in l {
                        write_string(writer, l)?;
                    }
                }
            },
            ValueVec::Enum(v) => {
                writer.write_u32::<LE>(v.len() as u32)?;
                for i in v {
                    write_string(writer, i)?;
                }
            }
            ValueVec::Str(v) | ValueVec::Object(v) | ValueVec::Name(v) => {
                writer.write_u32::<LE>(v.len() as u32)?;
                for i in v {
                    write_string(writer, i)?;
                }
            }
            ValueVec::Text(v) => {
                writer.write_u32::<LE>(v.len() as u32)?;
                for i in v {
                    i.write(writer)?;
                }
            }
            ValueVec::SoftObject(v) => {
                writer.write_u32::<LE>(v.len() as u32)?;
                for (a, b) in v {
                    write_string(writer, a)?;
                    write_string(writer, b)?;
                }
            }
            ValueVec::Box(v) => {
                writer.write_u32::<LE>(v.len() as u32)?;
                for i in v {
                    i.write(writer)?;
                }
            }
        }
        Ok(())
    }
}
impl ValueArray {
    fn read<R: Read + Seek>(
        reader: &mut Context<R>,
        t: &PropertyType,
        size: u64,
    ) -> TResult<ValueArray> {
        let count = reader.read_u32::<LE>()?;
        Ok(match t {
            PropertyType::StructProperty => {
                let _type = read_string(reader)?;
                let name = read_string(reader)?;
                let _size = reader.read_u64::<LE>()?;
                let struct_type = StructType::read(reader)?;
                let id = uuid::Uuid::read(reader)?;
                reader.read_u8()?;
                let mut value = vec![];
                for _ in 0..count {
                    value.push(StructValue::read(reader, &struct_type)?);
                }
                ValueArray::Struct(ValueArrayStruct::Struct {
                    _type,
                    name,
                    struct_type,
                    id,
                    value,
                })
            }
            _ => ValueArray::Base(ValueVec::read(reader, t, size, count)?),
        })
    }
    fn write<W: Write>(&self, writer: &mut Context<W>) -> TResult<()> {
        match &self {
            ValueArray::Struct(ValueArrayStruct::Struct {
                _type,
                name,
                struct_type,
                id,
                value,
            }) => {
                writer.write_u32::<LE>(value.len() as u32)?;
                write_string(writer, _type)?;
                write_string(writer, name)?;
                let mut buf = vec![];
                for v in value {
                    writer.stream(&mut buf, |writer| v.write(writer))?;
                }
                writer.write_u64::<LE>(buf.len() as u64)?;
                struct_type.write(writer)?;
                id.write(writer)?;
                writer.write_u8(0)?;
                writer.write_all(&buf)?;
            }
            ValueArray::Base(vec) => {
                vec.write(writer)?;
            }
        }
        Ok(())
    }
}
impl ValueSet {
    fn read<R: Read + Seek>(
        reader: &mut Context<R>,
        t: &PropertyType,
        st: Option<&StructType>,
        size: u64,
    ) -> TResult<ValueSet> {
        let count = reader.read_u32::<LE>()?;
        Ok(match t {
            PropertyType::StructProperty => ValueSet::Struct(read_array(count, reader, |r| {
                StructValue::read(r, st.unwrap())
            })?),
            _ => ValueSet::Base(ValueVec::read(reader, t, size, count)?),
        })
    }
    fn write<W: Write>(&self, writer: &mut Context<W>) -> TResult<()> {
        match &self {
            ValueSet::Struct(value) => {
                writer.write_u32::<LE>(value.len() as u32)?;
                for v in value {
                    v.write(writer)?;
                }
            }
            ValueSet::Base(vec) => {
                vec.write(writer)?;
            }
        }
        Ok(())
    }
}

/// Properties consist of an ID and a value and are present in [`Root`] and [`StructValue::Struct`]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Property {
    #[serde(skip_serializing_if = "Option::is_none")]
    id: Option<uuid::Uuid>,
    #[serde(flatten)]
    inner: PropertyInner,
}

/// Properties consist of an ID and a value and are present in [`Root`] and [`StructValue::Struct`]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum PropertyInner {
    Int8(Int8),
    Int16(Int16),
    Int(Int),
    Int64(Int64),
    UInt8(UInt8),
    UInt16(UInt16),
    UInt32(UInt32),
    UInt64(UInt64),
    Float(Float),
    Double(Double),
    Bool(Bool),
    Byte {
        value: Byte,
        enum_type: String,
    },
    Enum {
        value: Enum,
        enum_type: String,
    },
    Str(String),
    FieldPath(FieldPath),
    SoftObject {
        value: String,
        value2: String,
    },
    Name(String),
    Object(String),
    Text(Text),
    Delegate(Delegate),
    MulticastDelegate(MulticastDelegate),
    MulticastInlineDelegate(MulticastInlineDelegate),
    MulticastSparseDelegate(MulticastSparseDelegate),
    Set {
        set_type: PropertyType,
        value: ValueSet,
    },
    Map {
        key_type: PropertyType,
        value_type: PropertyType,
        value: Vec<MapEntry>,
    },
    Struct {
        struct_type: StructType,
        #[serde(flatten)]
        value: StructValue,
        #[serde(
            default = "uuid::Uuid::nil",
            skip_serializing_if = "uuid::Uuid::is_nil"
        )]
        struct_id: uuid::Uuid,
    },
    RawData {
        properties: Properties,
        struct_id: uuid::Uuid,
    },
    Array {
        array_type: PropertyType,
        value: ValueArray,
    },
}

impl Property {
    fn get_type(&self) -> PropertyType {
        match &self.inner {
            PropertyInner::Int8 { .. } => PropertyType::Int8Property,
            PropertyInner::Int16 { .. } => PropertyType::Int16Property,
            PropertyInner::Int { .. } => PropertyType::IntProperty,
            PropertyInner::Int64 { .. } => PropertyType::Int64Property,
            PropertyInner::UInt8 { .. } => PropertyType::UInt8Property,
            PropertyInner::UInt16 { .. } => PropertyType::UInt16Property,
            PropertyInner::UInt32 { .. } => PropertyType::UInt32Property,
            PropertyInner::UInt64 { .. } => PropertyType::UInt64Property,
            PropertyInner::Float { .. } => PropertyType::FloatProperty,
            PropertyInner::Double { .. } => PropertyType::DoubleProperty,
            PropertyInner::Bool { .. } => PropertyType::BoolProperty,
            PropertyInner::Byte { .. } => PropertyType::ByteProperty,
            PropertyInner::Enum { .. } => PropertyType::EnumProperty,
            PropertyInner::Name { .. } => PropertyType::NameProperty,
            PropertyInner::Str { .. } => PropertyType::StrProperty,
            PropertyInner::FieldPath { .. } => PropertyType::FieldPathProperty,
            PropertyInner::SoftObject { .. } => PropertyType::SoftObjectProperty,
            PropertyInner::Object { .. } => PropertyType::ObjectProperty,
            PropertyInner::Text { .. } => PropertyType::TextProperty,
            PropertyInner::Delegate { .. } => PropertyType::DelegateProperty,
            PropertyInner::MulticastDelegate { .. } => PropertyType::MulticastDelegateProperty,
            PropertyInner::MulticastInlineDelegate { .. } => {
                PropertyType::MulticastInlineDelegateProperty
            }
            PropertyInner::MulticastSparseDelegate { .. } => {
                PropertyType::MulticastSparseDelegateProperty
            }
            PropertyInner::Set { .. } => PropertyType::SetProperty,
            PropertyInner::Map { .. } => PropertyType::MapProperty,
            PropertyInner::Struct { .. } => PropertyType::StructProperty,
            PropertyInner::RawData { .. } => PropertyType::ArrayProperty,
            PropertyInner::Array { .. } => PropertyType::ArrayProperty,
        }
    }
    fn read<R: Read + Seek>(
        reader: &mut Context<R>,
        t: PropertyType,
        size: u64,
    ) -> TResult<Property> {
        match t {
            PropertyType::Int8Property => Ok(Property {
                id: read_optional_uuid(reader)?,
                inner: PropertyInner::Int8(reader.read_i8()?),
            }),
            PropertyType::Int16Property => Ok(Property {
                id: read_optional_uuid(reader)?,
                inner: PropertyInner::Int16(reader.read_i16::<LE>()?),
            }),
            PropertyType::IntProperty => Ok(Property {
                id: read_optional_uuid(reader)?,
                inner: PropertyInner::Int(reader.read_i32::<LE>()?),
            }),
            PropertyType::Int64Property => Ok(Property {
                id: read_optional_uuid(reader)?,
                inner: PropertyInner::Int64(reader.read_i64::<LE>()?),
            }),
            PropertyType::UInt8Property => Ok(Property {
                id: read_optional_uuid(reader)?,
                inner: PropertyInner::UInt8(reader.read_u8()?),
            }),
            PropertyType::UInt16Property => Ok(Property {
                id: read_optional_uuid(reader)?,
                inner: PropertyInner::UInt16(reader.read_u16::<LE>()?),
            }),
            PropertyType::UInt32Property => Ok(Property {
                id: read_optional_uuid(reader)?,
                inner: PropertyInner::UInt32(reader.read_u32::<LE>()?),
            }),
            PropertyType::UInt64Property => Ok(Property {
                id: read_optional_uuid(reader)?,
                inner: PropertyInner::UInt64(reader.read_u64::<LE>()?),
            }),
            PropertyType::FloatProperty => Ok(Property {
                id: read_optional_uuid(reader)?,
                inner: PropertyInner::Float(reader.read_f32::<LE>()?),
            }),
            PropertyType::DoubleProperty => Ok(Property {
                id: read_optional_uuid(reader)?,
                inner: PropertyInner::Double(reader.read_f64::<LE>()?),
            }),
            PropertyType::BoolProperty => Ok(Property {
                inner: PropertyInner::Bool(reader.read_u8()? > 0),
                id: read_optional_uuid(reader)?,
            }),
            PropertyType::ByteProperty => Ok({
                let enum_type = read_string(reader)?;
                let id = read_optional_uuid(reader)?;
                let value = if enum_type == "None" {
                    Byte::Byte(reader.read_u8()?)
                } else {
                    Byte::Label(read_string(reader)?)
                };
                Property {
                    id,
                    inner: PropertyInner::Byte { enum_type, value },
                }
            }),
            PropertyType::EnumProperty => {
                let enum_type = read_string(reader)?;
                let id = read_optional_uuid(reader)?;
                let value = read_string(reader)?;
                Ok(Property {
                    id,
                    inner: PropertyInner::Enum { enum_type, value },
                })
            }
            PropertyType::NameProperty => Ok(Property {
                id: read_optional_uuid(reader)?,
                inner: PropertyInner::Name(read_string(reader)?),
            }),
            PropertyType::StrProperty => Ok(Property {
                id: read_optional_uuid(reader)?,
                inner: PropertyInner::Str(read_string(reader)?),
            }),
            PropertyType::FieldPathProperty => Ok(Property {
                id: read_optional_uuid(reader)?,
                inner: PropertyInner::FieldPath(FieldPath::read(reader)?),
            }),
            PropertyType::SoftObjectProperty => Ok(Property {
                id: read_optional_uuid(reader)?,
                inner: PropertyInner::SoftObject {
                    value: read_string(reader)?,
                    value2: read_string(reader)?,
                },
            }),
            PropertyType::ObjectProperty => Ok(Property {
                id: read_optional_uuid(reader)?,
                inner: PropertyInner::Object(read_string(reader)?),
            }),
            PropertyType::TextProperty => Ok(Property {
                id: read_optional_uuid(reader)?,
                inner: PropertyInner::Text(Text::read(reader)?),
            }),
            PropertyType::DelegateProperty => Ok(Property {
                id: read_optional_uuid(reader)?,
                inner: PropertyInner::Delegate(Delegate::read(reader)?),
            }),
            PropertyType::MulticastDelegateProperty => Ok(Property {
                id: read_optional_uuid(reader)?,
                inner: PropertyInner::MulticastDelegate(MulticastDelegate::read(reader)?),
            }),
            PropertyType::MulticastInlineDelegateProperty => Ok(Property {
                id: read_optional_uuid(reader)?,
                inner: PropertyInner::MulticastInlineDelegate(MulticastInlineDelegate::read(
                    reader,
                )?),
            }),
            PropertyType::MulticastSparseDelegateProperty => Ok(Property {
                id: read_optional_uuid(reader)?,
                inner: PropertyInner::MulticastSparseDelegate(MulticastSparseDelegate::read(
                    reader,
                )?),
            }),
            PropertyType::SetProperty => {
                let set_type = PropertyType::read(reader)?;
                let id = read_optional_uuid(reader)?;
                reader.read_u32::<LE>()?;
                let struct_type = match set_type {
                    PropertyType::StructProperty => Some(reader.get_type_or(&StructType::Guid)?),
                    _ => None,
                };
                let value = ValueSet::read(reader, &set_type, struct_type, size - 8)?;
                Ok(Property {
                    id,
                    inner: PropertyInner::Set { set_type, value },
                })
            }
            PropertyType::MapProperty => {
                let key_type = PropertyType::read(reader)?;
                let value_type = PropertyType::read(reader)?;
                let id = read_optional_uuid(reader)?;
                reader.read_u32::<LE>()?;
                let count = reader.read_u32::<LE>()?;
                let mut value = vec![];

                let key_struct_type = match key_type {
                    PropertyType::StructProperty => {
                        Some(reader.scope("Key", |r| r.get_type_or(&StructType::Guid))?)
                    }
                    _ => None,
                };
                let value_struct_type = match value_type {
                    PropertyType::StructProperty => {
                        Some(reader.scope("Value", |r| r.get_type_or(&StructType::Struct(None)))?)
                    }
                    _ => None,
                };

                for _ in 0..count {
                    value.push(MapEntry::read(
                        reader,
                        &key_type,
                        key_struct_type,
                        &value_type,
                        value_struct_type,
                    )?)
                }

                Ok(Property {
                    id,
                    inner: PropertyInner::Map {
                        key_type,
                        value_type,
                        value,
                    },
                })
            }
            PropertyType::StructProperty => {
                let struct_type = StructType::read(reader)?;
                let struct_id = uuid::Uuid::read(reader)?;
                let id = read_optional_uuid(reader)?;
                let value = StructValue::read(reader, &struct_type)?;
                Ok(Property {
                    id,
                    inner: PropertyInner::Struct {
                        struct_type,
                        struct_id,
                        value,
                    },
                })
            }
            PropertyType::ArrayProperty => {
                let array_type = PropertyType::read(reader)?;
                let id = read_optional_uuid(reader)?;
                let value = ValueArray::read(reader, &array_type, size - 4)?;

                Ok(Property {
                    id,
                    inner: PropertyInner::Array { array_type, value },
                })
            }
        }
    }
    fn write<W: Write>(&self, writer: &mut Context<W>) -> TResult<usize> {
        Ok(match self {
            Property {
                id,
                inner: PropertyInner::Int8(value),
            } => {
                write_optional_uuid(writer, *id)?;
                writer.write_i8(*value)?;
                1
            }
            Property {
                id,
                inner: PropertyInner::Int16(value),
            } => {
                write_optional_uuid(writer, *id)?;
                writer.write_i16::<LE>(*value)?;
                2
            }
            Property {
                id,
                inner: PropertyInner::Int(value),
            } => {
                write_optional_uuid(writer, *id)?;
                writer.write_i32::<LE>(*value)?;
                4
            }
            Property {
                id,
                inner: PropertyInner::Int64(value),
            } => {
                write_optional_uuid(writer, *id)?;
                writer.write_i64::<LE>(*value)?;
                8
            }
            Property {
                id,
                inner: PropertyInner::UInt8(value),
            } => {
                write_optional_uuid(writer, *id)?;
                writer.write_u8(*value)?;
                1
            }
            Property {
                id,
                inner: PropertyInner::UInt16(value),
            } => {
                write_optional_uuid(writer, *id)?;
                writer.write_u16::<LE>(*value)?;
                2
            }
            Property {
                id,
                inner: PropertyInner::UInt32(value),
            } => {
                write_optional_uuid(writer, *id)?;
                writer.write_u32::<LE>(*value)?;
                4
            }
            Property {
                id,
                inner: PropertyInner::UInt64(value),
            } => {
                write_optional_uuid(writer, *id)?;
                writer.write_u64::<LE>(*value)?;
                8
            }
            Property {
                id,
                inner: PropertyInner::Float(value),
            } => {
                write_optional_uuid(writer, *id)?;
                writer.write_f32::<LE>(*value)?;
                4
            }
            Property {
                id,
                inner: PropertyInner::Double(value),
            } => {
                write_optional_uuid(writer, *id)?;
                writer.write_f64::<LE>(*value)?;
                8
            }
            Property {
                id,
                inner: PropertyInner::Bool(value),
            } => {
                writer.write_u8(u8::from(*value))?;
                write_optional_uuid(writer, *id)?;
                0
            }
            Property {
                id,
                inner: PropertyInner::Byte { enum_type, value },
            } => {
                write_string(writer, enum_type)?;
                write_optional_uuid(writer, *id)?;
                match value {
                    Byte::Byte(b) => {
                        writer.write_u8(*b)?;
                        1
                    }
                    Byte::Label(l) => {
                        write_string(writer, l)?;
                        l.len() + 5
                    }
                }
            }
            Property {
                id,
                inner: PropertyInner::Enum { enum_type, value },
            } => {
                write_string(writer, enum_type)?;
                write_optional_uuid(writer, *id)?;
                write_string(writer, value)?;
                value.len() + 5
            }
            Property {
                id,
                inner: PropertyInner::Name(value),
            } => {
                write_optional_uuid(writer, *id)?;
                let mut buf = vec![];
                writer.stream(&mut buf, |writer| write_string(writer, value))?;
                let size = buf.len();
                writer.write_all(&buf)?;
                size
            }
            Property {
                id,
                inner: PropertyInner::Str(value),
            } => {
                write_optional_uuid(writer, *id)?;
                let mut buf = vec![];
                writer.stream(&mut buf, |writer| write_string(writer, value))?;
                let size = buf.len();
                writer.write_all(&buf)?;
                size
            }
            Property {
                id,
                inner: PropertyInner::FieldPath(value),
            } => {
                write_optional_uuid(writer, *id)?;
                let mut buf = vec![];
                writer.stream(&mut buf, |writer| value.write(writer))?;
                let size = buf.len();
                writer.write_all(&buf)?;
                size
            }
            Property {
                id,
                inner: PropertyInner::SoftObject { value, value2 },
            } => {
                write_optional_uuid(writer, *id)?;
                let mut buf = vec![];
                writer.stream(&mut buf, |writer| write_string(writer, value))?;
                writer.stream(&mut buf, |writer| write_string(writer, value2))?;
                let size = buf.len();
                writer.write_all(&buf)?;
                size
            }
            Property {
                id,
                inner: PropertyInner::Object(value),
            } => {
                write_optional_uuid(writer, *id)?;
                let mut buf = vec![];
                writer.stream(&mut buf, |writer| write_string(writer, value))?;
                let size = buf.len();
                writer.write_all(&buf)?;
                size
            }
            Property {
                id,
                inner: PropertyInner::Text(value),
            } => {
                write_optional_uuid(writer, *id)?;
                let mut buf = vec![];
                writer.stream(&mut buf, |writer| value.write(writer))?;
                let size = buf.len();
                writer.write_all(&buf)?;
                size
            }
            Property {
                id,
                inner: PropertyInner::Delegate(value),
            } => {
                write_optional_uuid(writer, *id)?;
                let mut buf = vec![];
                writer.stream(&mut buf, |writer| value.write(writer))?;
                let size = buf.len();
                writer.write_all(&buf)?;
                size
            }
            Property {
                id,
                inner: PropertyInner::MulticastDelegate(value),
            } => {
                write_optional_uuid(writer, *id)?;
                let mut buf = vec![];
                writer.stream(&mut buf, |writer| value.write(writer))?;
                let size = buf.len();
                writer.write_all(&buf)?;
                size
            }
            Property {
                id,
                inner: PropertyInner::MulticastInlineDelegate(value),
            } => {
                write_optional_uuid(writer, *id)?;
                let mut buf = vec![];
                writer.stream(&mut buf, |writer| value.write(writer))?;
                let size = buf.len();
                writer.write_all(&buf)?;
                size
            }
            Property {
                id,
                inner: PropertyInner::MulticastSparseDelegate(value),
            } => {
                write_optional_uuid(writer, *id)?;
                let mut buf = vec![];
                writer.stream(&mut buf, |writer| value.write(writer))?;
                let size = buf.len();
                writer.write_all(&buf)?;
                size
            }
            Property {
                id,
                inner: PropertyInner::Set { set_type, value },
            } => {
                set_type.write(writer)?;
                write_optional_uuid(writer, *id)?;
                let mut buf = vec![];
                buf.write_u32::<LE>(0)?;
                writer.stream(&mut buf, |writer| value.write(writer))?;
                let size = buf.len();
                writer.write_all(&buf)?;
                size
            }
            Property {
                id,
                inner:
                    PropertyInner::Map {
                        key_type,
                        value_type,
                        value,
                    },
            } => {
                key_type.write(writer)?;
                value_type.write(writer)?;
                write_optional_uuid(writer, *id)?;
                let mut buf = vec![];
                buf.write_u32::<LE>(0)?;
                buf.write_u32::<LE>(value.len() as u32)?;
                for v in value {
                    writer.stream(&mut buf, |writer| v.write(writer))?;
                }
                let size = buf.len();
                writer.write_all(&buf)?;
                size
            }
            Property {
                id,
                inner:
                    PropertyInner::RawData {
                        struct_id,
                        properties,
                    },
            } => {
                let mut buf = vec![];
                let mut temp_buf = Cursor::new(&mut buf);
                let mut temp_writer = Context::<'_, '_, '_, '_, Cursor<&mut Vec<u8>>> {
                    stream: &mut temp_buf,
                    header: writer.header,
                    types: writer.types,
                    scope: writer.scope,
                };
                // new Property::Array of ByteArray::Byte
                for prop in properties {
                    write_property(prop, &mut temp_writer)?;
                }
                // write "None"
                write_string(&mut temp_writer, "None")?;
                temp_writer.write_u32::<LE>(0)?;
                struct_id.write(&mut temp_writer)?;

                let value = ValueArray::Base(ValueVec::Byte(ByteArray::Byte(buf)));
                PropertyType::ByteProperty.write(writer)?;
                write_optional_uuid(writer, *id)?;

                let mut buf = vec![];
                writer.stream(&mut buf, |writer| value.write(writer))?;
                let size = buf.len();
                writer.write_all(&buf)?;
                size
            }
            Property {
                id,
                inner:
                    PropertyInner::Struct {
                        struct_type,
                        struct_id,
                        value,
                    },
            } => {
                struct_type.write(writer)?;
                struct_id.write(writer)?;
                write_optional_uuid(writer, *id)?;
                let mut buf = vec![];
                writer.stream(&mut buf, |writer| value.write(writer))?;
                let size = buf.len();
                writer.write_all(&buf)?;
                size
            }
            Property {
                id,
                inner: PropertyInner::Array { array_type, value },
            } => {
                array_type.write(writer)?;
                write_optional_uuid(writer, *id)?;
                let mut buf = vec![];
                writer.stream(&mut buf, |writer| value.write(writer))?;
                let size = buf.len();
                writer.write_all(&buf)?;
                size
            }
        })
    }
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CustomFormatData {
    pub id: uuid::Uuid,
    pub value: i32,
}
impl<R: Read + Seek> Readable<R> for CustomFormatData {
    fn read(reader: &mut Context<R>) -> TResult<Self> {
        Ok(CustomFormatData {
            id: uuid::Uuid::read(reader)?,
            value: reader.read_i32::<LE>()?,
        })
    }
}
impl<W: Write> Writable<W> for CustomFormatData {
    fn write(&self, writer: &mut Context<W>) -> TResult<()> {
        self.id.write(writer)?;
        writer.write_i32::<LE>(self.value)?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum PackageVersion {
    Old(u32),
    New(u32, u32),
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Header {
    pub magic: u32,
    pub save_game_version: u32,
    pub package_version: PackageVersion,
    pub engine_version_major: u16,
    pub engine_version_minor: u16,
    pub engine_version_patch: u16,
    pub engine_version_build: u32,
    pub engine_version: String,
    pub custom_format_version: u32,
    pub custom_format: Vec<CustomFormatData>,
}
impl Header {
    fn large_world_coordinates(&self) -> bool {
        self.engine_version_major >= 5
    }
}
impl<R: Read + Seek> Readable<R> for Header {
    fn read(reader: &mut Context<R>) -> TResult<Self> {
        let magic = reader.read_u32::<LE>()?;
        if magic != u32::from_le_bytes(*b"GVAS") {
            eprintln!(
                "Found non-standard magic: {:02x?} ({}) expected: GVAS, continuing to parse...",
                &magic.to_le_bytes(),
                String::from_utf8_lossy(&magic.to_le_bytes())
            );
        }
        let save_game_version = reader.read_u32::<LE>()?;
        let package_version = if save_game_version < 3 {
            PackageVersion::Old(reader.read_u32::<LE>()?)
        } else {
            PackageVersion::New(reader.read_u32::<LE>()?, reader.read_u32::<LE>()?)
        };
        Ok(Header {
            magic,
            save_game_version,
            package_version,
            engine_version_major: reader.read_u16::<LE>()?,
            engine_version_minor: reader.read_u16::<LE>()?,
            engine_version_patch: reader.read_u16::<LE>()?,
            engine_version_build: reader.read_u32::<LE>()?,
            engine_version: read_string(reader)?,
            custom_format_version: reader.read_u32::<LE>()?,
            custom_format: read_array(reader.read_u32::<LE>()?, reader, CustomFormatData::read)?,
        })
    }
}
impl<W: Write> Writable<W> for Header {
    fn write(&self, writer: &mut Context<W>) -> TResult<()> {
        writer.write_u32::<LE>(self.magic)?;
        writer.write_u32::<LE>(self.save_game_version)?;
        match self.package_version {
            PackageVersion::Old(a) => {
                writer.write_u32::<LE>(a)?;
            }
            PackageVersion::New(a, b) => {
                writer.write_u32::<LE>(a)?;
                writer.write_u32::<LE>(b)?;
            }
        }
        writer.write_u16::<LE>(self.engine_version_major)?;
        writer.write_u16::<LE>(self.engine_version_minor)?;
        writer.write_u16::<LE>(self.engine_version_patch)?;
        writer.write_u32::<LE>(self.engine_version_build)?;
        write_string(writer, &self.engine_version)?;
        writer.write_u32::<LE>(self.custom_format_version)?;
        writer.write_u32::<LE>(self.custom_format.len() as u32)?;
        for cf in &self.custom_format {
            cf.write(writer)?;
        }
        Ok(())
    }
}

/// Root struct inside a save file which holds both the Unreal Engine class name and list of properties
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Root {
    pub save_game_type: String,
    pub properties: Properties,
}
impl Root {
    fn read<R: Read + Seek>(reader: &mut Context<R>) -> TResult<Self> {
        Ok(Self {
            save_game_type: read_string(reader)?,
            properties: read_properties_until_none(reader)?,
        })
    }
    fn write<W: Write>(&self, writer: &mut Context<W>) -> TResult<()> {
        write_string(writer, &self.save_game_type)?;
        write_properties_none_terminated(writer, &self.properties)?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Save {
    pub header: Header,
    pub root: Root,
    pub extra: Vec<u8>,
}
impl Save {
    /// Reads save from the given reader
    pub fn read<R: Read>(reader: &mut R) -> Result<Self, ParseError> {
        Self::read_with_types(reader, &Types::new())
    }
    /// Reads save from the given reader using the provided [`Types`]
    pub fn read_with_types<R: Read>(reader: &mut R, types: &Types) -> Result<Self, ParseError> {
        let mut reader = SeekReader::new(reader);

        Context::run_with_types(types, &mut reader, |reader| {
            let header = Header::read(reader)?;
            let (root, extra) = reader.header(&header, |reader| -> TResult<_> {
                let root = Root::read(reader)?;
                let extra = {
                    let mut buf = vec![];
                    reader.read_to_end(&mut buf)?;
                    if buf != [0; 4] {
                        eprintln!(
                            "{} extra bytes. Save may not have been parsed completely.",
                            buf.len()
                        );
                    }
                    buf
                };
                Ok((root, extra))
            })?;

            Ok(Self {
                header,
                root,
                extra,
            })
        })
        .map_err(|e| error::ParseError {
            offset: reader.stream_position().unwrap() as usize, // our own implemenation which cannot fail
            error: e,
        })
    }
    pub fn write<W: Write>(&self, writer: &mut W) -> TResult<()> {
        Context::run(writer, |writer| {
            writer.header(&self.header, |writer| {
                self.header.write(writer)?;
                self.root.write(writer)?;
                writer.write_all(&self.extra)?;
                Ok(())
            })
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::*;
    use pretty_assertions::assert_eq;

    use std::io::Cursor;

    const SAVE: &[u8] = include_bytes!("../drg-save-test.sav");
    #[test]
    fn test_header() -> TResult<()> {
        let original = [
            0x47, 0x56, 0x41, 0x53, 0x02, 0x00, 0x00, 0x00, 0x06, 0x02, 0x00, 0x00, 0x04, 0x00,
            0x19, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x17, 0x00, 0x00, 0x00, 0x2B, 0x2B,
            0x55, 0x45, 0x34, 0x2B, 0x52, 0x65, 0x6C, 0x65, 0x61, 0x73, 0x65, 0x2D, 0x34, 0x2E,
            0x32, 0x35, 0x50, 0x6C, 0x75, 0x73, 0x00, 0x03, 0x00, 0x00, 0x00, 0x31, 0x00, 0x00,
            0x00, 0xFA, 0x7A, 0xF5, 0xFC, 0x83, 0x42, 0x76, 0x50, 0x58, 0xE6, 0xA9, 0xB9, 0x32,
            0x2D, 0xA0, 0xFF, 0x3D, 0x00, 0x00, 0x00, 0xF3, 0x7A, 0xBB, 0x24, 0x83, 0x4F, 0x46,
            0x56, 0xC2, 0x2D, 0x2F, 0x1F, 0xFF, 0x96, 0xAD, 0x49, 0x05, 0x00, 0x00, 0x00, 0x29,
            0x23, 0xA5, 0x76, 0xB5, 0x45, 0x23, 0x09, 0x41, 0xD8, 0xAE, 0x98, 0xD8, 0x6A, 0x2F,
            0xCF, 0x02, 0x00, 0x00, 0x00, 0x07, 0x69, 0xBC, 0x5F, 0xAE, 0x40, 0xC8, 0x55, 0x84,
            0xF1, 0x67, 0x8E, 0x3F, 0xF1, 0xFF, 0x5E, 0x01, 0x00, 0x00, 0x00, 0x12, 0xE4, 0x26,
            0xFB, 0x4D, 0x4B, 0x15, 0x1F, 0x0A, 0x55, 0x72, 0x93, 0x70, 0x2F, 0x1D, 0x96, 0x03,
            0x00, 0x00, 0x00, 0x22, 0xD5, 0x54, 0x9C, 0xBE, 0x4F, 0x26, 0xA8, 0x46, 0x07, 0x21,
            0x94, 0xD0, 0x82, 0xB4, 0x61, 0x1E, 0x00, 0x00, 0x00, 0xE4, 0x32, 0xD8, 0xB0, 0x0D,
            0x4F, 0x89, 0x1F, 0xB7, 0x7E, 0xCF, 0xAC, 0xA2, 0x4A, 0xFD, 0x36, 0x0A, 0x00, 0x00,
            0x00, 0x28, 0x43, 0xC6, 0xE1, 0x53, 0x4D, 0x2C, 0xA2, 0x86, 0x8E, 0x6C, 0xA3, 0x8C,
            0xBD, 0x17, 0x64, 0x00, 0x00, 0x00, 0x00, 0x3C, 0xC1, 0x5E, 0x37, 0xFB, 0x48, 0xE4,
            0x06, 0xF0, 0x84, 0x00, 0xB5, 0x7E, 0x71, 0x2A, 0x26, 0x04, 0x00, 0x00, 0x00, 0xED,
            0x68, 0xB0, 0xE4, 0xE9, 0x42, 0x94, 0xF4, 0x0B, 0xDA, 0x31, 0xA2, 0x41, 0xBB, 0x46,
            0x2E, 0x26, 0x00, 0x00, 0x00, 0x3F, 0x74, 0xFC, 0xCF, 0x80, 0x44, 0xB0, 0x43, 0xDF,
            0x14, 0x91, 0x93, 0x73, 0x20, 0x1D, 0x17, 0x25, 0x00, 0x00, 0x00, 0xB5, 0x49, 0x2B,
            0xB0, 0xE9, 0x44, 0x20, 0xBB, 0xB7, 0x32, 0x04, 0xA3, 0x60, 0x03, 0xE4, 0x52, 0x02,
            0x00, 0x00, 0x00, 0x5C, 0x10, 0xE4, 0xA4, 0xB5, 0x49, 0xA1, 0x59, 0xC4, 0x40, 0xC5,
            0xA7, 0xEE, 0xDF, 0x7E, 0x54, 0x00, 0x00, 0x00, 0x00, 0xC9, 0x31, 0xC8, 0x39, 0xDC,
            0x47, 0xE6, 0x5A, 0x17, 0x9C, 0x44, 0x9A, 0x7C, 0x8E, 0x1C, 0x3E, 0x00, 0x00, 0x00,
            0x00, 0x33, 0x1B, 0xF0, 0x78, 0x98, 0x4F, 0xEA, 0xEB, 0xEA, 0x84, 0xB4, 0xB9, 0xA2,
            0x5A, 0xB9, 0xCC, 0x04, 0x00, 0x00, 0x00, 0x0F, 0x38, 0x31, 0x66, 0xE0, 0x43, 0x4D,
            0x2D, 0x27, 0xCF, 0x09, 0x80, 0x5A, 0xA9, 0x56, 0x69, 0x00, 0x00, 0x00, 0x00, 0x9F,
            0x8B, 0xF8, 0x12, 0xFC, 0x4A, 0x75, 0x88, 0x0C, 0xD9, 0x7C, 0xA6, 0x29, 0xBD, 0x3A,
            0x38, 0x2B, 0x00, 0x00, 0x00, 0x4C, 0xE7, 0x5A, 0x7B, 0x10, 0x4C, 0x70, 0xD2, 0x98,
            0x57, 0x58, 0xA9, 0x5A, 0x2A, 0x21, 0x0B, 0x0C, 0x00, 0x00, 0x00, 0x18, 0x69, 0x29,
            0xD7, 0xDD, 0x4B, 0xD6, 0x1D, 0xA8, 0x64, 0xE2, 0x9D, 0x84, 0x38, 0xC1, 0x3C, 0x03,
            0x00, 0x00, 0x00, 0x78, 0x52, 0xA1, 0xC2, 0xFE, 0x4A, 0xE7, 0xBF, 0xFF, 0x90, 0x17,
            0x6C, 0x55, 0xF7, 0x1D, 0x53, 0x01, 0x00, 0x00, 0x00, 0xD4, 0xA3, 0xAC, 0x6E, 0xC1,
            0x4C, 0xEC, 0x40, 0xED, 0x8B, 0x86, 0xB7, 0xC5, 0x8F, 0x42, 0x09, 0x03, 0x00, 0x00,
            0x00, 0xDD, 0x75, 0xE5, 0x29, 0x27, 0x46, 0xA3, 0xE0, 0x76, 0xD2, 0x10, 0x9D, 0xEA,
            0xDC, 0x2C, 0x23, 0x11, 0x00, 0x00, 0x00, 0x5D, 0xA6, 0x43, 0xAF, 0x47, 0x49, 0xD3,
            0x7F, 0x8E, 0x3E, 0x73, 0x98, 0x05, 0xBB, 0xC1, 0xD9, 0x07, 0x00, 0x00, 0x00, 0xEC,
            0x6C, 0x26, 0x6B, 0x8F, 0x4B, 0xC7, 0x1E, 0xD9, 0xE4, 0x0B, 0xA3, 0x07, 0xFC, 0x42,
            0x09, 0x01, 0x00, 0x00, 0x00, 0x61, 0x3D, 0xF7, 0x0D, 0xEA, 0x47, 0x3F, 0xA2, 0xE9,
            0x89, 0x27, 0xB7, 0x9A, 0x49, 0x41, 0x0C, 0x01, 0x00, 0x00, 0x00, 0x86, 0x18, 0x1D,
            0x60, 0x84, 0x4F, 0x64, 0xAC, 0xDE, 0xD3, 0x16, 0xAA, 0xD6, 0xC7, 0xEA, 0x0D, 0x1F,
            0x00, 0x00, 0x00, 0xD6, 0xBC, 0xFF, 0x9D, 0x58, 0x01, 0x4F, 0x49, 0x82, 0x12, 0x21,
            0xE2, 0x88, 0xA8, 0x92, 0x3C, 0x0A, 0x00, 0x00, 0x00, 0xAC, 0xD0, 0xAE, 0xF2, 0x6F,
            0x41, 0xFE, 0x9A, 0x7F, 0xAA, 0x64, 0x86, 0xFC, 0xD6, 0x26, 0xFA, 0x01, 0x00, 0x00,
            0x00, 0x0B, 0x1F, 0x4F, 0x17, 0xA5, 0x45, 0xC6, 0xB4, 0xE8, 0x2E, 0x3F, 0xB1, 0x7D,
            0x91, 0xFB, 0xD0, 0x0A, 0x00, 0x00, 0x00, 0x83, 0x4A, 0xF9, 0x35, 0x6C, 0x40, 0x58,
            0xE2, 0xF5, 0x09, 0x18, 0xA3, 0x7C, 0x24, 0x10, 0x96, 0x25, 0x00, 0x00, 0x00, 0x6E,
            0xC1, 0x8F, 0xB6, 0xE2, 0x42, 0x1B, 0x8B, 0x5C, 0x21, 0x53, 0xB4, 0xFE, 0x44, 0x88,
            0x05, 0x01, 0x00, 0x00, 0x00, 0x06, 0x85, 0xE1, 0xB2, 0xC2, 0xCF, 0x73, 0x42, 0xBB,
            0xF4, 0x4E, 0xA5, 0x07, 0xBA, 0x8B, 0x75, 0x01, 0x00, 0x00, 0x00, 0x50, 0x32, 0x68,
            0x54, 0xAF, 0x48, 0x99, 0x80, 0x96, 0x98, 0xC8, 0x8B, 0xB7, 0xF9, 0xAD, 0xFB, 0x00,
            0x00, 0x00, 0x00, 0x19, 0x4D, 0x0C, 0x43, 0x70, 0x49, 0x54, 0x71, 0x69, 0x9B, 0x69,
            0x87, 0xE5, 0xB0, 0x90, 0xDF, 0x0E, 0x00, 0x00, 0x00, 0xBD, 0x32, 0xFE, 0xAA, 0x14,
            0x4C, 0x95, 0x53, 0x25, 0x5E, 0x6A, 0xB6, 0xDD, 0xD1, 0x32, 0x10, 0x01, 0x00, 0x00,
            0x00, 0x8E, 0xE1, 0xAF, 0x23, 0x58, 0x4E, 0xE1, 0x4C, 0x52, 0xC2, 0x61, 0x8D, 0xB7,
            0xBE, 0x53, 0xB9, 0x0B, 0x00, 0x00, 0x00, 0xEA, 0xB7, 0x62, 0xA4, 0x3A, 0x4E, 0x99,
            0xF4, 0x1F, 0xEC, 0xC1, 0x99, 0xB2, 0xE1, 0x24, 0x82, 0x02, 0x00, 0x00, 0x00, 0xBD,
            0xFD, 0xB5, 0x2E, 0x10, 0x4D, 0xAC, 0x01, 0x8F, 0xF3, 0x36, 0x81, 0xDA, 0xA5, 0x93,
            0x33, 0x05, 0x00, 0x00, 0x00, 0x4F, 0x35, 0x9D, 0x50, 0x2F, 0x49, 0xE6, 0xF6, 0xB2,
            0x85, 0x49, 0xA7, 0x1C, 0x63, 0x3C, 0x07, 0x00, 0x00, 0x00, 0x00, 0xE7, 0x9E, 0x7F,
            0x71, 0x3A, 0x49, 0xB0, 0xE9, 0x32, 0x91, 0xB3, 0x88, 0x07, 0x81, 0x38, 0x1B, 0x06,
            0x00, 0x00, 0x00, 0x40, 0xEB, 0x56, 0x4A, 0xDC, 0x11, 0xF5, 0x10, 0x7E, 0x34, 0xD3,
            0x92, 0xE7, 0x6A, 0xC9, 0xB2, 0x02, 0x00, 0x00, 0x00, 0x00, 0x4A, 0x8A, 0xD7, 0x97,
            0x46, 0x58, 0xE8, 0xB5, 0x19, 0xA8, 0xBA, 0xB4, 0x46, 0x7D, 0x48, 0x11, 0x00, 0x00,
            0x00, 0x86, 0xF8, 0x79, 0x55, 0x1F, 0x4C, 0x3A, 0x93, 0x7B, 0x08, 0xBA, 0x83, 0x2F,
            0xB9, 0x61, 0x63, 0x01, 0x00, 0x00, 0x00, 0x52, 0xBE, 0x2F, 0x61, 0x0B, 0x40, 0x53,
            0xDA, 0x91, 0x4F, 0x0D, 0x91, 0x7C, 0x85, 0xB1, 0x9F, 0x01, 0x00, 0x00, 0x00, 0x36,
            0x7A, 0x23, 0xA4, 0xC9, 0x41, 0xEA, 0xCA, 0xF8, 0x18, 0xA2, 0x8F, 0xF3, 0x1B, 0x68,
            0x58, 0x04, 0x00, 0x00, 0x00, 0x75, 0x3F, 0x4E, 0x80, 0x49, 0x4B, 0x88, 0x70, 0x06,
            0x8C, 0xD6, 0xA4, 0xDC, 0xB6, 0x7E, 0x3C, 0x05, 0x00, 0x00, 0x00, 0xF2, 0x0A, 0x68,
            0xFB, 0xA3, 0x4B, 0xEF, 0x59, 0xB5, 0x19, 0xA8, 0xBA, 0x3D, 0x44, 0xC8, 0x73, 0x02,
            0x00, 0x00, 0x00, 0x0E, 0xB7, 0x50, 0x99, 0x17, 0x4E, 0x1A, 0xB4, 0x0D, 0xFA, 0xCC,
            0xBB, 0xD6, 0x7F, 0x81, 0x57, 0x01, 0x00, 0x00, 0x00, 0x96, 0x51, 0x96, 0xAB, 0xFC,
            0x08, 0xD8, 0x45, 0x8D, 0x22, 0xD7, 0xB7, 0x9E, 0x56, 0xAD, 0x78, 0x01, 0x00, 0x00,
            0x00,
        ];
        let mut reader = Cursor::new(&original);
        let header = Context::run(&mut reader, |reader| Header::read(reader))?;
        let mut reconstructed = vec![];
        Context::run(&mut reconstructed, |writer| header.write(writer))?;
        assert_eq!(original, &reconstructed[..]);
        Ok(())
    }

    #[test]
    fn test_uuid() -> TResult<()> {
        let id = uuid::uuid!("2eb5fdbd4d1001ac8ff33681daa59333");
        let mut writer = vec![];
        Context::run(&mut writer, |writer| id.write(writer))?;
        let mut reader = Cursor::new(&writer);
        let rid = Context::run(&mut reader, |reader| uuid::Uuid::read(reader))?;
        assert_eq!(id, rid);
        Ok(())
    }

    #[test]
    fn test_uuid2() -> TResult<()> {
        let id = uuid::uuid!("85b20ca1-49fb-7138-a154-c89a2c20e2cd");
        let mut writer = vec![];
        Context::run(&mut writer, |writer| id.write(writer))?;
        assert_eq!(
            writer,
            [
                0xa1, 0x0c, 0xb2, 0x85, 0x38, 0x71, 0xfb, 0x49, 0x9a, 0xc8, 0x54, 0xa1, 0xcd, 0xe2,
                0x20, 0x2c
            ]
        );
        Ok(())
    }

    #[test]
    fn test_rw_save1() -> TResult<()> {
        let mut reader = Cursor::new(&SAVE);
        let obj = Save::read(&mut reader).unwrap();
        let mut reconstructed: Vec<u8> = vec![];
        obj.write(&mut reconstructed)?;
        assert_eq!(SAVE, reconstructed);
        Ok(())
    }

    #[test]
    fn test_rw_property_meta() -> TResult<()> {
        let original = [
            0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x0A, 0x00, 0x00, 0x00,
        ];
        let mut reader = Cursor::new(&original);
        let obj = Context::run(&mut reader, |reader| {
            Property::read(reader, PropertyType::IntProperty, 0)
        })?;
        let mut reconstructed: Vec<u8> = vec![];
        Context::run(&mut reconstructed, |writer| obj.write(writer))?;
        assert_eq!(original, &reconstructed[..]);
        assert_eq!(
            obj,
            Property::Int {
                id: Some(uuid::uuid!("00000000000000000000000000000000")),
                value: 10
            }
        );
        Ok(())
    }

    #[test]
    fn test_rw_property_meta_str() -> TResult<()> {
        let original = [
            0x00, 0x32, 0x00, 0x00, 0x00, 0x32, 0x33, 0x37, 0x30, 0x32, 0x34, 0x32, 0x34, 0x31,
            0x31, 0x32, 0x37, 0x39, 0x31, 0x34, 0x35, 0x39, 0x30, 0x31, 0x31, 0x38, 0x31, 0x36,
            0x37, 0x31, 0x34, 0x32, 0x32, 0x34, 0x39, 0x34, 0x33, 0x31, 0x31, 0x32, 0x35, 0x32,
            0x35, 0x33, 0x31, 0x34, 0x30, 0x34, 0x30, 0x39, 0x35, 0x32, 0x30, 0x35, 0x00,
        ];
        let mut reader = Cursor::new(&original);
        let obj = Context::run(&mut reader, |reader| {
            Property::read(reader, PropertyType::StrProperty, 0)
        })?;
        println!("{obj:#?}");
        let mut reconstructed: Vec<u8> = vec![];
        Context::run(&mut reconstructed, |writer| obj.write(writer))?;
        assert_eq!(original, &reconstructed[..]);
        Ok(())
    }

    #[test]
    fn test_read_int_property() -> TResult<()> {
        let bytes = [
            0x0E, 0x00, 0x00, 0x00, 0x56, 0x65, 0x72, 0x73, 0x69, 0x6F, 0x6E, 0x4E, 0x75, 0x6D,
            0x62, 0x65, 0x72, 0x00, 0x0C, 0x00, 0x00, 0x00, 0x49, 0x6E, 0x74, 0x50, 0x72, 0x6F,
            0x70, 0x65, 0x72, 0x74, 0x79, 0x00, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x02, 0x00, 0x00, 0x00,
        ];
        let mut reader = Cursor::new(bytes);
        assert_eq!(
            Context::run(&mut reader, read_property)?,
            Some((
                "VersionNumber".to_string(),
                Property::Int { id: None, value: 2 }
            ))
        );
        Ok(())
    }

    #[test]
    fn test_read_struct_property() -> TResult<()> {
        let bytes = [
            0x12, 0x00, 0x00, 0x00, 0x56, 0x61, 0x6E, 0x69, 0x74, 0x79, 0x4D, 0x61, 0x73, 0x74,
            0x65, 0x72, 0x79, 0x53, 0x61, 0x76, 0x65, 0x00, 0x0F, 0x00, 0x00, 0x00, 0x53, 0x74,
            0x72, 0x75, 0x63, 0x74, 0x50, 0x72, 0x6F, 0x70, 0x65, 0x72, 0x74, 0x79, 0x00, 0x8D,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x12, 0x00, 0x00, 0x00, 0x56, 0x61, 0x6E,
            0x69, 0x74, 0x79, 0x4D, 0x61, 0x73, 0x74, 0x65, 0x72, 0x79, 0x53, 0x61, 0x76, 0x65,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x06, 0x00, 0x00, 0x00, 0x4C, 0x65, 0x76, 0x65, 0x6C, 0x00,
            0x0C, 0x00, 0x00, 0x00, 0x49, 0x6E, 0x74, 0x50, 0x72, 0x6F, 0x70, 0x65, 0x72, 0x74,
            0x79, 0x00, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x8C, 0x00, 0x00,
            0x00, 0x03, 0x00, 0x00, 0x00, 0x58, 0x50, 0x00, 0x0C, 0x00, 0x00, 0x00, 0x49, 0x6E,
            0x74, 0x50, 0x72, 0x6F, 0x70, 0x65, 0x72, 0x74, 0x79, 0x00, 0x04, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x3A, 0x23, 0x00, 0x00, 0x1A, 0x00, 0x00, 0x00, 0x48,
            0x61, 0x73, 0x41, 0x77, 0x61, 0x72, 0x64, 0x65, 0x64, 0x46, 0x6F, 0x72, 0x4F, 0x6C,
            0x64, 0x50, 0x75, 0x72, 0x63, 0x68, 0x61, 0x73, 0x65, 0x73, 0x00, 0x0D, 0x00, 0x00,
            0x00, 0x42, 0x6F, 0x6F, 0x6C, 0x50, 0x72, 0x6F, 0x70, 0x65, 0x72, 0x74, 0x79, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x05, 0x00, 0x00, 0x00,
            0x4E, 0x6F, 0x6E, 0x65, 0x00,
        ];
        let mut reader = Cursor::new(bytes);
        assert_eq!(
            Context::run(&mut reader, read_property)?,
            Some((
                "VanityMasterySave".to_string(),
                Property::Struct {
                    id: None,
                    value: StructValue::Struct(indexmap::IndexMap::from([
                        (
                            "Level".to_string(),
                            Property::Int {
                                id: None,
                                value: 140,
                            }
                        ),
                        (
                            "XP".to_string(),
                            Property::Int {
                                id: None,
                                value: 9018,
                            },
                        ),
                        (
                            "HasAwardedForOldPurchases".to_string(),
                            Property::Bool {
                                id: None,
                                value: true,
                            },
                        ),
                    ])),
                    struct_type: StructType::Struct(Some("VanityMasterySave".to_string())),
                    struct_id: uuid::uuid!("00000000000000000000000000000000"),
                }
            ))
        );
        Ok(())
    }

    #[test]
    fn test_read_array_property() -> TResult<()> {
        let bytes = [
            0x0C, 0x00, 0x00, 0x00, 0x53, 0x74, 0x61, 0x74, 0x49, 0x6E, 0x64, 0x69, 0x63, 0x65,
            0x73, 0x00, 0x0E, 0x00, 0x00, 0x00, 0x41, 0x72, 0x72, 0x61, 0x79, 0x50, 0x72, 0x6F,
            0x70, 0x65, 0x72, 0x74, 0x79, 0x00, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x0C, 0x00, 0x00, 0x00, 0x49, 0x6E, 0x74, 0x50, 0x72, 0x6F, 0x70, 0x65, 0x72, 0x74,
            0x79, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        ];
        let mut reader = Cursor::new(bytes);
        assert_eq!(
            Context::run(&mut reader, read_property)?,
            Some((
                "StatIndices".to_string(),
                Property::Array {
                    array_type: PropertyType::IntProperty,
                    id: None,
                    value: ValueArray::Base(ValueVec::Int(vec![0]))
                }
            ))
        );
        Ok(())
    }

    fn rw_property(original: &[u8]) -> TResult<()> {
        let mut reader = Cursor::new(&original);
        Context::run(&mut reader, |reader| {
            let property = read_property(reader)?.unwrap();
            println!("{property:#?}");
            let mut reconstructed: Vec<u8> = vec![];
            Context::run(&mut reconstructed, |writer| {
                write_property((&property.0, &property.1), writer)
            })?;
            assert_eq!(original, &reconstructed[..]);
            Ok(())
        })
    }

    #[test]
    fn test_rw_property_int() -> TResult<()> {
        let original = [
            0x0E, 0x00, 0x00, 0x00, 0x56, 0x65, 0x72, 0x73, 0x69, 0x6F, 0x6E, 0x4E, 0x75, 0x6D,
            0x62, 0x65, 0x72, 0x00, 0x0C, 0x00, 0x00, 0x00, 0x49, 0x6E, 0x74, 0x50, 0x72, 0x6F,
            0x70, 0x65, 0x72, 0x74, 0x79, 0x00, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x02, 0x00, 0x00, 0x00,
        ];
        rw_property(&original)
    }

    #[test]
    fn test_rw_property_bool() -> TResult<()> {
        let original = [
            0x13, 0x00, 0x00, 0x00, 0x48, 0x61, 0x76, 0x65, 0x53, 0x6B, 0x69, 0x6E, 0x73, 0x42,
            0x65, 0x65, 0x6E, 0x52, 0x65, 0x73, 0x65, 0x74, 0x00, 0x0D, 0x00, 0x00, 0x00, 0x42,
            0x6F, 0x6F, 0x6C, 0x50, 0x72, 0x6F, 0x70, 0x65, 0x72, 0x74, 0x79, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00,
        ];
        rw_property(&original)
    }

    #[test]
    fn test_rw_property_struct() -> TResult<()> {
        let original = [
            0x08, 0x00, 0x00, 0x00, 0x46, 0x6F, 0x72, 0x67, 0x69, 0x6E, 0x67, 0x00, 0x0F, 0x00,
            0x00, 0x00, 0x53, 0x74, 0x72, 0x75, 0x63, 0x74, 0x50, 0x72, 0x6F, 0x70, 0x65, 0x72,
            0x74, 0x79, 0x00, 0x2D, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0C, 0x00, 0x00,
            0x00, 0x46, 0x6F, 0x72, 0x67, 0x69, 0x6E, 0x67, 0x53, 0x61, 0x76, 0x65, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x03, 0x00, 0x00, 0x00, 0x58, 0x50, 0x00, 0x0C, 0x00, 0x00, 0x00, 0x49,
            0x6E, 0x74, 0x50, 0x72, 0x6F, 0x70, 0x65, 0x72, 0x74, 0x79, 0x00, 0x04, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xB3, 0x01, 0x00, 0x00, 0x05, 0x00, 0x00, 0x00,
            0x4E, 0x6F, 0x6E, 0x65, 0x00,
        ];
        rw_property(&original)
    }

    #[test]
    fn test_rw_property_struct2() -> TResult<()> {
        let original = [
            0x07, 0x00, 0x00, 0x00, 0x44, 0x72, 0x69, 0x6E, 0x6B, 0x73, 0x00, 0x0F, 0x00, 0x00,
            0x00, 0x53, 0x74, 0x72, 0x75, 0x63, 0x74, 0x50, 0x72, 0x6F, 0x70, 0x65, 0x72, 0x74,
            0x79, 0x00, 0x52, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0A, 0x00, 0x00, 0x00,
            0x44, 0x72, 0x69, 0x6E, 0x6B, 0x53, 0x61, 0x76, 0x65, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0F,
            0x00, 0x00, 0x00, 0x55, 0x6E, 0x6C, 0x6F, 0x63, 0x6B, 0x65, 0x64, 0x44, 0x72, 0x69,
            0x6E, 0x6B, 0x73, 0x00, 0x0C, 0x00, 0x00, 0x00, 0x53, 0x65, 0x74, 0x50, 0x72, 0x6F,
            0x70, 0x65, 0x72, 0x74, 0x79, 0x00, 0xD8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x0F, 0x00, 0x00, 0x00, 0x53, 0x74, 0x72, 0x75, 0x63, 0x74, 0x50, 0x72, 0x6F, 0x70,
            0x65, 0x72, 0x74, 0x79, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0D, 0x00, 0x00, 0x00,
            0x80, 0xC0, 0x60, 0x4B, 0x01, 0xA7, 0x22, 0x4F, 0x9D, 0xDD, 0x58, 0x9C, 0xAC, 0x18,
            0xF5, 0x23, 0x0A, 0x33, 0xAD, 0x36, 0xBB, 0xA5, 0x6E, 0x41, 0xB6, 0x2C, 0xC8, 0x8E,
            0xBB, 0xE4, 0x14, 0x13, 0xC0, 0x31, 0x45, 0x66, 0xAA, 0xDC, 0x7A, 0x43, 0x83, 0x22,
            0x26, 0xB0, 0x5B, 0xE0, 0x54, 0xCB, 0x85, 0x94, 0xDA, 0x82, 0xA8, 0xCC, 0x79, 0x41,
            0xB9, 0x96, 0xE7, 0x04, 0x71, 0xB1, 0x4D, 0xBA, 0x9E, 0x8C, 0x88, 0xA4, 0x30, 0xD8,
            0x3F, 0x45, 0xB3, 0x3B, 0xD1, 0x52, 0x57, 0x4A, 0xCB, 0xCA, 0x19, 0xB5, 0x58, 0x12,
            0xE5, 0xB4, 0x60, 0x42, 0xBE, 0xCD, 0xF0, 0x67, 0xB4, 0x33, 0xAF, 0xA3, 0x4E, 0x48,
            0x34, 0x2B, 0x70, 0x48, 0xC1, 0x4D, 0x92, 0xBD, 0x67, 0x1C, 0x1B, 0x8A, 0x76, 0x68,
            0x3F, 0x77, 0x73, 0x5B, 0x24, 0x29, 0x35, 0x4A, 0x88, 0x62, 0x18, 0xDE, 0xBA, 0x8C,
            0x79, 0x7D, 0x74, 0x51, 0xAD, 0x20, 0x57, 0x18, 0xFA, 0x45, 0xB0, 0x09, 0xF8, 0xF8,
            0xB1, 0x47, 0x37, 0x64, 0x88, 0x43, 0xC8, 0x85, 0x21, 0x73, 0x2D, 0x4B, 0x9D, 0x09,
            0x09, 0x26, 0xA0, 0x91, 0xA7, 0x9C, 0xA6, 0xAF, 0x43, 0xD6, 0xEE, 0x8B, 0x0D, 0x45,
            0xB3, 0xC4, 0xE3, 0x37, 0x46, 0x7B, 0xED, 0x5E, 0x5A, 0xA9, 0x4A, 0x29, 0xA9, 0x43,
            0xBD, 0x4C, 0xB8, 0xEE, 0x0D, 0x03, 0x42, 0x44, 0x29, 0x13, 0x8D, 0x21, 0xB5, 0x1B,
            0x4D, 0xC3, 0xA3, 0x4A, 0x8A, 0xA8, 0x19, 0x92, 0xBA, 0xB6, 0x94, 0x98, 0x13, 0x00,
            0x00, 0x00, 0x48, 0x61, 0x73, 0x55, 0x6E, 0x6C, 0x6F, 0x63, 0x6B, 0x65, 0x64, 0x53,
            0x70, 0x65, 0x63, 0x69, 0x61, 0x6C, 0x00, 0x0D, 0x00, 0x00, 0x00, 0x42, 0x6F, 0x6F,
            0x6C, 0x50, 0x72, 0x6F, 0x70, 0x65, 0x72, 0x74, 0x79, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x05, 0x00, 0x00, 0x00, 0x4E, 0x6F, 0x6E, 0x65,
            0x00,
        ];
        rw_property(&original)
    }

    #[test]
    fn test_rw_property_array() -> TResult<()> {
        let original = [
            0x0E, 0x00, 0x00, 0x00, 0x55, 0x6E, 0x6C, 0x6F, 0x63, 0x6B, 0x65, 0x64, 0x49, 0x74,
            0x65, 0x6D, 0x73, 0x00, 0x0E, 0x00, 0x00, 0x00, 0x41, 0x72, 0x72, 0x61, 0x79, 0x50,
            0x72, 0x6F, 0x70, 0x65, 0x72, 0x74, 0x79, 0x00, 0xCB, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x0F, 0x00, 0x00, 0x00, 0x53, 0x74, 0x72, 0x75, 0x63, 0x74, 0x50, 0x72,
            0x6F, 0x70, 0x65, 0x72, 0x74, 0x79, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00, 0x0E, 0x00,
            0x00, 0x00, 0x55, 0x6E, 0x6C, 0x6F, 0x63, 0x6B, 0x65, 0x64, 0x49, 0x74, 0x65, 0x6D,
            0x73, 0x00, 0x0F, 0x00, 0x00, 0x00, 0x53, 0x74, 0x72, 0x75, 0x63, 0x74, 0x50, 0x72,
            0x6F, 0x70, 0x65, 0x72, 0x74, 0x79, 0x00, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x05, 0x00, 0x00, 0x00, 0x47, 0x75, 0x69, 0x64, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x76,
            0x3A, 0x96, 0xF0, 0x20, 0x19, 0x26, 0x46, 0xA6, 0x1D, 0x41, 0xF4, 0x98, 0xBE, 0x42,
            0x85, 0xB9, 0x5E, 0xDE, 0xC1, 0xBD, 0xDB, 0x1C, 0x47, 0x8E, 0x5D, 0xC5, 0x73, 0x22,
            0x10, 0x51, 0x34, 0xDD, 0xB5, 0xDA, 0x90, 0x86, 0xCD, 0x44, 0x42, 0xAF, 0x60, 0x06,
            0xDE, 0x2A, 0x43, 0x90, 0xA9, 0x28, 0xD2, 0x24, 0x72, 0xC4, 0x09, 0x22, 0x4E, 0xAF,
            0xFA, 0xCA, 0xC8, 0xB1, 0x37, 0x72, 0x87, 0x82, 0x08, 0x2A, 0xAB, 0x25, 0x31, 0x9F,
            0x43, 0xB4, 0x3A, 0x84, 0x58, 0xF5, 0x22, 0x8E, 0x8C, 0x4E, 0x96, 0xF4, 0x32, 0xD0,
            0x18, 0x23, 0x43, 0xA6, 0x2E, 0x81, 0xD8, 0x21, 0xA1, 0x48, 0x0F, 0x79, 0xE9, 0x1B,
            0xF1, 0x5A, 0xC1, 0x69, 0x4B, 0xA7, 0x98, 0x7C, 0x69, 0x69, 0x59, 0xC0, 0xD1, 0x97,
            0x36, 0xEE, 0x11, 0x0D, 0x4C, 0x87, 0x49, 0xA2, 0x24, 0xAF, 0x6F, 0x2D, 0xBA, 0x01,
            0x60,
        ];
        rw_property(&original)
    }

    #[test]
    fn test_rw_property_str() -> TResult<()> {
        let original = [
            0x11, 0x00, 0x00, 0x00, 0x57, 0x61, 0x74, 0x63, 0x68, 0x65, 0x64, 0x54, 0x75, 0x74,
            0x6F, 0x72, 0x69, 0x61, 0x6C, 0x73, 0x00, 0x0E, 0x00, 0x00, 0x00, 0x41, 0x72, 0x72,
            0x61, 0x79, 0x50, 0x72, 0x6F, 0x70, 0x65, 0x72, 0x74, 0x79, 0x00, 0x01, 0x07, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x0F, 0x00, 0x00, 0x00, 0x53, 0x74, 0x72, 0x75, 0x63,
            0x74, 0x50, 0x72, 0x6F, 0x70, 0x65, 0x72, 0x74, 0x79, 0x00, 0x00, 0x0E, 0x00, 0x00,
            0x00, 0x11, 0x00, 0x00, 0x00, 0x57, 0x61, 0x74, 0x63, 0x68, 0x65, 0x64, 0x54, 0x75,
            0x74, 0x6F, 0x72, 0x69, 0x61, 0x6C, 0x73, 0x00, 0x0F, 0x00, 0x00, 0x00, 0x53, 0x74,
            0x72, 0x75, 0x63, 0x74, 0x50, 0x72, 0x6F, 0x70, 0x65, 0x72, 0x74, 0x79, 0x00, 0xA8,
            0x06, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x57, 0x61, 0x74,
            0x63, 0x68, 0x65, 0x64, 0x54, 0x75, 0x74, 0x6F, 0x72, 0x69, 0x61, 0x6C, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x0D, 0x00, 0x00, 0x00, 0x54, 0x75, 0x74, 0x6F, 0x72, 0x69, 0x61, 0x6C,
            0x4E, 0x61, 0x6D, 0x65, 0x00, 0x0C, 0x00, 0x00, 0x00, 0x53, 0x74, 0x72, 0x50, 0x72,
            0x6F, 0x70, 0x65, 0x72, 0x74, 0x79, 0x00, 0x1B, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x17, 0x00, 0x00, 0x00, 0x54, 0x75, 0x74, 0x6F, 0x72, 0x69, 0x61, 0x6C,
            0x5F, 0x48, 0x69, 0x6E, 0x74, 0x5F, 0x44, 0x65, 0x70, 0x6F, 0x73, 0x69, 0x74, 0x65,
            0x00, 0x06, 0x00, 0x00, 0x00, 0x63, 0x6F, 0x75, 0x6E, 0x74, 0x00, 0x0C, 0x00, 0x00,
            0x00, 0x49, 0x6E, 0x74, 0x50, 0x72, 0x6F, 0x70, 0x65, 0x72, 0x74, 0x79, 0x00, 0x04,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x05, 0x00,
            0x00, 0x00, 0x4E, 0x6F, 0x6E, 0x65, 0x00, 0x0D, 0x00, 0x00, 0x00, 0x54, 0x75, 0x74,
            0x6F, 0x72, 0x69, 0x61, 0x6C, 0x4E, 0x61, 0x6D, 0x65, 0x00, 0x0C, 0x00, 0x00, 0x00,
            0x53, 0x74, 0x72, 0x50, 0x72, 0x6F, 0x70, 0x65, 0x72, 0x74, 0x79, 0x00, 0x21, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x1D, 0x00, 0x00, 0x00, 0x54, 0x75, 0x74,
            0x6F, 0x72, 0x69, 0x61, 0x6C, 0x5F, 0x48, 0x69, 0x6E, 0x74, 0x5F, 0x45, 0x71, 0x75,
            0x69, 0x70, 0x43, 0x6C, 0x61, 0x73, 0x73, 0x54, 0x6F, 0x6F, 0x6C, 0x00, 0x06, 0x00,
            0x00, 0x00, 0x63, 0x6F, 0x75, 0x6E, 0x74, 0x00, 0x0C, 0x00, 0x00, 0x00, 0x49, 0x6E,
            0x74, 0x50, 0x72, 0x6F, 0x70, 0x65, 0x72, 0x74, 0x79, 0x00, 0x04, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x0A, 0x00, 0x00, 0x00, 0x05, 0x00, 0x00, 0x00, 0x4E,
            0x6F, 0x6E, 0x65, 0x00, 0x0D, 0x00, 0x00, 0x00, 0x54, 0x75, 0x74, 0x6F, 0x72, 0x69,
            0x61, 0x6C, 0x4E, 0x61, 0x6D, 0x65, 0x00, 0x0C, 0x00, 0x00, 0x00, 0x53, 0x74, 0x72,
            0x50, 0x72, 0x6F, 0x70, 0x65, 0x72, 0x74, 0x79, 0x00, 0x1F, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x1B, 0x00, 0x00, 0x00, 0x54, 0x75, 0x74, 0x6F, 0x72, 0x69,
            0x61, 0x6C, 0x5F, 0x48, 0x69, 0x6E, 0x74, 0x5F, 0x45, 0x71, 0x75, 0x69, 0x70, 0x47,
            0x72, 0x65, 0x6E, 0x61, 0x64, 0x65, 0x00, 0x06, 0x00, 0x00, 0x00, 0x63, 0x6F, 0x75,
            0x6E, 0x74, 0x00, 0x0C, 0x00, 0x00, 0x00, 0x49, 0x6E, 0x74, 0x50, 0x72, 0x6F, 0x70,
            0x65, 0x72, 0x74, 0x79, 0x00, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x0A, 0x00, 0x00, 0x00, 0x05, 0x00, 0x00, 0x00, 0x4E, 0x6F, 0x6E, 0x65, 0x00, 0x0D,
            0x00, 0x00, 0x00, 0x54, 0x75, 0x74, 0x6F, 0x72, 0x69, 0x61, 0x6C, 0x4E, 0x61, 0x6D,
            0x65, 0x00, 0x0C, 0x00, 0x00, 0x00, 0x53, 0x74, 0x72, 0x50, 0x72, 0x6F, 0x70, 0x65,
            0x72, 0x74, 0x79, 0x00, 0x1F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x1B,
            0x00, 0x00, 0x00, 0x54, 0x75, 0x74, 0x6F, 0x72, 0x69, 0x61, 0x6C, 0x5F, 0x48, 0x69,
            0x6E, 0x74, 0x5F, 0x4C, 0x61, 0x73, 0x65, 0x72, 0x50, 0x6F, 0x69, 0x6E, 0x74, 0x65,
            0x72, 0x00, 0x06, 0x00, 0x00, 0x00, 0x63, 0x6F, 0x75, 0x6E, 0x74, 0x00, 0x0C, 0x00,
            0x00, 0x00, 0x49, 0x6E, 0x74, 0x50, 0x72, 0x6F, 0x70, 0x65, 0x72, 0x74, 0x79, 0x00,
            0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x05,
            0x00, 0x00, 0x00, 0x4E, 0x6F, 0x6E, 0x65, 0x00, 0x0D, 0x00, 0x00, 0x00, 0x54, 0x75,
            0x74, 0x6F, 0x72, 0x69, 0x61, 0x6C, 0x4E, 0x61, 0x6D, 0x65, 0x00, 0x0C, 0x00, 0x00,
            0x00, 0x53, 0x74, 0x72, 0x50, 0x72, 0x6F, 0x70, 0x65, 0x72, 0x74, 0x79, 0x00, 0x27,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x23, 0x00, 0x00, 0x00, 0x54, 0x75,
            0x74, 0x6F, 0x72, 0x69, 0x61, 0x6C, 0x5F, 0x48, 0x69, 0x6E, 0x74, 0x5F, 0x45, 0x71,
            0x75, 0x69, 0x70, 0x53, 0x65, 0x63, 0x6F, 0x6E, 0x64, 0x61, 0x72, 0x79, 0x57, 0x65,
            0x61, 0x70, 0x6F, 0x6E, 0x00, 0x06, 0x00, 0x00, 0x00, 0x63, 0x6F, 0x75, 0x6E, 0x74,
            0x00, 0x0C, 0x00, 0x00, 0x00, 0x49, 0x6E, 0x74, 0x50, 0x72, 0x6F, 0x70, 0x65, 0x72,
            0x74, 0x79, 0x00, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0A, 0x00,
            0x00, 0x00, 0x05, 0x00, 0x00, 0x00, 0x4E, 0x6F, 0x6E, 0x65, 0x00, 0x0D, 0x00, 0x00,
            0x00, 0x54, 0x75, 0x74, 0x6F, 0x72, 0x69, 0x61, 0x6C, 0x4E, 0x61, 0x6D, 0x65, 0x00,
            0x0C, 0x00, 0x00, 0x00, 0x53, 0x74, 0x72, 0x50, 0x72, 0x6F, 0x70, 0x65, 0x72, 0x74,
            0x79, 0x00, 0x25, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x21, 0x00, 0x00,
            0x00, 0x54, 0x75, 0x74, 0x6F, 0x72, 0x69, 0x61, 0x6C, 0x5F, 0x48, 0x69, 0x6E, 0x74,
            0x5F, 0x45, 0x71, 0x75, 0x69, 0x70, 0x54, 0x72, 0x61, 0x76, 0x65, 0x72, 0x73, 0x61,
            0x6C, 0x54, 0x6F, 0x6F, 0x6C, 0x00, 0x06, 0x00, 0x00, 0x00, 0x63, 0x6F, 0x75, 0x6E,
            0x74, 0x00, 0x0C, 0x00, 0x00, 0x00, 0x49, 0x6E, 0x74, 0x50, 0x72, 0x6F, 0x70, 0x65,
            0x72, 0x74, 0x79, 0x00, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0A,
            0x00, 0x00, 0x00, 0x05, 0x00, 0x00, 0x00, 0x4E, 0x6F, 0x6E, 0x65, 0x00, 0x0D, 0x00,
            0x00, 0x00, 0x54, 0x75, 0x74, 0x6F, 0x72, 0x69, 0x61, 0x6C, 0x4E, 0x61, 0x6D, 0x65,
            0x00, 0x0C, 0x00, 0x00, 0x00, 0x53, 0x74, 0x72, 0x50, 0x72, 0x6F, 0x70, 0x65, 0x72,
            0x74, 0x79, 0x00, 0x2B, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x27, 0x00,
            0x00, 0x00, 0x54, 0x75, 0x74, 0x6F, 0x72, 0x69, 0x61, 0x6C, 0x5F, 0x48, 0x69, 0x6E,
            0x74, 0x5F, 0x45, 0x67, 0x67, 0x4D, 0x69, 0x73, 0x73, 0x69, 0x6F, 0x6E, 0x54, 0x65,
            0x72, 0x72, 0x61, 0x69, 0x6E, 0x53, 0x63, 0x61, 0x6E, 0x6E, 0x65, 0x72, 0x00, 0x06,
            0x00, 0x00, 0x00, 0x63, 0x6F, 0x75, 0x6E, 0x74, 0x00, 0x0C, 0x00, 0x00, 0x00, 0x49,
            0x6E, 0x74, 0x50, 0x72, 0x6F, 0x70, 0x65, 0x72, 0x74, 0x79, 0x00, 0x04, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00, 0x05, 0x00, 0x00, 0x00,
            0x4E, 0x6F, 0x6E, 0x65, 0x00, 0x0D, 0x00, 0x00, 0x00, 0x54, 0x75, 0x74, 0x6F, 0x72,
            0x69, 0x61, 0x6C, 0x4E, 0x61, 0x6D, 0x65, 0x00, 0x0C, 0x00, 0x00, 0x00, 0x53, 0x74,
            0x72, 0x50, 0x72, 0x6F, 0x70, 0x65, 0x72, 0x74, 0x79, 0x00, 0x19, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x15, 0x00, 0x00, 0x00, 0x54, 0x75, 0x74, 0x6F, 0x72,
            0x69, 0x61, 0x6C, 0x5F, 0x48, 0x69, 0x6E, 0x74, 0x5F, 0x46, 0x6C, 0x61, 0x72, 0x65,
            0x73, 0x00, 0x06, 0x00, 0x00, 0x00, 0x63, 0x6F, 0x75, 0x6E, 0x74, 0x00, 0x0C, 0x00,
            0x00, 0x00, 0x49, 0x6E, 0x74, 0x50, 0x72, 0x6F, 0x70, 0x65, 0x72, 0x74, 0x79, 0x00,
            0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x07, 0x00, 0x00, 0x00, 0x05,
            0x00, 0x00, 0x00, 0x4E, 0x6F, 0x6E, 0x65, 0x00, 0x0D, 0x00, 0x00, 0x00, 0x54, 0x75,
            0x74, 0x6F, 0x72, 0x69, 0x61, 0x6C, 0x4E, 0x61, 0x6D, 0x65, 0x00, 0x0C, 0x00, 0x00,
            0x00, 0x53, 0x74, 0x72, 0x50, 0x72, 0x6F, 0x70, 0x65, 0x72, 0x74, 0x79, 0x00, 0x1C,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x18, 0x00, 0x00, 0x00, 0x54, 0x75,
            0x74, 0x6F, 0x72, 0x69, 0x61, 0x6C, 0x5F, 0x48, 0x69, 0x6E, 0x74, 0x5F, 0x4C, 0x6F,
            0x77, 0x4F, 0x6E, 0x41, 0x6D, 0x6D, 0x6F, 0x00, 0x06, 0x00, 0x00, 0x00, 0x63, 0x6F,
            0x75, 0x6E, 0x74, 0x00, 0x0C, 0x00, 0x00, 0x00, 0x49, 0x6E, 0x74, 0x50, 0x72, 0x6F,
            0x70, 0x65, 0x72, 0x74, 0x79, 0x00, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x05, 0x00, 0x00, 0x00, 0x05, 0x00, 0x00, 0x00, 0x4E, 0x6F, 0x6E, 0x65, 0x00,
            0x0D, 0x00, 0x00, 0x00, 0x54, 0x75, 0x74, 0x6F, 0x72, 0x69, 0x61, 0x6C, 0x4E, 0x61,
            0x6D, 0x65, 0x00, 0x0C, 0x00, 0x00, 0x00, 0x53, 0x74, 0x72, 0x50, 0x72, 0x6F, 0x70,
            0x65, 0x72, 0x74, 0x79, 0x00, 0x18, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x14, 0x00, 0x00, 0x00, 0x54, 0x75, 0x74, 0x6F, 0x72, 0x69, 0x61, 0x6C, 0x5F, 0x48,
            0x69, 0x6E, 0x74, 0x5F, 0x4E, 0x69, 0x74, 0x72, 0x61, 0x00, 0x06, 0x00, 0x00, 0x00,
            0x63, 0x6F, 0x75, 0x6E, 0x74, 0x00, 0x0C, 0x00, 0x00, 0x00, 0x49, 0x6E, 0x74, 0x50,
            0x72, 0x6F, 0x70, 0x65, 0x72, 0x74, 0x79, 0x00, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x05, 0x00, 0x00, 0x00, 0x4E, 0x6F, 0x6E,
            0x65, 0x00, 0x0D, 0x00, 0x00, 0x00, 0x54, 0x75, 0x74, 0x6F, 0x72, 0x69, 0x61, 0x6C,
            0x4E, 0x61, 0x6D, 0x65, 0x00, 0x0C, 0x00, 0x00, 0x00, 0x53, 0x74, 0x72, 0x50, 0x72,
            0x6F, 0x70, 0x65, 0x72, 0x74, 0x79, 0x00, 0x1C, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x18, 0x00, 0x00, 0x00, 0x54, 0x75, 0x74, 0x6F, 0x72, 0x69, 0x61, 0x6C,
            0x5F, 0x54, 0x68, 0x72, 0x6F, 0x77, 0x43, 0x61, 0x72, 0x72, 0x69, 0x61, 0x62, 0x6C,
            0x65, 0x00, 0x06, 0x00, 0x00, 0x00, 0x63, 0x6F, 0x75, 0x6E, 0x74, 0x00, 0x0C, 0x00,
            0x00, 0x00, 0x49, 0x6E, 0x74, 0x50, 0x72, 0x6F, 0x70, 0x65, 0x72, 0x74, 0x79, 0x00,
            0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x05,
            0x00, 0x00, 0x00, 0x4E, 0x6F, 0x6E, 0x65, 0x00, 0x0D, 0x00, 0x00, 0x00, 0x54, 0x75,
            0x74, 0x6F, 0x72, 0x69, 0x61, 0x6C, 0x4E, 0x61, 0x6D, 0x65, 0x00, 0x0C, 0x00, 0x00,
            0x00, 0x53, 0x74, 0x72, 0x50, 0x72, 0x6F, 0x70, 0x65, 0x72, 0x74, 0x79, 0x00, 0x28,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x00, 0x00, 0x00, 0x54, 0x75,
            0x74, 0x6F, 0x72, 0x69, 0x61, 0x6C, 0x5F, 0x48, 0x69, 0x6E, 0x74, 0x5F, 0x42, 0x6F,
            0x73, 0x63, 0x6F, 0x46, 0x69, 0x72, 0x73, 0x74, 0x53, 0x6F, 0x6C, 0x6F, 0x4D, 0x69,
            0x73, 0x73, 0x69, 0x6F, 0x6E, 0x00, 0x06, 0x00, 0x00, 0x00, 0x63, 0x6F, 0x75, 0x6E,
            0x74, 0x00, 0x0C, 0x00, 0x00, 0x00, 0x49, 0x6E, 0x74, 0x50, 0x72, 0x6F, 0x70, 0x65,
            0x72, 0x74, 0x79, 0x00, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01,
            0x00, 0x00, 0x00, 0x05, 0x00, 0x00, 0x00, 0x4E, 0x6F, 0x6E, 0x65, 0x00, 0x0D, 0x00,
            0x00, 0x00, 0x54, 0x75, 0x74, 0x6F, 0x72, 0x69, 0x61, 0x6C, 0x4E, 0x61, 0x6D, 0x65,
            0x00, 0x0C, 0x00, 0x00, 0x00, 0x53, 0x74, 0x72, 0x50, 0x72, 0x6F, 0x70, 0x65, 0x72,
            0x74, 0x79, 0x00, 0x1F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x1B, 0x00,
            0x00, 0x00, 0x54, 0x75, 0x74, 0x6F, 0x72, 0x69, 0x61, 0x6C, 0x5F, 0x48, 0x69, 0x6E,
            0x74, 0x5F, 0x42, 0x6F, 0x73, 0x63, 0x6F, 0x41, 0x62, 0x69, 0x6C, 0x69, 0x74, 0x79,
            0x00, 0x06, 0x00, 0x00, 0x00, 0x63, 0x6F, 0x75, 0x6E, 0x74, 0x00, 0x0C, 0x00, 0x00,
            0x00, 0x49, 0x6E, 0x74, 0x50, 0x72, 0x6F, 0x70, 0x65, 0x72, 0x74, 0x79, 0x00, 0x04,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x05, 0x00, 0x00, 0x00, 0x05, 0x00,
            0x00, 0x00, 0x4E, 0x6F, 0x6E, 0x65, 0x00, 0x0D, 0x00, 0x00, 0x00, 0x54, 0x75, 0x74,
            0x6F, 0x72, 0x69, 0x61, 0x6C, 0x4E, 0x61, 0x6D, 0x65, 0x00, 0x0C, 0x00, 0x00, 0x00,
            0x53, 0x74, 0x72, 0x50, 0x72, 0x6F, 0x70, 0x65, 0x72, 0x74, 0x79, 0x00, 0x1B, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x17, 0x00, 0x00, 0x00, 0x54, 0x75, 0x74,
            0x6F, 0x72, 0x69, 0x61, 0x6C, 0x5F, 0x48, 0x69, 0x6E, 0x74, 0x5F, 0x52, 0x65, 0x73,
            0x75, 0x70, 0x70, 0x6C, 0x79, 0x00, 0x06, 0x00, 0x00, 0x00, 0x63, 0x6F, 0x75, 0x6E,
            0x74, 0x00, 0x0C, 0x00, 0x00, 0x00, 0x49, 0x6E, 0x74, 0x50, 0x72, 0x6F, 0x70, 0x65,
            0x72, 0x74, 0x79, 0x00, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x03,
            0x00, 0x00, 0x00, 0x05, 0x00, 0x00, 0x00, 0x4E, 0x6F, 0x6E, 0x65, 0x00,
        ];
        rw_property(&original)
    }

    #[test]
    fn test_rw_header() -> TResult<()> {
        let original = [
            0x47, 0x56, 0x41, 0x53, 0x02, 0x00, 0x00, 0x00, 0x0A, 0x02, 0x00, 0x00, 0x04, 0x00,
            0x1B, 0x00, 0x02, 0x00, 0xFA, 0x14, 0x01, 0x80, 0x05, 0x00, 0x00, 0x00, 0x6D, 0x61,
            0x69, 0x6E, 0x00, 0x03, 0x00, 0x00, 0x00, 0x32, 0x00, 0x00, 0x00, 0x4E, 0x7C, 0xE7,
            0x82, 0xA5, 0x43, 0x23, 0x33, 0xC5, 0x13, 0x6B, 0xB4, 0xF3, 0x0D, 0x31, 0x97, 0x00,
            0x00, 0x00, 0x00, 0xFA, 0x7A, 0xF5, 0xFC, 0x83, 0x42, 0x76, 0x50, 0x58, 0xE6, 0xA9,
            0xB9, 0x32, 0x2D, 0xA0, 0xFF, 0x44, 0x00, 0x00, 0x00, 0xF3, 0x7A, 0xBB, 0x24, 0x83,
            0x4F, 0x46, 0x56, 0xC2, 0x2D, 0x2F, 0x1F, 0xFF, 0x96, 0xAD, 0x49, 0x05, 0x00, 0x00,
            0x00, 0x12, 0xE4, 0x26, 0xFB, 0x4D, 0x4B, 0x15, 0x1F, 0x0A, 0x55, 0x72, 0x93, 0x70,
            0x2F, 0x1D, 0x96, 0x03, 0x00, 0x00, 0x00, 0x22, 0xD5, 0x54, 0x9C, 0xBE, 0x4F, 0x26,
            0xA8, 0x46, 0x07, 0x21, 0x94, 0xD0, 0x82, 0xB4, 0x61, 0x2B, 0x00, 0x00, 0x00, 0xE4,
            0x32, 0xD8, 0xB0, 0x0D, 0x4F, 0x89, 0x1F, 0xB7, 0x7E, 0xCF, 0xAC, 0xA2, 0x4A, 0xFD,
            0x36, 0x0A, 0x00, 0x00, 0x00, 0x28, 0x43, 0xC6, 0xE1, 0x53, 0x4D, 0x2C, 0xA2, 0x86,
            0x8E, 0x6C, 0xA3, 0x8C, 0xBD, 0x17, 0x64, 0x00, 0x00, 0x00, 0x00, 0x3C, 0xC1, 0x5E,
            0x37, 0xFB, 0x48, 0xE4, 0x06, 0xF0, 0x84, 0x00, 0xB5, 0x7E, 0x71, 0x2A, 0x26, 0x04,
            0x00, 0x00, 0x00, 0xED, 0x68, 0xB0, 0xE4, 0xE9, 0x42, 0x94, 0xF4, 0x0B, 0xDA, 0x31,
            0xA2, 0x41, 0xBB, 0x46, 0x2E, 0x28, 0x00, 0x00, 0x00, 0x3F, 0x74, 0xFC, 0xCF, 0x80,
            0x44, 0xB0, 0x43, 0xDF, 0x14, 0x91, 0x93, 0x73, 0x20, 0x1D, 0x17, 0x25, 0x00, 0x00,
            0x00, 0xB5, 0x49, 0x2B, 0xB0, 0xE9, 0x44, 0x20, 0xBB, 0xB7, 0x32, 0x04, 0xA3, 0x60,
            0x03, 0xE4, 0x52, 0x03, 0x00, 0x00, 0x00, 0x5C, 0x10, 0xE4, 0xA4, 0xB5, 0x49, 0xA1,
            0x59, 0xC4, 0x40, 0xC5, 0xA7, 0xEE, 0xDF, 0x7E, 0x54, 0x00, 0x00, 0x00, 0x00, 0xC9,
            0x31, 0xC8, 0x39, 0xDC, 0x47, 0xE6, 0x5A, 0x17, 0x9C, 0x44, 0x9A, 0x7C, 0x8E, 0x1C,
            0x3E, 0x00, 0x00, 0x00, 0x00, 0x33, 0x1B, 0xF0, 0x78, 0x98, 0x4F, 0xEA, 0xEB, 0xEA,
            0x84, 0xB4, 0xB9, 0xA2, 0x5A, 0xB9, 0xCC, 0x0E, 0x00, 0x00, 0x00, 0x0F, 0x38, 0x31,
            0x66, 0xE0, 0x43, 0x4D, 0x2D, 0x27, 0xCF, 0x09, 0x80, 0x5A, 0xA9, 0x56, 0x69, 0x00,
            0x00, 0x00, 0x00, 0x9F, 0x8B, 0xF8, 0x12, 0xFC, 0x4A, 0x75, 0x88, 0x0C, 0xD9, 0x7C,
            0xA6, 0x29, 0xBD, 0x3A, 0x38, 0x2D, 0x00, 0x00, 0x00, 0x4C, 0xE7, 0x5A, 0x7B, 0x10,
            0x4C, 0x70, 0xD2, 0x98, 0x57, 0x58, 0xA9, 0x5A, 0x2A, 0x21, 0x0B, 0x0D, 0x00, 0x00,
            0x00, 0x18, 0x69, 0x29, 0xD7, 0xDD, 0x4B, 0xD6, 0x1D, 0xA8, 0x64, 0xE2, 0x9D, 0x84,
            0x38, 0xC1, 0x3C, 0x03, 0x00, 0x00, 0x00, 0x78, 0x52, 0xA1, 0xC2, 0xFE, 0x4A, 0xE7,
            0xBF, 0xFF, 0x90, 0x17, 0x6C, 0x55, 0xF7, 0x1D, 0x53, 0x01, 0x00, 0x00, 0x00, 0xD4,
            0xA3, 0xAC, 0x6E, 0xC1, 0x4C, 0xEC, 0x40, 0xED, 0x8B, 0x86, 0xB7, 0xC5, 0x8F, 0x42,
            0x09, 0x03, 0x00, 0x00, 0x00, 0xDD, 0x75, 0xE5, 0x29, 0x27, 0x46, 0xA3, 0xE0, 0x76,
            0xD2, 0x10, 0x9D, 0xEA, 0xDC, 0x2C, 0x23, 0x11, 0x00, 0x00, 0x00, 0x5D, 0xA6, 0x43,
            0xAF, 0x47, 0x49, 0xD3, 0x7F, 0x8E, 0x3E, 0x73, 0x98, 0x05, 0xBB, 0xC1, 0xD9, 0x0F,
            0x00, 0x00, 0x00, 0xEC, 0x6C, 0x26, 0x6B, 0x8F, 0x4B, 0xC7, 0x1E, 0xD9, 0xE4, 0x0B,
            0xA3, 0x07, 0xFC, 0x42, 0x09, 0x01, 0x00, 0x00, 0x00, 0x61, 0x3D, 0xF7, 0x0D, 0xEA,
            0x47, 0x3F, 0xA2, 0xE9, 0x89, 0x27, 0xB7, 0x9A, 0x49, 0x41, 0x0C, 0x01, 0x00, 0x00,
            0x00, 0x86, 0x18, 0x1D, 0x60, 0x84, 0x4F, 0x64, 0xAC, 0xDE, 0xD3, 0x16, 0xAA, 0xD6,
            0xC7, 0xEA, 0x0D, 0x2F, 0x00, 0x00, 0x00, 0x68, 0x63, 0x08, 0xE7, 0x58, 0x4C, 0x23,
            0x6B, 0x70, 0x1B, 0x39, 0x84, 0x91, 0x5E, 0x26, 0x16, 0x01, 0x00, 0x00, 0x00, 0xD6,
            0xBC, 0xFF, 0x9D, 0x58, 0x01, 0x4F, 0x49, 0x82, 0x12, 0x21, 0xE2, 0x88, 0xA8, 0x92,
            0x3C, 0x0A, 0x00, 0x00, 0x00, 0xAC, 0xD0, 0xAE, 0xF2, 0x6F, 0x41, 0xFE, 0x9A, 0x7F,
            0xAA, 0x64, 0x86, 0xFC, 0xD6, 0x26, 0xFA, 0x01, 0x00, 0x00, 0x00, 0x0B, 0x1F, 0x4F,
            0x17, 0xA5, 0x45, 0xC6, 0xB4, 0xE8, 0x2E, 0x3F, 0xB1, 0x7D, 0x91, 0xFB, 0xD0, 0x0A,
            0x00, 0x00, 0x00, 0x83, 0x4A, 0xF9, 0x35, 0x6C, 0x40, 0x58, 0xE2, 0xF5, 0x09, 0x18,
            0xA3, 0x7C, 0x24, 0x10, 0x96, 0x29, 0x00, 0x00, 0x00, 0x6E, 0xC1, 0x8F, 0xB6, 0xE2,
            0x42, 0x1B, 0x8B, 0x5C, 0x21, 0x53, 0xB4, 0xFE, 0x44, 0x88, 0x05, 0x01, 0x00, 0x00,
            0x00, 0x06, 0x85, 0xE1, 0xB2, 0xC2, 0xCF, 0x73, 0x42, 0xBB, 0xF4, 0x4E, 0xA5, 0x07,
            0xBA, 0x8B, 0x75, 0x01, 0x00, 0x00, 0x00, 0x36, 0x89, 0xF5, 0x64, 0xBA, 0x42, 0x1B,
            0xFD, 0x89, 0x72, 0x96, 0xBA, 0x4E, 0xFA, 0xD0, 0xD5, 0x01, 0x00, 0x00, 0x00, 0x27,
            0xD8, 0x0E, 0x6F, 0x95, 0x48, 0x09, 0xA6, 0x8D, 0x99, 0x91, 0x9C, 0xA4, 0x0E, 0x18,
            0x90, 0x02, 0x00, 0x00, 0x00, 0xE7, 0x9E, 0x7F, 0x71, 0x3A, 0x49, 0xB0, 0xE9, 0x32,
            0x91, 0xB3, 0x88, 0x07, 0x81, 0x38, 0x1B, 0x08, 0x00, 0x00, 0x00, 0x50, 0x32, 0x68,
            0x54, 0xAF, 0x48, 0x99, 0x80, 0x96, 0x98, 0xC8, 0x8B, 0xB7, 0xF9, 0xAD, 0xFB, 0x00,
            0x00, 0x00, 0x00, 0x19, 0x4D, 0x0C, 0x43, 0x70, 0x49, 0x54, 0x71, 0x69, 0x9B, 0x69,
            0x87, 0xE5, 0xB0, 0x90, 0xDF, 0x0F, 0x00, 0x00, 0x00, 0xBD, 0x32, 0xFE, 0xAA, 0x14,
            0x4C, 0x95, 0x53, 0x25, 0x5E, 0x6A, 0xB6, 0xDD, 0xD1, 0x32, 0x10, 0x01, 0x00, 0x00,
            0x00, 0x8E, 0xE1, 0xAF, 0x23, 0x58, 0x4E, 0xE1, 0x4C, 0x52, 0xC2, 0x61, 0x8D, 0xB7,
            0xBE, 0x53, 0xB9, 0x0B, 0x00, 0x00, 0x00, 0xEA, 0xB7, 0x62, 0xA4, 0x3A, 0x4E, 0x99,
            0xF4, 0x1F, 0xEC, 0xC1, 0x99, 0xB2, 0xE1, 0x24, 0x82, 0x04, 0x00, 0x00, 0x00, 0xBD,
            0xFD, 0xB5, 0x2E, 0x10, 0x4D, 0xAC, 0x01, 0x8F, 0xF3, 0x36, 0x81, 0xDA, 0xA5, 0x93,
            0x33, 0x05, 0x00, 0x00, 0x00, 0x4F, 0x35, 0x9D, 0x50, 0x2F, 0x49, 0xE6, 0xF6, 0xB2,
            0x85, 0x49, 0xA7, 0x1C, 0x63, 0x3C, 0x07, 0x00, 0x00, 0x00, 0x00, 0x40, 0xEB, 0x56,
            0x4A, 0xDC, 0x11, 0xF5, 0x10, 0x7E, 0x34, 0xD3, 0x92, 0xE7, 0x6A, 0xC9, 0xB2, 0x02,
            0x00, 0x00, 0x00, 0x00, 0x4A, 0x8A, 0xD7, 0x97, 0x46, 0x58, 0xE8, 0xB5, 0x19, 0xA8,
            0xBA, 0xB4, 0x46, 0x7D, 0x48, 0x12, 0x00, 0x00, 0x00, 0x86, 0xF8, 0x79, 0x55, 0x1F,
            0x4C, 0x3A, 0x93, 0x7B, 0x08, 0xBA, 0x83, 0x2F, 0xB9, 0x61, 0x63, 0x02, 0x00, 0x00,
            0x00, 0x52, 0xBE, 0x2F, 0x61, 0x0B, 0x40, 0x53, 0xDA, 0x91, 0x4F, 0x0D, 0x91, 0x7C,
            0x85, 0xB1, 0x9F, 0x01, 0x00, 0x00, 0x00, 0x36, 0x7A, 0x23, 0xA4, 0xC9, 0x41, 0xEA,
            0xCA, 0xF8, 0x18, 0xA2, 0x8F, 0xF3, 0x1B, 0x68, 0x58, 0x04, 0x00, 0x00, 0x00, 0x75,
            0x3F, 0x4E, 0x80, 0x49, 0x4B, 0x88, 0x70, 0x06, 0x8C, 0xD6, 0xA4, 0xDC, 0xB6, 0x7E,
            0x3C, 0x05, 0x00, 0x00, 0x00, 0xF2, 0x0A, 0x68, 0xFB, 0xA3, 0x4B, 0xEF, 0x59, 0xB5,
            0x19, 0xA8, 0xBA, 0x3D, 0x44, 0xC8, 0x73, 0x02, 0x00, 0x00, 0x00, 0x0E, 0xB7, 0x50,
            0x99, 0x17, 0x4E, 0x1A, 0xB4, 0x0D, 0xFA, 0xCC, 0xBB, 0xD6, 0x7F, 0x81, 0x57, 0x01,
            0x00, 0x00, 0x00,
        ];
        let mut reader = Cursor::new(&original);
        Context::run(&mut reader, |reader| {
            let obj = Header::read(reader)?;
            let mut reconstructed: Vec<u8> = vec![];
            Context::run(&mut reconstructed, |writer| obj.write(writer))?;
            assert_eq!(original, &reconstructed[..]);
            Ok(())
        })
    }
}
