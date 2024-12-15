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

use uesave::{Property, PropertyInner, Save};

let save = Save::read(&mut File::open("drg-save-test.sav")?)?;
match save.root.properties["NumberOfGamesPlayed"] {
    Property { inner: PropertyInner::Int(value), .. } => {
        assert_eq!(2173, value);
    }
    _ => {}
}
# Ok::<(), Box<dyn std::error::Error>>(())

```
*/

mod error;

#[cfg(test)]
mod tests;

pub use error::{Error, ParseError};

use byteorder::{ReadBytesExt, WriteBytesExt, LE};
use std::io::{Read, Seek, Write};

use serde::{de::Visitor, Deserialize, Deserializer, Serialize, Serializer};

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
        self.reader.read(buf).inspect(|s| self.read_bytes += s)
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
        write_string_trailing(writer, string, None)?;
    }
    Ok(())
}

fn read_string_trailing<R: Read + Seek>(reader: &mut Context<R>) -> TResult<(String, Vec<u8>)> {
    let len = reader.read_i32::<LE>()?;
    if len < 0 {
        let bytes = (-len) as usize * 2;
        let mut chars = vec![];
        let mut rest = vec![];
        let mut read = 0;
        while read < bytes {
            let next = reader.read_u16::<LE>()?;
            read += 2;
            if next == 0 {
                rest.extend(next.to_le_bytes());
                break;
            } else {
                chars.push(next);
            }
        }
        while read < bytes {
            rest.push(reader.read_u8()?);
            read += 1;
        }
        Ok((String::from_utf16(&chars).unwrap(), rest))
    } else {
        let bytes = len as usize;
        let mut chars = vec![];
        let mut rest = vec![];
        let mut read = 0;
        while read < bytes {
            let next = reader.read_u8()?;
            read += 1;
            if next == 0 {
                rest.push(next);
                break;
            } else {
                chars.push(next);
            }
        }
        while read < bytes {
            rest.push(reader.read_u8()?);
            read += 1;
        }
        Ok((String::from_utf8(chars).unwrap(), rest))
    }
}
fn write_string_trailing<W: Write>(
    writer: &mut Context<W>,
    string: &str,
    trailing: Option<&[u8]>,
) -> TResult<()> {
    if string.is_empty() || string.is_ascii() {
        writer.write_u32::<LE>(
            (string.as_bytes().len() + trailing.map(|t| t.len()).unwrap_or(1)) as u32,
        )?;
        writer.write_all(string.as_bytes())?;
        writer.write_all(trailing.unwrap_or(&[0]))?;
    } else {
        let chars: Vec<u16> = string.encode_utf16().collect();
        writer.write_i32::<LE>(
            -((chars.len() + trailing.map(|t| t.len()).unwrap_or(2) / 2) as i32),
        )?;
        for c in chars {
            writer.write_u16::<LE>(c)?;
        }
        writer.write_all(trailing.unwrap_or(&[0, 0]))?;
    }
    Ok(())
}

#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PropertyKey(pub u32, pub String);
impl From<String> for PropertyKey {
    fn from(value: String) -> Self {
        Self(0, value)
    }
}
impl From<&str> for PropertyKey {
    fn from(value: &str) -> Self {
        Self(0, value.to_string())
    }
}

struct PropertyKeyVisitor;
impl Visitor<'_> for PropertyKeyVisitor {
    type Value = PropertyKey;
    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str(
            "a property key in the form of key name and index seperated by '_' e.g. property_2",
        )
    }
    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        let (name_str, index_str) = value
            .rsplit_once('_')
            .ok_or_else(|| serde::de::Error::custom("property key does not contain a '_'"))?;
        let index: u32 = index_str.parse().map_err(serde::de::Error::custom)?;

        Ok(PropertyKey(index, name_str.to_string()))
    }
}
impl<'de> Deserialize<'de> for PropertyKey {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(PropertyKeyVisitor)
    }
}
impl Serialize for PropertyKey {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&format!("{}_{}", self.1, self.0))
    }
}

#[derive(Debug, Default, PartialEq, Serialize, Deserialize)]
pub struct Properties(pub indexmap::IndexMap<PropertyKey, Property>);
impl Properties {
    fn insert(&mut self, k: impl Into<PropertyKey>, v: Property) -> Option<Property> {
        self.0.insert(k.into(), v)
    }
}
impl<K> std::ops::Index<K> for Properties
where
    K: Into<PropertyKey>,
{
    type Output = Property;
    fn index(&self, index: K) -> &Self::Output {
        self.0.index(&index.into())
    }
}
impl<K> std::ops::IndexMut<K> for Properties
where
    K: Into<PropertyKey>,
{
    fn index_mut(&mut self, index: K) -> &mut Property {
        self.0.index_mut(&index.into())
    }
}
impl<'a> IntoIterator for &'a Properties {
    type Item = <&'a indexmap::IndexMap<PropertyKey, Property> as IntoIterator>::Item;
    type IntoIter = <&'a indexmap::IndexMap<PropertyKey, Property> as IntoIterator>::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

fn read_properties_until_none<R: Read + Seek>(reader: &mut Context<R>) -> TResult<Properties> {
    let mut properties = Properties::default();
    while let Some((name, prop)) = read_property(reader)? {
        properties.insert(name, prop);
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

fn read_property<R: Read + Seek>(
    reader: &mut Context<R>,
) -> TResult<Option<(PropertyKey, Property)>> {
    let name = read_string(reader)?;
    if name == "None" {
        Ok(None)
    } else {
        reader.scope(&name, |reader| {
            let t = PropertyType::read(reader)?;
            let size = reader.read_u32::<LE>()?;
            let index = reader.read_u32::<LE>()?;
            let value = Property::read(reader, t, size)?;
            Ok(Some((PropertyKey(index, name.clone()), value)))
        })
    }
}
fn write_property<W: Write>(
    prop: (&PropertyKey, &Property),
    writer: &mut Context<W>,
) -> TResult<()> {
    write_string(writer, &prop.0 .1)?;
    prop.1.get_type().write(writer)?;

    let mut buf = vec![];
    let size = writer.stream(&mut buf, |writer| prop.1.write(writer))?;

    writer.write_u32::<LE>(size as u32)?;
    writer.write_u32::<LE>(prop.0 .0)?;
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

impl Scope<'_, '_> {
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

impl<'stream, 'types, 'scope, S> Context<'stream, '_, 'types, 'scope, S> {
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
impl<'types, R: Read + Seek> Context<'_, '_, 'types, '_, R> {
    fn get_type_or<'t>(&mut self, t: &'t StructType) -> TResult<&'t StructType>
    where
        'types: 't,
    {
        let offset = self.stream.stream_position()?;
        Ok(self.get_type().unwrap_or_else(|| {
            eprintln!(
                "offset {}: StructType for \"{}\" unspecified, assuming {:?}",
                offset,
                self.path(),
                t
            );
            t
        }))
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
    IntVector,
    Box,
    IntPoint,
    Quat,
    Rotator,
    LinearColor,
    Color,
    SoftObjectPath,
    GameplayTagContainer,
    UniqueNetIdRepl,
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
            "IntVector" => StructType::IntVector,
            "Box" => StructType::Box,
            "IntPoint" => StructType::IntPoint,
            "Quat" => StructType::Quat,
            "Rotator" => StructType::Rotator,
            "LinearColor" => StructType::LinearColor,
            "Color" => StructType::Color,
            "SoftObjectPath" => StructType::SoftObjectPath,
            "GameplayTagContainer" => StructType::GameplayTagContainer,
            "UniqueNetIdRepl" => StructType::UniqueNetIdRepl,
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
            "IntVector" => StructType::IntVector,
            "Box" => StructType::Box,
            "IntPoint" => StructType::IntPoint,
            "Quat" => StructType::Quat,
            "Rotator" => StructType::Rotator,
            "LinearColor" => StructType::LinearColor,
            "Color" => StructType::Color,
            "SoftObjectPath" => StructType::SoftObjectPath,
            "GameplayTagContainer" => StructType::GameplayTagContainer,
            "UniqueNetIdRepl" => StructType::UniqueNetIdRepl,
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
                StructType::IntVector => "IntVector",
                StructType::Box => "Box",
                StructType::IntPoint => "IntPoint",
                StructType::Quat => "Quat",
                StructType::Rotator => "Rotator",
                StructType::LinearColor => "LinearColor",
                StructType::Color => "Color",
                StructType::SoftObjectPath => "SoftObjectPath",
                StructType::GameplayTagContainer => "GameplayTagContainer",
                StructType::UniqueNetIdRepl => "UniqueNetIdRepl",
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
    pub x: f64,
    pub y: f64,
}
impl Vector2D {
    fn read<R: Read + Seek>(reader: &mut Context<R>) -> TResult<Self> {
        if reader.header.as_ref().unwrap().large_world_coordinates() {
            Ok(Self {
                x: reader.read_f64::<LE>()?,
                y: reader.read_f64::<LE>()?,
            })
        } else {
            Ok(Self {
                x: reader.read_f32::<LE>()? as f64,
                y: reader.read_f32::<LE>()? as f64,
            })
        }
    }
    fn write<W: Write>(&self, writer: &mut Context<W>) -> TResult<()> {
        if writer.header.as_ref().unwrap().large_world_coordinates() {
            writer.write_f64::<LE>(self.x)?;
            writer.write_f64::<LE>(self.y)?;
        } else {
            writer.write_f32::<LE>(self.x as f32)?;
            writer.write_f32::<LE>(self.y as f32)?;
        }
        Ok(())
    }
}
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct IntVector {
    pub x: i32,
    pub y: i32,
    pub z: i32,
}
impl IntVector {
    fn read<R: Read + Seek>(reader: &mut Context<R>) -> TResult<Self> {
        Ok(Self {
            x: reader.read_i32::<LE>()?,
            y: reader.read_i32::<LE>()?,
            z: reader.read_i32::<LE>()?,
        })
    }
    fn write<W: Write>(&self, writer: &mut Context<W>) -> TResult<()> {
        writer.write_i32::<LE>(self.x)?;
        writer.write_i32::<LE>(self.y)?;
        writer.write_i32::<LE>(self.z)?;
        Ok(())
    }
}
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Box {
    pub min: Vector,
    pub max: Vector,
    pub is_valid: bool,
}
impl Box {
    fn read<R: Read + Seek>(reader: &mut Context<R>) -> TResult<Self> {
        Ok(Self {
            min: Vector::read(reader)?,
            max: Vector::read(reader)?,
            is_valid: reader.read_u8()? > 0,
        })
    }
    fn write<W: Write>(&self, writer: &mut Context<W>) -> TResult<()> {
        self.min.write(writer)?;
        self.max.write(writer)?;
        writer.write_u8(self.is_valid as u8)?;
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
pub enum SoftObjectPath {
    Old {
        asset_path_name: String,
        sub_path_string: String,
    },
    New {
        asset_path_name: String,
        package_name: String,
        asset_name: String,
    },
}
impl SoftObjectPath {
    fn read<R: Read + Seek>(reader: &mut Context<R>) -> TResult<Self> {
        let is_new = reader
            .header
            .and_then(|h| h.package_version.ue5)
            .map(
                |ue5| ue5 >= 1007, /* FSOFTOBJECTPATH_REMOVE_ASSET_PATH_FNAMES */
            )
            .unwrap_or_default();
        Ok(if is_new {
            Self::New {
                asset_path_name: read_string(reader)?,
                package_name: read_string(reader)?,
                asset_name: read_string(reader)?,
            }
        } else {
            Self::Old {
                asset_path_name: read_string(reader)?,
                sub_path_string: read_string(reader)?,
            }
        })
    }
    fn write<W: Write>(&self, writer: &mut Context<W>) -> TResult<()> {
        match self {
            Self::Old {
                asset_path_name,
                sub_path_string,
            } => {
                write_string(writer, asset_path_name)?;
                write_string(writer, sub_path_string)?;
            }
            Self::New {
                asset_path_name,
                package_name,
                asset_name,
            } => {
                write_string(writer, asset_path_name)?;
                write_string(writer, package_name)?;
                write_string(writer, asset_name)?;
            }
        }
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
pub struct UniqueNetIdRepl {
    pub inner: Option<UniqueNetIdReplInner>,
}
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct UniqueNetIdReplInner {
    pub size: std::num::NonZeroU32,
    pub type_: String,
    pub contents: String,
}
impl UniqueNetIdRepl {
    fn read<R: Read + Seek>(reader: &mut Context<R>) -> TResult<Self> {
        let size = reader.read_u32::<LE>()?;
        let inner = if let Ok(size) = size.try_into() {
            Some(UniqueNetIdReplInner {
                size,
                type_: read_string(reader)?,
                contents: read_string(reader)?,
            })
        } else {
            None
        };
        Ok(Self { inner })
    }
    fn write<W: Write>(&self, writer: &mut Context<W>) -> TResult<()> {
        match &self.inner {
            Some(inner) => {
                writer.write_u32::<LE>(inner.size.into())?;
                write_string(writer, &inner.type_)?;
                write_string(writer, &inner.contents)?;
            }
            None => writer.write_u32::<LE>(0)?,
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
        namespace: (String, Vec<u8>),
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
                namespace: read_string_trailing(reader)?,
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
                // This particular string sometimes includes the trailing null byte and sometimes
                // does not. To preserve byte-for-byte equality we save the trailing bytes (null or
                // not) to the JSON so they can be retored later.
                write_string_trailing(writer, &namespace.0, Some(&namespace.1))?;
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
pub enum Byte {
    Byte(u8),
    Label(String),
}
/// Vectorized [`Byte`]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum ByteArray {
    Byte(Vec<u8>),
    Label(Vec<String>),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum PropertyValue {
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
    SoftObjectPath(SoftObjectPath),
    Object(String),
    Struct(StructValue),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum StructValue {
    Guid(uuid::Uuid),
    DateTime(DateTime),
    Timespan(Timespan),
    Vector2D(Vector2D),
    Vector(Vector),
    IntVector(IntVector),
    Box(Box),
    IntPoint(IntPoint),
    Quat(Quat),
    LinearColor(LinearColor),
    Color(Color),
    Rotator(Rotator),
    SoftObjectPath(SoftObjectPath),
    GameplayTagContainer(GameplayTagContainer),
    UniqueNetIdRepl(UniqueNetIdRepl),
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
pub enum ValueArray {
    Base(ValueVec),
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
            PropertyType::IntProperty => PropertyValue::Int(reader.read_i32::<LE>()?),
            PropertyType::Int8Property => PropertyValue::Int8(reader.read_i8()?),
            PropertyType::Int16Property => PropertyValue::Int16(reader.read_i16::<LE>()?),
            PropertyType::Int64Property => PropertyValue::Int64(reader.read_i64::<LE>()?),
            PropertyType::UInt16Property => PropertyValue::UInt16(reader.read_u16::<LE>()?),
            PropertyType::UInt32Property => PropertyValue::UInt32(reader.read_u32::<LE>()?),
            PropertyType::FloatProperty => PropertyValue::Float(reader.read_f32::<LE>()?),
            PropertyType::DoubleProperty => PropertyValue::Double(reader.read_f64::<LE>()?),
            PropertyType::BoolProperty => PropertyValue::Bool(reader.read_u8()? > 0),
            PropertyType::NameProperty => PropertyValue::Name(read_string(reader)?),
            PropertyType::StrProperty => PropertyValue::Str(read_string(reader)?),
            PropertyType::SoftObjectProperty => {
                PropertyValue::SoftObject(read_string(reader)?, read_string(reader)?)
            }
            PropertyType::ObjectProperty => PropertyValue::Object(read_string(reader)?),
            PropertyType::ByteProperty => PropertyValue::Byte(Byte::Label(read_string(reader)?)),
            PropertyType::EnumProperty => PropertyValue::Enum(read_string(reader)?),
            PropertyType::StructProperty => {
                PropertyValue::Struct(StructValue::read(reader, st.as_ref().unwrap())?)
            }
            _ => return Err(Error::Other(format!("unimplemented property {t:?}"))),
        })
    }
    fn write<W: Write>(&self, writer: &mut Context<W>) -> TResult<()> {
        match &self {
            PropertyValue::Int(v) => writer.write_i32::<LE>(*v)?,
            PropertyValue::Int8(v) => writer.write_i8(*v)?,
            PropertyValue::Int16(v) => writer.write_i16::<LE>(*v)?,
            PropertyValue::Int64(v) => writer.write_i64::<LE>(*v)?,
            PropertyValue::UInt16(v) => writer.write_u16::<LE>(*v)?,
            PropertyValue::UInt32(v) => writer.write_u32::<LE>(*v)?,
            PropertyValue::Float(v) => writer.write_f32::<LE>(*v)?,
            PropertyValue::Double(v) => writer.write_f64::<LE>(*v)?,
            PropertyValue::Bool(v) => writer.write_u8(u8::from(*v))?,
            PropertyValue::Name(v) => write_string(writer, v)?,
            PropertyValue::Str(v) => write_string(writer, v)?,
            PropertyValue::SoftObject(a, b) => {
                write_string(writer, a)?;
                write_string(writer, b)?;
            }
            PropertyValue::SoftObjectPath(v) => v.write(writer)?,
            PropertyValue::Object(v) => write_string(writer, v)?,
            PropertyValue::Byte(v) => match v {
                Byte::Byte(b) => writer.write_u8(*b)?,
                Byte::Label(l) => write_string(writer, l)?,
            },
            PropertyValue::Enum(v) => write_string(writer, v)?,
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
            StructType::IntVector => StructValue::IntVector(IntVector::read(reader)?),
            StructType::Box => StructValue::Box(Box::read(reader)?),
            StructType::IntPoint => StructValue::IntPoint(IntPoint::read(reader)?),
            StructType::Quat => StructValue::Quat(Quat::read(reader)?),
            StructType::LinearColor => StructValue::LinearColor(LinearColor::read(reader)?),
            StructType::Color => StructValue::Color(Color::read(reader)?),
            StructType::Rotator => StructValue::Rotator(Rotator::read(reader)?),
            StructType::SoftObjectPath => {
                StructValue::SoftObjectPath(SoftObjectPath::read(reader)?)
            }
            StructType::GameplayTagContainer => {
                StructValue::GameplayTagContainer(GameplayTagContainer::read(reader)?)
            }
            StructType::UniqueNetIdRepl => {
                StructValue::UniqueNetIdRepl(UniqueNetIdRepl::read(reader)?)
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
            StructValue::IntVector(v) => v.write(writer)?,
            StructValue::Box(v) => v.write(writer)?,
            StructValue::IntPoint(v) => v.write(writer)?,
            StructValue::Quat(v) => v.write(writer)?,
            StructValue::LinearColor(v) => v.write(writer)?,
            StructValue::Color(v) => v.write(writer)?,
            StructValue::Rotator(v) => v.write(writer)?,
            StructValue::SoftObjectPath(v) => v.write(writer)?,
            StructValue::GameplayTagContainer(v) => v.write(writer)?,
            StructValue::UniqueNetIdRepl(v) => v.write(writer)?,
            StructValue::Struct(v) => write_properties_none_terminated(writer, v)?,
        }
        Ok(())
    }
}
impl ValueVec {
    fn read<R: Read + Seek>(
        reader: &mut Context<R>,
        t: &PropertyType,
        size: u32,
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
                if size == count {
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
        size: u32,
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
                ValueArray::Struct {
                    _type,
                    name,
                    struct_type,
                    id,
                    value,
                }
            }
            _ => ValueArray::Base(ValueVec::read(reader, t, size, count)?),
        })
    }
    fn write<W: Write>(&self, writer: &mut Context<W>) -> TResult<()> {
        match &self {
            ValueArray::Struct {
                _type,
                name,
                struct_type,
                id,
                value,
            } => {
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
        size: u32,
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
    pub id: Option<uuid::Uuid>,
    #[serde(flatten)]
    pub inner: PropertyInner,
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
    SoftObject(SoftObjectPath),
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
        value: StructValue,
        struct_type: StructType,
        struct_id: uuid::Uuid,
    },
    Array {
        array_type: PropertyType,
        value: ValueArray,
    },
}

impl PropertyInner {
    fn get_type(&self) -> PropertyType {
        match &self {
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
            PropertyInner::Array { .. } => PropertyType::ArrayProperty,
        }
    }
}
impl Property {
    fn get_type(&self) -> PropertyType {
        self.inner.get_type()
    }
    fn read<R: Read + Seek>(
        reader: &mut Context<R>,
        t: PropertyType,
        size: u32,
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
                inner: PropertyInner::SoftObject(SoftObjectPath::read(reader)?),
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
                inner: PropertyInner::SoftObject(value),
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
pub struct PackageVersion {
    ue4: u32,
    ue5: Option<u32>,
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
        let package_version = PackageVersion {
            ue4: reader.read_u32::<LE>()?,
            ue5: (save_game_version >= 3)
                .then(|| reader.read_u32::<LE>())
                .transpose()?,
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
        writer.write_u32::<LE>(self.package_version.ue4)?;
        if let Some(ue5) = self.package_version.ue5 {
            writer.write_u32::<LE>(ue5)?;
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
