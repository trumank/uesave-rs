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
use std::{
    borrow::Cow,
    io::{Read, Seek, Write},
};

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
    if let Some(tag) = PropertyTagFull::read(reader)? {
        let value = reader.scope(&tag.name, |reader| Property::read(reader, tag.clone()))?;
        Ok(Some((PropertyKey(tag.index, tag.name.to_string()), value)))
    } else {
        Ok(None)
    }
}
fn write_property<W: Write>(
    prop: (&PropertyKey, &Property),
    writer: &mut Context<W>,
) -> TResult<()> {
    let mut tag = prop
        .1
        .tag
        .clone()
        .into_full(&prop.0 .1, 0, prop.0 .0, prop.1);
    let mut buf = vec![];
    let size = writer.stream(&mut buf, |writer| prop.1.write(writer, &tag))? as u32;
    tag.size = size;

    tag.write(writer)?;
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
    state: ContextState<'header, 'types, 'scope>,
}
#[derive(Debug)]
struct ContextState<'header, 'types, 'scope> {
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
            state: ContextState {
                header: None,
                types: &Types::new(),
                scope: &Scope::Root,
            },
        })
    }
    fn run_with_types<F, T>(types: &'types Types, stream: &'stream mut S, f: F) -> T
    where
        F: FnOnce(&mut Context<'stream, '_, 'types, 'scope, S>) -> T,
    {
        f(&mut Context::<'stream, '_, 'types, 'scope> {
            stream,
            state: ContextState {
                header: None,
                types,
                scope: &Scope::Root,
            },
        })
    }
    fn scope<'name, F, T>(&mut self, name: &'name str, f: F) -> T
    where
        F: FnOnce(&mut Context<'_, '_, 'types, '_, S>) -> T,
    {
        f(&mut Context {
            stream: self.stream,
            state: ContextState {
                header: self.state.header,
                types: self.state.types,
                scope: &Scope::Node {
                    name,
                    parent: self.state.scope,
                },
            },
        })
    }
    fn header<'h, F, T>(&mut self, header: &'h Header, f: F) -> T
    where
        F: FnOnce(&mut Context<'_, '_, 'types, '_, S>) -> T,
    {
        f(&mut Context {
            stream: self.stream,
            state: ContextState {
                header: Some(header),
                types: self.state.types,
                scope: self.state.scope,
            },
        })
    }
    fn stream<'s, F, T, S2>(&mut self, stream: &'s mut S2, f: F) -> T
    where
        F: FnOnce(&mut Context<'_, '_, 'types, '_, S2>) -> T,
    {
        f(&mut Context {
            stream,
            state: ContextState {
                header: self.state.header,
                types: self.state.types,
                scope: self.state.scope,
            },
        })
    }
    fn path(&self) -> String {
        self.state.scope.path()
    }
    fn get_type(&self) -> Option<&'types StructType> {
        self.state.types.types.get(&self.path())
    }
    fn unwrap_header(&self) -> &Header {
        self.state.header.unwrap()
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

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
struct PropertyTagFull<'a> {
    name: Cow<'a, str>,
    id: Option<uuid::Uuid>,
    size: u32,
    index: u32,
    data: PropertyTagDataFull,
}
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
enum PropertyTagDataFull {
    Array(std::boxed::Box<PropertyTagDataFull>),
    Struct {
        struct_type: StructType,
        id: uuid::Uuid,
    },
    Set {
        key_type: std::boxed::Box<PropertyTagDataFull>,
    },
    Map {
        key_type: std::boxed::Box<PropertyTagDataFull>,
        value_type: std::boxed::Box<PropertyTagDataFull>,
    },
    Byte(Option<String>),
    Enum(String, Option<String>),
    Bool(bool),
    Other(PropertyType),
}
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct PropertyTagPartial {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub id: Option<uuid::Uuid>,
    pub data: PropertyTagDataPartial,
}
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum PropertyTagDataPartial {
    Array(std::boxed::Box<PropertyTagDataPartial>),
    Struct {
        struct_type: StructType,
        id: uuid::Uuid,
    },
    Set {
        key_type: std::boxed::Box<PropertyTagDataPartial>,
    },
    Map {
        key_type: std::boxed::Box<PropertyTagDataPartial>,
        value_type: std::boxed::Box<PropertyTagDataPartial>,
    },
    Byte(Option<String>),
    Enum(String, Option<String>),
    Other(PropertyType),
}
impl PropertyTagDataFull {
    fn into_partial(self) -> PropertyTagDataPartial {
        match self {
            Self::Array(inner) => PropertyTagDataPartial::Array(inner.into_partial().into()),
            Self::Struct { struct_type, id } => PropertyTagDataPartial::Struct { struct_type, id },
            Self::Set { key_type } => PropertyTagDataPartial::Set {
                key_type: key_type.into_partial().into(),
            },
            Self::Map {
                key_type,
                value_type,
            } => PropertyTagDataPartial::Map {
                key_type: key_type.into_partial().into(),
                value_type: value_type.into_partial().into(),
            },
            Self::Byte(a) => PropertyTagDataPartial::Byte(a),
            Self::Enum(a, b) => PropertyTagDataPartial::Enum(a, b),
            Self::Bool(_) => PropertyTagDataPartial::Other(PropertyType::BoolProperty),
            Self::Other(t) => PropertyTagDataPartial::Other(t),
        }
    }
}
impl PropertyTagDataPartial {
    fn into_full(self, prop: &Property) -> PropertyTagDataFull {
        match self {
            Self::Array(inner) => PropertyTagDataFull::Array(inner.into_full(prop).into()),
            Self::Struct { struct_type, id } => PropertyTagDataFull::Struct { struct_type, id },
            Self::Set { key_type } => PropertyTagDataFull::Set {
                key_type: key_type.into_full(prop).into(),
            },
            Self::Map {
                key_type,
                value_type,
            } => PropertyTagDataFull::Map {
                key_type: key_type.into_full(prop).into(),
                value_type: value_type.into_full(prop).into(),
            },
            Self::Byte(a) => PropertyTagDataFull::Byte(a),
            Self::Enum(a, b) => PropertyTagDataFull::Enum(a, b),
            Self::Other(PropertyType::BoolProperty) => {
                PropertyTagDataFull::Bool(match prop.inner {
                    PropertyInner::Bool(value) => value,
                    _ => false,
                })
            }
            Self::Other(t) => PropertyTagDataFull::Other(t),
        }
    }
}

impl PropertyTagDataFull {
    fn basic_type(&self) -> PropertyType {
        match self {
            Self::Array(_) => PropertyType::ArrayProperty,
            Self::Struct { .. } => PropertyType::StructProperty,
            Self::Set { .. } => PropertyType::SetProperty,
            Self::Map { .. } => PropertyType::MapProperty,
            Self::Byte(_) => PropertyType::ByteProperty,
            Self::Enum(_, _) => PropertyType::EnumProperty,
            Self::Bool(_) => PropertyType::BoolProperty,
            Self::Other(property_type) => *property_type,
        }
    }
    fn from_type(inner_type: PropertyType, struct_type: Option<StructType>) -> Self {
        match inner_type {
            PropertyType::BoolProperty => Self::Bool(false),
            PropertyType::ByteProperty => Self::Byte(None),
            PropertyType::EnumProperty => Self::Enum("".to_string(), None),
            PropertyType::ArrayProperty => unreachable!("array of array is invalid"),
            PropertyType::SetProperty => unreachable!("array of set is invalid"),
            PropertyType::MapProperty => unreachable!("array of map is invalid"),
            PropertyType::StructProperty => Self::Struct {
                struct_type: struct_type.unwrap_or(StructType::Struct(None)),
                id: Default::default(),
            },
            other => Self::Other(other),
        }
    }
}
bitflags::bitflags! {
    #[derive(Debug, Clone, Copy)]
    struct EPropertyTagFlags : u8 {
        const None = 0x00;
        const HasArrayIndex = 0x01;
        const HasPropertyGuid = 0x02;
        const HasPropertyExtensions = 0x04;
        const HasBinaryOrNativeSerialize = 0x08;
        const BoolTrue = 0x10;
    }
}
impl PropertyTagPartial {
    fn into_full<'a>(
        self,
        name: &'a str,
        size: u32,
        index: u32,
        prop: &Property,
    ) -> PropertyTagFull<'a> {
        PropertyTagFull {
            name: name.into(),
            id: self.id,
            size,
            index,
            data: self.data.into_full(prop),
        }
    }
}
impl PropertyTagFull<'_> {
    fn into_full(self) -> PropertyTagPartial {
        PropertyTagPartial {
            id: self.id,
            data: self.data.into_partial(),
        }
    }
    fn read<R: Read + Seek>(reader: &mut Context<R>) -> TResult<Option<Self>> {
        let name = read_string(reader)?;
        if name == "None" {
            return Ok(None);
        }
        if reader.unwrap_header().property_tag() {
            let data = read_type(reader)?;

            let mut tag = Self {
                name: name.into(),
                size: 0,
                index: 0,
                id: None,
                data,
            };

            #[derive(Default, Debug)]
            struct Node {
                name: String,
                inner_count: u32,
            }
            fn read_node<R: Read + Seek>(reader: &mut Context<R>) -> TResult<Node> {
                Ok(Node {
                    name: read_string(reader)?,
                    inner_count: reader.read_u32::<LE>()?,
                })
            }
            fn read_path<R: Read + Seek>(reader: &mut Context<R>) -> TResult<String> {
                let name = read_node(reader)?;
                assert_eq!(1, name.inner_count);
                let package = read_node(reader)?;
                assert_eq!(0, package.inner_count);
                Ok(format!("{}.{}", package.name, name.name))
            }
            fn read_type<R: Read + Seek>(reader: &mut Context<R>) -> TResult<PropertyTagDataFull> {
                let node = read_node(reader)?;
                Ok(match node.name.as_str() {
                    "ArrayProperty" => PropertyTagDataFull::Array(read_type(reader)?.into()),
                    "StructProperty" => {
                        let struct_type = StructType::from_full(&read_path(reader)?);
                        let id = match node.inner_count {
                            1 => Default::default(),
                            2 => uuid::Uuid::parse_str(&read_node(reader)?.name)?,
                            _ => unimplemented!(),
                        };
                        PropertyTagDataFull::Struct { struct_type, id }
                    }
                    "SetProperty" => PropertyTagDataFull::Set {
                        key_type: read_type(reader)?.into(),
                    },
                    "MapProperty" => PropertyTagDataFull::Map {
                        key_type: read_type(reader)?.into(),
                        value_type: read_type(reader)?.into(),
                    },
                    "ByteProperty" => {
                        let inner = match node.inner_count {
                            0 => None,
                            1 => Some(read_path(reader)?),
                            _ => unimplemented!(),
                        };
                        PropertyTagDataFull::Byte(inner)
                    }
                    "EnumProperty" => {
                        assert_eq!(2, node.inner_count);
                        let inner = read_path(reader)?;
                        let container = read_node(reader)?;
                        assert_eq!(0, container.inner_count);
                        PropertyTagDataFull::Enum(inner, Some(container.name.to_owned()))
                    }
                    "BoolProperty" => PropertyTagDataFull::Bool(false),
                    other => {
                        assert_eq!(0, node.inner_count);
                        PropertyTagDataFull::Other(PropertyType::try_from(other)?)
                    }
                })
            }

            tag.size = reader.read_u32::<LE>()?;

            let flags = EPropertyTagFlags::from_bits(reader.read_u8()?)
                .ok_or_else(|| error::Error::Other("unknown EPropertyTagFlags bits".into()))?;

            if flags.contains(EPropertyTagFlags::BoolTrue) {
                if let PropertyTagDataFull::Bool(value) = &mut tag.data {
                    *value = true
                }
            }
            if flags.contains(EPropertyTagFlags::HasArrayIndex) {
                tag.index = reader.read_u32::<LE>()?;
            }
            if flags.contains(EPropertyTagFlags::HasPropertyGuid) {
                tag.id = Some(uuid::Uuid::read(reader)?);
            }
            if flags.contains(EPropertyTagFlags::HasPropertyExtensions) {
                unimplemented!();
            }

            Ok(Some(tag))
        } else {
            reader.scope(&name.clone(), |reader| {
                let type_ = PropertyType::read(reader)?;
                let size = reader.read_u32::<LE>()?;
                let index = reader.read_u32::<LE>()?;
                let id;
                let data = match type_ {
                    PropertyType::BoolProperty => {
                        let value = reader.read_u8()? > 0;
                        id = read_optional_uuid(reader)?;
                        PropertyTagDataFull::Bool(value)
                    }
                    PropertyType::IntProperty
                    | PropertyType::Int8Property
                    | PropertyType::Int16Property
                    | PropertyType::Int64Property
                    | PropertyType::UInt8Property
                    | PropertyType::UInt16Property
                    | PropertyType::UInt32Property
                    | PropertyType::UInt64Property
                    | PropertyType::FloatProperty
                    | PropertyType::DoubleProperty
                    | PropertyType::StrProperty
                    | PropertyType::ObjectProperty
                    | PropertyType::FieldPathProperty
                    | PropertyType::SoftObjectProperty
                    | PropertyType::NameProperty
                    | PropertyType::TextProperty
                    | PropertyType::DelegateProperty
                    | PropertyType::MulticastDelegateProperty
                    | PropertyType::MulticastInlineDelegateProperty
                    | PropertyType::MulticastSparseDelegateProperty => {
                        id = read_optional_uuid(reader)?;
                        PropertyTagDataFull::Other(type_)
                    }
                    PropertyType::ByteProperty => {
                        let enum_type = read_string(reader)?;
                        id = read_optional_uuid(reader)?;
                        PropertyTagDataFull::Byte((enum_type != "None").then_some(enum_type))
                    }
                    PropertyType::EnumProperty => {
                        let enum_type = read_string(reader)?;
                        id = read_optional_uuid(reader)?;
                        PropertyTagDataFull::Enum(enum_type, None)
                    }
                    PropertyType::ArrayProperty => {
                        let inner_type = PropertyType::read(reader)?;
                        id = read_optional_uuid(reader)?;

                        PropertyTagDataFull::Array(std::boxed::Box::new(
                            PropertyTagDataFull::from_type(inner_type, None),
                        ))
                    }
                    PropertyType::SetProperty => {
                        let key_type = PropertyType::read(reader)?;
                        let key_struct_type = match key_type {
                            PropertyType::StructProperty => {
                                Some(reader.get_type_or(&StructType::Guid)?.clone())
                            }
                            _ => None,
                        };
                        id = read_optional_uuid(reader)?;

                        let key_type =
                            PropertyTagDataFull::from_type(key_type, key_struct_type.clone())
                                .into();

                        PropertyTagDataFull::Set { key_type }
                    }
                    PropertyType::MapProperty => {
                        let key_type = PropertyType::read(reader)?;
                        let key_struct_type = match key_type {
                            PropertyType::StructProperty => Some(
                                reader
                                    .scope("Key", |r| r.get_type_or(&StructType::Guid))?
                                    .clone(),
                            ),
                            _ => None,
                        };
                        let value_type = PropertyType::read(reader)?;
                        let value_struct_type = match value_type {
                            PropertyType::StructProperty => Some(
                                reader
                                    .scope("Value", |r| r.get_type_or(&StructType::Struct(None)))?
                                    .clone(),
                            ),
                            _ => None,
                        };
                        id = read_optional_uuid(reader)?;

                        let key_type =
                            PropertyTagDataFull::from_type(key_type, key_struct_type.clone())
                                .into();
                        let value_type =
                            PropertyTagDataFull::from_type(value_type, value_struct_type.clone())
                                .into();

                        PropertyTagDataFull::Map {
                            key_type,
                            value_type,
                        }
                    }
                    PropertyType::StructProperty => {
                        let struct_type = StructType::read(reader)?;
                        let struct_id = uuid::Uuid::read(reader)?;
                        id = read_optional_uuid(reader)?;
                        PropertyTagDataFull::Struct {
                            struct_type,
                            id: struct_id,
                        }
                    }
                };
                Ok(Some(Self {
                    name: name.into(),
                    size,
                    index,
                    id,
                    data,
                }))
            })
        }
    }
    fn write<W: Write>(&self, writer: &mut Context<W>) -> TResult<()> {
        write_string(writer, &self.name)?;

        if writer.unwrap_header().property_tag() {
            fn write_node<W: Write>(
                writer: &mut Context<W>,
                name: &str,
                inner_count: u32,
            ) -> TResult<()> {
                write_string(writer, name)?;
                writer.write_u32::<LE>(inner_count)?;
                Ok(())
            }
            fn write_full_type<W: Write>(writer: &mut Context<W>, full_type: &str) -> TResult<()> {
                let (a, b) = full_type.split_once('.').unwrap(); // TODO
                write_node(writer, b, 1)?;
                write_node(writer, a, 0)?;
                Ok(())
            }
            fn write_nodes<W: Write>(
                writer: &mut Context<W>,
                flags: &mut EPropertyTagFlags,
                data: &PropertyTagDataFull,
            ) -> TResult<()> {
                match data {
                    PropertyTagDataFull::Array(inner) => {
                        write_node(writer, "ArrayProperty", 1)?;
                        write_nodes(writer, flags, inner)?;
                    }
                    PropertyTagDataFull::Struct { struct_type, id } => {
                        write_node(writer, "StructProperty", if id.is_nil() { 1 } else { 2 })?;
                        match struct_type {
                            StructType::Struct(Some(_)) => {}
                            _ => *flags |= EPropertyTagFlags::HasBinaryOrNativeSerialize,
                        }
                        write_full_type(writer, struct_type.full_str())?;

                        if !id.is_nil() {
                            write_node(writer, &id.to_string(), 0)?;
                        }
                    }
                    PropertyTagDataFull::Set { key_type } => {
                        write_node(writer, "SetProperty", 1)?;
                        write_nodes(writer, flags, key_type)?;
                    }
                    PropertyTagDataFull::Map {
                        key_type,
                        value_type,
                    } => {
                        write_node(writer, "MapProperty", 2)?;
                        write_nodes(writer, flags, key_type)?;
                        write_nodes(writer, flags, value_type)?;
                    }
                    PropertyTagDataFull::Byte(enum_type) => {
                        write_node(
                            writer,
                            "ByteProperty",
                            if enum_type.is_some() { 1 } else { 0 },
                        )?;
                        if let Some(enum_type) = enum_type {
                            write_full_type(writer, enum_type)?;
                        }
                    }
                    PropertyTagDataFull::Enum(enum_type, container) => {
                        write_node(writer, "EnumProperty", 2)?;
                        write_full_type(writer, enum_type)?;
                        write_node(writer, container.as_ref().unwrap(), 0)?;
                    }
                    PropertyTagDataFull::Bool(value) => {
                        if *value {
                            *flags |= EPropertyTagFlags::BoolTrue;
                        }
                        write_node(writer, "BoolProperty", 0)?;
                    }
                    PropertyTagDataFull::Other(property_type) => {
                        write_node(writer, property_type.get_name(), 0)?;
                    }
                }
                Ok(())
            }

            let mut flags = EPropertyTagFlags::empty();
            write_nodes(writer, &mut flags, &self.data)?;

            writer.write_u32::<LE>(self.size)?;

            if self.id.is_some() {
                flags |= EPropertyTagFlags::HasPropertyGuid;
            }

            writer.write_u8(flags.bits())?;
        } else {
            self.data.basic_type().write(writer)?;
            writer.write_u32::<LE>(self.size)?;
            writer.write_u32::<LE>(self.index)?;
            match &self.data {
                PropertyTagDataFull::Array(inner_type) => {
                    inner_type.basic_type().write(writer)?;
                }
                PropertyTagDataFull::Struct { struct_type, id } => {
                    struct_type.write(writer)?;
                    id.write(writer)?;
                }
                PropertyTagDataFull::Set { key_type, .. } => {
                    key_type.basic_type().write(writer)?;
                }
                PropertyTagDataFull::Map {
                    key_type,
                    value_type,
                    ..
                } => {
                    key_type.basic_type().write(writer)?;
                    value_type.basic_type().write(writer)?;
                }
                PropertyTagDataFull::Byte(enum_type) => {
                    write_string(writer, enum_type.as_deref().unwrap_or("None"))?;
                }
                PropertyTagDataFull::Enum(enum_type, _) => {
                    write_string(writer, enum_type)?;
                }
                PropertyTagDataFull::Bool(value) => {
                    writer.write_u8(*value as u8)?;
                }
                PropertyTagDataFull::Other(_) => {}
            }
            write_optional_uuid(writer, self.id)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
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
        Self::try_from(&read_string(reader)?)
    }
    fn try_from(name: &str) -> TResult<Self> {
        match name {
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
            _ => Err(Error::UnknownPropertyType(format!("{name:?}"))),
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
    fn from_full(t: &str) -> Self {
        match t {
            "/Script/CureUObject.Guid" => StructType::Guid,
            "/Script/CoreUObject.DateTime" => StructType::DateTime,
            "/Script/CoreUObject.Timespan" => StructType::Timespan,
            "/Script/CureUObject.Vector2D" => StructType::Vector2D,
            "/Script/CoreUObject.Vector" => StructType::Vector,
            "/Script/CureUObject.IntVector" => StructType::IntVector,
            "/Script/CoreUobject.Box" => StructType::Box,
            "/Script/CoreUobject.IntPoint" => StructType::IntPoint,
            "/Script/CoreUobject.Quat" => StructType::Quat,
            "/Script/CoreUObject.Rotator" => StructType::Rotator,
            "/Script/CoreUobject.LinearColor" => StructType::LinearColor,
            "/Script/CoreUobject.Color" => StructType::Color,
            "/Script/CoreUobject.SoftObjectPath" => StructType::SoftObjectPath,
            "/Script/GameplayTags.GameplayTagContainer" => StructType::GameplayTagContainer,
            "/Script/CoreUobject.UniqueNetIdRepl" => StructType::UniqueNetIdRepl,
            "/Script/CoreUobject.Struct" => StructType::Struct(None),
            _ => StructType::Struct(Some(t.to_owned())),
        }
    }
    fn full_str(&self) -> &str {
        match self {
            StructType::Guid => "/Script/CoreUobject.Guid",
            StructType::DateTime => "/Script/CoreUObject.DateTime",
            StructType::Timespan => "/Script/CoreUObject.Timespan",
            StructType::Vector2D => "/Script/CoreUobject.Vector2D",
            StructType::Vector => "/Script/CoreUObject.Vector",
            StructType::IntVector => "/Script/CoreUobject.IntVector",
            StructType::Box => "/Script/CoreUobject.Box",
            StructType::IntPoint => "/Script/CoreUobject.IntPoint",
            StructType::Quat => "/Script/CoreUobject.Quat",
            StructType::Rotator => "/Script/CoreUObject.Rotator",
            StructType::LinearColor => "/Script/CoreUobject.LinearColor",
            StructType::Color => "/Script/CoreUobject.Color",
            StructType::SoftObjectPath => "/Script/CoreUobject.SoftObjectPath",
            StructType::GameplayTagContainer => "/Script/GameplayTags.GameplayTagContainer",
            StructType::UniqueNetIdRepl => "/Script/CoreUobject.UniqueNetIdRepl",
            StructType::Struct(Some(t)) => t,
            _ => unreachable!(),
        }
    }
    fn as_str(&self) -> &str {
        match self {
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
        }
    }
    fn read<R: Read + Seek>(reader: &mut Context<R>) -> TResult<Self> {
        Ok(read_string(reader)?.into())
    }
    fn write<W: Write>(&self, writer: &mut Context<W>) -> TResult<()> {
        write_string(writer, self.as_str())?;
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
        key_type: &PropertyTagDataFull,
        value_type: &PropertyTagDataFull,
    ) -> TResult<MapEntry> {
        let key = PropertyValue::read(reader, key_type)?;
        let value = PropertyValue::read(reader, value_type)?;
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
        if reader.unwrap_header().large_world_coordinates() {
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
        if writer.unwrap_header().large_world_coordinates() {
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
        if reader.unwrap_header().large_world_coordinates() {
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
        if writer.unwrap_header().large_world_coordinates() {
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
        if reader.unwrap_header().large_world_coordinates() {
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
        if writer.unwrap_header().large_world_coordinates() {
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
        if reader.unwrap_header().large_world_coordinates() {
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
        if writer.unwrap_header().large_world_coordinates() {
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
            .state
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
        type_: PropertyType,
        struct_type: StructType,
        id: Option<uuid::Uuid>,
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
        t: &PropertyTagDataFull,
    ) -> TResult<PropertyValue> {
        Ok(match t {
            PropertyTagDataFull::Array(_) => unreachable!(),
            PropertyTagDataFull::Struct { struct_type, .. } => {
                PropertyValue::Struct(StructValue::read(reader, struct_type)?)
            }
            PropertyTagDataFull::Set { .. } => unreachable!(),
            PropertyTagDataFull::Map { .. } => unreachable!(),
            PropertyTagDataFull::Byte(_) => PropertyValue::Byte(Byte::Label(read_string(reader)?)),
            PropertyTagDataFull::Enum(_, _) => PropertyValue::Enum(read_string(reader)?),
            PropertyTagDataFull::Bool(_) => PropertyValue::Bool(reader.read_u8()? > 0),
            PropertyTagDataFull::Other(property_type) => match property_type {
                PropertyType::IntProperty => PropertyValue::Int(reader.read_i32::<LE>()?),
                PropertyType::Int8Property => PropertyValue::Int8(reader.read_i8()?),
                PropertyType::Int16Property => PropertyValue::Int16(reader.read_i16::<LE>()?),
                PropertyType::Int64Property => PropertyValue::Int64(reader.read_i64::<LE>()?),
                PropertyType::UInt16Property => PropertyValue::UInt16(reader.read_u16::<LE>()?),
                PropertyType::UInt32Property => PropertyValue::UInt32(reader.read_u32::<LE>()?),
                PropertyType::FloatProperty => PropertyValue::Float(reader.read_f32::<LE>()?),
                PropertyType::DoubleProperty => PropertyValue::Double(reader.read_f64::<LE>()?),
                PropertyType::NameProperty => PropertyValue::Name(read_string(reader)?),
                PropertyType::StrProperty => PropertyValue::Str(read_string(reader)?),
                PropertyType::SoftObjectProperty => {
                    PropertyValue::SoftObject(read_string(reader)?, read_string(reader)?)
                }
                PropertyType::ObjectProperty => PropertyValue::Object(read_string(reader)?),
                _ => return Err(Error::Other(format!("unimplemented property {t:?}"))),
            },
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
        tag: PropertyTagDataFull,
        size: u32,
    ) -> TResult<ValueArray> {
        let count = reader.read_u32::<LE>()?;
        Ok(match tag {
            PropertyTagDataFull::Struct { struct_type, id } => {
                let (struct_type, id) = if !reader.unwrap_header().property_tag() {
                    let tag = PropertyTagFull::read(reader)?.unwrap();
                    match tag.data {
                        PropertyTagDataFull::Struct { struct_type, id } => (struct_type, id),
                        _ => {
                            return Err(Error::Other(format!(
                                "expected StructProperty tag, found {tag:?}"
                            )))
                        }
                    }
                } else {
                    (struct_type, id)
                };

                let mut value = vec![];
                for _ in 0..count {
                    value.push(StructValue::read(reader, &struct_type)?);
                }
                ValueArray::Struct {
                    type_: PropertyType::StructProperty,
                    struct_type,
                    id: Some(id),
                    value,
                }
            }
            _ => ValueArray::Base(ValueVec::read(reader, &tag.basic_type(), size, count)?),
        })
    }
    fn write<W: Write>(&self, writer: &mut Context<W>, tag: &PropertyTagFull) -> TResult<()> {
        match &self {
            ValueArray::Struct {
                type_,
                struct_type,
                id,
                value,
            } => {
                writer.write_u32::<LE>(value.len() as u32)?;

                let mut buf = vec![];
                for v in value {
                    writer.stream(&mut buf, |writer| v.write(writer))?;
                }

                if !writer.unwrap_header().property_tag() {
                    write_string(writer, &tag.name)?;
                    type_.write(writer)?;
                    writer.write_u32::<LE>(buf.len() as u32)?;
                    writer.write_u32::<LE>(0)?;
                    struct_type.write(writer)?;
                    if let Some(id) = id {
                        id.write(writer)?;
                    }
                    writer.write_u8(0)?;
                }
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
        t: &PropertyTagDataFull,
        size: u32,
    ) -> TResult<ValueSet> {
        let count = reader.read_u32::<LE>()?;
        Ok(match t {
            PropertyTagDataFull::Struct { struct_type, .. } => {
                ValueSet::Struct(read_array(count, reader, |r| {
                    StructValue::read(r, struct_type)
                })?)
            }
            _ => ValueSet::Base(ValueVec::read(reader, &t.basic_type(), size, count)?),
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
    pub tag: PropertyTagPartial,
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
    Byte(Byte),
    Enum(Enum),
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
    Set(ValueSet),
    Map(Vec<MapEntry>),
    Struct(StructValue),
    Array(ValueArray),
}

impl Property {
    fn read<R: Read + Seek>(reader: &mut Context<R>, tag: PropertyTagFull) -> TResult<Property> {
        let inner = match &tag.data {
            PropertyTagDataFull::Bool(value) => PropertyInner::Bool(*value),
            PropertyTagDataFull::Byte(ref enum_type) => {
                let value = if enum_type.is_none() {
                    Byte::Byte(reader.read_u8()?)
                } else {
                    Byte::Label(read_string(reader)?)
                };
                PropertyInner::Byte(value)
            }
            PropertyTagDataFull::Enum { .. } => PropertyInner::Enum(read_string(reader)?),
            PropertyTagDataFull::Set { key_type } => {
                reader.read_u32::<LE>()?;
                PropertyInner::Set(ValueSet::read(reader, key_type, tag.size - 8)?)
            }
            PropertyTagDataFull::Map {
                key_type,
                value_type,
            } => {
                reader.read_u32::<LE>()?;
                let count = reader.read_u32::<LE>()?;
                let mut value = vec![];

                for _ in 0..count {
                    value.push(MapEntry::read(reader, key_type, value_type)?)
                }

                PropertyInner::Map(value)
            }
            PropertyTagDataFull::Struct { struct_type, .. } => {
                PropertyInner::Struct(StructValue::read(reader, struct_type)?)
            }
            PropertyTagDataFull::Array(data) => {
                PropertyInner::Array(ValueArray::read(reader, *data.clone(), tag.size - 4)?)
            }
            PropertyTagDataFull::Other(t) => match t {
                PropertyType::BoolProperty
                | PropertyType::ByteProperty
                | PropertyType::EnumProperty
                | PropertyType::SetProperty
                | PropertyType::MapProperty
                | PropertyType::StructProperty
                | PropertyType::ArrayProperty => unreachable!(),
                PropertyType::Int8Property => PropertyInner::Int8(reader.read_i8()?),
                PropertyType::Int16Property => PropertyInner::Int16(reader.read_i16::<LE>()?),
                PropertyType::IntProperty => PropertyInner::Int(reader.read_i32::<LE>()?),
                PropertyType::Int64Property => PropertyInner::Int64(reader.read_i64::<LE>()?),
                PropertyType::UInt8Property => PropertyInner::UInt8(reader.read_u8()?),
                PropertyType::UInt16Property => PropertyInner::UInt16(reader.read_u16::<LE>()?),
                PropertyType::UInt32Property => PropertyInner::UInt32(reader.read_u32::<LE>()?),
                PropertyType::UInt64Property => PropertyInner::UInt64(reader.read_u64::<LE>()?),
                PropertyType::FloatProperty => PropertyInner::Float(reader.read_f32::<LE>()?),
                PropertyType::DoubleProperty => PropertyInner::Double(reader.read_f64::<LE>()?),
                PropertyType::NameProperty => PropertyInner::Name(read_string(reader)?),
                PropertyType::StrProperty => PropertyInner::Str(read_string(reader)?),
                PropertyType::FieldPathProperty => {
                    PropertyInner::FieldPath(FieldPath::read(reader)?)
                }
                PropertyType::SoftObjectProperty => {
                    PropertyInner::SoftObject(SoftObjectPath::read(reader)?)
                }
                PropertyType::ObjectProperty => PropertyInner::Object(read_string(reader)?),
                PropertyType::TextProperty => PropertyInner::Text(Text::read(reader)?),
                PropertyType::DelegateProperty => PropertyInner::Delegate(Delegate::read(reader)?),
                PropertyType::MulticastDelegateProperty => {
                    PropertyInner::MulticastDelegate(MulticastDelegate::read(reader)?)
                }
                PropertyType::MulticastInlineDelegateProperty => {
                    PropertyInner::MulticastInlineDelegate(MulticastInlineDelegate::read(reader)?)
                }
                PropertyType::MulticastSparseDelegateProperty => {
                    PropertyInner::MulticastSparseDelegate(MulticastSparseDelegate::read(reader)?)
                }
            },
        };
        Ok(Property {
            tag: tag.into_full(),
            inner,
        })
    }
    fn write<W: Write>(&self, writer: &mut Context<W>, tag: &PropertyTagFull) -> TResult<usize> {
        Ok(match &self.inner {
            PropertyInner::Int8(value) => {
                writer.write_i8(*value)?;
                1
            }
            PropertyInner::Int16(value) => {
                writer.write_i16::<LE>(*value)?;
                2
            }
            PropertyInner::Int(value) => {
                writer.write_i32::<LE>(*value)?;
                4
            }
            PropertyInner::Int64(value) => {
                writer.write_i64::<LE>(*value)?;
                8
            }
            PropertyInner::UInt8(value) => {
                writer.write_u8(*value)?;
                1
            }
            PropertyInner::UInt16(value) => {
                writer.write_u16::<LE>(*value)?;
                2
            }
            PropertyInner::UInt32(value) => {
                writer.write_u32::<LE>(*value)?;
                4
            }
            PropertyInner::UInt64(value) => {
                writer.write_u64::<LE>(*value)?;
                8
            }
            PropertyInner::Float(value) => {
                writer.write_f32::<LE>(*value)?;
                4
            }
            PropertyInner::Double(value) => {
                writer.write_f64::<LE>(*value)?;
                8
            }
            PropertyInner::Bool(_) => 0,
            PropertyInner::Byte(value) => match value {
                Byte::Byte(b) => {
                    writer.write_u8(*b)?;
                    1
                }
                Byte::Label(l) => {
                    write_string(writer, l)?;
                    l.len() + 5
                }
            },
            PropertyInner::Enum(value) => {
                write_string(writer, value)?;
                value.len() + 5
            }
            PropertyInner::Name(value) => {
                let mut buf = vec![];
                writer.stream(&mut buf, |writer| write_string(writer, value))?;
                writer.write_all(&buf)?;
                buf.len()
            }
            PropertyInner::Str(value) => {
                let mut buf = vec![];
                writer.stream(&mut buf, |writer| write_string(writer, value))?;
                writer.write_all(&buf)?;
                buf.len()
            }
            PropertyInner::FieldPath(value) => {
                let mut buf = vec![];
                writer.stream(&mut buf, |writer| value.write(writer))?;
                writer.write_all(&buf)?;
                buf.len()
            }
            PropertyInner::SoftObject(value) => {
                let mut buf = vec![];
                writer.stream(&mut buf, |writer| value.write(writer))?;
                writer.write_all(&buf)?;
                buf.len()
            }
            PropertyInner::Object(value) => {
                let mut buf = vec![];
                writer.stream(&mut buf, |writer| write_string(writer, value))?;
                writer.write_all(&buf)?;
                buf.len()
            }
            PropertyInner::Text(value) => {
                let mut buf = vec![];
                writer.stream(&mut buf, |writer| value.write(writer))?;
                writer.write_all(&buf)?;
                buf.len()
            }
            PropertyInner::Delegate(value) => {
                let mut buf = vec![];
                writer.stream(&mut buf, |writer| value.write(writer))?;
                writer.write_all(&buf)?;
                buf.len()
            }
            PropertyInner::MulticastDelegate(value) => {
                let mut buf = vec![];
                writer.stream(&mut buf, |writer| value.write(writer))?;
                writer.write_all(&buf)?;
                buf.len()
            }
            PropertyInner::MulticastInlineDelegate(value) => {
                let mut buf = vec![];
                writer.stream(&mut buf, |writer| value.write(writer))?;
                writer.write_all(&buf)?;
                buf.len()
            }
            PropertyInner::MulticastSparseDelegate(value) => {
                let mut buf = vec![];
                writer.stream(&mut buf, |writer| value.write(writer))?;
                writer.write_all(&buf)?;
                buf.len()
            }
            PropertyInner::Set(value) => {
                let mut buf = vec![];
                buf.write_u32::<LE>(0)?;
                writer.stream(&mut buf, |writer| value.write(writer))?;
                writer.write_all(&buf)?;
                buf.len()
            }
            PropertyInner::Map(value) => {
                let mut buf = vec![];
                buf.write_u32::<LE>(0)?;
                buf.write_u32::<LE>(value.len() as u32)?;
                for v in value {
                    writer.stream(&mut buf, |writer| v.write(writer))?;
                }
                writer.write_all(&buf)?;
                buf.len()
            }
            PropertyInner::Struct(value) => {
                let mut buf = vec![];
                writer.stream(&mut buf, |writer| value.write(writer))?;
                writer.write_all(&buf)?;
                buf.len()
            }
            PropertyInner::Array(value) => {
                let mut buf = vec![];
                writer.stream(&mut buf, |writer| value.write(writer, tag))?;
                writer.write_all(&buf)?;
                buf.len()
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
    fn property_tag(&self) -> bool {
        self.engine_version_major >= 5 && self.engine_version_minor >= 4
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
        let save_game_type = read_string(reader)?;
        if reader.unwrap_header().property_tag() {
            reader.read_u8()?;
        }
        let properties = read_properties_until_none(reader)?;
        Ok(Self {
            save_game_type,
            properties,
        })
    }
    fn write<W: Write>(&self, writer: &mut Context<W>) -> TResult<()> {
        write_string(writer, &self.save_game_type)?;
        if writer.unwrap_header().property_tag() {
            writer.write_u8(0)?;
        }
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