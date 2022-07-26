use std::fs::File;
use std::io::{Cursor, Read, Write};
use byteorder::{BigEndian, LittleEndian, ReadBytesExt, WriteBytesExt};

use anyhow::Result;

type Reader<'a> = Cursor<&'a[u8]>;
type Writer<'a> = Vec<u8>;

pub trait Readable {
    fn read(reader: &mut Reader) -> Result<Self> where Self: Sized;
}
pub trait Writable {
    fn write(&self, writer: &mut Writer) -> Result<()>;
}

fn read_optional_uuid(reader: &mut Reader) -> Result<Option<uuid::Uuid>> {
    Ok(if reader.read_u8()? > 0 {
        Some(uuid::Uuid::read(reader)?)
    } else {
        None
    })
}

fn read_string(reader: &mut Reader) -> Result<String> {
    let mut chars = vec![0; reader.read_u32::<LittleEndian>()? as usize - 1];
    reader.read(&mut chars)?;
    reader.read_u8()?;
    Ok(String::from_utf8_lossy(&chars).into_owned())
}
fn write_string(writer: &mut Writer, string: &String) -> Result<()> {
    writer.write_u32::<LittleEndian>(string.as_bytes().len() as u32 + 1)?;
    writer.write(string.as_bytes())?;
    writer.write_u8(0)?;
    Ok(())
}
fn read_properties_until_none(reader: &mut Reader) -> Result<Vec<Property>> {
    let mut properties = vec![];
    loop {
        if let Some(prop) = read_property(reader)? {
            properties.push(prop);
        } else {
            break
        }
    }
    Ok(properties)
}
fn read_array<T>(length: u32, reader: &mut Reader, f: fn(&mut Reader) -> Result<T>) -> Result<Vec<T>> {
    (0..length).map(|_| -> Result<T> {
        Ok(f(reader)?)
    }).collect()
}

impl Readable for uuid::Uuid {
    fn read(reader: &mut Reader) -> Result<uuid::Uuid> {
        let mut buf = [0; 16];
        reader.read(&mut buf)?;
        Ok(uuid::Uuid::from_bytes(buf))
    }
}
impl Writable for uuid::Uuid {
    fn write(&self, writer: &mut Writer) -> Result<()> {
        writer.write(self.as_bytes())?;
        Ok(())
    }
}

fn read_type(reader: &mut Reader) -> Result<PropertyType> {
    let t = read_string(reader)?;
    match t.as_str() {
        "Guid" => Ok(PropertyType::Guid),
        "DateTime" => Ok(PropertyType::DateTime),
        "Box" => Ok(PropertyType::Box),
        "Vector2D" => Ok(PropertyType::Vector2D),
        "Vector" => Ok(PropertyType::Vector),
        "Quat" => Ok(PropertyType::Quat),
        "Rotator" => Ok(PropertyType::Rotator),
        "LinearColor" => Ok(PropertyType::LinearColor),
        "IntProperty" => Ok(PropertyType::IntProperty),
        "UInt32Property" => Ok(PropertyType::UInt32Property),
        "FloatProperty" => Ok(PropertyType::FloatProperty),
        "BoolProperty" => Ok(PropertyType::BoolProperty),
        "ByteProperty" => Ok(PropertyType::ByteProperty),
        "StructProperty" => Ok(PropertyType::StructProperty),
        "ArrayProperty" => Ok(PropertyType::ArrayProperty),
        "ObjectProperty" => Ok(PropertyType::ObjectProperty),
        "StrProperty" => Ok(PropertyType::StrProperty),
        "NameProperty" => Ok(PropertyType::NameProperty),
        "TextProperty" => Ok(PropertyType::TextProperty),
        "MulticastInlineDelegateProperty" => Ok(PropertyType::MulticastInlineDelegateProperty),
        "SetProperty" => Ok(PropertyType::SetProperty),
        "MapProperty" => Ok(PropertyType::MapProperty),
        _ => Ok(PropertyType::Other(t)),
    }
}

#[derive(Debug,PartialEq,Eq)]
pub enum PropertyType {
    Guid,
    DateTime,
    Box,
    Vector2D,
    Vector,
    Quat,
    Rotator,
    LinearColor,
    IntProperty,
    UInt32Property,
    FloatProperty,
    BoolProperty,
    ByteProperty,
    StructProperty,
    ArrayProperty,
    ObjectProperty,
    StrProperty,
    NameProperty,
    TextProperty,
    MulticastInlineDelegateProperty,
    SetProperty,
    MapProperty,
    Other(String),
}

type Int = i32;
type UInt32 = u32;
type Float = f32;
type Bool = bool;
type Byte = String;
type StructKey = uuid::Uuid;
type StructValue = Vec<Property>;
type Object = String;
type Name = String;
type Str = String;
type Text = String; // TOOD: Special text voodoo
type Set = Vec<ValueKey>;
#[derive(Debug,PartialEq)]
pub struct MapEntry {
    key: ValueKey,
    value: ValueStruct,
}
impl MapEntry {
    fn read(key_type: &PropertyType, value_type: &PropertyType, reader: &mut Reader) -> Result<MapEntry> {
        Ok(Self {
            key: read_value_key(key_type, reader)?,
            value: read_struct_value(value_type, reader)?,
        })
    }
}

type Map = Vec<MapEntry>;

#[derive(Debug,PartialEq)]
pub struct MulticastInlineDelegate(Vec<MulticastInlineDelegateEntry>);
impl Readable for MulticastInlineDelegate {
    fn read(reader: &mut Reader) -> Result<Self> {
        Ok(Self(read_array(reader.read_u32::<LittleEndian>()?, reader, |r| Ok(MulticastInlineDelegateEntry::read(r)?))?))
    }
}

#[derive(Debug,PartialEq)]
pub struct MulticastInlineDelegateEntry {
    path: String,
    name: String,
}
impl Readable for MulticastInlineDelegateEntry {
    fn read(reader: &mut Reader) -> Result<Self> {
        Ok(Self {
            path: read_string(reader)?,
            name: read_string(reader)?,
        })
    }
}

// Base value to be extended
#[derive(Debug,PartialEq)]
pub enum ValueBase {
    Guid(uuid::Uuid),
    DateTime(u64),
    Int(i32),
    UInt32(u32),
    Float(f32),
    Bool(bool),
    Byte(String),
}

// Full values
#[derive(Debug,PartialEq)]
pub enum ValueStruct {
    Base(ValueBase),
    Struct(Vec<Property>),
}
fn read_value_base(t: &PropertyType, reader: &mut Reader) -> Result<ValueBase> {
    Ok(match t {
        PropertyType::Guid => ValueBase::Guid(uuid::Uuid::read(reader)?),
        PropertyType::DateTime => ValueBase::DateTime(reader.read_u64::<LittleEndian>()?),
        PropertyType::IntProperty => ValueBase::Int(reader.read_i32::<LittleEndian>()?),
        PropertyType::UInt32Property => ValueBase::UInt32(reader.read_u32::<LittleEndian>()?),
        PropertyType::FloatProperty => ValueBase::Float(reader.read_f32::<LittleEndian>()?),
        PropertyType::BoolProperty => ValueBase::Bool(reader.read_u8()? > 0),
        _ => panic!("Missing ValueBase: {t:#?}")
    })
}
fn read_struct_value(t: &PropertyType, reader: &mut Reader) -> Result<ValueStruct> {
    Ok(match t {
        PropertyType::Guid
            | PropertyType::DateTime
            | PropertyType::IntProperty
            | PropertyType::UInt32Property
            | PropertyType::FloatProperty
            | PropertyType::BoolProperty
            => ValueStruct::Base(read_value_base(t, reader)?),
        _ => ValueStruct::Struct(read_properties_until_none(reader)?)
    })
}
fn read_value_key(t: &PropertyType, reader: &mut Reader) -> Result<ValueKey> {
    Ok(match t {
        PropertyType::Guid
            | PropertyType::DateTime
            | PropertyType::IntProperty
            | PropertyType::UInt32Property
            | PropertyType::FloatProperty
            | PropertyType::BoolProperty
            => ValueKey::Base(read_value_base(t, reader)?),
        PropertyType::StructProperty => ValueKey::Struct(uuid::Uuid::read(reader)?),
        _ => panic!("Missing ValueKey: {t:#?}")
        //_ => ValueKey::Base(read_struct_value(t, reader)?)
    })
}

// Values used as keys for SetProperty and MapProperty
#[derive(Debug,PartialEq)]
pub enum ValueKey {
    Base(ValueBase),
    Struct(StructKey),
}

// Array of values used by ArrayProperty
#[derive(Debug,PartialEq)]
pub enum ValueArray {
    Int(Vec<Int>),
    UInt32(Vec<UInt32>),
    Float(Vec<Float>),
    Byte(Vec<Byte>),
    Str(Vec<String>),
    Name(Vec<String>),
    Object(Vec<String>),
    Struct {
        _type: String,
        name: String,
        struct_type: PropertyType,
        id: uuid::Uuid,
        value: Vec<ValueStruct>
    },
}

// Values with IDs present in the top level object and StructProperty
#[derive(Debug,PartialEq)]
pub enum PropertyMeta {
    Int {
        id: Option<uuid::Uuid>,
        value: Int,
    },
    UInt32 {
        id: Option<uuid::Uuid>,
        value: UInt32,
    },
    Float {
        id: Option<uuid::Uuid>,
        value: Float,
    },
    Bool {
        id: Option<uuid::Uuid>,
        value: Bool,
    },
    Byte {
        id: Option<uuid::Uuid>,
        value: Byte,
        enum_type: String,
    },
    Str {
        id: Option<uuid::Uuid>,
        value: String,
    },
    Name {
        id: Option<uuid::Uuid>,
        value: String,
    },
    Object {
        id: Option<uuid::Uuid>,
        value: String,
    },
    MulticastInlineDelegate {
        id: Option<uuid::Uuid>,
        value: MulticastInlineDelegate,
    },
    Set {
        id: Option<uuid::Uuid>,
        set_type: PropertyType,
        value: Vec<ValueKey>,
    },
    Map {
        id: Option<uuid::Uuid>,
        key_type: PropertyType,
        value_type: PropertyType,
        value: Vec<MapEntry>,
    },
    Struct {
        id: Option<uuid::Uuid>,
        value: ValueStruct,
        struct_type: PropertyType,
        struct_id: uuid::Uuid,
    },
    Array {
        id: Option<uuid::Uuid>,
        value: ValueArray,
    }
}

#[derive(Debug,PartialEq)]
pub struct Property {
    name: String,
    value: PropertyMeta,
}

fn read_property(reader: &mut Reader) -> Result<Option<Property>> {
    let name = read_string(reader)?;
    println!("read_property {name}");
    if name == "None" {
        Ok(None)
    } else {
        let t = read_type(reader)?;
        let _size = reader.read_u64::<LittleEndian>()?;
        println!("{name:#?} {t:#?} {_size:#?}");
        let value = read_property_meta(t, reader)?;
        Ok(Some(Property { name, value }))
    }
}

fn read_property_meta(t: PropertyType, reader: &mut Reader) -> Result<PropertyMeta> {
    match t {
        PropertyType::IntProperty => {
            Ok(PropertyMeta::Int {
                id: read_optional_uuid(reader)?,
                value: reader.read_i32::<LittleEndian>()?,
            })
        },
        PropertyType::UInt32Property => {
            Ok(PropertyMeta::UInt32 {
                id: read_optional_uuid(reader)?,
                value: reader.read_u32::<LittleEndian>()?,
            })
        },
        PropertyType::FloatProperty => {
            Ok(PropertyMeta::Float {
                id: read_optional_uuid(reader)?,
                value: reader.read_f32::<LittleEndian>()?,
            })
        },
        PropertyType::BoolProperty => {
            Ok(PropertyMeta::Bool {
                value: reader.read_u8()? > 0,
                id: read_optional_uuid(reader)?,
            })
        },
        PropertyType::ByteProperty => {
            Ok(PropertyMeta::Byte {
                id: read_optional_uuid(reader)?,
                enum_type: read_string(reader)?,
                value: read_string(reader)?,
            })
        },
        PropertyType::StrProperty => {
            Ok(PropertyMeta::Str {
                id: read_optional_uuid(reader)?,
                value: read_string(reader)?,
            })
        },
        PropertyType::ObjectProperty => {
            Ok(PropertyMeta::Str {
                id: read_optional_uuid(reader)?,
                value: read_string(reader)?,
            })
        },
        PropertyType::MulticastInlineDelegateProperty => {
            Ok(PropertyMeta::MulticastInlineDelegate {
                id: read_optional_uuid(reader)?,
                value: MulticastInlineDelegate::read(reader)?,
            })
        },
        PropertyType::SetProperty => {
            let set_type = read_type(reader)?;
            let id = read_optional_uuid(reader)?;
            let idk = reader.read_u32::<LittleEndian>()?;
            let count = reader.read_u32::<LittleEndian>()?;
            match set_type {
                PropertyType::StructProperty => {
                    Ok(PropertyMeta::Set {
                        id,
                        set_type,
                        value: read_array(count, reader, |r| Ok(ValueKey::Base(ValueBase::Guid(uuid::Uuid::read(r)?))))?
                    })
                },
                _ => panic!("{set_type:#?} not implemented for SetProperty")
            }
        },
        PropertyType::MapProperty => {
            let key_type = read_type(reader)?;
            let value_type = read_type(reader)?;
            let id = read_optional_uuid(reader)?;
            let idk = reader.read_u32::<LittleEndian>()?;
            let count = reader.read_u32::<LittleEndian>()?;
            let mut value = vec![];
            for _ in 0..count {
                value.push(MapEntry::read(&key_type, &value_type, reader)?)
            }
            println!("Key: {key_type:#?} Value: {value_type:#?}");
            Ok(PropertyMeta::Map {
                key_type,
                value_type,
                id,
                value,
            })
        },
        PropertyType::StructProperty => {
            let struct_type = read_type(reader)?;
            let struct_id = uuid::Uuid::read(reader)?;
            let id = read_optional_uuid(reader)?;
            println!("{struct_type:#?} {struct_id:#?} {id:#?}");
            let value = read_struct_value(&struct_type, reader)?;
            Ok(PropertyMeta::Struct {
                struct_type,
                struct_id,
                id,
                value,
            })
        },
        PropertyType::ArrayProperty => {
            let array_type = read_type(reader)?;
            let id = read_optional_uuid(reader)?;
            let count = reader.read_u32::<LittleEndian>()?;

            Ok(PropertyMeta::Array {
                id,
                value: match array_type {
                    PropertyType::IntProperty => ValueArray::Int(read_array(count, reader, |r| Ok(r.read_i32::<LittleEndian>()?))?),
                    PropertyType::UInt32Property => ValueArray::UInt32(read_array(count, reader, |r| Ok(r.read_u32::<LittleEndian>()?))?),
                    PropertyType::FloatProperty => ValueArray::Float(read_array(count, reader, |r| Ok(r.read_f32::<LittleEndian>()?))?),
                    PropertyType::ByteProperty => ValueArray::Byte(read_array(count, reader, |r| Ok(read_string(r)?))?),
                    PropertyType::StrProperty => ValueArray::Byte(read_array(count, reader, |r| Ok(read_string(r)?))?),
                    PropertyType::NameProperty => ValueArray::Byte(read_array(count, reader, |r| Ok(read_string(r)?))?),
                    PropertyType::ObjectProperty => ValueArray::Byte(read_array(count, reader, |r| Ok(read_string(r)?))?),
                    PropertyType::StructProperty => {
                        let _type = read_string(reader)?;
                        let name = read_string(reader)?;
                        let size = reader.read_u64::<LittleEndian>()?;
                        let struct_type = read_type(reader)?;
                        let id = uuid::Uuid::read(reader)?;
                        reader.read_u8()?;
                        let mut value = vec![];
                        for _ in 0..count {
                            value.push(read_struct_value(&struct_type, reader)?);
                        }
                        ValueArray::Struct {
                            _type,
                            name,
                            struct_type,
                            id,
                            value,
                        }
                    },
                    _ => panic!("Missing ValueArray {:?}", array_type)
                }
            })
        },
        _ => panic!("Missing PropertyMeta {:?}", t)
    }
}


#[derive(Debug,PartialEq,Eq)]
pub struct CustomFormatData {
    pub id: uuid::Uuid,
    pub value: i32,
}
impl Readable for CustomFormatData {
    fn read(reader: &mut Reader) -> Result<Self> {
        Ok(CustomFormatData {
            id: uuid::Uuid::read(reader)?,
            value: reader.read_i32::<LittleEndian>()?
        })
    }
}
impl Writable for CustomFormatData {
    fn write(&self, writer: &mut Writer) -> Result<()> {
        self.id.write(writer)?;
        writer.write_i32::<LittleEndian>(self.value)?;
        Ok(())
    }
}

#[derive(Debug,PartialEq,Eq)]
pub struct Header {
    pub save_game_version: u32,
    pub package_version: u32,
    pub engine_version_major: u16,
    pub engine_version_minor: u16,
    pub engine_version_patch: u16,
    pub engine_version_build: u32,
    pub engine_version: String,
    pub custom_format_version: u32,
    pub custom_format: Vec<CustomFormatData>,
}
impl Readable for Header {
    fn read(reader: &mut Reader) -> Result<Self> {
        assert_eq!(reader.read_u32::<BigEndian>()?, 0x47564153); // GVAS
        Ok(Header {
            save_game_version: reader.read_u32::<LittleEndian>()?,
            package_version: reader.read_u32::<LittleEndian>()?,
            engine_version_major: reader.read_u16::<LittleEndian>()?,
            engine_version_minor: reader.read_u16::<LittleEndian>()?,
            engine_version_patch: reader.read_u16::<LittleEndian>()?,
            engine_version_build: reader.read_u32::<LittleEndian>()?,
            engine_version: read_string(reader)?,
            custom_format_version: reader.read_u32::<LittleEndian>()?,
            custom_format: read_array(reader.read_u32::<LittleEndian>()?, reader, |r| Ok(CustomFormatData::read(r)?))?,
        })
    }
}
impl Writable for Header {
    fn write(&self, writer: &mut Writer) -> Result<()> {
        writer.write_u32::<BigEndian>(0x47564153)?;
        writer.write_u32::<LittleEndian>(self.save_game_version)?;
        writer.write_u32::<LittleEndian>(self.package_version)?;
        writer.write_u16::<LittleEndian>(self.engine_version_major)?;
        writer.write_u16::<LittleEndian>(self.engine_version_minor)?;
        writer.write_u16::<LittleEndian>(self.engine_version_patch)?;
        writer.write_u32::<LittleEndian>(self.engine_version_build)?;
        write_string(writer, &self.engine_version)?;
        writer.write_u32::<LittleEndian>(self.custom_format_version)?;
        writer.write_u32::<LittleEndian>(self.custom_format.len() as u32)?;
        for cf in &self.custom_format {
            cf.write(writer)?;
        }
        Ok(())
    }
}

#[derive(Debug,PartialEq)]
pub struct Root {
    pub save_game_type: String,
    pub root: Vec<Property>,
}
impl Readable for Root {
    fn read(reader: &mut Reader) -> Result<Self> {
        Ok(Self {
            save_game_type: read_string(reader)?,
            root: read_properties_until_none(reader)?,
        })
    }
}

#[derive(Debug,PartialEq)]
pub struct Save {
    pub header: Header,
    pub root: Root,
}
impl Readable for Save {
    fn read(reader: &mut Reader) -> Result<Self> {
        Ok(Self {
            header: Header::read(reader)?,
            root: Root::read(reader)?,
        })
    }
}

fn main() {
    println!("Hello, world!");
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use crate::*;

    static SAVE: &'static [u8] = include_bytes!("../trash/test/76561198083438003_Experimental_Player_Slot_dsaf.sav");
    /*
    #[test]
    fn test_read_header() -> Result<()> {
        let mut reader = Cursor::new(SAVE);
        assert_eq!(
            Header::read(&mut reader)?,
            Header {
                save_game_version: 2,
                package_version: 522,
                engine_version_major: 4,
                engine_version_minor: 27,
                engine_version_patch: 2,
                engine_version_build: 2147554241,
                engine_version: "main".to_string(),
                custom_format_version: 3,
                custom_format: vec![
                    CustomFormatData { id: uuid::uuid!("82e77c4e43a53323c5136bb4f30d3197"), value: 0, },
                    CustomFormatData { id: uuid::uuid!("fcf57afa4283507658e6a9b9322da0ff"), value: 68 },
                    CustomFormatData { id: uuid::uuid!("24bb7af34f835646c22d2f1fff96ad49"), value: 5 },
                    CustomFormatData { id: uuid::uuid!("fb26e4124b4d1f150a557293702f1d96"), value: 3 },
                    CustomFormatData { id: uuid::uuid!("9c54d5224fbea82646072194d082b461"), value: 43 },
                    CustomFormatData { id: uuid::uuid!("b0d832e44f0d1f89b77ecfaca24afd36"), value: 10 },
                    CustomFormatData { id: uuid::uuid!("e1c643284d53a22c868e6ca38cbd1764"), value: 0 },
                    CustomFormatData { id: uuid::uuid!("375ec13c48fb06e4f08400b57e712a26"), value: 4 },
                    CustomFormatData { id: uuid::uuid!("e4b068ed42e9f4940bda31a241bb462e"), value: 40 },
                    CustomFormatData { id: uuid::uuid!("cffc743f448043b0df14919373201d17"), value: 37 },
                    CustomFormatData { id: uuid::uuid!("b02b49b544e9bb20b73204a36003e452"), value: 3 },
                    CustomFormatData { id: uuid::uuid!("a4e4105c49b559a1c440c5a7eedf7e54"), value: 0 },
                    CustomFormatData { id: uuid::uuid!("39c831c947dc5ae6179c449a7c8e1c3e"), value: 0 },
                    CustomFormatData { id: uuid::uuid!("78f01b334f98ebeaea84b4b9a25ab9cc"), value: 14 },
                    CustomFormatData { id: uuid::uuid!("6631380f43e02d4d27cf09805aa95669"), value: 0 },
                    CustomFormatData { id: uuid::uuid!("12f88b9f4afc88750cd97ca629bd3a38"), value: 45 },
                    CustomFormatData { id: uuid::uuid!("7b5ae74c4c10d270985758a95a2a210b"), value: 13 },
                    CustomFormatData { id: uuid::uuid!("d72969184bdd1dd6a864e29d8438c13c"), value: 3 },
                    CustomFormatData { id: uuid::uuid!("c2a152784afebfe7ff90176c55f71d53"), value: 1 },
                    CustomFormatData { id: uuid::uuid!("6eaca3d44cc140eced8b86b7c58f4209"), value: 3 },
                    CustomFormatData { id: uuid::uuid!("29e575dd4627e0a376d2109deadc2c23"), value: 17 },
                    CustomFormatData { id: uuid::uuid!("af43a65d49477fd38e3e739805bbc1d9"), value: 15 },
                    CustomFormatData { id: uuid::uuid!("6b266cec4b8f1ec7d9e40ba307fc4209"), value: 1 },
                    CustomFormatData { id: uuid::uuid!("0df73d6147eaa23fe98927b79a49410c"), value: 1 },
                    CustomFormatData { id: uuid::uuid!("601d18864f84ac64ded316aad6c7ea0d"), value: 47 },
                    CustomFormatData { id: uuid::uuid!("e70863684c586b23701b3984915e2616"), value: 1 },
                    CustomFormatData { id: uuid::uuid!("9dffbcd60158494f821221e288a8923c"), value: 10 },
                    CustomFormatData { id: uuid::uuid!("f2aed0ac416f9afe7faa6486fcd626fa"), value: 1 },
                    CustomFormatData { id: uuid::uuid!("174f1f0b45a5b4c6e82e3fb17d91fbd0"), value: 10 },
                    CustomFormatData { id: uuid::uuid!("35f94a83406ce258f50918a37c241096"), value: 41 },
                    CustomFormatData { id: uuid::uuid!("b68fc16e42e28b1b5c2153b4fe448805"), value: 1 },
                    CustomFormatData { id: uuid::uuid!("b2e18506cfc24273bbf44ea507ba8b75"), value: 1 },
                    CustomFormatData { id: uuid::uuid!("64f5893642bafd1b897296ba4efad0d5"), value: 1 },
                    CustomFormatData { id: uuid::uuid!("6f0ed8274895a6098d99919ca40e1890"), value: 2 },
                    CustomFormatData { id: uuid::uuid!("717f9ee7493ae9b03291b3880781381b"), value: 8 },
                    CustomFormatData { id: uuid::uuid!("5468325048af80999698c88bb7f9adfb"), value: 0 },
                    CustomFormatData { id: uuid::uuid!("430c4d1949707154699b6987e5b090df"), value: 15 },
                    CustomFormatData { id: uuid::uuid!("aafe32bd4c145395255e6ab6ddd13210"), value: 1 },
                    CustomFormatData { id: uuid::uuid!("23afe18e4e584ce152c2618db7be53b9"), value: 11 },
                    CustomFormatData { id: uuid::uuid!("a462b7ea4e3af4991fecc199b2e12482"), value: 4 },
                    CustomFormatData { id: uuid::uuid!("2eb5fdbd4d1001ac8ff33681daa59333"), value: 5 },
                    CustomFormatData { id: uuid::uuid!("509d354f492ff6e6b28549a71c633c07"), value: 0 },
                    CustomFormatData { id: uuid::uuid!("4a56eb4011dc10f57e34d392e76ac9b2"), value: 2 },
                    CustomFormatData { id: uuid::uuid!("d78a4a004697e858b519a8bab4467d48"), value: 18 },
                    CustomFormatData { id: uuid::uuid!("5579f8864c1f933a7b08ba832fb96163"), value: 2 },
                    CustomFormatData { id: uuid::uuid!("612fbe52400bda53914f0d917c85b19f"), value: 1 },
                    CustomFormatData { id: uuid::uuid!("a4237a3641c9caeaf818a28ff31b6858"), value: 4 },
                    CustomFormatData { id: uuid::uuid!("804e3f754b497088068cd6a4dcb67e3c"), value: 5 },
                    CustomFormatData { id: uuid::uuid!("fb680af24ba359efb519a8ba3d44c873"), value: 2 },
                    CustomFormatData { id: uuid::uuid!("9950b70e4e17b41a0dfaccbbd67f8157"), value: 1 },
                ],
            }
        );
        Ok(())
    }
    */
    #[test]
    fn test_header() -> Result<()> {
        let original = vec![0x47, 0x56, 0x41, 0x53, 0x02, 0x00, 0x00, 0x00, 0x06, 0x02, 0x00, 0x00, 0x04, 0x00, 0x19, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x17, 0x00, 0x00, 0x00, 0x2B, 0x2B, 0x55, 0x45, 0x34, 0x2B, 0x52, 0x65, 0x6C, 0x65, 0x61, 0x73, 0x65, 0x2D, 0x34, 0x2E, 0x32, 0x35, 0x50, 0x6C, 0x75, 0x73, 0x00, 0x03, 0x00, 0x00, 0x00, 0x31, 0x00, 0x00, 0x00, 0xFA, 0x7A, 0xF5, 0xFC, 0x83, 0x42, 0x76, 0x50, 0x58, 0xE6, 0xA9, 0xB9, 0x32, 0x2D, 0xA0, 0xFF, 0x3D, 0x00, 0x00, 0x00, 0xF3, 0x7A, 0xBB, 0x24, 0x83, 0x4F, 0x46, 0x56, 0xC2, 0x2D, 0x2F, 0x1F, 0xFF, 0x96, 0xAD, 0x49, 0x05, 0x00, 0x00, 0x00, 0x29, 0x23, 0xA5, 0x76, 0xB5, 0x45, 0x23, 0x09, 0x41, 0xD8, 0xAE, 0x98, 0xD8, 0x6A, 0x2F, 0xCF, 0x02, 0x00, 0x00, 0x00, 0x07, 0x69, 0xBC, 0x5F, 0xAE, 0x40, 0xC8, 0x55, 0x84, 0xF1, 0x67, 0x8E, 0x3F, 0xF1, 0xFF, 0x5E, 0x01, 0x00, 0x00, 0x00, 0x12, 0xE4, 0x26, 0xFB, 0x4D, 0x4B, 0x15, 0x1F, 0x0A, 0x55, 0x72, 0x93, 0x70, 0x2F, 0x1D, 0x96, 0x03, 0x00, 0x00, 0x00, 0x22, 0xD5, 0x54, 0x9C, 0xBE, 0x4F, 0x26, 0xA8, 0x46, 0x07, 0x21, 0x94, 0xD0, 0x82, 0xB4, 0x61, 0x1E, 0x00, 0x00, 0x00, 0xE4, 0x32, 0xD8, 0xB0, 0x0D, 0x4F, 0x89, 0x1F, 0xB7, 0x7E, 0xCF, 0xAC, 0xA2, 0x4A, 0xFD, 0x36, 0x0A, 0x00, 0x00, 0x00, 0x28, 0x43, 0xC6, 0xE1, 0x53, 0x4D, 0x2C, 0xA2, 0x86, 0x8E, 0x6C, 0xA3, 0x8C, 0xBD, 0x17, 0x64, 0x00, 0x00, 0x00, 0x00, 0x3C, 0xC1, 0x5E, 0x37, 0xFB, 0x48, 0xE4, 0x06, 0xF0, 0x84, 0x00, 0xB5, 0x7E, 0x71, 0x2A, 0x26, 0x04, 0x00, 0x00, 0x00, 0xED, 0x68, 0xB0, 0xE4, 0xE9, 0x42, 0x94, 0xF4, 0x0B, 0xDA, 0x31, 0xA2, 0x41, 0xBB, 0x46, 0x2E, 0x26, 0x00, 0x00, 0x00, 0x3F, 0x74, 0xFC, 0xCF, 0x80, 0x44, 0xB0, 0x43, 0xDF, 0x14, 0x91, 0x93, 0x73, 0x20, 0x1D, 0x17, 0x25, 0x00, 0x00, 0x00, 0xB5, 0x49, 0x2B, 0xB0, 0xE9, 0x44, 0x20, 0xBB, 0xB7, 0x32, 0x04, 0xA3, 0x60, 0x03, 0xE4, 0x52, 0x02, 0x00, 0x00, 0x00, 0x5C, 0x10, 0xE4, 0xA4, 0xB5, 0x49, 0xA1, 0x59, 0xC4, 0x40, 0xC5, 0xA7, 0xEE, 0xDF, 0x7E, 0x54, 0x00, 0x00, 0x00, 0x00, 0xC9, 0x31, 0xC8, 0x39, 0xDC, 0x47, 0xE6, 0x5A, 0x17, 0x9C, 0x44, 0x9A, 0x7C, 0x8E, 0x1C, 0x3E, 0x00, 0x00, 0x00, 0x00, 0x33, 0x1B, 0xF0, 0x78, 0x98, 0x4F, 0xEA, 0xEB, 0xEA, 0x84, 0xB4, 0xB9, 0xA2, 0x5A, 0xB9, 0xCC, 0x04, 0x00, 0x00, 0x00, 0x0F, 0x38, 0x31, 0x66, 0xE0, 0x43, 0x4D, 0x2D, 0x27, 0xCF, 0x09, 0x80, 0x5A, 0xA9, 0x56, 0x69, 0x00, 0x00, 0x00, 0x00, 0x9F, 0x8B, 0xF8, 0x12, 0xFC, 0x4A, 0x75, 0x88, 0x0C, 0xD9, 0x7C, 0xA6, 0x29, 0xBD, 0x3A, 0x38, 0x2B, 0x00, 0x00, 0x00, 0x4C, 0xE7, 0x5A, 0x7B, 0x10, 0x4C, 0x70, 0xD2, 0x98, 0x57, 0x58, 0xA9, 0x5A, 0x2A, 0x21, 0x0B, 0x0C, 0x00, 0x00, 0x00, 0x18, 0x69, 0x29, 0xD7, 0xDD, 0x4B, 0xD6, 0x1D, 0xA8, 0x64, 0xE2, 0x9D, 0x84, 0x38, 0xC1, 0x3C, 0x03, 0x00, 0x00, 0x00, 0x78, 0x52, 0xA1, 0xC2, 0xFE, 0x4A, 0xE7, 0xBF, 0xFF, 0x90, 0x17, 0x6C, 0x55, 0xF7, 0x1D, 0x53, 0x01, 0x00, 0x00, 0x00, 0xD4, 0xA3, 0xAC, 0x6E, 0xC1, 0x4C, 0xEC, 0x40, 0xED, 0x8B, 0x86, 0xB7, 0xC5, 0x8F, 0x42, 0x09, 0x03, 0x00, 0x00, 0x00, 0xDD, 0x75, 0xE5, 0x29, 0x27, 0x46, 0xA3, 0xE0, 0x76, 0xD2, 0x10, 0x9D, 0xEA, 0xDC, 0x2C, 0x23, 0x11, 0x00, 0x00, 0x00, 0x5D, 0xA6, 0x43, 0xAF, 0x47, 0x49, 0xD3, 0x7F, 0x8E, 0x3E, 0x73, 0x98, 0x05, 0xBB, 0xC1, 0xD9, 0x07, 0x00, 0x00, 0x00, 0xEC, 0x6C, 0x26, 0x6B, 0x8F, 0x4B, 0xC7, 0x1E, 0xD9, 0xE4, 0x0B, 0xA3, 0x07, 0xFC, 0x42, 0x09, 0x01, 0x00, 0x00, 0x00, 0x61, 0x3D, 0xF7, 0x0D, 0xEA, 0x47, 0x3F, 0xA2, 0xE9, 0x89, 0x27, 0xB7, 0x9A, 0x49, 0x41, 0x0C, 0x01, 0x00, 0x00, 0x00, 0x86, 0x18, 0x1D, 0x60, 0x84, 0x4F, 0x64, 0xAC, 0xDE, 0xD3, 0x16, 0xAA, 0xD6, 0xC7, 0xEA, 0x0D, 0x1F, 0x00, 0x00, 0x00, 0xD6, 0xBC, 0xFF, 0x9D, 0x58, 0x01, 0x4F, 0x49, 0x82, 0x12, 0x21, 0xE2, 0x88, 0xA8, 0x92, 0x3C, 0x0A, 0x00, 0x00, 0x00, 0xAC, 0xD0, 0xAE, 0xF2, 0x6F, 0x41, 0xFE, 0x9A, 0x7F, 0xAA, 0x64, 0x86, 0xFC, 0xD6, 0x26, 0xFA, 0x01, 0x00, 0x00, 0x00, 0x0B, 0x1F, 0x4F, 0x17, 0xA5, 0x45, 0xC6, 0xB4, 0xE8, 0x2E, 0x3F, 0xB1, 0x7D, 0x91, 0xFB, 0xD0, 0x0A, 0x00, 0x00, 0x00, 0x83, 0x4A, 0xF9, 0x35, 0x6C, 0x40, 0x58, 0xE2, 0xF5, 0x09, 0x18, 0xA3, 0x7C, 0x24, 0x10, 0x96, 0x25, 0x00, 0x00, 0x00, 0x6E, 0xC1, 0x8F, 0xB6, 0xE2, 0x42, 0x1B, 0x8B, 0x5C, 0x21, 0x53, 0xB4, 0xFE, 0x44, 0x88, 0x05, 0x01, 0x00, 0x00, 0x00, 0x06, 0x85, 0xE1, 0xB2, 0xC2, 0xCF, 0x73, 0x42, 0xBB, 0xF4, 0x4E, 0xA5, 0x07, 0xBA, 0x8B, 0x75, 0x01, 0x00, 0x00, 0x00, 0x50, 0x32, 0x68, 0x54, 0xAF, 0x48, 0x99, 0x80, 0x96, 0x98, 0xC8, 0x8B, 0xB7, 0xF9, 0xAD, 0xFB, 0x00, 0x00, 0x00, 0x00, 0x19, 0x4D, 0x0C, 0x43, 0x70, 0x49, 0x54, 0x71, 0x69, 0x9B, 0x69, 0x87, 0xE5, 0xB0, 0x90, 0xDF, 0x0E, 0x00, 0x00, 0x00, 0xBD, 0x32, 0xFE, 0xAA, 0x14, 0x4C, 0x95, 0x53, 0x25, 0x5E, 0x6A, 0xB6, 0xDD, 0xD1, 0x32, 0x10, 0x01, 0x00, 0x00, 0x00, 0x8E, 0xE1, 0xAF, 0x23, 0x58, 0x4E, 0xE1, 0x4C, 0x52, 0xC2, 0x61, 0x8D, 0xB7, 0xBE, 0x53, 0xB9, 0x0B, 0x00, 0x00, 0x00, 0xEA, 0xB7, 0x62, 0xA4, 0x3A, 0x4E, 0x99, 0xF4, 0x1F, 0xEC, 0xC1, 0x99, 0xB2, 0xE1, 0x24, 0x82, 0x02, 0x00, 0x00, 0x00, 0xBD, 0xFD, 0xB5, 0x2E, 0x10, 0x4D, 0xAC, 0x01, 0x8F, 0xF3, 0x36, 0x81, 0xDA, 0xA5, 0x93, 0x33, 0x05, 0x00, 0x00, 0x00, 0x4F, 0x35, 0x9D, 0x50, 0x2F, 0x49, 0xE6, 0xF6, 0xB2, 0x85, 0x49, 0xA7, 0x1C, 0x63, 0x3C, 0x07, 0x00, 0x00, 0x00, 0x00, 0xE7, 0x9E, 0x7F, 0x71, 0x3A, 0x49, 0xB0, 0xE9, 0x32, 0x91, 0xB3, 0x88, 0x07, 0x81, 0x38, 0x1B, 0x06, 0x00, 0x00, 0x00, 0x40, 0xEB, 0x56, 0x4A, 0xDC, 0x11, 0xF5, 0x10, 0x7E, 0x34, 0xD3, 0x92, 0xE7, 0x6A, 0xC9, 0xB2, 0x02, 0x00, 0x00, 0x00, 0x00, 0x4A, 0x8A, 0xD7, 0x97, 0x46, 0x58, 0xE8, 0xB5, 0x19, 0xA8, 0xBA, 0xB4, 0x46, 0x7D, 0x48, 0x11, 0x00, 0x00, 0x00, 0x86, 0xF8, 0x79, 0x55, 0x1F, 0x4C, 0x3A, 0x93, 0x7B, 0x08, 0xBA, 0x83, 0x2F, 0xB9, 0x61, 0x63, 0x01, 0x00, 0x00, 0x00, 0x52, 0xBE, 0x2F, 0x61, 0x0B, 0x40, 0x53, 0xDA, 0x91, 0x4F, 0x0D, 0x91, 0x7C, 0x85, 0xB1, 0x9F, 0x01, 0x00, 0x00, 0x00, 0x36, 0x7A, 0x23, 0xA4, 0xC9, 0x41, 0xEA, 0xCA, 0xF8, 0x18, 0xA2, 0x8F, 0xF3, 0x1B, 0x68, 0x58, 0x04, 0x00, 0x00, 0x00, 0x75, 0x3F, 0x4E, 0x80, 0x49, 0x4B, 0x88, 0x70, 0x06, 0x8C, 0xD6, 0xA4, 0xDC, 0xB6, 0x7E, 0x3C, 0x05, 0x00, 0x00, 0x00, 0xF2, 0x0A, 0x68, 0xFB, 0xA3, 0x4B, 0xEF, 0x59, 0xB5, 0x19, 0xA8, 0xBA, 0x3D, 0x44, 0xC8, 0x73, 0x02, 0x00, 0x00, 0x00, 0x0E, 0xB7, 0x50, 0x99, 0x17, 0x4E, 0x1A, 0xB4, 0x0D, 0xFA, 0xCC, 0xBB, 0xD6, 0x7F, 0x81, 0x57, 0x01, 0x00, 0x00, 0x00, 0x96, 0x51, 0x96, 0xAB, 0xFC, 0x08, 0xD8, 0x45, 0x8D, 0x22, 0xD7, 0xB7, 0x9E, 0x56, 0xAD, 0x78, 0x01, 0x00, 0x00, 0x00];
        let mut reader = Reader::new(&original);
        let header = Header::read(&mut reader)?;
        let mut reconstructed = vec![];
        header.write(&mut reconstructed)?;
        assert_eq!(original, reconstructed);
        Ok(())
    }

    #[test]
    fn test_uuid() -> Result<()> {
        let id = uuid::uuid!("2eb5fdbd4d1001ac8ff33681daa59333");
        let mut writer = vec![];
        id.write(&mut writer)?;
        let mut reader = Reader::new(&writer);
        let rid = uuid::Uuid::read(&mut reader)?;
        assert_eq!(id, rid);
        Ok(())
    }

    #[test]
    fn test_read_save1() -> Result<()> {
        //let save = finalize(Save::read.context("what"))(from_buffer(SAVE));
        let mut reader = Reader::new(SAVE);
        let save = Save::read(&mut reader)?;
        println!("{:#?}", save);
        //println!("{:?}", sequence::tuple((Header::parse, Root::parse))(SAVE).unwrap().1.1);
        //assert_eq!(read_string(b"\x01\x00\x00\x00\x00").unwrap(), (&b""[..], ""));
        Ok(())
    }

    #[test]
    fn test_read_property_meta() -> Result<()> {
        //let bytes = b"\x01\x19\x4D\x0C\x43\x70\x49\x54\x71\x69\x9B\x69\x87\xE5\xB0\x90\xDF\x0A\x00\x00\x00";
        let bytes = b"\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0A\x00\x00\x00";
        let mut reader = Reader::new(bytes);
        assert_eq!(
            read_property_meta(PropertyType::IntProperty, &mut reader)?,
            PropertyMeta::Int {
                id: Some(uuid::uuid!("00000000000000000000000000000000")),
                value: 10
            }
        );
        Ok(())
    }

    #[test]
    fn test_read_int_property() -> Result<()> {
        let bytes = b"\x0E\x00\x00\x00\x56\x65\x72\x73\x69\x6F\x6E\x4E\x75\x6D\x62\x65\x72\x00\x0C\x00\x00\x00\x49\x6E\x74\x50\x72\x6F\x70\x65\x72\x74\x79\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00";
        //let bytes = b"\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0A\x00\x00\x00";
        let mut reader = Reader::new(bytes);
        assert_eq!(
            read_property(&mut reader)?,
            Some(Property {
                name: "VersionNumber".to_string(),
                value: PropertyMeta::Int {
                    id: None,
                    value: 2,
                }
            })
        );
        Ok(())
    }

    /*
    #[test]
    fn test_read_struct_property() -> Result<()> {
        let bytes = b"\x12\x00\x00\x00\x56\x61\x6E\x69\x74\x79\x4D\x61\x73\x74\x65\x72\x79\x53\x61\x76\x65\x00\x0F\x00\x00\x00\x53\x74\x72\x75\x63\x74\x50\x72\x6F\x70\x65\x72\x74\x79\x00\x8D\x00\x00\x00\x00\x00\x00\x00\x12\x00\x00\x00\x56\x61\x6E\x69\x74\x79\x4D\x61\x73\x74\x65\x72\x79\x53\x61\x76\x65\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x4C\x65\x76\x65\x6C\x00\x0C\x00\x00\x00\x49\x6E\x74\x50\x72\x6F\x70\x65\x72\x74\x79\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x8C\x00\x00\x00\x03\x00\x00\x00\x58\x50\x00\x0C\x00\x00\x00\x49\x6E\x74\x50\x72\x6F\x70\x65\x72\x74\x79\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x3A\x23\x00\x00\x1A\x00\x00\x00\x48\x61\x73\x41\x77\x61\x72\x64\x65\x64\x46\x6F\x72\x4F\x6C\x64\x50\x75\x72\x63\x68\x61\x73\x65\x73\x00\x0D\x00\x00\x00\x42\x6F\x6F\x6C\x50\x72\x6F\x70\x65\x72\x74\x79\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x05\x00\x00\x00\x4E\x6F\x6E\x65\x00";
        let mut reader = Reader::new(bytes);
        assert_eq!(
            read_property(&mut reader)?,
            Some(Property {
                name: "VanityMasterySave".to_string(),
                value: PropertyMeta::Struct {
                    id: None,
                    value: StructValue(vec![
                        Property {
                            name: "Level".to_string(),
                            value: PropertyMeta::Int {
                                id: None,
                                value: 140,
                            },
                        },
                        Property {
                            name: "XP".to_string(),
                            value: PropertyMeta::Int {
                                id: None,
                                value: 9018,
                            },
                        },
                        Property {
                            name: "HasAwardedForOldPurchases".to_string(),
                            value: PropertyMeta::Bool {
                                id: None,
                                value: true,
                            },
                        },
                    ]),
                    struct_type: PropertyType::Other("VanityMasterySave".to_string()),
                    struct_id: uuid::uuid!("00000000000000000000000000000000"),
                }
            })
        );
        Ok(())
    }
    */

    #[test]
    fn test_read_array_property() -> Result<()> {
        let bytes = b"\x0C\x00\x00\x00\x53\x74\x61\x74\x49\x6E\x64\x69\x63\x65\x73\x00\x0E\x00\x00\x00\x41\x72\x72\x61\x79\x50\x72\x6F\x70\x65\x72\x74\x79\x00\x08\x00\x00\x00\x00\x00\x00\x00\x0C\x00\x00\x00\x49\x6E\x74\x50\x72\x6F\x70\x65\x72\x74\x79\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00";
        let mut reader = Reader::new(bytes);
        assert_eq!(
            read_property(&mut reader)?,
            Some(Property {
                name: "StatIndices".to_string(),
                value: PropertyMeta::Array {
                    id: None,
                    value: ValueArray::Int(vec![0])
                }
            })
        );
        Ok(())
    }



    static SAVE2: &'static [u8] = include_bytes!("../trash/test/Mods/Speedster/Config2.sav");
    #[test]
    fn test_read_save2() -> Result<()> {
        let mut reader = Reader::new(SAVE2);
        let save = Save::read(&mut reader)?;
        //let save = Save::read(from_buffer(SAVE2)).unwrap();
        //println!("{:?}", save);
        Ok(())
    }

    /*
    #[test]
    fn test_read_struct() -> Result<()>{
        let bytes = b"\x0A\x00\x00\x00\x4D\x75\x6C\x65\x53\x70\x65\x65\x64\x00\x0E\x00\x00\x00\x46\x6C\x6F\x61\x74\x50\x72\x6F\x70\x65\x72\x74\x79\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\xC0\x79\x44\x05\x00\x00\x00\x4E\x6F\x6E\x65\x00";
        let mut reader = Reader::new(bytes);
        assert_eq!(
            StructValue::read(&mut reader)?,
            StructValue (
                vec![
                    Property {
                        name: "MuleSpeed".to_string(),
                        value: PropertyMeta::Float {
                            id: None,
                            value: Float(999.0)
                        },
                    },
                ]
            )
        );
        Ok(())
    }
    */






}
