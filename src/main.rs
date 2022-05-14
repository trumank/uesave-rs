use nom::*;

use nom_derive::*;

fn read_bool(data: &[u8]) -> nom::IResult<&[u8], bool> {
    combinator::map(number::complete::u8, |n: u8| n > 0)(data)
}

fn read_uuid(data: &[u8]) -> nom::IResult<&[u8], uuid::Uuid> {
    let (data, uuid) = combinator::map(number::complete::le_u128, |n: u128| uuid::Builder::from_bytes_le(n.to_le_bytes()).into_uuid())(data)?;
    Ok((data, uuid))
}

fn read_optional_uuid(data: &[u8]) -> nom::IResult<&[u8], Option<uuid::Uuid>> {
    let (data, flag) = number::complete::u8(data)?;
    combinator::cond(flag > 0, read_uuid)(data)
}

fn read_string(data: &[u8]) -> nom::IResult<&[u8], &str> {
    sequence::terminated(
        combinator::map_res(
            multi::length_data(combinator::map(number::complete::le_u32, |n| n - 1)),
            std::str::from_utf8,
        ),
        bytes::complete::take(1usize)
    )(data)
}

fn read_property(data: &[u8]) -> nom::IResult<&[u8], Property> {
    let (data, name) = read_string(data)?;
    let (data, (t, _)) = sequence::tuple((
        read_type,
        number::complete::le_u64,
    ))(data)?;
    let (data, value) = PropertyValue::parse(data, t)?;
    Ok((data, Property { name, value }))
}

fn read_type(data: &[u8]) -> nom::IResult<&[u8], PropertyType, nom::error::Error<&[u8]>> {
    let (data, t) = read_string(data)?;
    Ok((data, match t {
        "IntProperty" => PropertyType::IntProperty,
        "UInt32Property" => PropertyType::UInt32Property,
        "FloatProperty" => PropertyType::FloatProperty,
        "BoolProperty" => PropertyType::BoolProperty,
        "ByteProperty" => PropertyType::ByteProperty,
        "StructProperty" => PropertyType::StructProperty,
        "ArrayProperty" => PropertyType::ArrayProperty,
        "ObjectProperty" => PropertyType::ObjectProperty,
        "StrProperty" => PropertyType::StrProperty,
        "NameProperty" => PropertyType::NameProperty,
        "TextProperty" => PropertyType::TextProperty,
        "MulticastInlineDelegateProperty" => PropertyType::MulticastInlineDelegateProperty,
        "SetProperty" => PropertyType::SetProperty,
        "MapProperty" => PropertyType::MapProperty,
        _ => panic!("Unknown property type: {}", t), // TODO: Figure out how tf to handle errors with nom
    }))
}

fn read_none(data: &[u8]) -> nom::IResult<&[u8], &[u8]> {
    bytes::complete::tag("\x05\x00\x00\x00None\x00")(data)
}

#[derive(PartialEq,Eq)]
pub enum PropertyType {
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
}

#[derive(Debug,PartialEq)]
pub struct Property<'a> {
    name: &'a str,
    value: PropertyValue<'a>,
}

type IntProperty = i32;
type UInt32Property = i32;
type FloatProperty = f32;
type BoolProperty = bool;

#[derive(Debug,PartialEq,Eq,NomLE)]
pub struct ByteProperty<'a> {
    #[nom(Parse = "read_string")]
    enum_value: &'a str
}

#[derive(Debug,PartialEq,NomLE)]
pub struct StructProperty<'a> {
    #[nom(Parse = "combinator::map(multi::many_till(read_property, read_none), |r| r.0)")]
    properties: Vec<Property<'a>>,
}

#[derive(Debug,PartialEq,NomLE)]
#[nom(Selector = "PropertyType")]
pub enum PropertyValue<'a> {
    #[nom(Selector = "PropertyType::IntProperty")]
    IntProperty {
        #[nom(Parse = "read_optional_uuid")]
        id: Option<uuid::Uuid>,
        value: IntProperty,
    },
    #[nom(Selector = "PropertyType::UInt32Property")]
    UInt32Property {
        #[nom(Parse = "read_optional_uuid")]
        id: Option<uuid::Uuid>,
        value: UInt32Property,
    },
    #[nom(Selector = "PropertyType::FloatProperty")]
    FloatProperty {
        #[nom(Parse = "read_optional_uuid")]
        id: Option<uuid::Uuid>,
        value: FloatProperty,
    },
    #[nom(Selector = "PropertyType::BoolProperty")]
    BoolProperty {
        #[nom(Parse = "read_bool")]
        value: BoolProperty,
        #[nom(Parse = "read_optional_uuid")]
        id: Option<uuid::Uuid>,
    },
    #[nom(Selector = "PropertyType::ByteProperty")]
    ByteProperty {
        #[nom(Parse = "read_string")]
        enum_type: &'a str,
        #[nom(Parse = "read_optional_uuid")]
        id: Option<uuid::Uuid>,
        value: ByteProperty<'a>,
    },
    #[nom(Selector = "PropertyType::StructProperty")]
    StructProperty {
        #[nom(Parse = "read_string")]
        struct_type: &'a str,
        #[nom(Parse = "read_uuid")]
        struct_id: uuid::Uuid,
        #[nom(Parse = "read_optional_uuid")]
        id: Option<uuid::Uuid>,
        value: StructProperty<'a>,
    },
    #[nom(Selector = "PropertyType::ArrayProperty")]
    ArrayProperty {
        #[nom(Parse = "read_string")]
        array_type: &'a str,
        #[nom(Parse = "read_optional_uuid")]
        id: Option<uuid::Uuid>,
        value: StructProperty<'a>,
    },
}

pub trait Read<T> {
    fn read(data: T) -> nom::IResult<T, Self> where Self: Sized;
}

pub struct Int(i32);
pub struct UInt32(u32);
pub struct Float(f32);
pub struct Byte<'a>(&'a str);
pub struct StructKey<'a>(&'a str);
pub struct StructValue<'a>(&'a str);

pub enum PropertyBase <'a> {
    Int(Int),
    UInt32(UInt32),
    Float(Float),
    Byte(Byte<'a>),
}

pub enum PropertyKey<'a> {
    Base(PropertyBase<'a>),
    Struct(StructKey<'a>),
}

pub enum PropertyValueASDF<'a> {
    Base(PropertyBase<'a>),
    StructValue(StructKey<'a>),
}

pub enum PropertyArray<'a> {
    Int(Vec<Int>),
    UInt32(Vec<UInt32>),
    Float(Vec<Float>),
    Byte(Vec<Byte<'a>>),
}

impl Read<&[u8]> for Int    { fn read(data: &[u8]) -> nom::IResult<&[u8], Self> { combinator::map(number::complete::le_i32, Int   )(data) } }
impl Read<&[u8]> for UInt32 { fn read(data: &[u8]) -> nom::IResult<&[u8], Self> { combinator::map(number::complete::le_u32, UInt32)(data) } }
impl Read<&[u8]> for Float  { fn read(data: &[u8]) -> nom::IResult<&[u8], Self> { combinator::map(number::complete::le_f32, Float )(data) } }
impl<'a> Read<&'a [u8]> for Byte<'a> {
    fn read(data: &'a [u8]) -> nom::IResult<&'a [u8], Self> {
        combinator::map(read_string, Byte)(data)
    }
}


fn read_property_array_helper<'a, T, R, F>(data: &'a [u8], count: usize, map: F) -> nom::IResult<&'a [u8], R>
    where T: Read<&'a [u8]>,
          F: FnMut(Vec<T>) -> R {
    combinator::map(multi::count(T::read, count), map)(data)
}
fn read_property_array(data: &[u8], t: PropertyType, count: usize) -> nom::IResult<&[u8], PropertyArray> {
    match t {
        PropertyType::IntProperty => read_property_array_helper(data, count, PropertyArray::Int),
        PropertyType::UInt32Property => read_property_array_helper(data, count, PropertyArray::UInt32),
        PropertyType::FloatProperty => read_property_array_helper(data, count, PropertyArray::Float),
        PropertyType::ByteProperty => read_property_array_helper(data, count, PropertyArray::Byte),
        _ => panic!()
    }
}

#[derive(Debug,PartialEq,Eq,NomLE)]
pub struct CustomFormatData {
    #[nom(Parse = "read_uuid")]
    pub id: uuid::Uuid,
    pub value: i32,
}
#[derive(Debug,PartialEq,Eq,NomLE)]
pub struct Header<'a> {
    #[nom(Parse = "nom::bytes::complete::tag(\"GVAS\")")]
    pub magic: &'a[u8],
    pub save_game_version: u32,
    pub package_version: u32,
    pub engine_version_major: u16,
    pub engine_version_minor: u16,
    pub engine_version_patch: u16,
    pub engine_version_build: u32,
    #[nom(Parse = "read_string")]
    pub engine_version: &'a str,
    pub custom_format_version: u32,
    #[nom(LengthCount = "nom::number::complete::le_u32")]
    pub custom_format: Vec<CustomFormatData>,
}

#[derive(Debug,PartialEq,NomLE)]
pub struct Root<'a> {
    #[nom(Parse = "read_string")]
    pub save_game_type: &'a str,
    pub root: StructProperty<'a>,
}

#[derive(Debug,PartialEq,NomLE)]
pub struct Save<'a> {
    pub header: Header<'a>,
    pub root: Root<'a>,
}

fn main() {
    println!("Hello, world!");
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use crate::*;

    #[test]
    fn test_uuid() {
        assert_eq!(read_uuid(b"\x67\xB6\x6F\x72\xB3\xE8\x10\x4D\xA0\xE5\x82\xF5\xBF\x0F\x20\xDB").unwrap(), (&b""[..], uuid::uuid!("726FB667-E8B3-4D10-A0E5-82F5BF0F20DB")));
    }

    #[test]
    fn test_optional_uuid() {
        assert_eq!(read_optional_uuid(b"\x01\x67\xB6\x6F\x72\xB3\xE8\x10\x4D\xA0\xE5\x82\xF5\xBF\x0F\x20\xDB").unwrap(), (&b""[..], Some(uuid::uuid!("726FB667-E8B3-4D10-A0E5-82F5BF0F20DB"))));
        assert_eq!(read_optional_uuid(b"\x00").unwrap(), (&b""[..], None));
    }

    #[test]
    fn test_read_string() {
        assert_eq!(read_string(b"\x04\x00\x00\x00\x4D\x61\x78\x00").unwrap(), (&b""[..], "Max"));
        assert_eq!(read_string(b"\x01\x00\x00\x00\x00").unwrap(), (&b""[..], ""));
    }


    static SAVE: &'static [u8] = include_bytes!("../trash/test/76561198083438003_Experimental_Player_Slot_dsaf.sav");
    #[test]
    fn test_read_header() {
        assert_eq!(nom::error::dbg_dmp(Header::parse, "asdf")(SAVE).unwrap().1, Header {
            magic: "GVAS".as_bytes(),
            save_game_version: 2,
            package_version: 522,
            engine_version_major: 4,
            engine_version_minor: 27,
            engine_version_patch: 2,
            engine_version_build: 2147554241,
            engine_version: "main",
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
        });
    }

    #[test]
    fn test_read_save() {
        let data = SAVE;
        let (data, header) = Header::parse(data).unwrap();
        let (data, root) = Root::parse(data).unwrap();
        //println!("{:?}", property1);
        //println!("{:?}", sequence::tuple((Header::parse, Root::parse))(SAVE).unwrap().1.1);
        //assert_eq!(read_string(b"\x01\x00\x00\x00\x00").unwrap(), (&b""[..], ""));
    }

    #[test]
    fn test_read_property_value() {
        assert_eq!(PropertyValue::parse(b"\x01\x19\x4D\x0C\x43\x70\x49\x54\x71\x69\x9B\x69\x87\xE5\xB0\x90\xDF\x0A\x00\x00\x00", PropertyType::IntProperty).unwrap(), (&b""[..], PropertyValue::IntProperty { id: Some(uuid::uuid!("430c4d1949707154699b6987e5b090df")), value: 10 } ));
        assert_eq!(PropertyValue::parse(b"\x00\x0A\x00\x00\x00", PropertyType::IntProperty).unwrap(), (&b""[..], PropertyValue::IntProperty { id: None, value: 10 } ));
    }

    #[test]
    fn test_read_int_property() {
        assert_eq!(
            read_property(b"\x0E\x00\x00\x00\x56\x65\x72\x73\x69\x6F\x6E\x4E\x75\x6D\x62\x65\x72\x00\x0C\x00\x00\x00\x49\x6E\x74\x50\x72\x6F\x70\x65\x72\x74\x79\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00").unwrap(),
            (&b""[..], Property {
                name: "VersionNumber",
                value: PropertyValue::IntProperty {
                    id: None,
                    value: 2,
                }
            })
        );
    }

    static SAVE2: &'static [u8] = include_bytes!("../trash/test/Mods/Speedster/Config2.sav");
    #[test]
    fn test_read_save2() {
        let save = Save::parse(SAVE2).unwrap();
        println!("{:?}", save);
    }

    #[test]
    fn test_read_struct() {
        assert_eq!(
            StructProperty::parse(b"\x0A\x00\x00\x00\x4D\x75\x6C\x65\x53\x70\x65\x65\x64\x00\x0E\x00\x00\x00\x46\x6C\x6F\x61\x74\x50\x72\x6F\x70\x65\x72\x74\x79\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\xC0\x79\x44\x05\x00\x00\x00\x4E\x6F\x6E\x65\x00").unwrap(),
            (&b""[..], StructProperty {
                properties: vec![
                    Property {
                        name: "MuleSpeed",
                        value: PropertyValue::FloatProperty {
                            id: None,
                            value: 999.0
                        },
                    },
                ]
            })
        );
    }







}
