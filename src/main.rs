use nom::*;
use nom_locate::LocatedSpan;

type Span<'a> = LocatedSpan<&'a [u8]>;


//trait NewTrait<'a>: nom::error::ParseError<Span<'a>> + nom::error::FromExternalError<Span<'a>, std::str::Utf8Error> {}

fn read_bool<'a, E: nom::error::ParseError<Span<'a>>>(data: Span<'a>) -> nom::IResult<Span<'a>, bool, E> {
    combinator::map(number::complete::u8, |n: u8| n > 0)(data)
}

fn read_uuid<'a, E>(data: Span<'a>) -> nom::IResult<Span<'a>, uuid::Uuid, E>
    where E: error::ParseError<Span<'a>> + error::ContextError<Span<'a>> + error::FromExternalError<Span<'a>, std::str::Utf8Error> {
    let (data, uuid) = combinator::map(number::complete::le_u128, |n: u128| uuid::Builder::from_bytes_le(n.to_le_bytes()).into_uuid())(data)?;
    Ok((data, uuid))
}

fn read_optional_uuid<'a, E>(data: Span<'a>) -> nom::IResult<Span<'a>, Option<uuid::Uuid>, E>
    where E: error::ParseError<Span<'a>> + error::ContextError<Span<'a>> + error::FromExternalError<Span<'a>, std::str::Utf8Error> {
    let (data, flag) = number::complete::u8(data)?;
    combinator::cond(flag > 0, read_uuid)(data)
}

fn read_string<'a, E>(data: Span<'a>) -> nom::IResult<Span<'a>, &'a str, E>
    where
        E: error::ParseError<Span<'a>> + error::ContextError<Span<'a>> + error::FromExternalError<Span<'a>, std::str::Utf8Error> {
    error::context("read_string",
        sequence::terminated(
            combinator::map_res(
                multi::length_data(combinator::map(number::complete::le_u32, |n| n - 1)),
                |r: Span| std::str::from_utf8(r.fragment()),
            ),
            bytes::complete::take(1usize)
        )
    )(data)
}

fn read_type<'a, E>(data: Span<'a>) -> nom::IResult<Span<'a>, PropertyType, E>
    where E: error::ParseError<Span<'a>> + error::ContextError<Span<'a>> + error::FromExternalError<Span<'a>, std::str::Utf8Error> {
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
        "None" => panic!("PropertyType cannot be None"),
        _ => PropertyType::Other(t),
    }))
}

fn read_none<'a, E>(data: Span<'a>) -> nom::IResult<Span<'a>, Span<'a>, E>
    where E: error::ParseError<Span<'a>> + error::ContextError<Span<'a>> + error::FromExternalError<Span<'a>, std::str::Utf8Error> {
    bytes::complete::tag("\x05\x00\x00\x00None\x00")(data)
}

#[derive(Debug,PartialEq,Eq)]
pub enum PropertyType<'a> {
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
    Other(&'a str),
}

pub trait Read<T> {
    fn read(data: T) -> nom::IResult<T, Self> where Self: Sized;
}

#[derive(Debug,PartialEq)] pub struct Int(i32);
#[derive(Debug,PartialEq)] pub struct UInt32(u32);
#[derive(Debug,PartialEq)] pub struct Float(f32);
#[derive(Debug,PartialEq)] pub struct Bool(bool);
#[derive(Debug,PartialEq)] pub struct Byte<'a>(&'a str);
#[derive(Debug,PartialEq,Eq,Hash)] pub struct StructKey(uuid::Uuid);
#[derive(Debug,PartialEq)] pub struct StructValue<'a>(Vec<Property<'a>>);
#[derive(Debug,PartialEq)] pub struct Object<'a>(&'a str);
#[derive(Debug,PartialEq)] pub struct Name<'a>(&'a str);
#[derive(Debug,PartialEq)] pub struct Str<'a>(&'a str);
#[derive(Debug,PartialEq)] pub struct Text<'a>(&'a str); // TOOD: Special text voodoo
#[derive(Debug,PartialEq)] pub struct Set(Vec<ValueKey>);
#[derive(Debug,PartialEq)] pub struct MapEntry<'a> {
    key: ValueKey,
    value: ValueValue<'a>,
}
#[derive(Debug,PartialEq)] pub struct Map<'a>(Vec<MapEntry<'a>>);

// Base value to be extended
#[derive(Debug,PartialEq)]
pub enum ValueBase <'a> {
    Int(Int),
    UInt32(UInt32),
    Float(Float),
    Byte(Byte<'a>),
}

// Full values
#[derive(Debug,PartialEq)]
pub enum ValueValue<'a> {
    Base(ValueBase<'a>),
    Struct(StructValue<'a>),
}

// Values used as keys for SetProperty and MapProperty
#[derive(Debug,PartialEq,Eq,Hash)]
pub enum ValueKey {
    //Base(ValueBase<'a>),
    Struct(StructKey),
}

// Array of values used by ArrayProperty
#[derive(Debug,PartialEq)]
pub enum ValueArray<'a> {
    Int(Vec<Int>),
    UInt32(Vec<UInt32>),
    Float(Vec<Float>),
    Byte(Vec<Byte<'a>>),
}

// Values with IDs present in the top level object and StructProperty
#[derive(Debug,PartialEq)]
pub enum PropertyValue<'a> {
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
        value: Byte<'a>,
        enum_type: &'a str,
    },
    Struct {
        id: Option<uuid::Uuid>,
        value: StructValue<'a>,
        struct_type: &'a str,
        struct_id: uuid::Uuid,
    },
    Array {
        id: Option<uuid::Uuid>,
        value: ValueArray<'a>,
    }
}

#[derive(Debug,PartialEq)]
pub struct Property<'a> {
    name: &'a str,
    value: PropertyValue<'a>,
}

impl Property<'_> {
    fn read<'a, E>(data: Span<'a>) -> nom::IResult<Span<'a>, Property, E>
        where
            E: error::ParseError<Span<'a>> + error::ContextError<Span<'a>> + error::FromExternalError<Span<'a>, std::str::Utf8Error> {
        let (data, (name, t, _length)) = sequence::tuple((
            read_string,
            read_type,
            number::complete::le_u64,
        ))(data)?;
        let (data, value) = read_property_value(data, t)?;
        Ok((data, Property { name, value }))
    }
}

impl Int {
    fn read<'a, E>(data: Span<'a>) -> nom::IResult<Span<'a>, Self, E>
        where
            E: error::ParseError<Span<'a>> + error::ContextError<Span<'a>> + error::FromExternalError<Span<'a>, std::str::Utf8Error> {
        combinator::map(number::complete::le_i32, Self)(data)
    }
}
impl UInt32 {
    fn read<'a, E>(data: Span<'a>) -> nom::IResult<Span<'a>, Self, E>
        where
            E: error::ParseError<Span<'a>> + error::ContextError<Span<'a>> + error::FromExternalError<Span<'a>, std::str::Utf8Error> {
        combinator::map(number::complete::le_u32, Self)(data)
    }
}
impl Float {
    fn read<'a, E>(data: Span<'a>) -> nom::IResult<Span<'a>, Self, E>
        where
            E: error::ParseError<Span<'a>> + error::ContextError<Span<'a>> + error::FromExternalError<Span<'a>, std::str::Utf8Error> {
        combinator::map(number::complete::le_f32, Self)(data)
    }
}
impl Bool {
    fn read<'a, E>(data: Span<'a>) -> nom::IResult<Span<'a>, Self, E>
        where
            E: error::ParseError<Span<'a>> + error::ContextError<Span<'a>> + error::FromExternalError<Span<'a>, std::str::Utf8Error> {
        combinator::map(read_bool, Self)(data)
    }
}
impl Str<'_> {
    fn read<'a, E>(data: Span<'a>) -> nom::IResult<Span<'a>, Str, E>
        where
            E: error::ParseError<Span<'a>> + error::ContextError<Span<'a>> + error::FromExternalError<Span<'a>, std::str::Utf8Error> {
        combinator::map(read_string, Str)(data)
    }
}
impl Name<'_> {
    fn read<'a, E>(data: Span<'a>) -> nom::IResult<Span<'a>, Name, E>
        where
            E: error::ParseError<Span<'a>> + error::ContextError<Span<'a>> + error::FromExternalError<Span<'a>, std::str::Utf8Error> {
        combinator::map(read_string, Name)(data)
    }
}
impl Object<'_> {
    fn read<'a, E>(data: Span<'a>) -> nom::IResult<Span<'a>, Object, E>
        where
            E: error::ParseError<Span<'a>> + error::ContextError<Span<'a>> + error::FromExternalError<Span<'a>, std::str::Utf8Error> {
        combinator::map(read_string, Object)(data)
    }
}
impl Byte<'_> {
    fn read<'a, E>(data: Span<'a>) -> nom::IResult<Span<'a>, Byte, E>
        where
            E: error::ParseError<Span<'a>> + error::ContextError<Span<'a>> + error::FromExternalError<Span<'a>, std::str::Utf8Error> {
        combinator::map(read_string, Byte)(data)
    }
}
impl StructValue<'_> {
    fn read<'a, E>(data: Span<'a>) -> nom::IResult<Span<'a>, StructValue, E>
        where
            E: error::ParseError<Span<'a>> + error::ContextError<Span<'a>> + error::FromExternalError<Span<'a>, std::str::Utf8Error> {
        error::context(
            "StructValue",
            combinator::map(multi::many_till(Property::read, read_none), |(value, _)| StructValue(value))
        )(data)
    }
}

fn read_value_array<'a, E>(data: Span<'a>, t: PropertyType<'a>, count: usize) -> nom::IResult<Span<'a>, ValueArray<'a>, E>
    where
        E: error::ParseError<Span<'a>> + error::ContextError<Span<'a>> + error::FromExternalError<Span<'a>, std::str::Utf8Error> {
    match t {
        PropertyType::IntProperty => combinator::map(multi::count(Int::read, count), ValueArray::Int)(data),
        PropertyType::UInt32Property => combinator::map(multi::count(UInt32::read, count), ValueArray::UInt32)(data),
        PropertyType::FloatProperty => combinator::map(multi::count(Float::read, count), ValueArray::Float)(data),
        PropertyType::ByteProperty => combinator::map(multi::count(Byte::read, count), ValueArray::Byte)(data),
        _ => panic!("Missing ValueArray {:?}", t)
    }
}

fn read_value_value<'a>(data: Span<'a>, t: PropertyType<'a>) -> nom::IResult<Span<'a>, ValueValue<'a>> {
    match t {
        PropertyType::IntProperty => combinator::map(Int::read, |r| ValueValue::Base(ValueBase::Int(r)))(data),
        PropertyType::UInt32Property => combinator::map(UInt32::read, |r| ValueValue::Base(ValueBase::UInt32(r)))(data),
        PropertyType::FloatProperty => combinator::map(Float::read, |r| ValueValue::Base(ValueBase::Float(r)))(data),
        PropertyType::ByteProperty => combinator::map(Byte::read, |r| ValueValue::Base(ValueBase::Byte(r)))(data),
        //PropertyType::StructProperty => read_value_value_helper(data, |r| ValueValue::Struct(r)),
        _ => panic!("Missing ValueValue {:?}", t)
    }
}
fn read_property_value<'a, E>(data: Span<'a>, t: PropertyType<'a>) -> nom::IResult<Span<'a>, PropertyValue<'a>, E>
    where
        E: error::ParseError<Span<'a>> + error::ContextError<Span<'a>> + error::FromExternalError<Span<'a>, std::str::Utf8Error> {
    match t {
        PropertyType::IntProperty => {
            combinator::map(sequence::tuple((read_optional_uuid, Int::read)), |(id, value)| PropertyValue::Int { id, value })(data)
        },
        PropertyType::UInt32Property => {
            combinator::map(sequence::tuple((read_optional_uuid, UInt32::read)), |(id, value)| PropertyValue::UInt32 { id, value })(data)
        },
        PropertyType::FloatProperty => {
            combinator::map(sequence::tuple((read_optional_uuid, Float::read)), |(id, value)| PropertyValue::Float { id, value })(data)
        },
        PropertyType::BoolProperty => {
            combinator::map(sequence::tuple((Bool::read, read_optional_uuid)), |(value, id)| PropertyValue::Bool { id, value })(data)
        },
        PropertyType::ByteProperty => {
            combinator::map(sequence::tuple((read_optional_uuid, read_string, Byte::read)), |(id, enum_type, value)| PropertyValue::Byte { id, enum_type, value })(data)
        },
        PropertyType::StructProperty => {
            combinator::map(sequence::tuple((read_string, read_uuid, read_optional_uuid, StructValue::read)), |(struct_type, struct_id, id, value)| PropertyValue::Struct { id, value, struct_type, struct_id })(data)
        },
        PropertyType::ArrayProperty => {
            let (data, (array_type, id, count)) = sequence::tuple((read_type, read_optional_uuid, number::complete::le_u32))(data)?;
            let (data, value) = read_value_array(data, array_type, count as usize)?;
            Ok((data, PropertyValue::Array { id, value }))
        },
        _ => panic!("Missing PropertyValue {:?}", t)
    }
}

#[derive(Debug,PartialEq,Eq)]
pub struct CustomFormatData {
    pub id: uuid::Uuid,
    pub value: i32,
}
impl CustomFormatData {
    fn read<'a, E>(data: Span<'a>) -> nom::IResult<Span<'a>, CustomFormatData, E>
        where
            E: error::ParseError<Span<'a>> + error::ContextError<Span<'a>> + error::FromExternalError<Span<'a>, std::str::Utf8Error> {
        combinator::map(sequence::tuple((
            read_uuid,
            number::complete::le_i32,
        )), |(
            id,
            value,
        )| CustomFormatData {
            id,
            value,
        })(data)
    }
}

#[derive(Debug,PartialEq,Eq)]
pub struct Header<'a> {
    pub save_game_version: u32,
    pub package_version: u32,
    pub engine_version_major: u16,
    pub engine_version_minor: u16,
    pub engine_version_patch: u16,
    pub engine_version_build: u32,
    pub engine_version: &'a str,
    pub custom_format_version: u32,
    pub custom_format: Vec<CustomFormatData>,
}
impl Header<'_> {
    fn read<'a, E>(data: Span<'a>) -> nom::IResult<Span<'a>, Header, E>
        where
            E: error::ParseError<Span<'a>> + error::ContextError<Span<'a>> + error::FromExternalError<Span<'a>, std::str::Utf8Error> {
        combinator::map(sequence::tuple((
            bytes::complete::tag("GVAS"),
            number::complete::le_u32,
            number::complete::le_u32,
            number::complete::le_u16,
            number::complete::le_u16,
            number::complete::le_u16,
            number::complete::le_u32,
            read_string,
            number::complete::le_u32,
            multi::length_count(number::complete::le_u32, CustomFormatData::read),
        )), |(
            _,
            save_game_version,
            package_version,
            engine_version_major,
            engine_version_minor,
            engine_version_patch,
            engine_version_build,
            engine_version,
            custom_format_version,
            custom_format,
        )| Header {
            save_game_version,
            package_version,
            engine_version_major,
            engine_version_minor,
            engine_version_patch,
            engine_version_build,
            engine_version,
            custom_format_version,
            custom_format,
        })(data)
    }
}

#[derive(Debug,PartialEq)]
pub struct Root<'a> {
    pub save_game_type: &'a str,
    pub root: StructValue<'a>,
}
impl Root<'_> {
    fn read<'a, E>(data: Span<'a>) -> nom::IResult<Span<'a>, Root, E>
        where
            E: error::ParseError<Span<'a>> + error::ContextError<Span<'a>> + error::FromExternalError<Span<'a>, std::str::Utf8Error> {
        error::context("Root", combinator::map(sequence::tuple((
            read_string,
            StructValue::read,
        )), |(
            save_game_type,
            root,
        )| Root {
            save_game_type,
            root,
        }))(data)
    }
}

#[derive(Debug,PartialEq)]
pub struct Save<'a> {
    pub header: Header<'a>,
    pub root: Root<'a>,
}
impl Save<'_> {
    fn read<'a, E>(data: Span<'a>) -> nom::IResult<Span<'a>, Save, E>
        where
            E: error::ParseError<Span<'a>> + error::ContextError<Span<'a>> + error::FromExternalError<Span<'a>, std::str::Utf8Error> {
        error::context("Save::read",
            combinator::map(sequence::tuple((
                Header::read,
                Root::read,
            )), |(
                header,
                root,
            )| Save {
                header,
                root,
            })
        )(data)
    }
}

fn main() {
    println!("Hello, world!");
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use crate::*;

    fn map_tuple<'a, R>(data: (Span<'a>, R)) -> (&'a [u8], R) {
        (data.0.fragment(), data.1)
    }

    fn read_helper<'a, R, F, E>(mut f: F, data: &'a [u8]) -> (&'a [u8], R)
        where
            E: error::ParseError<Span<'a>> + error::ContextError<Span<'a>> + error::FromExternalError<Span<'a>, std::str::Utf8Error> + std::fmt::Debug,
            F: FnMut(Span<'a>) -> nom::IResult<Span<'a>, R, E>,
            R: std::fmt::Debug + std::cmp::PartialEq {
        map_tuple(f(Span::new(data)).unwrap())
    }

    fn assert_read<'a, R, F, E>(f: F, data: &'a [u8], expected: R)
        where
            E: error::ParseError<Span<'a>> + error::ContextError<Span<'a>> + error::FromExternalError<Span<'a>, std::str::Utf8Error> + std::fmt::Debug,
            F: FnMut(Span<'a>) -> nom::IResult<Span<'a>, R, E>,
            R: std::fmt::Debug + std::cmp::PartialEq {
        assert_eq!(read_helper(f, data), (&b""[..], expected));
    }

    #[test]
    fn test_uuid() {
        assert_read(
            read_uuid::<nom::error::VerboseError<Span>>,
            b"\x67\xB6\x6F\x72\xB3\xE8\x10\x4D\xA0\xE5\x82\xF5\xBF\x0F\x20\xDB",
            uuid::uuid!("726FB667-E8B3-4D10-A0E5-82F5BF0F20DB")
        );
    }

    #[test]
    fn test_optional_uuid() {
        assert_read(
            read_optional_uuid::<nom::error::VerboseError<Span>>,
            b"\x01\x67\xB6\x6F\x72\xB3\xE8\x10\x4D\xA0\xE5\x82\xF5\xBF\x0F\x20\xDB",
            Some(uuid::uuid!("726FB667-E8B3-4D10-A0E5-82F5BF0F20DB"))
        );
        assert_read(
            read_optional_uuid::<nom::error::VerboseError<Span>>,
            b"\x00",
            None
        );
    }

    #[test]
    fn test_read_string() {
        assert_eq!(map_tuple(read_string::<nom::error::VerboseError<Span>>(Span::new(b"\x04\x00\x00\x00\x4D\x61\x78\x00")).unwrap()), (&b""[..], "Max"));
        assert_read(
            read_string::<nom::error::VerboseError<Span>>,
            b"\x01\x00\x00\x00\x00",
            ""
        );
    }

    #[test]
    fn test_read_int() {
        assert_read(
            Int::read::<nom::error::VerboseError<Span>>,
            b"\x02\x00\x00\x00",
            Int(2)
        );
        assert_read(
            sequence::tuple((read_optional_uuid::<nom::error::VerboseError<Span>>, Int::read)),
            b"\x00\x02\x00\x00\x00",
            (None, Int(2))
        );
        assert_read(
            combinator::map(sequence::tuple((read_optional_uuid::<nom::error::VerboseError<Span>>, Int::read)), |(id, value)| PropertyValue::Int { id, value }),
            b"\x00\x02\x00\x00\x00",
            PropertyValue::Int{id: None, value: Int(2)}
        );
    }


    static SAVE: &'static [u8] = include_bytes!("../trash/test/76561198083438003_Experimental_Player_Slot_dsaf.sav");
    #[test]
    fn test_read_header() {
        assert_eq!(
            read_helper(Header::read::<error::VerboseError<Span>>, SAVE).1,
            Header {
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
            }
        );
    }

    #[test]
    fn test_read_save() {
        let save = Save::read::<error::VerboseError<Span>>(Span::new(SAVE)).unwrap();
        //println!("{:?}", property1);
        //println!("{:?}", sequence::tuple((Header::parse, Root::parse))(SAVE).unwrap().1.1);
        //assert_eq!(read_string(b"\x01\x00\x00\x00\x00").unwrap(), (&b""[..], ""));
    }

    #[test]
    fn test_read_property_value() {
        assert_read(
            |data| read_property_value::<nom::error::VerboseError<Span>>(data, PropertyType::IntProperty),
            b"\x01\x19\x4D\x0C\x43\x70\x49\x54\x71\x69\x9B\x69\x87\xE5\xB0\x90\xDF\x0A\x00\x00\x00",
            PropertyValue::Int {
                id: Some(uuid::uuid!("430c4d1949707154699b6987e5b090df")),
                value: Int(10)
            }
        );
        //assert_eq!(PropertyValue::parse(b"\x00\x0A\x00\x00\x00", PropertyType::IntProperty).unwrap(), (&b""[..], PropertyValue::IntProperty { id: None, value: 10 } ));
    }

    #[test]
    fn test_read_int_property() {
        assert_read(
            Property::read::<nom::error::VerboseError<Span>>,
            b"\x0E\x00\x00\x00\x56\x65\x72\x73\x69\x6F\x6E\x4E\x75\x6D\x62\x65\x72\x00\x0C\x00\x00\x00\x49\x6E\x74\x50\x72\x6F\x70\x65\x72\x74\x79\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00",
            Property {
                name: "VersionNumber",
                value: PropertyValue::Int {
                    id: None,
                    value: Int(2),
                }
            }
        );
    }

    #[test]
    fn test_read_struct_property() {
        assert_read(
            error::context("Property::read", Property::read::<nom::error::VerboseError<Span>>),
            b"\x12\x00\x00\x00\x56\x61\x6E\x69\x74\x79\x4D\x61\x73\x74\x65\x72\x79\x53\x61\x76\x65\x00\x0F\x00\x00\x00\x53\x74\x72\x75\x63\x74\x50\x72\x6F\x70\x65\x72\x74\x79\x00\x8D\x00\x00\x00\x00\x00\x00\x00\x12\x00\x00\x00\x56\x61\x6E\x69\x74\x79\x4D\x61\x73\x74\x65\x72\x79\x53\x61\x76\x65\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x4C\x65\x76\x65\x6C\x00\x0C\x00\x00\x00\x49\x6E\x74\x50\x72\x6F\x70\x65\x72\x74\x79\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x8C\x00\x00\x00\x03\x00\x00\x00\x58\x50\x00\x0C\x00\x00\x00\x49\x6E\x74\x50\x72\x6F\x70\x65\x72\x74\x79\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x3A\x23\x00\x00\x1A\x00\x00\x00\x48\x61\x73\x41\x77\x61\x72\x64\x65\x64\x46\x6F\x72\x4F\x6C\x64\x50\x75\x72\x63\x68\x61\x73\x65\x73\x00\x0D\x00\x00\x00\x42\x6F\x6F\x6C\x50\x72\x6F\x70\x65\x72\x74\x79\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x05\x00\x00\x00\x4E\x6F\x6E\x65\x00",
            Property {
                name: "VanityMasterySave",
                value: PropertyValue::Struct {
                    id: None,
                    value: StructValue(vec![
                        Property {
                            name: "Level",
                            value: PropertyValue::Int {
                                id: None,
                                value: Int(140),
                            },
                        },
                        Property {
                            name: "XP",
                            value: PropertyValue::Int {
                                id: None,
                                value: Int(9018),
                            },
                        },
                        Property {
                            name: "HasAwardedForOldPurchases",
                            value: PropertyValue::Bool {
                                id: None,
                                value: Bool(true),
                            },
                        },
                    ]),
                    struct_type: "VanityMasterySave",
                    struct_id: uuid::uuid!("00000000000000000000000000000000"),
                }
            }
        );
    }

    #[test]
    fn test_read_array_property() {
        assert_read(
            Property::read::<nom::error::VerboseError<Span>>,
            b"\x0C\x00\x00\x00\x53\x74\x61\x74\x49\x6E\x64\x69\x63\x65\x73\x00\x0E\x00\x00\x00\x41\x72\x72\x61\x79\x50\x72\x6F\x70\x65\x72\x74\x79\x00\x08\x00\x00\x00\x00\x00\x00\x00\x0C\x00\x00\x00\x49\x6E\x74\x50\x72\x6F\x70\x65\x72\x74\x79\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00",
            Property {
                name: "StatIndices",
                value: PropertyValue::Array {
                    id: None,
                    value: ValueArray::Int(vec![Int(0)])
                }
            }
        );
    }



    static SAVE2: &'static [u8] = include_bytes!("../trash/test/Mods/Speedster/Config2.sav");
    #[test]
    fn test_read_save2() {
        let save = Save::read::<error::VerboseError<Span>>(Span::new(SAVE2)).unwrap();
        //println!("{:?}", save);
    }

    #[test]
    fn test_read_struct() {
        assert_read(
            StructValue::read::<nom::error::VerboseError<Span>>,
            b"\x0A\x00\x00\x00\x4D\x75\x6C\x65\x53\x70\x65\x65\x64\x00\x0E\x00\x00\x00\x46\x6C\x6F\x61\x74\x50\x72\x6F\x70\x65\x72\x74\x79\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\xC0\x79\x44\x05\x00\x00\x00\x4E\x6F\x6E\x65\x00",
            StructValue (
                vec![
                    Property {
                        name: "MuleSpeed",
                        value: PropertyValue::Float {
                            id: None,
                            value: Float(999.0)
                        },
                    },
                ]
            )
        );
    }






}
