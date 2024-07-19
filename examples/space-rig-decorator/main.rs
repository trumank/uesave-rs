use std::fs::File;

use anyhow::Result;

use uesave::{Property, PropertyInner, Save, StructValue, ValueArray};

#[rustfmt::skip]
fn main() -> Result<()> {
    let save = Save::read(&mut File::open(
        "examples/space-rig-decorator/PropPack.sav",
    )?)?;
    let props = &save.root.properties["PropList"];
    if let Property { inner: PropertyInner::Array { value: ValueArray::Struct { value, .. }, .. }, .. } = props {
        for prop in value {
            if let StructValue::Struct(p) = prop {
                if let Property { inner: PropertyInner::Struct { value: StructValue::Struct(value), .. }, .. } = &p["PropPosition_7_B8CD81CD4E138D8E06FBBA8056FE4C85"] {
                    if let Property { inner: PropertyInner::Struct { value: StructValue::Quat(value), .. }, .. } = &value["Rotation"] {
                        print!("{}:{}:{}:{}:", value.x, value.y, value.z, value.w);
                    }
                    if let Property { inner: PropertyInner::Struct { value: StructValue::Vector(value), .. }, .. } = &value["Translation"] {
                        print!("{}:{}:{}:", value.x, value.y, value.z);
                    }
                    if let Property { inner: PropertyInner::Struct { value: StructValue::Vector(value), .. }, .. } = &value["Scale3D"] {
                        print!("{}:{}:{}:", value.x, value.y, value.z);
                    }
                }
                if let Property { inner: PropertyInner::Str(value), .. } = &p["PropName_10_4BB2A20D47DA97D8ECB7D888147BBB97"] {
                    print!("{value}:");
                }
                if let Property { inner: PropertyInner::Bool(value), .. } = &p["IsStaticMesh_20_AB977B2F47FD519F53FB8CB85490631B"] {
                    print!("{value}:");
                }
                if let Property { inner: PropertyInner::Object(value), .. } = &p["DynamicPropClass_19_AA6C35BE4D24AB6B42E8999E55661065"] {
                    print!("{value}:");
                }
                if let Property { inner: PropertyInner::Object(value), .. } = &p["StaticMesh_18_BAF2BF524DA3EA4A985B9D87B727223A"] {
                    println!("{value}");
                }
            }
        }
    }
    Ok(())
}
