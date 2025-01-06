use std::fs::File;

use uesave::{PropertyInner, Save, StructValue, ValueArray};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    const PROP_POSITION: &str = "PropPosition_7_B8CD81CD4E138D8E06FBBA8056FE4C85";
    const PROP_NAME: &str = "PropName_10_4BB2A20D47DA97D8ECB7D888147BBB97";
    const IS_STATIC_MESH: &str = "IsStaticMesh_20_AB977B2F47FD519F53FB8CB85490631B";
    const DYNAMIC_PROP_CLASS: &str = "DynamicPropClass_19_AA6C35BE4D24AB6B42E8999E55661065";
    const STATIC_MESH: &str = "StaticMesh_18_BAF2BF524DA3EA4A985B9D87B727223A";

    let save = Save::read(&mut File::open(
        "examples/space-rig-decorator/PropPack.sav",
    )?)?;

    use PropertyInner::*;
    let props = &save.root.properties["PropList"];
    if let Array(ValueArray::Struct { value, .. }) = &props.inner {
        for prop in value {
            if let StructValue::Struct(p) = prop {
                if let Struct(StructValue::Struct(value)) = &p[PROP_POSITION].inner {
                    if let Struct(StructValue::Quat(value)) = &value["Rotation"].inner {
                        print!("{}:{}:{}:{}:", value.x, value.y, value.z, value.w);
                    }
                    if let Struct(StructValue::Vector(value)) = &value["Translation"].inner {
                        print!("{}:{}:{}:", value.x, value.y, value.z);
                    }
                    if let Struct(StructValue::Vector(value)) = &value["Scale3D"].inner {
                        print!("{}:{}:{}:", value.x, value.y, value.z);
                    }
                }
                if let Str(value) = &p[PROP_NAME].inner {
                    print!("{value}:");
                }
                if let Bool(value) = &p[IS_STATIC_MESH].inner {
                    print!("{value}:");
                }
                if let Object(value) = &p[DYNAMIC_PROP_CLASS].inner {
                    print!("{value}:");
                }
                if let Object(value) = &p[STATIC_MESH].inner {
                    println!("{value}");
                }
            }
        }
    }
    Ok(())
}
