use std::fs::File;

use anyhow::Result;

use uesave::{PropertyMeta, Save, StructValue, ValueArray};

#[rustfmt::skip]
fn main() -> Result<()> {
    let save = Save::read(&mut File::open(
        "examples/space-rig-decorator/PropPack.sav",
    )?)?;
    let props = &save.root.root[0].value;
    if let PropertyMeta::Array { value: ValueArray::Struct { value, .. }, .. } = props {
        for prop in value {
            if let StructValue::Struct(p) = prop {
                if let PropertyMeta::Struct { value: StructValue::Struct(value), .. } = &p[0].value {
                    if let PropertyMeta::Struct { value: StructValue::Quat(value) , .. } = &value[0].value {
                        print!("{}:{}:{}:{}:", value.x, value.y, value.z, value.w);
                    }
                    if let PropertyMeta::Struct { value: StructValue::Vector(value), .. } = &value[1].value {
                        print!("{}:{}:{}:", value.x, value.y, value.z);
                    }
                    if let PropertyMeta::Struct { value: StructValue::Vector(value), .. } = &value[2].value {
                        print!("{}:{}:{}:", value.x, value.y, value.z);
                    }
                }
                if let PropertyMeta::Str { value, .. } = &p[1].value {
                    print!("{value}:");
                }
                if let PropertyMeta::Bool { value, .. } = &p[2].value {
                    print!("{value}:");
                }
                if let PropertyMeta::Object { value, .. } = &p[3].value {
                    print!("{value}:");
                }
                if let PropertyMeta::Object { value, .. } = &p[4].value {
                    println!("{value}");
                }
            }
        }
    }
    Ok(())
}
