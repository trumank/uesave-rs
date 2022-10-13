use std::io::Cursor;

use anyhow::Result;

use uesave::{Save, PropertyMeta, ValueArray, ValueStruct, ValueBase};

fn main() -> Result<()> {
    let buffer = uesave::read_file(&"PropPack.sav".to_owned());
    let mut rdr = Cursor::new(&buffer[..]);

    let save = Save::read(&mut rdr)?;
    let props = &save.root.root[0].value;
    if let PropertyMeta::Array{value, ..} = props {
        if let ValueArray::Struct{value, ..} = value {
            for prop in value {
                if let ValueStruct::Struct(p) = prop {
                    if let PropertyMeta::Struct{value, ..} = &p[0].value {
                        if let ValueStruct::Struct(value) = value {
                            if let PropertyMeta::Struct{value, ..} = &value[0].value {
                                if let ValueStruct::Base(ValueBase::Quat(value)) = value {
                                    print!("{}:{}:{}:{}:", value.x, value.y, value.z, value.w);
                                }
                            }
                            if let PropertyMeta::Struct{value, ..} = &value[1].value {
                                if let ValueStruct::Base(ValueBase::Vector(value)) = value {
                                    print!("{}:{}:{}:", value.x, value.y, value.z);
                                }
                            }
                            if let PropertyMeta::Struct{value, ..} = &value[2].value {
                                if let ValueStruct::Base(ValueBase::Vector(value)) = value {
                                    print!("{}:{}:{}:", value.x, value.y, value.z);
                                }
                            }
                        }
                    }
                    if let PropertyMeta::Str{value, ..} = &p[1].value {
                        print!("{value}:");
                    }
                    if let PropertyMeta::Bool{value, ..} = &p[2].value {
                        print!("{value}:");
                    }
                    if let PropertyMeta::Object{value, ..} = &p[3].value {
                        print!("{value}:");
                    }
                    if let PropertyMeta::Object{value, ..} = &p[4].value {
                        println!("{value}");
                    }
                }
            }
        }
    }
    Ok(())
}
