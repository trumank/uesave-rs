use std::io::Cursor;

use anyhow::{Result, anyhow};

use uesave::Save;

fn main() -> Result<()> {
    if let Some(save) = std::env::args().nth(1) {
        let buffer = uesave::read_file(&save);
        let mut rdr = Cursor::new(&buffer[..]);
        let save = Save::read(&mut rdr)?;
        println!("{:#?}", save);
        Ok(())
    } else {
        Err(anyhow!("Expected path to save"))
    }
}
