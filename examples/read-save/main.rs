use anyhow::{anyhow, Result};

use uesave::Save;

fn main() -> Result<()> {
    if let Some(save) = std::env::args().nth(1) {
        let save = Save::read(&mut std::fs::File::open(&save)?)?;
        println!("{:#?}", save);
        Ok(())
    } else {
        Err(anyhow!("Expected path to save"))
    }
}
