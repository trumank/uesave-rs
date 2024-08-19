use uesave::Save;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    if let Some(save) = std::env::args().nth(1) {
        let save = Save::read(&mut std::fs::File::open(save)?)?;
        println!("{save:#?}");
        Ok(())
    } else {
        Err("Expected path to save".into())
    }
}
