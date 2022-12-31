use std::fs::File;
use std::io::{Read, Write};

use clap::Parser;
use anyhow::Result;

use uesave::Save;

#[derive(Parser, Debug)]
struct IO {
   #[arg(short, long, default_value = "-")]
   input: String,

   #[arg(short, long, default_value = "-")]
   output: String,
}

#[derive(Parser, Debug)]
struct Edit {
   #[arg(required = true, index = 1)]
   path: String,

   #[arg(short, long)]
   editor: Option<String>,
}

#[derive(clap::Subcommand, Debug)]
enum Action {
   /// Convert binary save to plain text JSON
   ToJson(IO),
   /// Convert JSON back to binary save
   FromJson(IO),
   /// Launch $EDITOR to edit a save file as JSON in place
   Edit(Edit),
}

#[derive(clap::Parser, Debug)]
#[command(author, version)]
struct Args {
   #[command(subcommand)]
   action: Action,
}

pub fn main() -> Result<()> {
    let args = Args::parse();

    match args.action {
        Action::ToJson(io) => {
            let buf = input(&*io.input)?;
            let mut cur = std::io::Cursor::new(&buf[..]);
            let save = Save::read(&mut cur)?;
            let v = serde_json::to_value(&save)?;
            output(&*io.output, v.to_string().as_bytes())?;
        },
        Action::FromJson(io) => {
            let buf = input(&*io.input)?;
            let save: Save = serde_json::from_slice(&buf)?;
            let mut out_buf = vec![];
            save.write(&mut out_buf)?;
            output(&*io.output, &out_buf)?;
        },
        Action::Edit(edit) => {
            let editor = match edit.editor {
                Some(editor) => editor,
                None => std::env::var("EDITOR").unwrap_or("vim".to_string())
            };

            // read and parse save file
            let mut f = File::open(&edit.path)?;
            let mut buffer = vec![];
            f.read_to_end(&mut buffer)?;
            let mut cur = std::io::Cursor::new(&buffer[..]);
            let save = Save::read(&mut cur)?;
            let value = serde_json::to_value(&save)?;

            // create temp file and write formatted JSON to it
            let mut temp = tempfile::Builder::new()
                .suffix(".json")
                .tempfile()?;
            temp.write_all(serde_json::to_string_pretty(&value)?.as_bytes())?;

            // launch editor
            std::process::Command::new(editor)
                .arg(temp.path())
                .stdin(std::process::Stdio::piped())
                .spawn()?
                .wait()?;

            // rebuild save if modified
            let modified_save: Save = serde_json::from_reader(std::io::BufReader::new(temp.reopen()?))?;
            let mut out_buffer = vec![];
            modified_save.write(&mut out_buffer)?;
            if buffer == out_buffer {
                println!("File unchanged, doing nothing.");
            } else {
                println!("File modified, writing new save.");
                std::fs::OpenOptions::new()
                    .create(true)
                    .truncate(true)
                    .write(true)
                    .open(edit.path)?
                    .write_all(&out_buffer)?;
            }
        },
    }
    Ok(())
}

fn input(path: &str) -> Result<Vec<u8>> {
    Ok(match path {
        "-" => {
            let mut input =  Vec::new();
            let stdin = std::io::stdin();
            let mut handle = stdin.lock();
            handle.read_to_end(&mut input)?;
            input
        },
        p => {
            let mut f = File::open(p)?;
            let mut buffer = vec![];
            f.read_to_end(&mut buffer)?;
            buffer
        },
    })
}

fn output(path: &str, buf: &[u8]) -> Result<()> {
    match path {
        "-" => {
            std::io::stdout()
                .lock()
                .write_all(&buf)?;
        },
        p => {
            std::fs::OpenOptions::new()
                .create(true)
                .truncate(true)
                .write(true)
                .open(p)?
                .write_all(&buf)?;
        }
    }
    Ok(())
}
