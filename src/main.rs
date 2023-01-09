use std::fs::{File, OpenOptions};
use std::io::{stdin, stdout, BufRead, BufReader, BufWriter, Write, Cursor};

use clap::{Parser, Subcommand};
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

#[derive(Subcommand, Debug)]
enum Action {
   /// Convert binary save to plain text JSON
   ToJson(IO),
   /// Convert JSON back to binary save
   FromJson(IO),
   /// Launch $EDITOR to edit a save file as JSON in place
   Edit(Edit),
}

#[derive(Parser, Debug)]
#[command(author, version)]
struct Args {
   #[command(subcommand)]
   action: Action,
}

pub fn main() -> Result<()> {
    let args = Args::parse();

    match args.action {
        Action::ToJson(io) => {
            let save = Save::read(&mut input(&io.input)?)?;
            serde_json::to_writer(output(&io.output)?, &save)?;
        },
        Action::FromJson(io) => {
            let save: Save = serde_json::from_reader(&mut input(&io.input)?)?;
            save.write(&mut output(&io.output)?)?;
        },
        Action::Edit(edit) => {
            let editor = match edit.editor {
                Some(editor) => editor,
                None => std::env::var("EDITOR").unwrap_or_else(|_| "vim".to_string())
            };

            // read and parse save file
            let buffer = std::fs::read(&edit.path)?;
            let save = Save::read(&mut Cursor::new(&buffer))?;
            let value = serde_json::to_value(save)?;

            // create temp file and write formatted JSON to it
            let temp = tempfile::Builder::new()
                .suffix(".json")
                .tempfile()?;
            serde_json::to_writer_pretty(BufWriter::new(&temp), &value)?;

            // launch editor
            std::process::Command::new(editor)
                .arg(temp.path())
                .stdin(std::process::Stdio::piped())
                .spawn()?
                .wait()?;

            // rebuild save if modified
            let modified_save: Save = serde_json::from_reader(BufReader::new(temp.reopen()?))?;
            let mut out_buffer = vec![];
            modified_save.write(&mut out_buffer)?;
            if buffer == out_buffer {
                println!("File unchanged, doing nothing.");
            } else {
                println!("File modified, writing new save.");
                OpenOptions::new()
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

fn input<'a>(path: &str) -> Result<Box<dyn BufRead + 'a>> {
    Ok(match path {
        "-" => {
            Box::new(BufReader::new(stdin().lock()))
        },
        p => {
            Box::new(BufReader::new(File::open(p)?))
        },
    })
}

fn output<'a>(path: &str) -> Result<Box<dyn Write + 'a>> {
    Ok(match path {
        "-" => {
            Box::new(BufWriter::new(stdout().lock()))
        },
        p => {
            Box::new(BufWriter::new(OpenOptions::new()
                .create(true)
                .truncate(true)
                .write(true)
                .open(p)?))
        }
    })
}
