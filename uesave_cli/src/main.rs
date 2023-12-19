use std::fs::{self, File, OpenOptions};
use std::io::{stdin, stdout, BufRead, BufReader, BufWriter, Cursor, Write};

use anyhow::{anyhow, Result};
use clap::{Parser, Subcommand};

use uesave::{Save, SaveReader, StructType, Types};

#[derive(Parser, Debug)]
struct ActionToJson {
    #[arg(short, long, default_value = "-")]
    input: String,

    #[arg(short, long, default_value = "-")]
    output: String,

    /// Silence any parse warnings
    #[arg(long)]
    no_warn: bool,

    /// Save files do not contain enough context to parse structs inside MapProperty or SetProperty.
    /// uesave will attempt to guess, but if it is incorrect the save will fail to parse and the
    /// type must be manually specified.
    ///
    /// Examples:
    ///   -t .UnlockedItemSkins.Skins=Guid
    ///   -t .EnemiesKilled.Key=Guid
    ///   -t .EnemiesKilled.Value=Struct
    #[arg(short, long, value_parser = parse_type)]
    r#type: Vec<(String, StructType)>,
}

#[derive(Parser, Debug)]
struct ActionFromJson {
    #[arg(short, long, default_value = "-")]
    input: String,

    #[arg(short, long, default_value = "-")]
    output: String,
}

#[derive(Parser, Debug)]
struct ActionEdit {
    #[arg(required = true, index = 1)]
    path: String,

    /// Silence any parse warnings
    #[arg(long)]
    no_warn: bool,

    /// Save files do not contain enough context to parse structs inside MapProperty or SetProperty.
    /// uesave will attempt to guess, but if it is incorrect the save will fail to parse and the
    /// type must be manually specified.
    ///
    /// Examples:
    ///   -t .UnlockedItemSkins.Skins=Guid
    ///   -t .EnemiesKilled.Key=Guid
    ///   -t .EnemiesKilled.Value=Struct
    #[arg(short, long, value_parser = parse_type)]
    r#type: Vec<(String, StructType)>,
}

#[derive(Parser, Debug)]
struct ActionTestResave {
    #[arg(required = true, index = 1)]
    path: String,

    /// If resave fails, write input.sav and output.sav to working directory for debugging
    #[arg(short, long)]
    debug: bool,

    /// Silence any parse warnings
    #[arg(long)]
    no_warn: bool,

    /// Trace and generate trace.json file
    #[arg(long)]
    trace: bool,

    /// Save files do not contain enough context to parse structs inside MapProperty or SetProperty.
    /// uesave will attempt to guess, but if it is incorrect the save will fail to parse and the
    /// type must be manually specified.
    ///
    /// Examples:
    ///   -t .UnlockedItemSkins.Skins=Guid
    ///   -t .EnemiesKilled.Key=Guid
    ///   -t .EnemiesKilled.Value=Struct
    #[arg(short, long, value_parser = parse_type)]
    r#type: Vec<(String, StructType)>,
}

#[derive(Subcommand, Debug)]
enum Action {
    /// Convert binary save to plain text JSON
    ToJson(ActionToJson),
    /// Convert JSON back to binary save
    FromJson(ActionFromJson),
    /// Launch editor to edit a save file as JSON in place
    Edit(ActionEdit),
    /// Test resave
    TestResave(ActionTestResave),
}

#[derive(Parser, Debug)]
#[command(author, version)]
struct Args {
    #[command(subcommand)]
    action: Action,
}

fn parse_type(t: &str) -> Result<(String, StructType)> {
    if let Some((l, r)) = t.rsplit_once('=') {
        Ok((l.to_owned(), r.into()))
    } else {
        Err(anyhow!("Malformed type"))
    }
}

pub fn main() -> Result<()> {
    let args = Args::parse();

    match args.action {
        Action::ToJson(action) => {
            let mut types = Types::new();
            for (path, t) in action.r#type {
                types.add(path, t);
            }

            let save = SaveReader::new()
                .log(!action.no_warn)
                .types(&types)
                .read(input(&action.input)?)?;
            serde_json::to_writer_pretty(output(&action.output)?, &save)?;
        }
        Action::FromJson(io) => {
            let save: Save = serde_json::from_reader(&mut input(&io.input)?)?;
            save.write(&mut output(&io.output)?)?;
        }
        Action::TestResave(action) => {
            let mut types = Types::new();
            for (path, t) in action.r#type {
                types.add(path, t);
            }

            let path = std::path::Path::new(&action.path);

            if let Ok(types_file) = fs::read_to_string(path.with_extension("types")) {
                for t in types_file.lines() {
                    if let Ok((path, t)) = parse_type(t) {
                        types.add(path, t);
                    }
                }
            }

            let mut input = std::io::Cursor::new(fs::read(path)?);
            let mut output = std::io::Cursor::new(vec![]);

            let sr = SaveReader::new().log(!action.no_warn).types(&types);
            if action.trace {
                ser_hex::read("trace.json", &mut input, |reader| sr.read(reader))?
            } else {
                sr.read(&mut input)?
            }
            .write(&mut output)?;

            let (input, output) = (input.into_inner(), output.into_inner());
            if input != output {
                if action.debug {
                    fs::write("input.sav", input)?;
                    fs::write("output.sav", output)?;
                }
                return Err(anyhow!("Resave did not match"));
            }
            println!("Resave successful");
        }
        Action::Edit(action) => {
            let mut types = Types::new();
            for (path, t) in action.r#type {
                types.add(path, t);
            }

            let save = SaveReader::new()
                .log(!action.no_warn)
                .types(&types)
                .read(Cursor::new(fs::read(&action.path)?))?;
            let modified_save: Save = serde_json::from_slice(&edit::edit_bytes_with_builder(
                serde_json::to_vec_pretty(&save)?,
                tempfile::Builder::new().suffix(".json"),
            )?)?;

            if save == modified_save {
                println!("File unchanged, doing nothing.");
            } else {
                println!("File modified, writing new save.");
                modified_save.write(&mut BufWriter::new(
                    OpenOptions::new()
                        .create(true)
                        .truncate(true)
                        .write(true)
                        .open(action.path)?,
                ))?;
            }
        }
    }
    Ok(())
}

fn input<'a>(path: &str) -> Result<Box<dyn BufRead + 'a>> {
    Ok(match path {
        "-" => Box::new(BufReader::new(stdin().lock())),
        p => Box::new(BufReader::new(File::open(p)?)),
    })
}

fn output<'a>(path: &str) -> Result<Box<dyn Write + 'a>> {
    Ok(match path {
        "-" => Box::new(BufWriter::new(stdout().lock())),
        p => Box::new(BufWriter::new(
            OpenOptions::new()
                .create(true)
                .truncate(true)
                .write(true)
                .open(p)?,
        )),
    })
}
