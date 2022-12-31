use std::fs::File;
use std::io::{Read, Write};

use uesave::Save;

use clap::Parser;

#[derive(Parser, Debug)]
struct IO {
   #[arg(short, long, default_value = "-")]
   input: String,

   #[arg(short, long, default_value = "-")]
   output: String,
}

#[derive(clap::Subcommand, Debug)]
enum Action {
   ToJson(IO),
   FromJson(IO),
}

#[derive(clap::Parser, Debug)]
#[command(author, version)]
struct Args {
   #[command(subcommand)]
   action: Action,
}

pub fn main() {
    let args = Args::parse();

    match args.action {
        Action::ToJson(io) => {
            let buf = input(&*io.input);
            let mut cur = std::io::Cursor::new(&buf[..]);
            let save = Save::read(&mut cur).unwrap();
            let v = serde_json::to_value(&save).unwrap();
            output(&*io.output, v.to_string().as_bytes());
        },
        Action::FromJson(io) => {
            let buf = input(&*io.input);
            let save: Save = serde_json::from_slice(&buf).expect("Failed to parse input");
            let mut out_buf = vec![];
            save.write(&mut out_buf).expect("Failed to serialize save");
            output(&*io.output, &out_buf);
        },
    }
}

fn input(path: &str) -> Vec<u8> {
    match path {
        "-" => {
            let mut input =  Vec::new();
            let stdin = std::io::stdin();
            let mut handle = stdin.lock();
            handle.read_to_end(&mut input).expect("Error reading stdin");
            input
        },
        p => {
            let mut f = File::open(p).expect("no file found");
            let mut buffer = vec![];
            f.read_to_end(&mut buffer).expect("read failed");
            buffer
        },
    }
}

fn output(path: &str, buf: &[u8]) {
    match path {
        "-" => {
            std::io::stdout()
                .lock()
                .write_all(&buf)
                .expect("Failed to write output");
        },
        p => {
            std::fs::OpenOptions::new()
                .create(true)
                .truncate(true)
                .write(true)
                .open(p)
                .expect("Failed to open output file")
                .write_all(&buf).expect("Failed to write output");
        }
    }
}
