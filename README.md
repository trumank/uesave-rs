# uesave

A library for reading and writing Unreal Engine save files (commonly referred to
as GVAS).

It has been tested on an extensive set of object structures and can fully read
and write Deep Rock Galactic save files (and likely a lot more).

There is a small binary utility to quickly convert saves to and from a plain
text JSON format which can be used for manual save editing.

## Usage

```console
$ cargo install --git https://github.com/trumank/uesave-rs.git
$ usave --help
Usage: uesave <COMMAND>

Commands:
  to-json    Convert binary save to plain text JSON
  from-json  Convert JSON back to binary save
  edit       Launch $EDITOR to edit a save file as JSON in place
  help       Print this message or the help of the given subcommand(s)

Options:
  -h, --help     Print help information
  -V, --version  Print version information

```
![edit](https://user-images.githubusercontent.com/1144160/210157064-234da188-ad20-416f-9ea5-7d2956168a20.svg)
