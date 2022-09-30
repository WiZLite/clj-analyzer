#![allow(unused)]

use clap::Parser;

/// Search for a pattern in a file and display the lines that contain it.
#[derive(Parser)]
struct Cli {
    /// The path to the file to read
    path: std::path::PathBuf,
}

fn main() {
    let args = Cli::parse();
    let path = &args.path;
    let content = std::fs::read_to_string(path).expect("could not read file");
    let mut line_index = 0;
    for line in content.lines() {
        line_index += 1;
        if let Some(column_index) = line.find("hoge") {
            println!("{}:{}:{}: warning: the word 'hoge' is prohibited for political reason😂", path.display(), line_index, column_index);
        }
    }
}
