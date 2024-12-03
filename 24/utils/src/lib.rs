use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

pub fn read_lines(filename: impl AsRef<Path>) -> io::Lines<io::BufReader<File>> {
    let file = File::open(filename).unwrap();
    io::BufReader::new(file).lines()
}
