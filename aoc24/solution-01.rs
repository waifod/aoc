use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

const FILE_PATH: &str = "./input-01.txt";

fn read_lines(filename: impl AsRef<Path>) -> io::Lines<io::BufReader<File>> {
    let file = File::open(filename).unwrap();
    io::BufReader::new(file).lines()
}

fn parse_input(file_path: &str) -> (Vec<i32>, Vec<i32>) {
    read_lines(file_path).flatten().map(|line| {
        let mut split_line = line.split_whitespace();
        (
            split_line.next().unwrap().parse::<i32>().unwrap(),
            split_line.next().unwrap().parse::<i32>().unwrap()
        )
    }).collect()
}

fn solve_a(input: &(Vec<i32>, Vec<i32>)) -> i32 {
    let (mut left_ids, mut right_ids) = input.clone();
    left_ids.sort();
    right_ids.sort();
    left_ids.iter().zip(right_ids.iter()).map(|(l,r)| (l-r).abs()).sum()
}

fn solve_b(input: &(Vec<i32>, Vec<i32>)) -> i32 {
    let mut freq: HashMap<i32, i32> = HashMap::new();
    let (ref left_ids, ref right_ids) = input;
    right_ids.iter().for_each(|&n| *freq.entry(n).or_insert(0) += 1);
    left_ids.iter().map(|&l| l * *freq.entry(l).or_insert(0)).sum()
}

fn main() {
    let input = parse_input(FILE_PATH);
    println!("Solution 1: {}", solve_a(&input));
    println!("Solution 2: {}", solve_b(&input));
}
