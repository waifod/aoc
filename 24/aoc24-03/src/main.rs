use regex::Regex;
use utils;

const FILE_PATH: &str = "./input/input.txt";

fn parse_input(file_path: &str) -> Vec<String> {
    utils::read_lines(file_path).flatten().map(|line|
        String::from(line)
    ).collect()
}

fn solve_a(input: &Vec<String>) -> i32 {
    let re = Regex::new(r"mul\(([0-9]{1,3}),([0-9]{1,3})\)").unwrap();
    input.into_iter()
        .map(|line| re.captures_iter(line))
        .flatten()
        .map(|c| c.extract())
        .map(|(_,[m1,m2])| m1.parse::<i32>().unwrap() * m2.parse::<i32>().unwrap())
        .sum()
}

fn solve_b(input: &Vec<String>) -> usize {
    0
}

fn main() {
    let input = parse_input(FILE_PATH);
    println!("Solution 1: {}", solve_a(&input));
    println!("Solution 2: {}", solve_b(&input));
}
