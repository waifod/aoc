use regex::Regex;
use std::fs;

const FILE_PATH: &str = "./input/input.txt";

fn parse_input(file_path: &str) -> String {
    fs::read_to_string(file_path).unwrap()
}

fn solve_a(input: &str) -> usize {
    let re = Regex::new(r"mul\(([0-9]{1,3}),([0-9]{1,3})\)").unwrap();
    re.captures_iter(input)
        .map(|c| c.extract())
        .map(|(_,[m1,m2])| m1.parse::<usize>().unwrap() * m2.parse::<usize>().unwrap())
        .sum()
}

fn solve_b(input: &str) -> usize {
    let re = Regex::new(r"mul\(([0-9]{1,3}),([0-9]{1,3})\)|do\(\)|don't\(\)").unwrap();
    let mut enabled = true;

    re.captures_iter(input)
        .filter(|capture| {
            let fst = capture.get(0).unwrap().as_str();
            if fst == "do()" {
                enabled = true;
                return false;
            } else if fst == "don't()" {
                enabled = false;
            }
            enabled
        })
        .map(|capture|
            capture.get(1).unwrap().as_str().parse::<usize>().unwrap() 
                * capture.get(2).unwrap().as_str().parse::<usize>().unwrap()
        )
        .sum()
}

fn main() {
    let input = parse_input(FILE_PATH);
    println!("Solution 1: {}", solve_a(&input));
    println!("Solution 2: {}", solve_b(&input));
}
