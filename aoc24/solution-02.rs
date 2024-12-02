use std::collections::HashSet;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

const FILE_PATH: &str = "./input-02.txt";

fn read_lines(filename: impl AsRef<Path>) -> io::Lines<io::BufReader<File>> {
    let file = File::open(filename).unwrap();
    io::BufReader::new(file).lines()
}

fn parse_input(file_path: &str) -> Vec<Vec<i32>> {
    read_lines(file_path).flatten().map(|line|
        line.split_whitespace().map(|num| num.parse::<i32>().unwrap()).collect()
    ).collect()
}

fn is_safe(nums: &Vec<i32>) -> bool {
    let pos_diffs: HashSet<i32> = (1..4).collect();
    let neg_diffs: HashSet<i32> = (-3..0).collect();
    let diffs: HashSet<i32> = nums.windows(2).map(|w| w[0]-w[1]).collect();
    diffs.union(&pos_diffs).count() == 3 || diffs.union(&neg_diffs).count() == 3
}

fn solve_a(input: &Vec<Vec<i32>>) -> usize {
    input.into_iter().filter(|nums| is_safe(nums)).count()
}

fn solve_b(input: &Vec<Vec<i32>>) -> usize {
    input.into_iter().filter(|nums| {
        if is_safe(&nums) {
            return true
        }

        return (0..nums.len()).into_iter().any(|i| {
            let new_nums: Vec<i32> = nums.iter().enumerate().filter_map(|(j,n)| (j!=i).then_some(*n)).collect();
            is_safe(&new_nums)
        })
    }).count()
}

fn main() {
    let input = parse_input(FILE_PATH);
    println!("Solution 1: {}", solve_a(&input));
    println!("Solution 2: {}", solve_b(&input));
}
