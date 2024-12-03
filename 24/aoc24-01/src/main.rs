use std::collections::HashMap;

use utils;

const INPUT_PATH: &str = "./input/input.txt";

fn parse_input(input: &str) -> (Vec<i32>, Vec<i32>) {
    input.lines().map(|line| {
        let mut split_line = line.split_whitespace();
        (
            split_line.next().unwrap().parse::<i32>().unwrap(),
            split_line.next().unwrap().parse::<i32>().unwrap()
        )
    }).collect()
}

fn solve1(input: &(Vec<i32>, Vec<i32>)) -> i32 {
    let (mut left_ids, mut right_ids) = input.clone();
    left_ids.sort();
    right_ids.sort();
    left_ids.iter()
        .zip(right_ids.iter())
        .map(|(l,r)| (l-r).abs())
        .sum()
}

fn solve2(input: &(Vec<i32>, Vec<i32>)) -> i32 {
    let mut freq: HashMap<i32, i32> = HashMap::new();
    let (ref left_ids, ref right_ids) = input;
    right_ids.iter().for_each(|&n| *freq.entry(n).or_insert(0) += 1);
    left_ids.iter().map(|&l| l * *freq.entry(l).or_insert(0)).sum()
}

fn main() {
    println!("Solving AoC24, day 1...");
    let input = parse_input(&utils::get_input(INPUT_PATH));
    println!("Part 1: {}", solve1(&input));
    println!("Part 2: {}", solve2(&input));
}

#[cfg(test)]
mod test {
    use super::*;

    const TEST_INPUT: &str = "3   4
4   3
2   5
1   3
3   9
3   3";

    #[test]
    fn parsing() {
        let left: Vec<i32> = vec![3,4,2,1,3,3];
        let right: Vec<i32> = vec![4,3,5,3,9,3];
        assert_eq!((left, right), parse_input(TEST_INPUT));
    }

    #[test]
    fn part1() {
        let input = parse_input(TEST_INPUT);
        assert_eq!(solve1(&input), 11);
    }

    #[test]
    fn part2() {
        let input = parse_input(TEST_INPUT);
        assert_eq!(solve2(&input), 31);
    }
}
