use std::collections::HashSet;

const FILE_PATH: &str = "./input/input.txt";

fn parse_input(input: &str) -> Vec<Vec<i32>> {
    input
        .lines()
        .map(|line| {
            line.split_whitespace()
                .map(|num| num.parse::<i32>().unwrap())
                .collect()
        })
        .collect()
}

fn is_safe(nums: &[i32]) -> bool {
    let pos_diffs: HashSet<i32> = (1..4).collect();
    let neg_diffs: HashSet<i32> = (-3..0).collect();
    let diffs: HashSet<i32> = nums.windows(2).map(|w| w[0] - w[1]).collect();
    diffs.union(&pos_diffs).count() == 3 || diffs.union(&neg_diffs).count() == 3
}

fn solve1(input: &[Vec<i32>]) -> usize {
    input.iter().filter(|nums| is_safe(nums)).count()
}

fn solve2(input: &[Vec<i32>]) -> usize {
    input
        .iter()
        .filter(|nums| {
            if is_safe(nums) {
                return true;
            }

            (0..nums.len()).any(|i| {
                let new_nums: Vec<i32> = nums
                    .iter()
                    .enumerate()
                    .filter_map(|(j, n)| (j != i).then_some(*n))
                    .collect();
                is_safe(&new_nums)
            })
        })
        .count()
}

fn main() {
    println!("Solving AoC24, day 2...");
    let input = parse_input(&utils::get_input(FILE_PATH));
    println!("Part 1: {}", solve1(&input));
    println!("Part 2: {}", solve2(&input));
}
