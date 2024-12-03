use regex::Regex;

use utils;

const INPUT_PATH: &str = "./input/input.txt";

fn solve1(input: &str) -> usize {
    let re = Regex::new(r"mul\(([0-9]{1,3}),([0-9]{1,3})\)").unwrap();
    re.captures_iter(input)
        .map(|capture| capture.extract())
        .map(|(_,[m1,m2])| m1.parse::<usize>().unwrap() * m2.parse::<usize>().unwrap())
        .sum()
}

fn solve2(input: &str) -> usize {
    let re = Regex::new(r"(mul\(([0-9]{1,3}),([0-9]{1,3})\)|do\(\)|don't\(\))").unwrap();
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
            capture.get(2).unwrap().as_str().parse::<usize>().unwrap() 
                * capture.get(3).unwrap().as_str().parse::<usize>().unwrap()
        )
        .sum()
}

fn main() {
    println!("Solving AoC24, day 3...");
    let input = utils::get_input(INPUT_PATH);
    println!("Part 1: {}", solve1(&input));
    println!("Part 2: {}", solve2(&input));
}
