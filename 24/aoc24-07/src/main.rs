const INPUT_PATH: &str = "./input/input.txt";

fn parse_input(input: &str) -> Vec<(u64, Vec<u64>)> {
    input
        .lines()
        .filter_map(|line| {
            let mut splits = line.split(": ");
            let target = splits.next()?.parse::<u64>().unwrap();
            let values = splits
                .next()?
                .split_whitespace()
                .map(|val| val.parse::<u64>().unwrap())
                .collect();
            Some((target, values))
        })
        .collect()
}

fn chain(mut prefix: u64, suffix: u64) -> u64 {
    let mut tmp = suffix;
    while tmp != 0 {
        tmp /= 10;
        prefix *= 10;
    }
    prefix + suffix
}

fn is_valid<Op>(values: &[u64], cur: u64, target: u64, ops: &[Op]) -> bool
where
    Op: Fn(u64, u64) -> u64,
{
    if cur > target {
        return false;
    }
    if values.is_empty() {
        return cur == target;
    }
    ops.iter()
        .any(|op| is_valid(&values[1..], op(cur, values[0]), target, ops))
}

fn helper<Op>(input: &[(u64, Vec<u64>)], ops: &[Op]) -> u64
where
    Op: Fn(u64, u64) -> u64,
{
    input
        .iter()
        .filter_map(|(target, values)| is_valid(values, 0, *target, ops).then_some(target))
        .sum()
}

fn solve1(input: &[(u64, Vec<u64>)]) -> u64 {
    let ops = [|a, b| a + b, |a, b| a * b];
    helper(input, &ops)
}

fn solve2(input: &[(u64, Vec<u64>)]) -> u64 {
    let ops = [|a, b| a + b, |a, b| a * b, chain];
    helper(input, &ops)
}

fn main() {
    println!("Solving AoC24, day 7...");
    let input = parse_input(&utils::get_input(INPUT_PATH));
    println!("Part 1: {}", solve1(&input));
    println!("Part 2: {}", solve2(&input));
}

#[cfg(test)]
mod test {
    use super::*;

    const TEST_INPUT: &str = "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20";

    #[test]
    fn parsing() {
        let input = [
            (190, vec![10, 19]),
            (3267, vec![81, 40, 27]),
            (83, vec![17, 5]),
            (156, vec![15, 6]),
            (7290, vec![6, 8, 6, 15]),
            (161011, vec![16, 10, 13]),
            (192, vec![17, 8, 14]),
            (21037, vec![9, 7, 18, 13]),
            (292, vec![11, 6, 16, 20]),
        ];
        assert_eq!(parse_input(TEST_INPUT), input);
    }

    #[test]
    fn part1() {
        let input = parse_input(TEST_INPUT);
        assert_eq!(solve1(&input), 3749);
    }

    #[test]
    fn part2() {
        let input = parse_input(TEST_INPUT);
        assert_eq!(solve2(&input), 11387);
    }
}
