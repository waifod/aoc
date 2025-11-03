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

fn unchain_rtl(target: u64, suffix: u64) -> Option<u64> {
    if suffix > target {
        return None;
    }
    if suffix == 0 {
        if target == 0 {
            return Some(0);
        }
        return (target % 10 == 0).then_some(target / 10);
    }

    let num_digits = (suffix as f64).log10().floor() as u32 + 1;
    let power = 10u64.pow(num_digits);

    (target % power == suffix).then_some(target / power)
}

fn is_valid_rtl<Op>(values: &[u64], index: isize, target: u64, ops: &[Op]) -> bool
where
    Op: Fn(u64, u64) -> Option<u64>,
{
    if index < 0 {
        return target == 0;
    }

    let val = values[index as usize];

    ops.iter().any(|op| {
        if let Some(next_target) = op(target, val) {
            is_valid_rtl(values, index - 1, next_target, ops)
        } else {
            false
        }
    })
}

fn helper_rtl<Op>(input: &[(u64, Vec<u64>)], ops: &[Op]) -> u64
where
    Op: Fn(u64, u64) -> Option<u64>,
{
    input
        .iter()
        .filter_map(|(target, values)| {
            is_valid_rtl(values, values.len() as isize - 1, *target, ops).then_some(target)
        })
        .sum()
}

fn solve1(input: &[(u64, Vec<u64>)]) -> u64 {
    let ops: [fn(u64, u64) -> Option<u64>; 2] = [
        |t, v| t.checked_sub(v),
        |t, v| (v != 0 && t % v == 0).then_some(t / v),
    ];
    helper_rtl(input, &ops)
}

fn solve2(input: &[(u64, Vec<u64>)]) -> u64 {
    let ops: [fn(u64, u64) -> Option<u64>; 3] = [
        |t, v| t.checked_sub(v),
        |t, v| (v != 0 && t % v == 0).then_some(t / v),
        unchain_rtl,
    ];
    helper_rtl(input, &ops)
}

fn main() {
    println!("Solving AoC24, day 7...");
    let input_str = utils::get_input(INPUT_PATH);
    let input = parse_input(&input_str);
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
