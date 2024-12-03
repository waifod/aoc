use std::collections::HashMap;
use std::collections::HashSet;

use utils;

const INPUT_PATH: &str = "./input/input.txt";

fn parse_input(input: &str) -> (HashMap<i32, HashSet<i32>>, Vec<Vec<i32>>) {
    let mut rules = HashMap::<i32,HashSet<i32>>::new();
    let mut updates = Vec::<Vec<i32>>::new();
    let mut flag = true;
    input.lines().for_each(|line|
        if line == "" {
            flag = false;
        } else if flag  {
            let mut it = line.split('|').map(|n| n.parse::<i32>().unwrap());
            if let (Some(fst), Some(snd)) = (it.next(), it.next()) {
                rules.entry(fst).or_insert(HashSet::<i32>::new()).insert(snd);
            }
        } else {
            updates.push(line.split(',').map(|n| {
                let num = n.parse::<i32>().unwrap();
                rules.entry(num).or_insert(HashSet::<i32>::new());
                num
            }).collect());
        }
    );
    (rules, updates)
}

fn solve1(input: &(HashMap<i32, HashSet<i32>>, Vec<Vec<i32>>)) -> i32 {
    let (ref rules, ref updates) = input;
    updates.iter()
        .filter_map(|u| {
            let mut n_so_far = HashSet::<i32>::new();
            u.iter().all(|n| {
                n_so_far.insert(*n);
                rules.get(n).unwrap().intersection(&n_so_far).count() == 0
            }).then_some(u[u.len()/2])
        }).sum()
}

fn solve2(input: &(HashMap<i32, HashSet<i32>>, Vec<Vec<i32>>)) -> i32 {
    let (ref rules, ref updates) = input;
    updates.iter()
        .filter_map(|u| {
            let mut n_so_far = HashSet::<i32>::new();
            return u.iter().any(|n| {
                n_so_far.insert(*n);
                rules.get(n).unwrap().intersection(&n_so_far).count() != 0
            }).then(|| {
                let mut ordered_u = u.clone();

                ordered_u.sort_unstable_by(|a,b|
                    if *a == *b {
                        std::cmp::Ordering::Equal
                    } else if rules.get(a).unwrap().contains(b) {
                        std::cmp::Ordering::Less
                    } else {
                        std::cmp::Ordering::Greater
                    }
                );

                ordered_u[u.len()/2]
            })
        }).sum()
}

fn main() {
    println!("Solving AoC24, day 5...");
    let input = parse_input(&utils::get_input(INPUT_PATH));
    println!("Part 1: {}", solve1(&input));
    println!("Part 2: {}", solve2(&input));
}

#[cfg(test)]
mod test {
    use super::*;

    const TEST_INPUT: &str = "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47";

    #[test]
    fn parsing() {
        let rules: HashMap<i32, HashSet<i32>> = HashMap::from([
            (97, HashSet::from([13,61,47,29,53,75])),
            (75, HashSet::from([29,53,47,61,13])),
            (61, HashSet::from([13,53,29])),
            (29, HashSet::from([13])),
            (53, HashSet::from([29,13])),
            (47, HashSet::from([53,13,61,29])),
            (13, HashSet::from([])),
        ]);
        let updates: Vec<Vec<i32>> = vec![
            vec![75,47,61,53,29],
            vec![97,61,53,29,13],
            vec![75,29,13],
            vec![75,97,47,61,53],
            vec![61,13,29],
            vec![97,13,75,29,47],
        ];
        assert_eq!((rules, updates), parse_input(TEST_INPUT));
    }

    #[test]
    fn part1() {
        let input = parse_input(TEST_INPUT);
        assert_eq!(solve1(&input), 143);
    }

    #[test]
    fn part2() {
        let input = parse_input(TEST_INPUT);
        assert_eq!(solve2(&input), 123);
    }
}
