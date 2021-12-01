import sys

name = 'aoc21-01.txt'

def format(name: str) -> list[int]:
    lines = open(name, 'r').readlines()
    ints = [int(ln) for ln in lines]
    return ints

def count_increases(ls: list[int]) -> int:
    count = 0
    for i in range(len(ls) - 3):
        count += ls[i] < ls[i + 3]
    return count

if __name__ == '__main__':
    ints = format(name)
    increases = count_increases(ints)
    print(increases)
