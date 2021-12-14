from collections import Counter

def apply(s: str, rules: list) -> str:
    for i in reversed(range(len(s) - 1)):
        for [lhs, rhs] in rules:
            if s[i:(i + 2)] == lhs:
                s = s[:(i + 1)] + rhs + s[(i + 1):]
                break
    return s

with open('resources/input.txt', 'r') as f:
    lines = [l.strip() for l in f.readlines()]
    polymer = lines[0]
    rules = [l.split(' -> ') for l in lines[2:]]

    for i in range(10):
        polymer = apply(polymer, rules)
    
    counts = Counter(polymer).most_common()
    part1 = counts[0][1] - counts[-1][1]
    print(f'Part 1: {part1}')
