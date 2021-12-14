from collections import Counter

with open('resources/demo.txt', 'r') as f:
    lines = [l.strip() for l in f.readlines()]
    polymer = lines[0]
    rules = {lhs: rhs for [lhs, rhs] in (l.split(' -> ') for l in lines[2:])}

    def score(elem_counts: dict) -> int:
        values = sorted(elem_counts.values())
        print(elem_counts)
        return values[-1] - values[0]

    def compute(rounds: int) -> int:
        elem_counts = dict(Counter(polymer))
        pair_counts = {polymer[i:(i + 2)]: 1 for i in range(len(polymer) - 1)}

        for i in range(rounds):
            for pair in list(pair_counts.keys()):
                new = rules.get(pair, None)
                if new:
                    l = pair[0] + new
                    r = new + pair[1]

                    pair_counts[pair] -= 1
                    if pair_counts[pair] == 0:
                        del pair_counts[pair]

                    pair_counts[l] = pair_counts.get(l, 0) + 1
                    pair_counts[r] = pair_counts.get(r, 0) + 1
                    elem_counts[new] = elem_counts.get(new, 0) + 1

        return score(elem_counts)

    print(f'Test: {compute(6)}')
    print(f'Part 1: {compute(10)}')
    print(f'Part 2: {compute(40)}')
