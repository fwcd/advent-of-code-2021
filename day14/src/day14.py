from collections import Counter

with open('resources/input.txt', 'r') as f:
    lines = [l.strip() for l in f.readlines()]
    polymer = lines[0]
    rules = {lhs: rhs for [lhs, rhs] in (l.split(' -> ') for l in lines[2:])}

    def score(elem_counts: dict) -> int:
        values = sorted(elem_counts.values())
        return values[-1] - values[0]

    def compute(rounds: int) -> int:
        elem_counts = dict(Counter(polymer))
        pair_counts = dict(Counter([polymer[i:(i + 2)] for i in range(len(polymer) - 1)]))

        for i in range(rounds):
            for pair, pair_count in list(pair_counts.items()):
                new = rules[pair]
                l = pair[0] + new
                r = new + pair[1]

                pair_counts[pair] -= pair_count
                if pair_counts[pair] == 0:
                    del pair_counts[pair]

                pair_counts[l] = pair_counts.get(l, 0) + pair_count
                pair_counts[r] = pair_counts.get(r, 0) + pair_count
                elem_counts[new] = elem_counts.get(new, 0) + pair_count

        return score(elem_counts)

    print(f'Part 1: {compute(10)}')
    print(f'Part 2: {compute(40)}')
