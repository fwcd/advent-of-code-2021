with open('resources/demo.txt', 'r') as f:
    lines = [l.strip() for l in f.readlines()]
    polymer = lines[0]
    rules = {lhs: rhs for [lhs, rhs] in (l.split(' -> ') for l in lines[2:])}

    def score(counts: dict) -> int:
        element_counts = {}
        for pair, count in counts.items():
            for element in pair:
                element_counts[element] = element_counts.get(element, 0) + count
        print(element_counts)
        values = sorted(element_counts.values())
        return values[-1] - values[0]

    def compute(rounds: int) -> int:
        counts = {polymer[i:(i + 2)]: 1 for i in range(len(polymer) - 1)}
        for i in range(rounds):
            for pair in list(counts.keys()):
                new = rules.get(pair, None)
                if new:
                    l = pair[0] + new
                    r = new + pair[1]
                    counts[pair] -= 1
                    if counts[pair] == 0:
                        del counts[pair]
                    counts[l] = counts.get(l, 0) + 1
                    counts[r] = counts.get(r, 0) + 1
        return score(counts)

    print(f'Test: {compute(0)}')
    print(f'Part 1: {compute(10)}')
    print(f'Part 2: {compute(40)}')
