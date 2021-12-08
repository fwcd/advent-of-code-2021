with open('resources/input.txt', 'r') as f:
    part1 = 0

    for line in f.readlines():
        line = line.strip()
        [digit_patterns, output_patterns] = [
            [p.strip() for p in ps.split(" ") if p.strip()]
            for ps in line.split("|")
        ]
        part1 += sum(1 if len(p) in [2, 3, 4, 7] else 0 for p in output_patterns)
    
    print(f'Part 1: {part1}')
