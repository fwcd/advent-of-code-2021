import sys

with open('resources/input.txt', 'r') as f:
    lines = [l.strip() for l in f.readlines()]
    width = len(lines[0])
    counts = [sum(int(word[i]) for word in lines) for i in range(width)]
    threshold = len(lines) / 2
    raw_gamma_rate = [('1' if c > threshold else '0') for c in counts]
    gamma_rate = int(''.join(raw_gamma_rate), 2)
    epsilon_rate = gamma_rate ^ ((1 << width) - 1) # python has no unary bitwise negation operator, therefore this
    print(f'Part 1: {gamma_rate * epsilon_rate}')
