import functools

with open('resources/input.txt', 'r') as f:
    lines = [l.strip() for l in f.readlines()]
    xs = [int(l, 2) for l in lines]
    width = len(lines[0])
    threshold = len(xs) / 2

    def most_common_bit(i):
        count = sum((word >> i) & 1 for word in xs)
        return 1 if count > threshold else 0

    gamma_rate = functools.reduce(lambda x, y: x | y, [most_common_bit(i) << i for i in range(width)])
    epsilon_rate = gamma_rate ^ ((1 << width) - 1) # python has no unary bitwise negation operator, therefore this
    print(f'Part 1: {gamma_rate * epsilon_rate}')
