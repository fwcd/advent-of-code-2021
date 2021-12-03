import functools

with open('resources/input.txt', 'r') as f:
    lines = [l.strip() for l in f.readlines()]
    xs = [int(l, 2) for l in lines]
    width = len(lines[0])
    threshold = len(xs) / 2

    def most_common_bit(i, words):
        count = sum((word >> i) & 1 for word in words)
        return 1 if count >= (len(words) / 2) else 0

    gamma_rate = functools.reduce(lambda x, y: x | y, [most_common_bit(i, xs) << i for i in range(width)])
    epsilon_rate = gamma_rate ^ ((1 << width) - 1) # python has no unary bitwise negation operator, therefore this
    print(f'Part 1: {gamma_rate * epsilon_rate}')

    def compute_rating(use_least):
        i = width - 1
        remaining = xs
        while i >= 0 and len(remaining) > 1:
            b = most_common_bit(i, remaining) ^ use_least
            print(b, [bin(x) for x in remaining])
            remaining = [x for x in remaining if ((x >> i) & 1) == b]
            print(b, [bin(x) for x in remaining])
            print()
            i -= 1
        return remaining[0]

    oxygen_rating = compute_rating(0)
    co2_rating = compute_rating(1)
    print(f'Part 2: {oxygen_rating * co2_rating}')
