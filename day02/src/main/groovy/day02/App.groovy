package day02

input = getClass().getResource('/input.txt').text.lines().collect()

def part1Command(op, n) {
    switch (op) {
    case 'forward':
        x += n
        break
    case 'down':
        depth += n
        break
    case 'up':
        depth -= n
        break
    default:
        break
    }
}

def part2Command(op, n) {
    switch (op) {
    case 'forward':
        x += n
        depth += aim * n
        break
    case 'down':
        aim += n
        break
    case 'up':
        aim -= n
        break
    default:
        break
    }
}

def interpret(command) {
    x = 0
    depth = 0
    aim = 0

    for (line in input) {
        def (op, rawN) = line.split(' ')
        command(op, Integer.parseInt(rawN))
    }

    return x * depth
}

println "Part 1: ${interpret(this.&part1Command)}"
println "Part 2: ${interpret(this.&part2Command)}"
