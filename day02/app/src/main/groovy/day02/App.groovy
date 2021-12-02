package day02

def input = new File('resources/input.txt').text.lines().collect()

def x = 0
def depth = 0

for (line in input) {
    def inst = line.split(' ')
    def n = Integer.parseInt(inst[1])
    switch (inst[0]) {
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

println "Part 1: ${x * depth}"

x = 0
depth = 0
def aim = 0

for (line in input) {
    def inst = line.split(' ')
    def n = Integer.parseInt(inst[1])
    switch (inst[0]) {
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

println "Part 2: ${x * depth}"
