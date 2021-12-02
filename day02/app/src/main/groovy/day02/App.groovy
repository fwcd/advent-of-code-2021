package day02

def x = 0
def depth = 0

new File('resources/input.txt').eachLine { line ->
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
