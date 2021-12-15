import Collections
import Foundation

struct Point: Hashable {
    let y: Int
    let x: Int

    func isInBounds(of grid: [[Int]]) -> Bool {
        y >= 0 && y < grid.count && x >= 0 && x < grid[y].count
    }

    func neighbors(on grid: [[Int]]) -> [Point] {
        [
            Point(y: y - 1, x: x),
            Point(y: y + 1, x: x),
            Point(y: y, x: x - 1),
            Point(y: y, x: x + 1),
        ].filter { $0.isInBounds(of: grid) }
    }
}

struct WeightedPoint: Comparable {
    let point: Point
    let weight: Int

    static func <(lhs: WeightedPoint, rhs: WeightedPoint) -> Bool {
        lhs.weight < rhs.weight
    }

    static func ==(lhs: WeightedPoint, rhs: WeightedPoint) -> Bool {
        lhs.weight == rhs.weight
    }
}

func dijkstra(
    on grid: [[Int]],
    from start: Point = Point(y: 0, x: 0),
    to dest: Point = Point(y: grid.count - 1, x: grid[0].count - 1)
) -> Int {
    var heap = Heap<WeightedPoint>()
    var visited = Set<Point>()

    heap.insert(WeightedPoint(point: start, weight: 0))

    while let next = heap.popMin() {
        if next.point == dest {
            return next.weight
        }
        visited.insert(next.point)
        for neighbor in next.point.neighbors(on: grid) where !visited.contains(neighbor) {
            heap.insert(WeightedPoint(point: neighbor, weight: next.weight + grid[neighbor.y][neighbor.x]))
        }
    }

    fatalError("No path found!")
}

let input = try! String(contentsOfFile: "Resources/demo.txt")
let grid: [[Int]] = input.split(separator: "\n")
    .map { $0.compactMap { Int(String($0)) } }

print("Part 1: \(dijkstra(on: grid))")
