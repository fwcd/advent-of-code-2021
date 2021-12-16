import Collections
import Foundation

struct Point: Hashable {
    let y: Int
    let x: Int
}

protocol Grid {
    var height: Int { get }
    var width: Int { get }

    subscript(point: Point) -> Int { get }
}

extension Grid {
    func isInBounds(point: Point) -> Bool {
        point.y >= 0 && point.y < height && point.x >= 0 && point.x < width
    }

    func neighbors(of point: Point) -> [Point] {
        [
            Point(y: point.y - 1, x: point.x),
            Point(y: point.y + 1, x: point.x),
            Point(y: point.y, x: point.x - 1),
            Point(y: point.y, x: point.x + 1),
        ].filter(isInBounds(point:))
    }
}

extension Array: Grid where Element == [Int] {
    var height: Int { count }
    var width: Int { self[0].count }

    subscript(point: Point) -> Int { self[point.y][point.x] }
}

struct TiledGrid<Inner>: Grid where Inner: Grid {
    let inner: Inner
    let times: Int

    var height: Int { inner.height * times }
    var width: Int { inner.width * times }

    subscript(point: Point) -> Int {
        let offset = point.y / inner.height + point.x / inner.width
        let rawValue = inner[Point(y: point.y % inner.height, x: point.x % inner.width)] + offset
        return ((rawValue - 1) % 9) + 1
    }
}

struct Path: Comparable {
    let point: Point
    let length: Int
    let cost: Int

    static func <(lhs: Path, rhs: Path) -> Bool {
        lhs.cost < rhs.cost
    }

    static func ==(lhs: Path, rhs: Path) -> Bool {
        lhs.cost == rhs.cost
    }
}

func shortestPath<G>(
    on grid: G,
    from start: Point = Point(y: 0, x: 0),
    to dest: Point
) -> Int where G: Grid {
    // Perform A* search (essentially Dijkstra + heuristic)

    var heap = Heap<Path>()
    var visited = Set<Point>()

    heap.insert(Path(point: start, length: 0, cost: 0)) // cost is irrelevant here

    while let next = heap.popMin() {
        if next.point == dest {
            return next.length
        }
        visited.insert(next.point)
        for neighbor in grid.neighbors(of: next.point) where !visited.contains(neighbor) {
            let length = next.length + grid[neighbor]
            let distToDest = abs(next.point.y - dest.y) + abs(next.point.x - dest.x) // Manhattan distance
            heap.insert(Path(point: neighbor, length: length, cost: length + distToDest))
        }
    }

    fatalError("No path found!")
}

let input = try! String(contentsOfFile: "Resources/input.txt")
let grid: [[Int]] = input.split(separator: "\n")
    .map { $0.compactMap { Int(String($0)) } }
let tiled = TiledGrid(inner: grid, times: 5)

print("Part 1: \(shortestPath(on: grid, to: Point(y: grid.height - 1, x: grid.width - 1)))")
print("Part 2: \(shortestPath(on: tiled, to: Point(y: tiled.height - 1, x: tiled.width - 1)))")
