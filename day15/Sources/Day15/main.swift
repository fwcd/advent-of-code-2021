import Foundation

let input = try! String(contentsOfFile: "Resources/demo.txt")
let grid: [[Int]] = input.split(separator: "\n")
    .map { $0.compactMap { Int(String($0)) } }


