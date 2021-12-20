#!/usr/bin/env ruby
require 'set'

algo, input = File.read('resources/input.txt').split("\n\n")
input = input.split

def neighborhood(point)
  y, x = point
  (-1..1).flat_map do |dy|
    (-1..1).filter_map do |dx|
      [y + dy, x + dx]
    end
  end
end

def encode(ps, points)
  ps
    .map { |p| points.include?(p) ? 1 : 0 }
    .inject(0) { |v, b| (v << 1) | b }
end

def step(points, algo)
  points
    .flat_map { |p| neighborhood(p) }
    .filter { |p| algo[encode(neighborhood(p), points)] == '#' }
    .to_set
end

def stringify(points)
  min_point = points.reduce { |p, q| p.zip(q).map { |vs| vs.min } }
  max_point = points.reduce { |p, q| p.zip(q).map { |vs| vs.max } }
  (min_point[0]..max_point[0]).map do |y|
    (min_point[1]..max_point[1]).map do |x|
      points.include?([y, x]) ? '#' : '.'
    end.join
  end.join("\n")
end

points = (0...input.length).flat_map do |y|
  (0...input[y].length).filter_map do |x|
    [y, x] if input[y][x] == '#'
  end
end.to_set

puts "Part 1: #{step(step(points, algo), algo).length}"
