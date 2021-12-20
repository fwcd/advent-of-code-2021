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

def point_at?(p, steps, points, algo)
  if steps == 0
    points.include?(p)
  else
    encoded = neighborhood(p)
      .map { |q| point_at?(q, steps - 1, points, algo) ? 1 : 0 }
      .inject(0) { |v, b| (v << 1) | b }
    algo[encoded] == '#'
  end
end

def extreme_point(points)
  points.reduce { |p, q| p.zip(q).map { |vs| yield vs } }
end

def stringify(steps, points, algo)
  min_x, min_y = extreme_point(points) { |vs| vs.min }
  max_x, max_y = extreme_point(points) { |vs| vs.max }
  ((min_x - steps)..(max_x + steps)).map do |y|
    ((min_y - steps)..(max_y + steps)).map do |x|
      point_at?([y, x], steps, points, algo) ? '#' : '.'
    end.join
  end.join("\n")
end

points = (0...input.length).flat_map do |y|
  (0...input[y].length).filter_map do |x|
    [y, x] if input[y][x] == '#'
  end
end.to_set

steps = 2
part1 = ((-steps)...(input.length + steps)).map do |y|
  ((-steps)...(input[0].length + steps)).count do |x|
    point_at?([y, x], steps, points, algo)
  end
end.sum

puts "Part 1: #{part1}"
