#!/usr/bin/env ruby
require 'set'

$point_memo = {}

def neighborhood(point)
  y, x = point
  (-1..1).flat_map do |dy|
    (-1..1).filter_map do |dx|
      [y + dy, x + dx]
    end
  end
end

def point_at?(p, steps, points, algo)
  key = p + [steps]
  memo = $point_memo[key]

  unless memo.nil?
    return memo
  end

  value = if steps == 0
    points.include?(p)
  else
    encoded = neighborhood(p)
      .map { |q| point_at?(q, steps - 1, points, algo) ? 1 : 0 }
      .inject(0) { |v, b| (v << 1) | b }
    algo[encoded] == '#'
  end

  $point_memo[key] = value
  value
end

def extreme_point(points)
  points.reduce { |p, q| p.zip(q).map { |vs| yield vs } }
end

def stringify(steps, points, algo)
  min_y, min_x = extreme_point(points) { |vs| vs.min }
  max_y, max_x = extreme_point(points) { |vs| vs.max }
  ((min_y - steps)..(max_y + steps)).map do |y|
    ((min_x - steps)..(max_x + steps)).map do |x|
      point_at?([y, x], steps, points, algo) ? '#' : '.'
    end.join
  end.join("\n")
end

def compute_count(steps, points, height, width, algo)
  ((-steps)...(height + steps)).map do |y|
    ((-steps)...(width + steps)).count do |x|
      point_at?([y, x], steps, points, algo)
    end
  end.sum
end

algo, input = File.read('resources/input.txt').split("\n\n")
input = input.split
height = input.length
width = input[0].length
points = (0...height).flat_map do |y|
  (0...width).filter_map do |x|
    [y, x] if input[y][x] == '#'
  end
end.to_set

puts "Part 1: #{compute_count(2, points, height, width, algo)}"
puts "Part 2: #{compute_count(50, points, height, width, algo)}"
