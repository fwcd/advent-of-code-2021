#!/usr/bin/env ruby
require 'set'

img_enhancement_algo, input = File.read('resources/demo.txt').split("\n\n")
input = input.split

points = (0...input.length).flat_map do |y|
  (0...input[y].length).filter_map do |x|
    [y, x] if input[y][x] == '#'
  end
end.to_set

puts points
