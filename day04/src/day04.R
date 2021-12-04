library(readr)

input <- read_file("resources/input.txt")
components <- strsplit(input, "\n\n")[[1]]
draws <- strsplit(components[1], ",")[[1]]
boards <- strsplit(components[2:length(components)], "\n")

print("Draws")
print(draws)
print("Boards")
print(boards)
