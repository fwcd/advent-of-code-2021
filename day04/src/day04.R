library(readr)
library(stringr)

input <- str_trim(read_file("resources/input.txt"))
components <- str_split(input, "\n\n")[[1]]
draws <- str_split(components[1], ",")[[1]]
raw_boards <- str_split(components[2:length(components)], "\n")
boards <- lapply(raw_boards, function(b) { do.call(rbind, str_split(str_trim(b), "\\s+")) })

print("Draws")
print(draws)
print("Boards")
print(boards)
