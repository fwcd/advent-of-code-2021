library(readr)
library(stringr)

# Load input data
input <- str_trim(read_file("resources/input.txt"))
components <- str_split(input, "\n\n")[[1]]
draws <- strtoi(str_split(components[1], ",")[[1]])
raw_boards <- str_split(components[2:length(components)], "\n")
boards <- lapply(raw_boards, function(b) { do.call(rbind, lapply(str_split(str_trim(b), "\\s+"), strtoi)) })

bingo_sum <- function(board) {
  
}

for (i in 1:length(draws)) {
  prefix <- draws[1:i]
}
