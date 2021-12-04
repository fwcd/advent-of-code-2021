library(readr)
library(stringr)

# Load input data
input <- str_trim(read_file("resources/input.txt"))
components <- str_split(input, "\n\n")[[1]]
all_draws <- strtoi(str_split(components[1], ",")[[1]])
raw_boards <- str_split(components[2:length(components)], "\n")
boards <- lapply(raw_boards, function(b) { do.call(rbind, lapply(str_split(str_trim(b), "\\s+"), strtoi)) })

horizontal_bingo <- function(draws, board) {
  for (i in 1:dim(board)[1]) {
    if (setequal(board[i,], intersect(draws, board[i,]))) {
      return(sum(board[i,]))
    }
  }
  NA
}

vertical_bingo <- function(draws, board) {
  horizontal_bingo(draws, t(board))
}

bingo <- function(draws, board) {
  h <- horizontal_bingo(draws, board)
  if (is.na(h)) vertical_bingo(draws, board) else h
}

side_length <- dim(boards[[1]])[1]

compute_scores <- function() {
  results <- c()
  remaining <- 1:length(boards)
  for (i in side_length:length(all_draws)) {
    draws <- all_draws[1:i]
    current_remaining <- remaining
    for (j in current_remaining) {
      board <- boards[[j]]
      b <- bingo(draws, board)
      if (!is.na(b)) {
        results[length(results) + 1] = ((sum(board) - sum(intersect(board, draws))) * all_draws[i])
        remaining = setdiff(remaining, j)
      }
    }
  }
  results
}

scores <- compute_scores()
print(str_interp("Part 1: ${scores[1]}"))
print(str_interp("Part 2: ${scores[length(scores)]}"))
