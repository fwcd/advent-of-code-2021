library(readr)
library(stringr)

input <- read_file("resources/input.txt")
components <- str_split(input, "\n\n")

print(components)
