deps <- c("readr", "stringr")

missing <- deps[!deps %in% installed.packages()]
install.packages(missing, repos="http://cran.r-project.org")
