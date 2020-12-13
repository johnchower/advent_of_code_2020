library(tidyverse)

project_directory <- rprojroot::find_root(
  criterion = rprojroot::has_dir(".git")
)

day <- 1
part <- 1
day_part <- paste0("day", day, "_", "part", part)

input <- read_csv(
  paste0(
    project_directory
  , "/"
  , "inputs/"
  , day_part
  , ".txt"
  )
, col_names = "entry"
)

answer <- 2

write_lines(
  answer
, paste0(
    project_directory
  , "/"
  , "answers/"
  , day_part
  , ".txt"
  )
)
