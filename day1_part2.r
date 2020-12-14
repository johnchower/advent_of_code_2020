library(tidyverse)

project_directory <- rprojroot::find_root(
  criterion = rprojroot::has_dir(".git")
)

day <- 1
part <- 2

input <- read_csv(
  paste0(
    project_directory
  , "/"
  , "inputs/day"
  , day
  , ".txt"
  )
, col_names = "entry"
)

answer <- 0

write_lines(
  answer
, paste0(
    project_directory
  , "/"
  , "answers/day"
  , day
  , "_part"
  , part
  , ".txt"
  )
)
