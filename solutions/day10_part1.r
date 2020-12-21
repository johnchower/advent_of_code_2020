library(tidyverse)
library(igraph)

project_directory <- rprojroot::find_root(
  criterion = rprojroot::has_dir(".git")
)

day <- 10
part <- 1

input <- read_lines(
  paste0(
    project_directory
  , "/"
  , "inputs/day"
  , day
  , ".txt"
  )
)

test_input <- read_lines(
  paste0(
    project_directory
  , "/"
  , "test_inputs/day"
  , day
  , "_part"
  , part
  , ".txt"
  )
)

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
