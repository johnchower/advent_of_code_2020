library(tidyverse)

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

input_clean <- input %>%
  as.integer() %>%
  sort() %>% {
    c(0, ., max(.) + 3)
  }

answer <- data.frame(joltages = input_clean) %>%
  mutate(diff = c(diff(joltages), NA)) %>%
  filter(!is.na(diff)) %>%
  group_by(diff) %>%
  summarise(n_diff = n()) %>%
  {.$n_diff} %>%
  prod()

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
