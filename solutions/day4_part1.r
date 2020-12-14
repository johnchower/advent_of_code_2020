library(tidyverse)

project_directory <- rprojroot::find_root(
  criterion = rprojroot::has_dir(".git")
)

day <- 4
part <- 1

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

answer <- input %>%
  mutate(
    complement = 2020 - entry
  ) %>%
  inner_join(
    input
  , by = c("complement" = "entry")
  ) %>%
  mutate(
    answer = complement * entry
  ) %>%
  distinct(answer) %>% {
    .$answer
  }

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
