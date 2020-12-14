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

cross_sum <- input %>%
  merge(
    rename(
      input
    , entry2 = entry
    )
  ) %>%
  mutate(
    entry_sum = entry + entry2
  , complement = 2020 - entry_sum
  )

answer <- cross_sum %>%
  inner_join(
    rename(
      input
    , entry3 = entry
    )
  , by = c("complement" = "entry3")
  ) %>%
  mutate(answer = entry * entry2 * complement) %>%
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
