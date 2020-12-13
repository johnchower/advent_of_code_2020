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
  , "answers/"
  , day_part
  , ".txt"
  )
)
