library(tidyverse)

project_directory <- rprojroot::find_root(
  criterion = rprojroot::has_dir(".git")
)

day <- 6
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

tibble(
  yes_answers = input
) %>%
  mutate(
    group_id = cumsum(yes_answers == "")
  ) %>%
  filter(yes_answers != "") %>%
  group_by(group_id) %>%
  nest() %>%
  mutate(
    yes_answers = map(data, ~ .x$yes_answers)
  ) %>% {
    .$yes_answers
  } %>%
  map(~ strsplit(.x, "")) %>%
  map(~ Reduce(intersect, .x)) %>%
  map(length) %>%
  unlist() %>%
  sum()

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
