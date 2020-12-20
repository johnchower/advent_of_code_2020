library(tidyverse)
library(igraph)

project_directory <- rprojroot::find_root(
  criterion = rprojroot::has_dir(".git")
)

day <- 9
part <- 1

input <- read_lines(
  paste0(
    project_directory
  , "/"
  , "inputs/day"
  , day
  , ".txt"
  )
) %>%
  as.numeric()
preamble <- 25

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
) %>%
  as.integer()
test_preamble <- 5

input_clean <- input
preamble_length <- preamble

sum_matrix <- input_clean %>%
  outer(., ., "+") %>% {
    diag(.) <- NA
    return(.)
  }

indices_to_check <- (preamble_length + 1):length(input_clean)

values_to_check_against <- indices_to_check %>%
  map(~ (. - preamble_length):(. - 1)) %>%
  map(~ sum_matrix[., .]) %>%
  map(as.vector) %>%
  map(unique) %>%
  map(~ .[!is.na(.)])

answer <- tibble(
  value = input_clean[indices_to_check]
, values_to_check_against
) %>%
  mutate(
    valid_value = map2(
      value
    , values_to_check_against
    , ~ .x %in% .y
    )
  ) %>%
  unnest(valid_value) %>%
  filter(!valid_value) %>%
  slice(1) %>% {
    .$value
  }
answer

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
