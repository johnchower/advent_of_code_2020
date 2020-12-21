library(tidyverse)
library(memoise)

project_directory <- rprojroot::find_root(
  criterion = rprojroot::has_dir(".git")
)

day <- 10
part <- 2

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

tribonacci <- function(n) {
  if (n <= 3) {
    return(2 ^ (n - 1))
  } else {
    return(
      tribonacci(n - 1) +
      tribonacci(n - 2) +
      tribonacci(n - 3)
    )
  }
}
tribonacci <- memoise(tribonacci)

answer <- data.frame(joltages = input_clean) %>%
  mutate(diff = c(0, diff(joltages))) %>%
  mutate(
    one_block_id = cumsum(diff == 3)
  ) %>%
  filter(diff == 1) %>%
  group_by(one_block_id) %>%
  summarise(one_block_length = sum(diff == 1)) %>%
  mutate(
    n_options = map(one_block_length, tribonacci)
  ) %>%
  unnest() %>% {
    .$n_options
  } %>%
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
