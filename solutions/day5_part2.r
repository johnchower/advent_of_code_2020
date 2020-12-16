library(tidyverse)
library(stringr)

project_directory <- rprojroot::find_root(
  criterion = rprojroot::has_dir(".git")
)

day <- 5
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

row_powers <- 2 ^ (6:0)
seat_powers <- 2 ^ (2:0)

rows <- input %>%
  str_sub(1,7) %>%
  str_replace_all("F", "0") %>%
  str_replace_all("B", "1") %>%
  str_split("") %>%
  map(~ as.integer(.x) %*% row_powers) %>%
  map(as.vector)

columns <- input %>%
  str_sub(8, 10) %>%
  str_replace_all("L", "0") %>%
  str_replace_all("R", "1") %>%
  str_split("") %>%
  map(~ as.integer(.x) %*% seat_powers) %>%
  map(as.vector)

seat_ids <- map2(
  rows, columns
, ~ .x * 8 + .y
) %>%
  unlist()

seat_dim <- tibble(
  assignment = input
, row = unlist(rows)
, column = unlist(columns)
, id = seat_ids
) 

first_occupied_row <- min(seat_dim$row)
last_occupied_row <- max(seat_dim$row)

my_seat_row <- seat_dim %>%
  group_by(row) %>%
  summarise(n_occupied = 

seat_dim %>%
  ggplot(
    aes(
      x = seat_column
    , y = seat_row
    )
  ) +
  geom_point()
  

answer <- seat_ids %>%
  unlist() %>%
  max()

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
