library(tidyverse)
library(igraph)

project_directory <- rprojroot::find_root(
  criterion = rprojroot::has_dir(".git")
)

day <- 8
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
  tibble(instruction = .) %>%
  separate(instruction, into = c("instruction_type", "value"), sep = " ") %>%
  mutate(
    value = as.numeric(value)
  , id = 1:n() 
  , accumulator_increment = ifelse(
      instruction_type == "acc"
    , value
    , 0
    )
  , id_increment = ifelse(
      instruction_type == "jmp"
    , value
    , 1
    )
  , next_id = id + id_increment
  )

input_clean %>%
  select(
    id
  , next_id
  ) %>%
  graph_from_data_frame() %>%
  plot()

accumulator <- 0
current_id <- 1
id_list <- c(current_id)
while (length(unique(id_list)) == length(id_list)) {
  next_instruction <- input_clean %>%
    filter(id == current_id)
  next_id <- next_instruction$next_id
  accumulator <- accumulator + next_instruction$accumulator_increment
  id_list <- c(id_list, next_id)
  current_id <- next_id
}

answer <- accumulator

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
