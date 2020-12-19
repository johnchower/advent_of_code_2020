library(tidyverse)
library(igraph)

project_directory <- rprojroot::find_root(
  criterion = rprojroot::has_dir(".git")
)

day <- 8
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
  tibble(instruction = .) %>%
  separate(instruction, into = c("instruction_type", "value"), sep = " ") %>%
  mutate(
    value = as.numeric(value)
  , id = 1:n() 
  )

get_increments <- function(input_clean) {
  increments <- input_clean %>%
    mutate(
      accumulator_increment = ifelse(
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
  return(increments)
}

switch_instruction <- function(input_clean, id_to_switch) {
  input_clean[id == id_to_switch, "instruction_type"]
    mutate(
      instruction_type = ifelse(
        id != id_to_switch | instruction_type == "acc"
      , instruction_type
      , ifelse(
          instruction_type == "nop"
        , "jmp"
        , "nop"
        )
      )
    )
}

run_program <- function(altered_input) {
  end_state <- max(altered_input$id) + 1
  accumulator <- 0
  current_id <- 1
  id_list <- c(current_id)
  while (
    length(unique(id_list)) == length(id_list) & !(end_state %in% id_list)
  ) {
    next_instruction <- altered_input %>%
      filter(id == current_id)
    next_id <- next_instruction$next_id
    accumulator <- accumulator + next_instruction$accumulator_increment
    id_list <- c(id_list, next_id)
    current_id <- next_id
  }
  tibble(
    terminated = end_state %in% id_list
  , accumulator_value = accumulator
  )
}

start_time <- Sys.time()
answer <- input_clean %>%
  filter(instruction_type != "nop") %>%
  {.$id} %>%
  map(~ switch_instruction(input_clean, .)) %>%
  map(get_increments) %>%
  map(run_program) %>%
  reduce(bind_rows) %>%
  filter(terminated) %>% {
    .$accumulator_value
  }
end_time <- Sys.time()
end_time - start_time

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
