library(tidyverse)

project_directory <- rprojroot::find_root(
  criterion = rprojroot::has_dir(".git")
)

day <- 4
part <- 2

input <- read_lines(
  paste0(
    project_directory
  , "/"
  , "inputs/day"
  , day
  , ".txt"
  )
, na = ""
)

valid_passports <- input %>%
  strsplit(" ") %>%
  unlist() %>%
  {data.frame(key_value = .)} %>%
  mutate(
    passport_id = cumsum(is.na(key_value))
  ) %>%
  filter(!is.na(key_value)) %>%
  separate(
    key_value
  , into = c("key", "value")
  , sep = ":"
  ) %>%
  mutate(
    required_field = key != "cid"
  ) %>%
  group_by(passport_id) %>%
  summarise(
    valid_passport = sum(required_field) == 7
  ) %>%
  ungroup()

answer <- sum(valid_passports$valid_passport)
  
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
