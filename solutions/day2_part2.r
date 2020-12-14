library(tidyverse)

project_directory <- rprojroot::find_root(
  criterion = rprojroot::has_dir(".git")
)

day <- 2
part <- 2

input <- read_delim(
  paste0(
    project_directory
  , "/"
  , "inputs/day"
  , day
  , ".txt"
  )
, col_names = c(
    "policy"
  , "password"
  )
, delim = ":"
) %>%
  separate(
    policy
  , into = c(
      "range"
    , "target_character"
    )
  , sep = " "
  ) %>%
  separate(
    range
  , into = c(
      "first_position"
    , "second_position"
    )
  , sep = "-"
  ) %>%
  mutate(
    first_position = as.integer(first_position)
  , second_position = as.integer(second_position)
  )

character_count_df <- input %>%
  mutate(
    password_split = purrr::map(
      password
    , ~ unlist(stringr::str_split(.x, pattern = ""))
    )
  ) %>%
  gather(key = "position", value = "index", contains("_position")) %>%
  select(-position) %>%
  mutate(
    given_character = purrr::map(
      password_split, first_position
    , function(password_split, first_position){
        length(
          password_split[password_split == character]
        )
      }
    )
  ) %>%
  unnest(character_count) %>%
  mutate(
    valid_password = character_count >= first_position &
      character_count <= second_position
  )

answer <- sum(character_count_df$valid_password)

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
