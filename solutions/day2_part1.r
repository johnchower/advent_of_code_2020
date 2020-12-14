library(tidyverse)

project_directory <- rprojroot::find_root(
  criterion = rprojroot::has_dir(".git")
)

day <- 2
part <- 1

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
    , "character"
    )
  , sep = " "
  ) %>%
  separate(
    range
  , into = c(
      "lower_limit"
    , "upper_limit"
    )
  , sep = "-"
  ) %>%
  mutate(
    lower_limit = as.integer(lower_limit)
  , upper_limit = as.integer(upper_limit)
  )

character_count_df <- input %>%
  mutate(
    password_split = purrr::map(
      password
    , ~ unlist(stringr::str_split(.x, pattern = ""))
    )
  , character_count = purrr::map2(
      password_split, character
    , function(password_split, character){
        length(
          password_split[password_split == character]
        )
      }
    )
  ) %>%
  unnest(character_count) %>%
  mutate(
    valid_password = character_count >= lower_limit &
      character_count <= upper_limit
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
