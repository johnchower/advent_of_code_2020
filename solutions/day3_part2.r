library(tidyverse)

project_directory <- rprojroot::find_root(
  criterion = rprojroot::has_dir(".git")
)

day <- 3
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
  stringr::str_replace_all("\\.", "0") %>%
  stringr::str_replace_all("\\#", "1") %>%
  stringr::str_split("") %>%
  map(as.integer) %>%
  {do.call(rbind, .)}

slopes <- tibble(
  right = c(
    1
  , 3
  , 5
  , 7
  , 1
  )
, down = c(
    1
  , 1
  , 1
  , 1
  , 2
  )
) 

answer <- slopes %>%
  mutate(
    row_indices = purrr::map(
      down
    , function(down){
        seq(
          from = 1
        , by = down
        , to = nrow(input)
        )
      }
    )
  , col_indices = purrr::map2(
      right, down
    , function(right, down){
        seq(
          from = 1
        , by = right
        , length.out = nrow(input) / down
        ) %% 
          ncol(input) %>% {
            .[.==0] <- ncol(input)
            .
          }
      }
    )
  ) %>%
  unnest() %>%
  mutate(
    has_tree = purrr::map2(row_indices, col_indices, ~ input[.x, .y])
  ) %>%
  unnest() %>%
  group_by(right, down) %>%
  summarise(n_trees = sum(has_tree)) %>%
  ungroup() %>% {
    .$n_trees
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
