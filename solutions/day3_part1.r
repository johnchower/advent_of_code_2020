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

row_indices <- 1:nrow(input)
col_indices <- seq(
  from = 1
, by = 3
, length.out = nrow(input)
) %% 
  ncol(input) %>% {
    .[.==0] <- ncol(input)
    .
  }

answer <- purrr::map2(
  row_indices
, col_indices
, ~ input[.x, .y]
) %>%
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
