library(tidyverse)
library(stringr)
library(igraph)

project_directory <- rprojroot::find_root(
  criterion = rprojroot::has_dir(".git")
)

day <- 7
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

input_clean <- test_input %>% {
  tibble(rule = .) 
} %>%
  mutate(
    rule = str_replace_all(rule, "bags?", "")
  , rule = str_replace_all(rule, " .$", "")
  ) %>%
  separate(
    rule
  , into = c("container", "containees")
  , sep = "  contain "
  ) %>%
  mutate(
    container = str_trim(container)
  , containees = str_split(containees, " , ")
  , containees = map(
      containees
    , ~ tibble(containee = str_trim(.))
    )
  ) %>%
  unnest() %>%
  mutate(
    data = str_split(containee, " ", n = 2)
  , containee_count = map(data, ~ tibble(containee_count = .[1]))
  , containee_color = map(data, ~ tibble(containee_color = .[2]))
  ) %>% 
  unnest(
    containee_count, containee_color
  ) %>%
  select(- data, - containee) %>%
  mutate(
    containee_count = ifelse(
      containee_count == "no"
    , 0
    , as.numeric(containee_count)
    )
  ) %>%
  group_by(container, containee_color) %>%
  mutate(
    data = map(
      containee_count
    , ~ tibble(dummy_variable = rep(0, times = .))
    )
  ) %>%
  unnest() %>%
  select(- containee_count, - dummy_variable)

edges <- input_clean

adjacency_matrix <- edges %>%
  graph_from_data_frame() %>%
  as_adj()

exponent <- 1
latest_power <- adjacency_matrix
power_list <- list(latest_power)
while (sum(latest_power["shiny gold", ]) > 0){
  exponent <- exponent + 1
  latest_power <- adjacency_matrix %*% latest_power
  power_list <- c(power_list, latest_power)
}

answer <- power_list %>%
  map(~ .["shiny gold", ]) %>%
  map(sum) %>%
  unlist() %>%
  sum()
  
part <- 2
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
