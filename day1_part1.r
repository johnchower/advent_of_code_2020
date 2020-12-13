library(tidyverse)

project_directory <- rprojroot::find_root(criterion = rprojroot::has_dir(".git"))
setwd(project_directory)

input <- read_csv("inputs/day1_part1.txt", col_names = "entry")

input %>%
  mutate(
    complement = 2020 - entry
  ) %>%
  left_join(
    input
  , by = c("complement" = "entry")
  )
