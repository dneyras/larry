library(tidyverse)
library(ggthemes)

files <- list.files("data/", pattern = "*.csv", full.names = TRUE)

data <- read_csv(files, id = "year", skip = 1, col_names = c("id", "num_police", "cout_sin"), col_select = all_of(2:3)) |> 
  mutate(year = parse_number(year))



 