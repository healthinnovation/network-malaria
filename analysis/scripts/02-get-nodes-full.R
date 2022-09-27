library(dplyr)
library(readr)
library(fs)
library(purrr)

input_path <- path("analysis/data/interim/")
file_paths <- dir_ls(input_path, glob = "*.csv")

datasets_raw <- map(file_paths, read_csv, show_col_type = FALSE)

datasets_names <- names(datasets_raw) %>%
  path_file() %>%
  path_ext_remove()

names(datasets_raw) <- datasets_names

datasets <- datasets_raw[c("villages", setdiff(datasets_names, "villages"))]

# Join datasets -----------------------------------------------------------

nodes_full <- reduce(datasets, left_join, by = "village_id")

output_path <- path("analysis/data/processed/")
output_filename <- "nodes-full.csv"
output_filepath <- path(output_path, output_filename)

write_csv(nodes_full, output_filepath, na = "")
