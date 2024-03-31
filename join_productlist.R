
# Packages ----------------------------------------------------------------

library(tidyverse)


# Data --------------------------------------------------------------------

# all_files <- dir("C:/Users/erika.borelli_skysp2/Documents/Personal/supermarketsimple")
# 
# ids <- all_files |> str_detect("products_")
# 
# all_files_selected = all_files[ids] 
# 
#   
# product_names <- all_files_selected |>
#   map_df( read_rds) |>
#   arrange(Products) |>
#   unique()

product_names <- read_rds("all_products.rds") |>
  arrange(Products) |>
  unique()
# product_names |> saveRDS("all_products.rds")
