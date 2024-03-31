
# Bibliotecas -------------------------------------------------------------

library(tidyverse)
library(plotly)
library(lubridate)


# Data --------------------------------------------------------------------

all_files <- dir("C:/Users/erika.borelli_skysp2/Documents/Personal/supermarketsimple")

ids <- all_files |> str_detect("grocery_")

all_files_selected = all_files[ids]


all_groceries <- all_files_selected |>
  map_df( read_rds)  |> 
  mutate(id_groc = ifelse(id_groc |> is.na(),
                          paste(Supermarket, Date, Product, Price, Quantity,
                                Discount , sep = "_") |>  str_to_lower(),
                          id_groc),
         Date = Date |> ymd(),
         Discount = Discount |> replace_na(0)) |>
  mutate(month = Date |> month(),
         Total = ifelse(Total |> is.na(), (Price - Discount)*Quantity,
         Total),
         year = Date |> year()
         )

# Plots -------------------------------------------------------------------

all_groceries |>
  group_by(Date) |>
  summarise(total_day = Total |> sum()) |>
  plot_ly( x = ~Date, y = ~total_day, mode = 'lines+markers')

all_groceries |>
  group_by(Date) |>
  summarise(total_day = Total |> sum()) |>
  plot_ly(
    x = ~Date, y = ~total_day, type = "bar"
  )

all_groceries |>
  group_by(Date, Supermarket ) |>
  summarise(total_day = Total |> sum()) |>
  plot_ly(
    x = ~Date, y = ~total_day, type = "bar", split = ~Supermarket
  )

all_groceries |> 
  group_by(Supermarket, month) |>
  summarise(sum_total = Total |> sum()) |>
  plot_ly(
    x = ~month, y = ~sum_total, split = ~Supermarket, type = "bar")
  )

all_groceries |> 
  group_by(Supermarket, month, Date) |>
  summarise(sum_day = Total |> sum()) |> 
  plot_ly( x = ~Date, y = ~sum_day, type = "bar", split = ~Supermarket)

all_groceries |> 
  group_by(Supermarket, month, Date) |>
  summarise(sum_day = Total |> sum()) |> 
  plot_ly( x = ~Date, y = ~sum_day,, type = 'scatter', mode = 'lines+markers', split = ~Supermarket)

all_groceries |> 
  group_by(Supermarket, month, Date, year) |>
  summarise(sum_day = Total |> sum()) |>
  ungroup() |>
  group_by(Supermarket, month, year) |>
  summarise(total = sum_day |> sum(),
            count_grocery = n()) |>
  arrange(year, month)
