# install.packages("fpp3")

library(fpp3)

global_economy
# Ctrl + Enter (Cmd + Enter)

global_economy |> 
  filter(Country == "Australia")

tourism |> 
  filter(Purpose == "Holiday")

prison <- readr::read_csv("https://raw.githubusercontent.com/Forecasting-for-Social-Good/workshop-f4sg-africa/refs/heads/master/materials/data/prison_population.csv")

prison |> 
  as_tsibble(index = date, key = c(state, gender, legal, indigenous))

prison |> 
  mutate(quarter = yearquarter(date)) |> 
  as_tsibble(index = quarter, key = c(state, gender, legal, indigenous))

PBS
