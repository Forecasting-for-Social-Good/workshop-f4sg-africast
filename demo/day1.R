# install.packages("fpp3")

library(fpp3)

global_economy
# Ctrl + Enter (Cmd + Enter)

global_economy |> 
  filter(Country == "Australia")

tourism |> 
  filter(Purpose == "Holiday")

prison <- readr::read_csv("https://raw.githubusercontent.com/Forecasting-for-Social-Good/workshop-f4sg-africast/refs/heads/main/materials/data/prison_population.csv")

prison |> 
  as_tsibble(index = date, key = c(state, gender, legal, indigenous))

prison |> 
  mutate(quarter = yearquarter(date)) |> 
  as_tsibble(index = quarter, key = c(state, gender, legal, indigenous))

PBS


## Session 2

ansett %>%
  filter(Airports=="MEL-SYD", Class=="Economy") %>%
  autoplot(Passengers)

ansett %>%
  filter(Airports=="MEL-SYD") %>%
  autoplot(Passengers)

PBS %>%
  filter(ATC2 == "A10") %>%
  summarise(Cost = sum(Cost)/1e6) %>%
  autoplot(Cost) +
  ylab("$ million") + xlab("Year") +
  ggtitle("Antidiabetic drug sales")

library(fpp3)
holidays <- tourism |>
  filter(Purpose == "Holiday") |>
  group_by(State) |>
  summarise(Trips = sum(Trips))

## gg_season(), gg_subseries()
holidays |> 
  autoplot(Trips)

holidays |> 
  gg_season(Trips)

holidays |> 
  gg_subseries(Trips)


# No seasonality in annual data
global_economy |> 
  gg_season(GDP)
