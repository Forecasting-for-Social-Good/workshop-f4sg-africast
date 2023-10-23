library(fpp3)
# install.packages("fpp3")
global_economy
as_tibble(global_economy)
# Ctrl + Enter

global_economy |> 
  filter(Year == 1960)

tourism |> 
  filter(Purpose == "Holiday")

prison <- readr::read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv")

prison |> 
  mutate(Quarter = yearquarter(Date)) |> 
  as_tsibble(index = Quarter, key = c(State, Gender, Legal, Indigenous))

PBS %>%
  distinct(ATC1)

PBS |>
  distinct(ATC1)

1:10 %>% mean
1:10 |> mean()

PBS |> 
  summarise(Cost = sum(Cost)) |> 
  slice_max(Month, n = 10)



PBS |> 
  group_by(Concession) |> 
  summarise(Cost = sum(Cost)) |> 
  slice_max(Month, n = 10)

PBS |> 
  index_by(Year = year(Month)) |> 
  summarise(Cost = sum(Cost)) |> 
  slice_max(Year, n = 10)

ansett |>
  filter(Airports == "ADL-PER", Class == "Economy") |> 
  autoplot(Passengers)


ansett |>
  filter(Airports == "ADL-PER") |> 
  autoplot(Passengers)

ansett |> 
  group_by(Airports) |> 
  summarise(Passengers = sum(Passengers)) |> 
  autoplot(Passengers)

aus_production |> 
  autoplot(Beer)

aus_production |> 
  gg_season(Beer)

aus_production |> 
  autoplot(Cement)

aus_production |> 
  gg_season(Cement)
vic_elec |> 
  autoplot(Demand)

vic_elec |> 
  gg_season(Demand)

vic_elec |> 
  # index_by(month = yearmonth(Time)) |>
  index_by(date = as.Date(Time)) |>
  # index_by(week = yearweek(Time)) |>
  summarise(Demand = sum(Demand)) |> 
  gg_season(Demand)

vic_elec |> 
  # index_by(date = as.Date(Time)) |>
  # summarise(Demand = sum(Demand)) |> 
  gg_season(Demand, period = "1 week")

vic_elec |> 
  gg_season(Demand, period = "1 day")

vic_elec |> 
  index_by(month = yearmonth(Time)) |> 
  summarise(Demand = sum(Demand)) |> 
  autoplot(Demand)


aus_production |> 
  autoplot(Beer)

recent_production <- aus_production |> 
  filter_index("1995 Q1" ~ .)

recent_production |> 
  autoplot(Beer)

recent_production |> 
  gg_season(Beer)

recent_production |> 
  gg_subseries(Beer)

aus_holiday <- tourism |> 
  filter(Purpose == "Holiday") |> 
  group_by(State) |> 
  summarise(Trips = sum(Trips))

aus_holiday |> 
  autoplot(Trips)

aus_holiday |> 
  gg_season(Trips)

aus_holiday |> 
  gg_subseries(Trips)

# install.packages("sugrrants")
library(sugrrants)
vic_elec |>
  filter(year(Date) == 2014) |>
  mutate(Hour = hour(Time)) |>
  frame_calendar(x = Hour, y = Demand, date = Date, nrow = 4) |>
  ggplot(aes(x = .Hour, y = .Demand, group = Date)) +
  geom_line() -> p1
p1
prettify(p1,
         size = 3,
         label.padding = unit(0.15, "lines")
)


recent_production |> 
  autoplot(Beer)

recent_production |> 
  gg_lag(Beer)

recent_production |> 
  gg_lag(Beer, geom = "point")

recent_production |> 
  ACF(Beer) |> 
  autoplot()

aus_holiday |> 
  distinct(State)

aus_holiday |> 
  filter(State == "Northern Territory") |> 
  ACF(Trips, lag_max = 40) |> 
  autoplot()

pelt |> 
  autoplot(Hare)
pelt |> 
  ACF(Hare) |> 
  autoplot()

as_tsibble(USAccDeaths) |> 
  ACF(value) |> 
  autoplot()

random <- tsibble(
  year = 1990:2023,
  y = rnorm(34),
  index = year
)

random |> 
  autoplot(y)

random |> 
  ACF(y) |> 
  autoplot()
# white noise, no pattern