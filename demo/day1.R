library(fpp3)
ansett
ansett %>%
  filter(Airports=="MEL-SYD", Class=="Economy") %>%
  autoplot(Passengers)
ansett %>%
  filter(Airports=="MEL-SYD", Class=="Business") %>%
  autoplot(Passengers)

PBS %>%
  filter(., ATC2 == "A10") %>%
  summarise(Cost = sum(Cost)/1e6) %>%
  autoplot(Cost)

# Help information for `aus_production`
?aus_production
aus_production

beer <- aus_production |>
  select(Quarter, Beer) |>
  filter(year(Quarter) >= 1992)

# Time plot
beer |> autoplot(Beer)

# Seasonal plot
beer |> gg_season(Beer)

# Sub-series plot
beer |> gg_subseries(Beer)


holidays <- tourism |>
  filter(Purpose == "Holiday") |>
  group_by(State) |>
  summarise(Trips = sum(Trips))

holidays |>
  autoplot(Trips)

holidays |>
  gg_season(Trips)

holidays |>
  gg_subseries(Trips)


beer |>
  mutate(lag4 = lag(Beer, 4))

beer |>
  autoplot(Beer) +
  geom_line(aes(y = lag(Beer, 4)), colour = "steelblue")

beer |>
  ACF(Beer) |>
  autoplot()

pelt |>
  autoplot(Lynx)

pelt |>
  ACF(Lynx) |>
  autoplot()

usethis::use_course("https://workshop.f4sg.org/africast/exercises.zip")
