library(fpp3)

global_economy |> 
  filter(Country == "United Kingdom") |> 
  autoplot(GDP)

global_economy |> 
  filter(Country == "Australia") |> 
  autoplot(GDP)

global_economy |> 
  filter(Country %in% c("China", "United Kingdom", "Australia")) |> 
  autoplot(GDP / Population)
  

aus_retail |> 
  distinct(Industry)
aus_print <- aus_retail |> 
  filter(Industry == "Newspaper and book retailing") |> 
  summarise(Turnover = sum(Turnover))

aus_print |> 
  autoplot(Turnover) + 
  ylab("Turnover (AU$ Millions)")

aus_economy <- global_economy |> 
  filter(Country == "Australia")
aus_economy

aus_print |> 
  mutate(Year = year(Month)) |> 
  left_join(aus_economy, by = "Year") |> 
  autoplot(Turnover / CPI)

us_employment |> 
  filter(Month >= yearmonth("1990 Jan"), Title == "Retail Trade") |> 
  autoplot(Employed)

us_employment |> 
  filter(Month >= yearmonth("1990 Jan"), Title == "Retail Trade") |> 
  gg_season(Employed)

aus_print |> 
  gg_season(Turnover / days_in_month(Month))


dcmp <- us_employment |> 
  filter(Month >= yearmonth("1990 Jan"), Title == "Retail Trade") |> 
  model(STL(Employed))
dcmp |> 
  components() |> 
  autoplot(Employed)

dcmp |> 
  components() |> 
  gg_season(season_year)

dcmp |> 
  components() |> 
  gg_subseries(season_year)

us_employment |> 
  filter(Month >= yearmonth("1990 Jan"), Title == "Retail Trade") |> 
  autoplot(Employed)

components(dcmp) |> 
  as_tsibble() |> 
  autoplot(Employed) + 
  geom_line(aes(y = trend), colour = "steelblue", linewidth = 2)

components(dcmp) |> 
  as_tsibble() |> 
  autoplot(Employed) + 
  geom_line(aes(y = season_adjust), colour = "steelblue", linewidth = 1.5)

components(dcmp) |> 
  as_tsibble() |> 
  autoplot(Employed) + 
  geom_line(aes(y = trend + remainder), colour = "steelblue", linewidth = 1.5)

components(dcmp) |> 
  as_tsibble() |> 
  autoplot(Employed) + 
  geom_line(aes(y = Employed - season_year), colour = "steelblue", linewidth = 1.5)


dcmp <- us_employment |> 
  filter(Month >= yearmonth("1990 Jan"), Title == "Retail Trade") |> 
  model(STL(Employed ~ trend(window = 15) + season(window = 7)))
dcmp |> 
  components() |> 
  autoplot()

tourism |> 
  filter(Purpose == "Holiday") |> 
  summarise(Trips = sum(Trips)) |> 
  autoplot()

tourism |> 
  filter(Purpose == "Holiday") |> 
  summarise(Trips = sum(Trips)) |> 
  features(Trips, feat_stl)


tourism |> 
  filter(Purpose == "Holiday") |> 
  # summarise(Trips = sum(Trips)) |> 
  features(Trips, feat_stl) |> 
  ggplot(aes(x = trend_strength, y = seasonal_strength_year)) + 
  geom_point()

tourism |> 
  filter(Purpose == "Holiday") |> 
  # summarise(Trips = sum(Trips)) |> 
  features(Trips, feat_stl) |> 
  filter(trend_strength < 0.3)

tourism |> 
  filter(Region == "Barossa", Purpose == "Holiday") |> 
  autoplot(Trips)

tourism_trendy_holidays <- tourism |> 
  filter(Purpose == "Holiday") |> 
  # summarise(Trips = sum(Trips)) |> 
  features(Trips, feat_stl) |> 
  filter(trend_strength > 0.8)
tourism_trendy_holidays

tourism |> 
  filter(Region == "Australia's South West", Purpose == "Holiday") |> 
  autoplot(Trips)

tourism |> 
  # right_join(tourism_trendy_holidays, by = c("Region", "State", "Purpose"))
  semi_join(tourism_trendy_holidays, by = key_vars(tourism)) |> 
  autoplot(Trips)


tourism |> 
  features(Trips, feat_stl) |> 
  ggplot(aes(x = trend_strength, seasonal_strength_year)) + 
  geom_point()


tourism |> 
  features(Trips, feat_stl) |> 
  ggplot(aes(x = trend_strength, seasonal_strength_year)) + 
  geom_point(aes(colour = Purpose))

tourism |> 
  filter(Purpose == "Holiday") |> 
  group_by(State) |> 
  summarise(Trips = sum(Trips)) |> 
  features(Trips, feat_stl) |> 
  ggplot(aes(x = trend_strength, seasonal_strength_year)) + 
  geom_point(aes(colour = State))

tourism |> 
  semi_join(tourism_trendy_holidays, by = key_vars(tourism)) |> 
  ACF(Trips) |> 
  autoplot()

tourism |> 
  semi_join(tourism_trendy_holidays, by = key_vars(tourism)) |> 
  features(Trips, feat_acf)
  

tourism_feat <- tourism |> 
  features(Trips, feature_set(pkgs = "feasts"))

tourism_pc <- tourism_feat |> 
  select(-Region, -State, -Purpose) |> 
  prcomp(scale = TRUE) |> 
  broom::augment(tourism_feat)

tourism_pc |> 
  ggplot(aes(x = .fittedPC1, y = .fittedPC2)) + 
  geom_point(aes(colour = Purpose))


tourism_pc |> 
  ggplot(aes(x = .fittedPC1, y = .fittedPC2)) + 
  geom_point(aes(colour = State))

tourism_outliers <- tourism_pc |> 
  filter(.fittedPC1 > 10)

# semi_join to filter the data based on matching rows
tourism |> 
  semi_join(tourism_outliers, by = key_vars(tourism)) |> 
  autoplot(Trips)
# right_join keeps all right data rows, and adds left data rows that match
tourism |> 
  right_join(tourism_outliers, by = key_vars(tourism)) |> 
  autoplot(Trips)

tourism_pc |> 
  ggplot(aes(x = .fittedPC1, y = .fittedPC2)) + 
  geom_point(data = tourism_pc |> filter(.fittedPC1 > 10), size = 3) +
  geom_point(aes(colour = Purpose))

# Scree plots
tourism_feat |> 
  select(-Region, -State, -Purpose) |> 
  prcomp(scale = TRUE) |> 
  screeplot()
