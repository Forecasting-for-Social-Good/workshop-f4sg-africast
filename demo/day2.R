library(fpp3)

as_tibble(aus_retail) |>
  as_tsibble(key = `Series ID`)

food <- aus_retail |>
  filter(Industry == "Food retailing") |>
  summarise(Turnover = sum(Turnover))


# sqrt(y), y^(1/3), log(y)

# Identity transformation
food |>
  autoplot(Turnover)

# Square root transformation
food |>
  autoplot(sqrt(Turnover))

# Cubed root transformation
food |>
  autoplot(Turnover^(1/3))

# Log transformation
food |>
  autoplot(log(Turnover))

# Inverse transformation
food |>
  autoplot(-1/Turnover)

# Identity -> sqrt() -> y^(1/3) -> log(y) -> -1/y

food |>
  autoplot(box_cox(Turnover, lambda = 0.1))

food |>
  features(Turnover, guerrero)

food |>
  autoplot(box_cox(Turnover, lambda = 0.0895))

library(fpp3)
PBS |>
  summarise(Cost = sum(Cost)) |>
  autoplot(box_cox(Cost, lambda = 0.364))


PBS |>
  summarise(Cost = sum(Cost)) |>
  features(Cost, guerrero)

log(0)


us_retail_employment <- us_employment |>
  filter(year(Month) >= 1990, Title == "Retail Trade") |>
  select(-Series_ID)
us_retail_employment |>
  autoplot(Employed)

us_retail_employment |>
  gg_subseries(Employed)
us_retail_employment |>
  gg_season(Employed)

us_retail_employment |>
  model(STL(Employed)) |>
  components() |>
  gg_subseries(season_year)

us_retail_employment |>
  model(STL(Employed)) |>
  components() |>
  gg_season(season_year)


food |>
  autoplot(Turnover)

# 1. Transform the data (if it is multiplicative)
food |>
  autoplot(log(Turnover))

# 2. Perform the STL decomposition
food_dcmp <- food |>
  model(
    STL(log(Turnover) ~ trend(window = 21) + season(window = 13))) |>
  components()

# 3. Plot the decomposition
food_dcmp |>
  autoplot()

# 4. Adjust the decomposition
# Choosing an appropriate seasonal window

# 5. Use the decomposition (make some plots)
food_dcmp |>
  autoplot()
food_dcmp |>
  gg_season(season_year)
food_dcmp |>
  gg_subseries(season_year)

# Try it yourself - decompose the number of tourists to Australia
aus_travel <- tourism |>
  summarise(Trips = sum(Trips))
aus_travel |>
  autoplot(Trips)

# 1. Transform if the data is multiplicative
# Data doesn't need any transformations.
# aus_travel |>
#   autoplot(log(Trips))
aus_travel |>
  features(Trips, guerrero) # Don't trust because it says lambda=2
?guerrero

# 2. STL decomposition
aus_dcmp <- aus_travel |>
  model(STL(Trips ~ trend(window = 21) + season(window = 13), robust = TRUE)) |>
  components()

# 3. Plot the decomposition, and adjust the windows if needed
aus_dcmp |>
  autoplot()

# 4. Explore the seasonality
aus_dcmp |>
  gg_season(season_year)
aus_travel |>
  gg_season(Trips)

pbs_total <- PBS |>
  summarise(Scripts = sum(Scripts))

pbs_total |>
  autoplot(sqrt(Scripts))
pbs_total |>
  features(Scripts, guerrero)

pbs_total |>
  model(STL(sqrt(Scripts))) |>
  components() |>
  autoplot()

pbs_total |>
  model(STL(sqrt(Scripts))) |>
  components() |>
  gg_season(season_year)


tourism

tourism |>
  autoplot(Trips) +
  guides(colour = "none")

tourism |>
  features(Trips, guerrero)

tourism |>
  filter(Region == "Adelaide", Purpose == "Visiting") |>
  autoplot(Trips)

tourism |>
  features(Trips, feat_stl) |>
  arrange(trend_strength)

tourism |>
  filter(Region == "East Coast", Purpose == "Other") |>
  autoplot(Trips)

tourism |>
  features(Trips, feat_stl) |>
  arrange(desc(trend_strength))

tourism |>
  filter(Region == "Australia's North West", Purpose == "Business") |>
  autoplot(Trips)

# What is the most seasonal time series in the tourism dataset?
tourism |>
  features(Trips, feat_stl) |>
  arrange(desc(seasonal_strength_year))

tourism |>
  filter(Region == "Snowy Mountains", Purpose == "Holiday") |>
  autoplot(Trips)


tourism |>
  features(Trips, feat_stl) |>
  ggplot(aes(x = trend_strength, y = seasonal_strength_year)) +
  geom_point(aes(colour = Purpose))


most_seasonal <- tourism |>
  features(Trips, feat_stl) |>
  filter(seasonal_strength_year == max(seasonal_strength_year))

tourism |>
  right_join(most_seasonal, by = c("State", "Region", "Purpose")) |>
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() + facet_grid(vars(State, Region, Purpose))



top_5_seasonal <- tourism |>
  features(Trips, feat_stl) |>
  slice_max(seasonal_strength_year, n = 5)

tourism |>
  right_join(top_5_seasonal, by = c("State", "Region", "Purpose")) |>
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() + facet_grid(vars(State, Region, Purpose))




top5_trends <- tourism |>
  features(Trips, feat_stl) |>
  slice_max(trend_strength, n = 5)

tourism |>
  right_join(top5_trends, by = c("State", "Region", "Purpose")) |>
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(
    vars(State, Region, Purpose),
    scale = "free_y"
  )


aus_retail |>
  summarise(Turnover = sum(Turnover)) |>
  autoplot(Turnover)

aus_retail |>
  summarise(Turnover = sum(Turnover)) |>
  features(-Turnover, feat_stl)

# Create a new feature for estimating the trend slope
estimate_trend <- function(x) {
  c(trend = unname(lm(x ~ seq_along(x))$coef[2]))
}

aus_retail |>
  summarise(Turnover = sum(Turnover)) |>
  features(Turnover, estimate_trend)


aus_retail |>
  summarise(Turnover = sum(Turnover)) |>
  ACF(Turnover) |>
  autoplot()


aus_retail |>
  summarise(Turnover = sum(Turnover)) |>
  features(Turnover, feat_acf)



aus_retail |>
  summarise(Turnover = sum(Turnover)) |>
  features(Turnover, list(feat_stl, feat_acf))


aus_retail |>
  summarise(Turnover = sum(Turnover)) |>
  features(Turnover, feature_set(pkgs = "feasts"))



tourism_features <- tourism |>
  features(Trips, feature_set(pkgs = "feasts"))

tourism_features

pca_fit <- tourism_features |>
  select(-State, -Region, -Purpose) |>
  prcomp(scale = TRUE)
pca_fit |> plot()

pcs <- tourism_features |>
  select(-State, -Region, -Purpose) |>
  prcomp(scale = TRUE) |>
  broom::augment(tourism_features)

pcs |>
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, colour = Purpose)) +
  geom_point()


tourism_outliers <- pcs |>
  filter(.fittedPC1 > 10)

tourism |>
  right_join(tourism_outliers, by = c("State", "Region", "Purpose")) |>
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(
    vars(State, Region, Purpose),
    scale = "free_y"
  )


as_tsibble(ChickWeight,
           index = Time, key = Chick)
