library(fpp3)
food <- aus_retail |>
  filter(Industry == "Food retailing") |>
  summarise(Turnover = sum(Turnover))

food |> 
  autoplot(Turnover)

food |> 
  autoplot(sqrt(Turnover))
# need stronger transformation

food |> 
  autoplot(log(Turnover))

?log

food |> 
  autoplot(log10(Turnover))

food |> 
  autoplot(box_cox(Turnover, 0.5))

# lambda < 0.5 (stronger transformation needed)
# lambda = 0.5 (just right - keep it)
# lambda > 0.5 (transformation was too strong)

food |> 
  autoplot(box_cox(Turnover, 0.05))

food |> 
  features(Turnover, guerrero)


food |> 
  autoplot(box_cox(Turnover, 0.0895))

food |> 
  autoplot(box_cox(Turnover, guerrero(Turnover))) + 
  ylab("Box-Cox transformed Turnover (lambda = 0.895")

food |> 
  autoplot(box_cox(Turnover, 0.09))

food |> 
  mutate(
    Turnover_bc = box_cox(Turnover, guerrero(Turnover))
  )

food |> 
  autoplot(log(Turnover))
food |> 
  model(stl = STL(log(Turnover))) |> 
  components() |> 
  autoplot()

food |> 
  gg_season(Turnover)

food |> 
  model(stl = STL(log(Turnover))) |> 
  components() |> 
  gg_season(season_year)

food |> 
  model(stl = STL(log(Turnover))) |> 
  components() |> 
  as_tsibble() |> 
  autoplot(season_adjust)


food |> 
  model(stl = STL(log(Turnover) ~ trend(window = 9) + season(window = 13))) |> 
  components() |> 
  autoplot()

# lm(y ~ x)

food |> 
  features(Turnover, feat_stl)

tourism |> 
  autoplot(Trips) + 
  guides(colour = "none")

tourism |> 
  features(Trips, feat_stl) |> 
  ggplot(aes(x = trend_strength, y = seasonal_strength_year)) + 
  geom_point(aes(colour = Purpose))

tourism_feat <- tourism |> 
  features(Trips, feat_stl)

tourism_most_seasonal <- tourism_feat |> 
  # top_n(1, seasonal_strength_year)
  filter(seasonal_strength_year == max(seasonal_strength_year))

tourism_most_seasonal

tourism |> 
  semi_join(tourism_most_seasonal) |> 
  # filter(
  #   Purpose == "Holiday",
  #   State == "New South Wales",
  #   Region == "Snowy Mountains"
  # ) |> 
  autoplot(Trips)



tourism_trendy <- tourism_feat |> 
  top_n(3, trend_strength)

tourism_feat |> 
  # arrange(-trend_strength) |> 
  head(3)

tourism_trendy$Region

tourism |> 
  semi_join(tourism_trendy) |> 
  autoplot(Trips)


tourism |> 
  semi_join(tourism_trendy) |> 
  ACF(Trips) |> 
  autoplot()


tourism_features <- tourism |>
  features(Trips, feature_set(pkgs = "feasts"))

tourism_features
# 48 features from the data!

tourism_features |> 
  select(seasonal_strength_year, trend_strength, acf1, spectral_entropy)

tourism_features |>
  select(where(is.numeric)) |> 
  # select(-State, -Region, -Purpose) |>
  prcomp(scale = TRUE)

pcs <- tourism_features |>
  select(where(is.numeric)) |> 
  # select(-State, -Region, -Purpose) |>
  prcomp(scale = TRUE) |>
  broom::augment(tourism_features)

pcs

pcs |> 
  ggplot(aes(x=.fittedPC1, y=.fittedPC2)) +
  geom_point(aes(colour = Purpose)) + 
  theme(aspect.ratio=1)

tourism_outliers <- pcs |> 
  filter(.fittedPC1 > 10)

tourism |> 
  semi_join(tourism_outliers) |> 
  autoplot()

pc_fit <- tourism_features |>
  select(where(is.numeric)) |> 
  # select(-State, -Region, -Purpose) |>
  prcomp(scale = TRUE)

plot(pc_fit)
