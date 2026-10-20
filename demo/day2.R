# ==============================================================================
# Day 2 demo: transformations, decomposition, features and basic forecasting
# ==============================================================================
# How to run this script:
#   - Ctrl + Enter runs the current line (or the selected code) in the console.
#   - Run the sections in order: later sections reuse objects created earlier.
#
# Contents:
#   1. Setup
#   2. Mathematical transformations
#   3. Seasonal patterns: subseries plots and STL
#   4. Features of tourism time series (STL strength)
#   5. Basic forecasting models
# ==============================================================================


# 1. Setup ---------------------------------------------------------------------

# fpp3 loads tsibble, feasts, fable, ggplot2, dplyr and the example datasets.
library(fpp3)


# 2. Mathematical transformations ----------------------------------------------

# aus_retail has one series per State and Industry. We filter to one industry,
# then summarise() adds up all the States for each month, giving a single
# national series of monthly food retailing turnover.
food <- aus_retail |>
  filter(Industry == "Food retailing") |>
  summarise(Turnover = sum(Turnover))
food

# The seasonal variation grows as the level of the series grows, which is a
# sign that a transformation could help. Compare the plots below: we want the
# size of the seasonal variation to be about the same across the whole series.

# The original (untransformed) series.
food |>
  autoplot(Turnover)

# A square root transformation (weaker).
food |>
  autoplot(sqrt(Turnover))

# A log transformation (stronger). The seasonal variation is now much more
# constant, and changes in the log scale are percentage changes.
food |>
  autoplot(log(Turnover))

# The Box-Cox transformation is a family of transformations controlled by lambda:
# lambda = 1 is no transformation, 0.5 is similar to sqrt(), and 0 is log().
# The guerrero feature chooses the lambda that makes the seasonal variation
# most constant.
food |>
  features(Turnover, features = guerrero)

# Apply the Box-Cox transformation with the chosen lambda.
food |>
  autoplot(box_cox(Turnover, lambda = 0.0895))
# lambda ~= 0 -- better to just use log(), as it is easier to interpret.

# Transformations also help when comparing series of different sizes.
# Australia and Kenya's GDP are on very different scales, so on their own we
# can see each trend clearly...
global_economy |>
  filter(Country == "Australia") |>
  autoplot(GDP)
global_economy |>
  filter(Country == "Kenya") |>
  autoplot(GDP)

# ...but plotted together, Kenya's GDP is flattened against the axis.
global_economy |>
  filter(Country %in% c("Australia", "Kenya")) |>
  autoplot(GDP)

# Adjusting for population (GDP per capita) puts the countries on a more
# comparable scale.
global_economy |>
  filter(Country %in% c("Australia", "Kenya")) |>
  autoplot(GDP / Population)


# 3. Seasonal patterns: subseries plots and STL --------------------------------

# Monthly US retail employment from 1990 onwards. year() extracts the year of
# each Month, and select(-Series_ID) drops a column we don't need.
us_retail_employment <- us_employment |>
  filter(year(Month) >= 1990, Title == "Retail Trade") |>
  select(-Series_ID)
us_retail_employment

# A subseries plot has one panel for each month, showing how that month changes
# over the years (the blue line is the mean of each month).
us_retail_employment |>
  gg_subseries(Employed)

# STL decomposition splits the series into trend, seasonal and remainder
# components. model() fits the STL model and components() extracts the pieces.
# Plotting only the seasonal component (season_year) removes the trend, so the
# change in the seasonal pattern over time is easier to see.
us_retail_employment |>
  model(stl = STL(Employed)) |>
  components() |>
  gg_subseries(season_year)

# A seasonal plot overlays each year. For the original data the trend dominates,
# so the years are stacked on top of each other...
us_retail_employment |>
  gg_season(Employed)

# ...but the seasonal component alone shows the pattern clearly.
us_retail_employment |>
  model(stl = STL(Employed)) |>
  components() |>
  gg_season(season_year)


# 4. Features of tourism time series (STL strength) ----------------------------

# Total holiday trips for each State: filter to the Holiday purpose, then add
# up all regions within each state (group_by + summarise).
holidays <- tourism |>
  filter(Purpose == "Holiday") |>
  group_by(State) |>
  summarise(Trips = sum(Trips))

holidays |>
  autoplot(Trips)

# Fit an STL model to each State's series (this prints the model table)...
holidays |>
  model(STL(Trips))

# ...and extract the decomposition components for every State.
holidays |>
  model(STL(Trips)) |>
  components()

# features() with feat_stl computes one row of STL-based features for each time
# series, such as trend_strength and seasonal_strength_year (both between 0 and
# 1, where values near 1 mean a strong trend or strong seasonality).
# Each point below is one of the many series in the tourism dataset.
tourism |>
  features(Trips, feat_stl) |>
  ggplot(aes(x = trend_strength, y = seasonal_strength_year)) +
  geom_point()

# Colour the points by State...
tourism |>
  features(Trips, feat_stl) |>
  ggplot(aes(x = trend_strength, y = seasonal_strength_year)) +
  geom_point(aes(colour = State))

# ...or by the Purpose of the trip. Purpose separates the series better: for
# example, holiday trips tend to be the most seasonal.
tourism |>
  features(Trips, feat_stl) |>
  ggplot(aes(x = trend_strength, y = seasonal_strength_year)) +
  geom_point(aes(colour = Purpose))


# 5. Basic forecasting models --------------------------------------------------

# model() can fit several models at once to the same data. Each argument is a
# name for the model and the model specification:
#   - SNAIVE(): seasonal naive, forecasts the last observed value of each season
#   - NAIVE():  naive, forecasts the last observed value
#   - RW(~ drift()): random walk with drift, extends the average historical change
#   - MEAN():   forecasts the average of all the historical data
beer_fit <- aus_production |>
  model(
    Seasonal_naive = SNAIVE(Beer),
    Naive = NAIVE(Beer),
    Drift = RW(Beer ~ drift()),
    Mean = MEAN(Beer)
  )
# The result is a mable (model table): one column for each model.
beer_fit

# select() chooses a single model, and report() shows its details.
beer_fit |>
  select(Drift) |>
  report()

# tidy() and coef() give the estimated parameters of each model, as a table.
beer_fit |>
  tidy()
beer_fit |>
  coef()

# glance() gives a one-row summary of each model, with measures of model fit.
beer_fit |>
  glance()

# forecast() produces forecasts from each model for the next 5 years. The
# forecasts are distributions (not single numbers), shown as a mean and
# a distribution such as N(mean, variance).
beer_fit |>
  forecast(h = "5 years")

# Plot the forecasts with the historical data. The shaded areas are the 80%
# and 95% prediction intervals.
beer_fit |>
  forecast(h = "5 years") |>
  autoplot(aus_production)

# By default, the prediction intervals assume normally distributed errors.
# bootstrap = TRUE instead resamples the model's past errors, which can be
# better if the errors aren't normally distributed.
beer_fit |>
  forecast(h = "5 years", bootstrap = TRUE)
beer_fit |>
  forecast(h = "5 years", bootstrap = TRUE) |>
  autoplot(aus_production)

# hilo() extracts the 95% prediction interval from the forecast distribution
# into its own column.
beer_fit |>
  forecast(h = "5 years") |>
  mutate(hilo(Beer, 95))
