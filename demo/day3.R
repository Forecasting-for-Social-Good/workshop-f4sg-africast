# ==============================================================================
# Day 3 demo: regression models for forecasting
# ==============================================================================
# How to run this script:
#   - Ctrl + Enter runs the current line (or the selected code) in the console.
#   - Run the sections in order: later sections reuse objects created earlier.
#
# Contents:
#   1. Setup and exploring the data
#   2. Multiple linear regression
#   3. Comparing regression models
#   4. Trend and seasonality with TSLM
#   5. Transformations (log)
#   6. Forecasting with future predictors
#   7. Scenario forecasting
#   8. Next dataset: souvenirs
# ==============================================================================


# 1. Setup and exploring the data ----------------------------------------------

# fpp3 loads the tidy forecasting packages (tsibble, feasts, fable, dplyr,
# ggplot2, ...) and the example datasets used below.
library(fpp3)

# `?` opens the help page of a dataset or function.
# us_change contains quarterly percentage changes in US consumption, income,
# production, savings and unemployment.
?us_change
us_change

# Time plot of Consumption and Income. pivot_longer() stacks the two columns
# into one `Measure` column (with the values in `Change`) so that autoplot()
# draws them as two lines on the same plot.
us_change |>
  pivot_longer(c(Consumption, Income), names_to = "Measure", values_to = "Change") |>
  autoplot(Change) +
  labs(x = "Year", y = "% change")


# 2. Multiple linear regression ------------------------------------------------

# model() fits one or more models to the data, and returns a "mable" (model
# table) with one column per model.
# TSLM() fits a time series linear model. The formula `Consumption ~ ...`
# says to predict Consumption using Income, Production, Unemployment and Savings.
fit_full <- us_change |>
  model(lm = TSLM(Consumption ~ Income + Production + Unemployment + Savings))

# Printing a mable shows the model type for each fitted model.
fit_full

# gg_tsresiduals() checks the residuals (what the model couldn't explain):
# a time plot, an ACF and a histogram. Good residuals look like white noise.
gg_tsresiduals(fit_full)

# The same ACF of the residuals, drawn manually. residuals() returns the
# residuals as a tsibble (in the `.resid` column), and `level = 99` widens the
# significance bounds to 99%.
residuals(fit_full) |>
  ACF(.resid) |>
  autoplot(level = 99)

# glance() summarises how well each model fits in a single row, using measures
# like adjusted R^2 (adj_r_squared) and AICc.
glance(fit_full)


# 3. Comparing regression models -----------------------------------------------

# model() can fit several models at once, by naming each one. Here we compare:
#   - slr:     simple linear regression, using only Income,
#   - full:    all four predictors,
#   - no_prod: all predictors except Production.
fit_compare <- us_change |>
  model(
    slr = TSLM(Consumption ~ Income),
    full = TSLM(Consumption ~ Income + Production + Unemployment + Savings),
    no_prod = TSLM(Consumption ~ Income + Unemployment + Savings)
  )
fit_compare

# glance() gives one row per model. A higher adj_r_squared and a lower AICc
# (or CV) indicates a better model.
glance(fit_compare)

# select() keeps one model (a column of the mable), and forecast() predicts it.
# This fails without future values for the predictors, see section 6!
fit_compare |>
  select(full) |>
  forecast()


# 4. Trend and seasonality with TSLM -------------------------------------------

# aus_production is quarterly Australian production. Cement shows a trend,
# seasonality, and variation that grows with the level of the series.
aus_production |>
  autoplot(Cement)

# trend() and season() are special terms that TSLM() creates for us: a linear
# time trend, and seasonal dummy variables (one per quarter).
# This needs no future predictors, since they are made from the time index.
fit <- aus_production |>
  model(
    TSLM(Cement ~ trend() + season())
  )

# `h` is the forecast horizon. A string such as "10 years" is converted into
# the right number of observations (here 40 quarters).
# Passing the original data to autoplot() draws it alongside the forecasts.
fit |>
  forecast(h = "10 years") |>
  autoplot(aus_production)

# The residuals show leftover patterns, and their spread grows over time.
gg_tsresiduals(fit)


# 5. Transformations (log) -----------------------------------------------------

# A log transformation stabilises variation that grows with the level.
# Compare this plot to the time plot of Cement above.
aus_production |>
  autoplot(log(Cement))

# Transformations can be written directly in the model formula.
# Forecasts are automatically back-transformed to the original scale.
fit <- aus_production |>
  model(
    TSLM(log(Cement) ~ trend() + season())
  )

# Forecasting far into the future: the intervals widen, and the log model
# grows exponentially rather than linearly.
fit |>
  forecast(h = "50 years") |>
  autoplot(aus_production)

# The residuals now have a more consistent spread.
gg_tsresiduals(fit)


# 6. Forecasting with future predictors ----------------------------------------

# A model with predictors needs values of those predictors in the future.
# tail() shows the last rows of the data, i.e. where the forecasts begin.
us_change |> tail()

# new_data() creates a tsibble of the next 4 quarters, to which we add an
# assumed value for each predictor (here: income grows 1%, savings 0.5%, and
# no change in unemployment or production).
future_a <- new_data(us_change, 4) |>
  mutate(Income = 1, Savings = 0.5, Unemployment = 0, Production = 0)

# `new_data` supplies the predictor values for the forecast.
fit_full |>
  forecast(new_data = future_a) |>
  autoplot(us_change)


# 7. Scenario forecasting ------------------------------------------------------

# scenarios() bundles several named future paths together, so that we can
# forecast each one. `names_to` is the name of the column that identifies them.
# Increase: income and savings rise. Decrease: income and savings fall.
future_scenarios <- scenarios(
  Increase = new_data(us_change, 4) |>
    mutate(Income = 1, Savings = 0.5, Unemployment = 0, Production = 0),
  Decrease = new_data(us_change, 4) |>
    mutate(Income = -1, Savings = -0.5, Unemployment = 0, Production = 0),
  names_to = "Scenario"
)
future_scenarios <- scenarios(
  Increase = new_data(us_change, 4) |>
    mutate(Income = 1, Savings = 0.5, Unemployment = 0, Production = 0),
  Decrease = new_data(us_change, 4) |>
    mutate(Income = -1, Savings = -0.5, Unemployment = 0, Production = 0),
  names_to = "Scenario"
)

# Forecast every scenario at once, giving one set of forecasts for each.
fc_scenarios <- forecast(fit_full, new_data = future_scenarios)

# autolayer() adds the scenario forecasts on top of the time plot of the data.
us_change |> autoplot(Consumption) +
  autolayer(fc_scenarios)


# 8. Next dataset: souvenirs ---------------------------------------------------

# souvenirs is monthly sales at a souvenir shop in Queensland. It has strong
# seasonality and a trend, which we will model next.
souvenirs |>
  autoplot(Sales)
