# ==============================================================================
# Day 4 demo: exponential smoothing (ETS), forecasting, and bootstrapped intervals
# ==============================================================================
# How to run this script:
#   - Ctrl + Enter runs the current line (or the selected code) in the console.
#   - Run the sections in order: later sections reuse objects created earlier.
#
# Contents:
#   1. Setup
#   2. Fitting ETS models
#   3. Inspecting the fitted models
#   4. Forecasting
#   5. Recap: the workflow for a time series analysis
#   6. Visualising the ETS forecast
#   7. Forecast intervals
#   8. Bootstrapped forecasts
# ==============================================================================


# 1. Setup ---------------------------------------------------------------------

# fpp3 loads the tidy forecasting packages (tsibble, feasts, fable, dplyr,
# ggplot2, ...) and example datasets such as tourism and aus_production.
library(fpp3)

# ggdist draws distributions in ggplot2, e.g. stat_halfeye() for forecasts.
library(ggdist)

# distributional provides tools for working with distributions, such as the
# variance() and quantile() of a forecast distribution.
library(distributional)


# 2. Fitting ETS models --------------------------------------------------------

# tourism is quarterly Australian domestic overnight trips. We keep the holiday
# trips to the Snowy Mountains. The result is still a tsibble, so models and
# forecasts keep the time index.
tourism_holiday <- tourism |>
  filter(Purpose == "Holiday", Region == "Snowy Mountains")

# model() fits several ETS (error, trend, seasonal) models to the same series.
# In the formulas, "A" is additive, "N" is none (and "M" would be multiplicative):
#   ets_holiday: ETS() chooses the best specification automatically,
#   simple_ses:  additive error, no trend, no seasonality (simple exponential
#                smoothing),
#   holt:        additive error, additive trend, no seasonality (Holt's linear
#                method).
holiday_fit <- tourism_holiday |>
  model(
    ets_holiday = ETS(Trips),
    simple_ses = ETS(Trips ~ error("A") + trend("N") + season("N")),
    holt = ETS(Trips ~ error("A") + trend("A") + season("N"))
  )


# 3. Inspecting the fitted models ----------------------------------------------

# The same five functions work on most fable models, not just ETS.

# report() shows the model specification, the estimated smoothing parameters
# and the initial states.
holiday_fit |> report()

# components() gives the decomposed level, trend and season components of each
# model over time.
holiday_fit |> components()

# tidy() gives the parameter estimates (coefficients) of each model.
holiday_fit |> tidy()

# glance() gives one row of model-level fit statistics per model: AIC, AICc,
# BIC and sigma^2. Lower information criteria indicate a better model.
holiday_fit |> glance()
holiday_fit |> tidy()

# augment() gives the one-step-ahead fitted values and residuals.
holiday_fit |> augment()


# 4. Forecasting ---------------------------------------------------------------

# forecast() predicts `h` periods ahead (here 6 quarters). The result is a
# fable of distributional forecasts: each forecast is a full distribution, with
# the mean stored in `.mean`.
holiday_fcst <- holiday_fit |> forecast(h = 6)


# 5. Recap: the workflow for a time series analysis ----------------------------

# Step 1: read the data. It will be a tibble (tidyverse), so make it tidy if
# needed, then convert it to a tsibble and check the data quality.
# Here we select the Beer column from aus_production (quarterly Australian
# production) and use filter_index() to keep 2000 Q1 to 2009 Q1.
aus_beer <- aus_production |>
  select(Beer) |>
  filter_index("2000 Q1" ~ "2009 Q1")

# Step 2: time series graphics. Plot the series to look for trend, seasonality,
# patterns and anomalies.

# Step 3: fit models. If needed, tune hyperparameters, then inspect the objects
# with report(), glance(), tidy(), augment() and components().
# We fit the benchmarks (NAIVE and SNAIVE) alongside ETS and ARIMA, to compare
# against.
fit_beer <- aus_beer |>
  model(
    naive = NAIVE(Beer),
    snaive = SNAIVE(Beer),
    ets = ETS(Beer),
    arima = ARIMA(Beer)
  )

# Step 4: generate forecasts, 6 quarters ahead.
fcst_beer <- fit_beer |> forecast(h = 6)

# Keep only the ETS forecasts and the ETS fitted values, for plotting below.
fcst_ets <- fcst_beer |> filter(.model == "ets")
fitted_ets <- fit_beer |> augment() |> filter(.model == "ets")


# 6. Visualising the ETS forecast ----------------------------------------------

# We build the plot from layers: the forecast distribution (half-eye), the point
# forecast, the fitted values, and the actual data from 2005 Q1 onward.
# `colour = "label"` inside aes() maps each layer to a legend entry, which
# scale_color_manual() then gives a colour and an order.
# `ydist = Beer` tells ggdist which column holds the forecast distribution.
ggplot(data = fcst_ets, mapping = aes(x = Quarter, ydist = Beer)) +
  # Forecast distribution
  ggdist::stat_halfeye(alpha = .4) +
  # Point forecast (mean) line
  geom_line(aes(y = .mean, colour = "Point Forecast")) +
  # Fitted values line and points
  geom_line(
    aes(y = .fitted, colour = "Fitted"),
    data = filter_index(fitted_ets, "2005 Q1" ~ .)
  ) +
  geom_point(
    aes(y = .fitted, colour = "Fitted"),
    data = filter_index(fitted_ets, "2005 Q1" ~ .)
  ) +
  # Actual data line and points
  geom_line(
    aes(y = Beer, colour = "Data"),
    data = filter_index(aus_beer, "2005 Q1" ~ .)
  ) +
  geom_point(
    aes(y = Beer, colour = "Data"),
    data = filter_index(aus_beer, "2005 Q1" ~ .)
  ) +
  # Manual colour palette and legend order
  scale_color_manual(
    name = NULL,
    breaks = c('Fitted', 'Data', "Point Forecast"),
    values = c(
      'Fitted' = '#E69F00',
      'Data' = '#0072B2',
      "Point Forecast" = "#000000"
    )
  )
# To compare models, facet by model (uncomment the line below, but you also
# need to change `data =` to include all of the models).
#facet_wrap(~.model, ncol = 1)


# 7. Forecast intervals --------------------------------------------------------

# Because forecasts are distributions, we can extract any summary of them.

# hilo() computes prediction intervals, and unpack_hilo() splits them into
# separate lower and upper bound columns. The default is 95%.
fcst_beer |> hilo() |> unpack_hilo("95%") |> View()

# Other levels can be requested, e.g. 70% and 90% intervals.
fcst_beer |> hilo(c(70, 90)) |> unpack_hilo(c("70%", "90%")) |> View()

# quantile() and variance() summarise each forecast distribution directly.
# Here we add the 10th and 90th percentiles, and the variance, as new columns.
fcst_beer |>
  mutate(
    q10 = quantile(Beer, 0.1),
    q90 = quantile(Beer, 0.9),
    variance = distributional::variance(Beer)
  ) |>
  View()


# 8. Bootstrapped forecasts ----------------------------------------------------

# Instead of assuming normally distributed errors, `bootstrap = TRUE` simulates
# future paths by resampling the model's past residuals. These are possible
# future scenarios, and `times` sets how many are generated (kept small here
# so they are easy to look at; use many more, e.g. 1000, in practice).
fcst_beer_bootstarp <- fit_beer |> forecast(h = 6, bootstrap = TRUE, times = 5)

# 95% intervals from the bootstrapped forecasts.
fcst_beer_bootstarp |> hilo() |> unpack_hilo("95%") |> View()
