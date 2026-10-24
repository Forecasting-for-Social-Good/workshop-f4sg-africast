# ==============================================================================
# Day 5 demo: time series cross-validation and forecast accuracy
# ==============================================================================
# How to run this script:
#   - Ctrl + Enter runs the current line (or the selected code) in the console.
#   - Run the sections in order: later sections reuse objects created earlier.
#
# Contents:
#   1. Setup and exploring the data
#   2. Splitting into train and test sets
#   3. Creating the time series cross-validation sets
#   4. Fitting models and forecasting
#   5. Evaluating forecast accuracy
#   6. Accuracy by origin (.id)
#   7. Accuracy by forecast horizon
#   8. Forecasting with the selected model
# ==============================================================================


# 1. Setup and exploring the data ----------------------------------------------

# fpp3 loads the tidy forecasting packages (tsibble, feasts, fable, dplyr,
# ggplot2, ...) and example datasets such as aus_retail.
library(fpp3)

# aus_retail is monthly Australian retail turnover, with keys for State and
# Industry. We use the total Turnover (summed over all states) for the Cafes,
# restaurants and catering services industry.
cafe_retail <- aus_retail |>
  filter(Industry == "Cafes, restaurants and catering services") |>
  summarise(Turnover = sum(Turnover))

# Visualise the time series. You would normally do more graphics than this
# (seasonal plots, subseries plots, ACF), as we have done on earlier days.
cafe_retail |> autoplot()


# 2. Splitting into train and test sets ----------------------------------------

# The forecast horizon is the number of periods we want to forecast (here 12
# months).
forecast_horizon <- 12

# The proportion of the time series used for testing: 30%. The other 70% is
# used for training.
percentage_test <- 0.3

# The test set is the last 30% of the months. We work out the first month of
# the test set (the last month minus the number of test months, plus one), and
# filter_index() keeps everything from then until the end (`~ .`).
test <- cafe_retail |>
  filter_index(
    as.character(
      max(cafe_retail$Month) -
        as.integer(round(percentage_test * length(unique(cafe_retail$Month)))) +
        1
    ) ~ .
  )

# The training set is the first 70% of the months (100% - 30%): everything up
# until the month before the test set begins (`. ~` means from the start).
train <- cafe_retail |>
  filter_index(
    . ~ as.character(
      max(cafe_retail$Month) -
        as.integer(round(percentage_test * length(unique(cafe_retail$Month))))
    )
  )


# 3. Creating the time series cross-validation sets ----------------------------

# Time series cross-validation uses rolling origins: the models are fitted to
# a training set, forecast, then the training set is extended and the process
# repeats. stretch_tsibble() creates these extending training sets, identified
# by the `.id` column.
# We drop the final `forecast_horizon` months first, so that every origin has
# a full set of future observations to compare its forecasts against.
#   - `.init` is the length of the first training set (the length of `train`),
#   - `.step` is how far the origin moves each time. This is generally 1, or it
#     can match how often forecasts are made, e.g. for daily data forecasting
#     every 7 days, use .step = 7.
tscv_cafe_retail <- cafe_retail |>
  filter_index(. ~ as.character(max(cafe_retail$Month) - forecast_horizon)) |>
  stretch_tsibble(.init = length(unique(train$Month)), .step = 1)


# 4. Fitting models and forecasting --------------------------------------------

# model() fits each model to every training set (one per .id), so this fits a
# lot of models and can take a while.
# We compare:
#   snaive: seasonal naive, a benchmark,
#   arima:  automatically selected ARIMA,
#   ets:    ETS with multiplicative error, additive trend and additive season.
# The commented out stl model is another option, an STL decomposition with a
# naive forecast of the seasonally adjusted data.
fit <- tscv_cafe_retail |>
  model(
    snaive = SNAIVE(Turnover),
    arima = ARIMA(Turnover),
    ets = ETS(Turnover ~ error("M") + trend("A") + season("A"))
    # stl = decomposition_model(
    #   STL(Turnover ~ trend(window = 21), robust = TRUE),
    #   NAIVE(season_adjust))
  ) |>
  # Models can be combined with arithmetic. This is a forecast combination: the
  # average of the ARIMA and ETS forecasts.
  mutate(combination = (arima + ets) / 2)

# Forecast `forecast_horizon` months ahead from every origin.
fcst <- fit |> forecast(h = forecast_horizon)


# 5. Evaluating forecast accuracy ----------------------------------------------

# accuracy() compares the forecasts to the actual data (`cafe_retail`). We ask
# for three sets of measures: point forecast accuracy (e.g. RMSE, MASE),
# prediction interval accuracy (e.g. winkler) and distribution accuracy
# (e.g. CRPS).
fcst_accuracy <- fcst |>
  accuracy(
    cafe_retail,
    measures = list(
      point_accuracy_measures,
      interval_accuracy_measures,
      distribution_accuracy_measures
    )
  )

# Show a few of the measures for each model. You can select any measure.
# For these measures, a lower value is better.
fcst_accuracy |> select(.model, RMSSE, CRPS)

# So far, the accuracy measures are averages across all origins (.id) and all
# forecast horizons. We can analyse the accuracy in more detail, by looking at
# each origin or each horizon.


# 6. Accuracy by origin (.id) --------------------------------------------------

# `by` chooses the groups that accuracy is calculated for. Adding ".id" gives
# the accuracy of each model from each origin.
accuracy_by_id <- fcst |>
  accuracy(
    cafe_retail,
    measures = list(
      point_accuracy_measures,
      interval_accuracy_measures,
      distribution_accuracy_measures
    ),
    by = c(".model", ".id")
  )

View(accuracy_by_id)

# Visualise it: a boxplot of the RMSE across origins for each model, ordered
# by RMSE with fct_reorder(). A narrower, lower box is a more accurate and more
# consistent model.
ggplot(
  data = accuracy_by_id,
  mapping = aes(x = RMSE, y = fct_reorder(.model, RMSE))
) +
  geom_boxplot() +
  ggthemes::theme_few()


# 7. Accuracy by forecast horizon ----------------------------------------------

# accuracy() has no built-in grouping by horizon, so we add a horizon column
# `h` to the forecasts: row_number() counts 1, 2, ... within each origin and
# model. as_fable() then turns the result back into a fable, so that accuracy()
# works on it.
fc_h <- fcst |>
  group_by(.id, .model) |>
  mutate(h = row_number()) |>
  ungroup() |>
  as_fable(response = "Turnover", distribution = "Turnover")

# Calculate the accuracy for each model and horizon.
accuracy_by_h <- fc_h |>
  accuracy(
    cafe_retail,
    measures = list(
      point_accuracy_measures,
      interval_accuracy_measures,
      distribution_accuracy_measures
    ),
    by = c(".model", "h")
  )

# Visualise the forecast accuracy by horizon and model. Forecasts are generally
# less accurate (higher RMSE) further into the future.
# ggthemes::scale_color_colorblind() uses colours that are distinguishable by
# people with colour blindness.
ggplot(data = accuracy_by_h, mapping = aes(x = h, y = RMSE, colour = .model)) +
  geom_point() +
  geom_line() +
  ggthemes::scale_color_colorblind() +
  ggthemes::theme_few()


# 8. Forecasting with the selected model ---------------------------------------

# Having chosen the most accurate model using time series cross-validation, we
# refit it to all of the data and generate the real forecasts. Here we use ARIMA.
future <- cafe_retail |>
  model(arima = ARIMA(Turnover)) |>
  forecast(h = forecast_horizon)
