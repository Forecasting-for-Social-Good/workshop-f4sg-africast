library(fpp3)
library(ggdist)
library(tidyverse)
aus_tourism <- tourism |> index_by(Quarter) |> summarise(Trips = sum(Trips))

fit <- aus_tourism |> 
  model(average = MEAN(Trips),
        naive = NAIVE(Trips),
        snaiev = SNAIVE(Trips),
        regression = TSLM(Trips ~ trend() + season()),
        automatic_ets = ETS(Trips),
        automatc_arima = ARIMA(Trips)
        )
#tidy(), glance(), report(), components(), augment()
fitted_ets <- fit |> select(automatic_ets) |> augment()
fcst <- fit |> forecast(h = 4)
fcst_ets <- fcst |> filter(.model == "automatic_ets")
ggplot(data = fcst_ets, mapping = aes(x = Quarter, ydist = Trips))+
  stat_halfeye(alpha = .4)+
  geom_line(aes(y = .fitted, colour ="Fitted"), data = filter_index(fitted_ets, "2014 Q1" ~ .))+
  geom_line(aes(y = Trips, colour ="Data"),data = filter_index(fitted_ets, "2014 Q1" ~ .))
  
## Basic accuracy- split to Test and Train
forecast_horizon <- 4
test <- aus_tourism |> 
  filter_index(as.character(max(aus_tourism$Quarter)-forecast_horizon +1) ~ .)
train <- aus_tourism |> filter_index(. ~ as.character(max(aus_tourism$Quarter)-forecast_horizon))
fit <- train |> 
  model(average = MEAN(Trips),
        naive = NAIVE(Trips),
        snaiev = SNAIVE(Trips),
        regression = TSLM(Trips ~ trend() + season()),
        automatic_ets = ETS(Trips),
        automatc_arima = ARIMA(Trips)
  )

fcst <- fit |> forecast(h = forecast_horizon)

fcst_accuracy <- fcst |> 
accuracy(aus_tourism,
        measures = list(point_accuracy_measures,
                        interval_accuracy_measures,
                        distribution_accuracy_measures))

fcst_accuracy |> select(.model, MAE, RMSE, winkler, CRPS)

# Time series cross validation
forecast_horizon <- 4
percentage_test <- 0.3

test <- aus_tourism |> filter_index(as.character(max(aus_tourism$Quarter) - 
                              round(percentage_test*length(unique(aus_tourism$Quarter)))+1) ~ .)
                            
train <- aus_tourism |> filter_index(. ~ as.character(max(aus_tourism$Quarter) - 
                                                   round(percentage_test*length(unique(aus_tourism$Quarter)))))                         

tscv_aus_tourism <- aus_tourism |> 
  filter_index(. ~ as.character(max(aus_tourism$Quarter)-forecast_horizon)) |> 
  stretch_tsibble(.init = length(unique(train$Quarter)), .step = 1)

fit <- tscv_aus_tourism |> 
  model(average = MEAN(Trips),
        naive = NAIVE(Trips),
        snaiev = SNAIVE(Trips),
        regression = TSLM(Trips ~ trend() + season()),
        automatic_ets = ETS(Trips),
        automatc_arima = ARIMA(Trips)
  ) |> 
  mutate(combination = (automatc_arima+automatic_ets+snaiev)/3)

fcst <- fit |> forecast(h = forecast_horizon)

fcst_accuracy <- fcst |> 
  accuracy(aus_tourism,
           measures = list(point_accuracy_measures,
                           interval_accuracy_measures,
                           distribution_accuracy_measures))

fcst_accuracy |> select(.model, MAE, RMSE, winkler, CRPS)

# Winkler score alone
fcst |> 
  accuracy(aus_tourism, list(qs = winkler_score), level = .9)

# quantile score alone
fcst |> 
  accuracy(aus_tourism, list(qs = quantile_score), probs = .9)

# Accuracy by model and .id
fcst_accuracy <- fcst |> 
  accuracy(aus_tourism, by = c(".model", ".id"))

fcst_accuracy |> ggplot(aes( x = RMSE, y = fct_reorder(.model, RMSE)))+
  geom_boxplot()
