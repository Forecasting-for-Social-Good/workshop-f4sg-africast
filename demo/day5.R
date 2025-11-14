library(fpp3)
# Load necessary libraries

# use aus_retail data , turnover of cafe industry in Australia
cafe_retail <- aus_retail |>
  filter(Industry == "Cafes, restaurants and catering services") |>
  summarise(Turnover = sum(Turnover))
# visualize the data
cafe_retail |> autoplot(Turnover)

## Basic accuracy- split to Test and Train
forecast_horizon <- 12 # 12 months forecast horizon

# split data to train and test
test <- cafe_retail |>
  filter_index(as.character(max(cafe_retail$Month)-forecast_horizon +1) ~ .)

train <- cafe_retail |> filter_index(. ~ as.character(max(cafe_retail$Month)-forecast_horizon))

# fit models on train data
fit_basic <- train |> model(
  naive = NAIVE(Turnover),
  snaive = SNAIVE(Turnover),
  arima = ARIMA(Turnover),
  ets = ETS(Turnover),
  regression = TSLM(Turnover ~ trend() + season()),
  stl = decomposition_model(
    STL(Turnover ~ trend(window = 21), robust =TRUE),
    NAIVE(season_adjust)
  )
) |> 
  mutate(comb = (arima+ets+stl)/3)# forecast combination, simple average of top 3 models
# generate forecasts
fcst_basic <- fit_basic |> forecast(h = forecast_horizon)
# evaluate forecast accuracy
fcst_accuracy <- fcst_basic |> accuracy(cafe_retail,
                       measures = list(point_accuracy_measures,
                                       interval_accuracy_measures,
                                       distribution_accuracy_measures))
# display accuracy metrics fro selected metrics
fcst_accuracy |> select(.model,ME, RMSE, MAE, winkler, pinball, CRPS)

#--------time series cross validation
percentage_test <- 0.3
# split data to train and test, 30 percent of data for test
test <- cafe_retail |> filter_index(as.character(max(cafe_retail$Month) -
                                                   round(percentage_test*length(unique(cafe_retail$Month)))+1) ~ .)
# 70% of data for train
train <- cafe_retail |> filter_index(. ~ as.character(max(cafe_retail$Month) -
                                                        round(percentage_test*length(unique(cafe_retail$Month)))))
# create time series cross validation sets
tscv_cafe_retail <- cafe_retail |>
  filter_index(. ~ as.character(max(cafe_retail$Month)-forecast_horizon)) |>
  stretch_tsibble(.init = length(unique(train$Month)), .step = 1)
# fit models on tscv data
fit <- tscv_cafe_retail |> model(
  naive = NAIVE(Turnover),
  snaive = SNAIVE(Turnover),
  arima = ARIMA(Turnover),
  ets = ETS(Turnover),
  regression = TSLM(Turnover ~ trend() + season()),
  stl = decomposition_model(
    STL(Turnover ~ trend(window = 21), robust =TRUE),
    NAIVE(season_adjust)
  )
) |> 
  mutate(comb = (arima+ets+stl)/3)
# generate forecasts
fcst <- fit |> forecast(h= forecast_horizon)
# evaluate forecast accuracy
fcst_accuracy <- fcst |> accuracy(cafe_retail,
                                        measures = list(point_accuracy_measures,
                                                        interval_accuracy_measures,
                                                        distribution_accuracy_measures))
# display accuracy metrics fro selected metrics
fcst_accuracy |> select(.model,ME, RMSE, MAE, winkler, pinball, CRPS)
# accuracy report for specific pediction interval score
fcst |> accuracy(cafe_retail, list(winkler = winkler_score), level=.95)
# accuracy report for specific quantile score
fcst |> accuracy(cafe_retail, list(qs = quantile_score), probs=.95)

# accuracy report based on each id and model

#.id is the identifier for each rolling origin created in tscv
# calculate accuracy by .id and .model
accuracy_by_id <- fcst |> accuracy(cafe_retail, 
                                   measures = list(point_accuracy_measures,
                                                   interval_accuracy_measures,
                                                   distribution_accuracy_measures),
                                   by = c(".model", ".id"))
# visualize the variation of RMSE across different .id for each model
ggplot(data = accuracy_by_id, mapping = aes( x = RMSE, y = fct_reorder(.model, RMSE)))+
  geom_boxplot()+
  ggthemes::theme_few()
  
# accuracy by forecast horizon
# add horizon column to forecast data
fc_h <- fcst |>
  group_by(.id,.model) |>
  mutate(h=row_number()) |> ungroup() |>
  as_fable(response = "Turnover", distribution = "Turnover")
# calculate accuracy by h and .model
fc_accuracy_h <- fc_h |>
  accuracy(cafe_retail,
           measures = list(point_accuracy_measures,
                           interval_accuracy_measures,
                           distribution_accuracy_measures),
           by = c(".model","h"))



#You can now create a line chart to show how forecast accuracy may change over the forecast horizon.

ggplot(data = fc_accuracy_h,
       mapping = aes(x = h, y = RMSE, color = .model))+
  geom_point()+
  geom_line()+
  ggthemes::scale_color_colorblind()+
  ggthemes::theme_clean()

