library(fpp3)
library(ggdist)
library(tidyverse)
aus_beer <- aus_production |> select(Beer)

## Basic accuracy- split to Test and Train
forecast_horizon <- 4
#test <- aus_beer |>
  filter_index(as.character(max(aus_beer$Quarter)-forecast_horizon +1) ~ .)

test <- aus_beer |>
  filter_index("2009 Q3" ~ .)

#train <- aus_beer |> filter_index(. ~ as.character(max(aus_beer$Quarter)-forecast_horizon))

train <- aus_beer |>
  filter_index(. ~ "2009 Q2")

fit <- train |>
  model(average = MEAN(Beer),
        snaiev = SNAIVE(Beer),
        regression = TSLM(Beer ~ trend() + season()),
        automatic_ets = ETS(Beer),
        automatc_arima = ARIMA(Beer)
  )

fcst <- fit |> forecast(h = forecast_horizon)

fcst_accuracy <- fcst |>
accuracy(aus_beer,
        measures = list(point_accuracy_measures,
                        interval_accuracy_measures,
                        distribution_accuracy_measures))

fcst_accuracy |> select(.model, MAE, RMSE, winkler, CRPS)

# Time series cross validation
forecast_horizon <- 4
percentage_test <- 0.3

test <- aus_beer |> filter_index(as.character(max(aus_beer$Quarter) -
                              round(percentage_test*length(unique(aus_beer$Quarter)))+1) ~ .)

train <- aus_beer |> filter_index(. ~ as.character(max(aus_beer$Quarter) -
                                                   round(percentage_test*length(unique(aus_beer$Quarter)))))

tscv_aus_beer <- aus_beer |>
  filter_index(. ~ as.character(max(aus_beer$Quarter)-forecast_horizon)) |>
  stretch_tsibble(.init = length(unique(train$Quarter)), .step = 1)

fit <- tscv_aus_beer |>
  model(average = MEAN(Beer),
        naive = NAIVE(Beer),
        snaiev = SNAIVE(Beer),
        regression = TSLM(Beer ~ trend() + season()),
        automatic_ets = ETS(Beer),
        automatc_arima = ARIMA(Beer)
  ) |>
  mutate(combination = (automatc_arima+automatic_ets+snaiev)/3)

fcst <- fit |> forecast(h = forecast_horizon)

fcst_accuracy <- fcst |>
  accuracy(aus_beer,
           measures = list(point_accuracy_measures,
                           interval_accuracy_measures,
                           distribution_accuracy_measures))

fcst_accuracy |> select(.model, MAE, RMSE, winkler, CRPS)

# Winkler score alone
fcst |>
  accuracy(aus_beer, list(qs = winkler_score), level = .9)

# quantile score alone
fcst |>
  accuracy(aus_beer, list(qs = quantile_score), probs = .9)

# Accuracy by model and .id
fcst_accuracy <- fcst |>
  accuracy(aus_beer, by = c(".model", ".id"))

fcst_accuracy |> ggplot(aes( x = RMSE, y = fct_reorder(.model, RMSE)))+
  geom_boxplot()+
  ggthemes::theme_few()

# Density plot
fcst_accuracy |>
  ggplot(aes(RMSE))+
  ggridges::geom_density_ridges(aes(y=fct_reorder(.model, RMSE)))+
  ggthemes::theme_few()

#### accuracy across horizon

View(fcst[1:24,])


#We first need to group by `id` and `.model` and then create a new variable called `h` and assign `row_number()` to it (you can type ?row_number in your Console to see what this function does, it simply returns the number of row):

fc_h <- fcst |>
  group_by(.id,.model) |>
  mutate(h=row_number()) |> ungroup() |>
  as_fable(response = "Beer", distribution = "Beer")


View(fc_h[1:24,])# view the first 24 rows of ae_fc and observe h


#Now check rows from 12 to 24 to see the difference.

#To calculate the accuracy measures for each horizon and model, complete the following code :

  fc_accuracy_h <- fc_h |>
  accuracy(aus_beer,
           measures = list(point_accuracy_measures,
                           interval_accuracy_measures,
                           distribution_accuracy_measures),
           by = c(".model","h"))



#You can now create a line chart to show how forecast accuracy may change over the forecast horizon. Please complete the R code for a metric of your preference. You can replicate this process by changing the chosen metric:

  ggplot(data = fc_accuracy_h,
         mapping = aes(x = h, y = RMSE, color = .model))+
  geom_point()+
  geom_line()+
  ggthemes::scale_color_colorblind()+
  ggthemes::theme_clean()


  ggplot(data = fcst, mapping = aes(x = Quarter, ydist = Beer))+
    ggdist::stat_halfeye(alpha = .4)+
    geom_line(aes(y=.mean, colour ="Point Forecast"))+
    geom_line(aes(y = .fitted, colour ="Fitted"), data = filter_index(fitted_ets, "2005 Q1" ~ .))+
    geom_point(aes(y = .fitted, colour ="Fitted"), data = filter_index(fitted_ets, "2005 Q1" ~ .))+
    geom_line(aes(y = Beer, colour ="Data"),data = filter_index(aus_beer, "2005 Q1" ~ .))+
    geom_point(aes(y = Beer, colour ="Data"),data = filter_index(aus_beer, "2005 Q1" ~ .))+
    scale_color_manual(name=NULL,
                       breaks=c('Fitted', 'Data',"Point Forecast"),
                       values=c('Fitted'='#E69F00', 'Data'='#0072B2',"Point Forecast"="#000000"))


