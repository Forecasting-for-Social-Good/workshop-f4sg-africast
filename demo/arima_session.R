library(fpp3) # for forecast and tsibble)
library(ggdist)# for visualising distributions

# ARIMA (session 2)

## Example: National populations

fit_pop <- global_economy |>
  model(arima = ARIMA(Population))
fit_pop ## we can see the ARIMA model fitted to the population data for each country

## Report the ARIMA model for Australia to see the model parameters and diagnostics
fit_pop |>
  filter(Country == "Australia") |>
  report()

## Forecasting the population for Australia for the next 10 years to see how trend looks like when using second differencing in ARIMA(0,2,1)
fit_pop |>
  forecast(h = 10) |>
  filter(Country == "Australia") |>
  autoplot(global_economy) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M"))

## Plot the GDP for Australia to see if the time series is stationary or not
global_economy |>
  filter(Country == "Australia") |>
  autoplot(GDP)

## Stationary time series
## Constant variance (transform the time series log/box_cox)
global_economy |>
  filter(Country == "Australia") |>
  autoplot(log(GDP))

## Constant mean (no trend, if trended we need to difference the data)
global_economy |>
  filter(Country == "Australia") |>
  autoplot(difference(log(GDP)))

## Construct ARIMA model by taking log of GDP
fit_arima <- global_economy |>
  filter(Country == "Australia") |>
  model(arima = ARIMA(log(GDP), trace = TRUE))

## Stepwise selection of ARIMA model
fit_arima <- global_economy |>
  filter(Country == "Australia") |>
  model(arima = ARIMA(log(GDP), stepwise = FALSE, trace = TRUE))

## Forecasting with ARIMA model
fit_arima |>
  forecast(h = "10 years") |>
  autoplot(global_economy)

## SARIMA Examples
## Corticosteroid drug sales: Monthly Australian Pharmaceutical Benefits Scheme (PBS) expenditure on corticosteroid (H02) drugs.

h02 <- PBS |>
  filter(ATC2 == "H02") |>
  summarise(Cost = sum(Cost) / 1e6)
h02 |> autoplot(Cost) + labs(y = "$AUD million")

## log transformation to stabilise variance
h02 |> autoplot(log(Cost))

## Seasonal differencing to stabilise mean to make the time series stationary
h02 |> autoplot(log(Cost) |> difference(12))

## Seasonal differencing and first differencing to stabilise mean to make the time series stationary
h02 |> autoplot(log(Cost) |> difference(12) |> difference(1))

## automatic ARIMA 
fit_h02 <- h02 |>
  model(auto = ARIMA(log(Cost)))
fit_h02 |> report()

## manual ARIMA: Specify the ARIMA model with parameters (p,d,q)(P,D,Q)[s] where s is the seasonal period (12 for monthly data)
fit_h02_manual <- h02 |>
  model(manual = ARIMA(log(Cost) ~ 0 + pdq(2, 1, 0) + PDQ(0, 1, 1)))
fit_h02_manual |> report()

## Forecasting with the automatic ARIMA model
fit_h02 |>
  forecast(h = "3 years") |>
  autoplot(h02) +
  labs(y = "$AUD million")

## Searching harder for the best ARIMA model by setting stepwise = FALSE and approximation = FALSE to search all possible models
fit_h02_best <- h02 |>
  model(best = ARIMA(log(Cost),
    stepwise = FALSE,
    approximation = FALSE,
    order_constraint = p + q + P + Q <= 9
  ))
fit_h02_best |> report()

## Residual diagnostics for the best ARIMA model to check if the residuals are white noise and the model is a good fit for the data
gg_tsresiduals(fit_h02_best, lag_max = 36)

## Ljung-Box test for the residuals of the best ARIMA model to confirm if the residuals are white noise and the model is a good fit for the data when there are slight autocorrelations in the residuals and we are not sure if the residuals are white noise or not
bind_rows(
  augment(fit_h02) |> features(.innov, ljung_box, lag = 36, dof = 3),
  augment(fit_h02_best) |> features(.innov, ljung_box, lag = 36, dof = 9)
)

## Forecasting with the best ARIMA model
fit_h02_best |>
  forecast(h = "3 years") |>
  autoplot(h02) +
  labs(y = "$AUD million")