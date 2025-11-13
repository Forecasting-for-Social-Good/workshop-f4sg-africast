library(fpp3) # for forecast and tsibble)
library(ggdist)# for visualizing distributions
#

tourism_holiday <- tourism |> filter(Purpose == "Holiday") |> summarise(Trips = sum(Trips)) #
# Fit a seasonal naive model to the holiday tourism data
fit <- tourism_holiday |> model(snaive = SNAIVE(Trips))
# Generate forecasts for the next 6 quarters
fcst <- fit |> forecast(h=6)
# Fit multiple models to the holiday tourism data
fit_all <- tourism |> model(snaive = SNAIVE(Trips),
                            naive = NAIVE(Trips),
                            average = MEAN(Trips),
                            regression = TSLM(Trips ~ trend() + season())
)
# Generate forecasts for the next 6 quarters from all models
fcst_all <- fit_all |> forecast(h=6)
# Plot the forecast distribution along with fitted values and actual data (this plot is not suitabel for many time series, you need to selct which series befoe using the plot)
ggplot(data = fcst, aes(x = Quarter, ydist = Trips))+
  ggdist::stat_halfeye(alpha = .4) +
  geom_line(aes(y = .mean, colour = "Point Forecast"))+
  geom_line(aes(y = .fitted, colour = "Fitted"), data = augment(fit))+
  geom_point(aes(y = .fitted, colour = "Fitted"), data = augment(fit))+
  geom_line(aes(y = Trips, colour = "Data"), data = tourism_holiday)+
  geom_point(aes(y = Trips, colour = "Data"), data = tourism_holiday)+
  scale_color_manual(name = NULL,
                     breaks = c("Fitted", "Data", "Point Forecast"),
                     values = c("Fitted" = "#E69F00", "Data" = "#0072B2", "Point Forecast" = "#000000"))


# Fit an ETS model to the holiday tourism data
fit_ets <- tourism_holiday |> model(exponential_smoothing = ETS(Trips))
# Fit different models of ETS to the holiday tourism data, with specific components and smoothing parameters
fit_ets <- tourism_holiday |> model(exponential_smoothing = ETS(Trips), # automatic ETS
                                    additive_damped = ETS(Trips ~ error("A") + trend("Ad") + season("A")),#
                                    multiplicative = ETS(Trips ~ error("M") + trend("A") + season("M")), #
                                    multiplicative_damped = ETS(Trips ~ error("M") + trend("Md") + season("M")),#
                                    additive = ETS(Trips ~ error("A") + trend("A") + season("A")),#
                                    simple_ets = ETS(Trips ~ error("A") + trend("N") + season("N")),#
                                    simple_ets_alpha = ETS(Trips ~ error("A") + trend("N", alpha=0.1) + season("N")),#
                                    simple_ets_alpha_beta = ETS(Trips ~ error("A") + trend("A", alpha=0.1, beta=0.01) + season("N")),#
                                    simple_ets_beta = ETS(Trips ~ error("A") + trend("A", beta=0.01) + season("N")),#
                                    simple_ets_alpha_beta_gamma = ETS(Trips ~ error("A") + trend("A") + season("A", gamma=0.1))#
)


# Generate forecasts for the next 6 quarters from the ETS model
fcst_ets <- fit_ets |> forecast(h=6)
# Extract and view the components of the ETS model
fit_ets |> components()
# Generate a report of the ETS model
fit_ets |> report()
# Tidy the ETS model output
fit_ets |> tidy()
# Glance at the ETS model summary
fit_ets |> glance()
# Augment the ETS model with fitted values
fit_ets |> augment()


## ARIMA (session 2)
global_economy |>
  filter(Country == "Australia") |>
  autoplot(GDP)

# Stationary time series
# 1. Constant variance (transform the time series log/box_cox)
global_economy |>
  filter(Country == "Australia") |>
  autoplot(log(GDP))

# 2. Constant mean (no trend, if trended we need to difference the data)
global_economy |>
  filter(Country == "Australia") |>
  autoplot(difference(log(GDP)))

fit_arima <- global_economy |>
  filter(Country == "Australia") |>
  model(arima = ARIMA(log(GDP), trace = TRUE))

fit_arima <- global_economy |>
  filter(Country == "Australia") |>
  model(arima = ARIMA(log(GDP), stepwise = FALSE, trace = TRUE))

fit_arima |>
  forecast(h = "10 years") |>
  autoplot(global_economy)


tourism_holiday |>
  autoplot(Trips)

tourism_holiday |>
  model(ETS(Trips))

tourism_holiday |>
  autoplot(log(Trips))

fit <- tourism_holiday |>
  model(
    arima = ARIMA(log(Trips)),
    ets = ETS(Trips)
  )

fit |>
  forecast(h = "3 years") |>
  autoplot(tourism_holiday)

fit_ensemble <- fit |>
  mutate(
    ensemble = (arima+ets)/2
  )

fit_ensemble |>
  forecast(h = "3 years") |>
  autoplot(tourism_holiday |> tail(12))
