library(fpp3)
## Basic of modelling and forecasting


## Time Series Regression Model (TSLM)

us_change |> 
  autoplot(Consumption)

us_change |> 
  autoplot(Income)

us_change |> 
  autoplot(Consumption) + 
  geom_line(aes(y = Income), colour = "steelblue")

us_change |> 
  ggplot(aes(y = Consumption, x = Income)) + 
  geom_point() + 
  geom_smooth(method = "lm")

fit <- us_change |> 
  model(
    # TSLM(Consumption ~ 1), 
    TSLM(Consumption ~ Income),
    TSLM(Consumption ~ Income + Production + Savings + Unemployment),
    TSLM(Consumption ~ Income + Savings)
  )

tidy(fit)
glance(fit)

# Coefficients
fit |> tidy()

# Summary
fit |> glance()

# Fits
fit |> augment()

fit |> 
  augment() |> 
  autoplot(Consumption) + 
  geom_line(aes(y = .fitted), colour = "steelblue")

# Report
fit |> report()

us_retail <- us_employment |> 
  filter(Month >= yearmonth("1990 Jan"), Title == "Retail Trade")

us_retail |> 
  autoplot(Employed)

fit <- us_retail |> 
  model(
    # TSLM(Employed ~ 1),
    # TSLM(Employed ~ 1 + trend()),
    # TSLM(Employed ~ 1 + season()),
    linear = TSLM(Employed ~ 1 + trend() + season()),
    piecewise = TSLM(Employed ~ 1 + trend(knots = yearmonth(c("2000 Jan", "2010 Jan"))) + season())
  )
tidy(fit)

us_retail |> 
  autoplot(Employed) + 
  geom_line(aes(y = .fitted, colour = .model), data = augment(fit))

fit |> 
  forecast(h = "1 year") |> 
  autoplot(us_retail)

library(ggdist)
fit |> 
  forecast(h = "1 year") |> 
  filter(.model == "linear") |> 
  ggplot(aes(ydist = Employed, x = Month)) + 
  stat_dist_halfeye() + 
  autolayer(tail(us_retail, 20), Employed)

fit <- us_change |> 
  model(
    TSLM(Consumption ~ Income + Savings)
  )

fit |> 
  forecast(h = "1 year")
future_us_change <- new_data(us_change, 1) |> 
  mutate(Income = mean(us_change$Income) + 1, Savings = mean(us_change$Savings) + 0.3)
fit |> 
  forecast(new_data = future_us_change) |> 
  autoplot(us_change)

fit <- us_change |> 
  model(
    TSLM(Consumption ~ lag(Income, 1) + lag(Savings, 1))
  )
tidy(fit)

future_us_change <- new_data(us_change, 1) |> 
  mutate(Income = 1, Savings = 1)

fit |> 
  forecast(future_us_change)


ansett |> 
  summarise(Passengers = sum(Passengers)) |> 
  filter_index("1989 W26" ~ "1990 W1") |> 
  autoplot(Passengers) + 
  scale_x_yearweek(date_breaks = "3 week")

fit <- ansett |> 
  summarise(Passengers = sum(Passengers)) |> 
  mutate(strike = Passengers == 0) |>  #time_in(Week, "1989 W34" ~ "1989 W40")
  model(
    TSLM(Passengers ~ trend(yearweek("1990 W1"))*strike + lag(strike))
  )

augment(fit) |> 
  autoplot(Passengers) + 
  geom_line(aes(y = .fitted), colour = "steelblue")

vic_elec |> 
  print(n = 100)

insurance |> 
  autoplot(Quotes) + 
  geom_line(aes(y = TVadverts), colour = "steelblue")

insurance |> 
  ggplot(aes(y = Quotes, x = lag(TVadverts, 1))) + 
  geom_point() + 
  geom_smooth(method = "lm")

fit <- insurance |> 
  model(
    TSLM(Quotes ~ TVadverts),
    TSLM(Quotes ~ TVadverts + lag(TVadverts, 1))
    # TSLM(Quotes ~ TVadverts + lag(TVadverts, 1) + lag(TVadverts, 2))
  )

glance(fit)
