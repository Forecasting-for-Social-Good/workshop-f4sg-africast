library(fpp3)
library(ggdist)

aus_beer <- aus_production |> select(Beer)

fit <- aus_beer |> filter_index(. ~ "2009 Q1") |> 
  model(snaive = SNAIVE(Beer),
        naive = NAIVE(Beer),
        average = MEAN(Beer))

fcst <- fit |> forecast(h = 5)
fitted_ets <- fit |> augment()

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


library(fpp3)
us_change |> 
  autoplot(Consumption)
us_change |> 
  autoplot(Income)
us_change |> 
  autoplot(vars(Consumption, Income))
ggplot() + 
  autolayer(us_change, Consumption, colour = "red") + 
  autolayer(us_change, Income, colour = "steelblue")

us_change |> 
  ggplot(aes(x = Income, y = Consumption)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# lm()
fit <- us_change |> 
  model(lm = TSLM(Consumption ~ Income))
fit
report(fit)
tidy(fit)

# Ctrl + Enter
fit_mr <- us_change |> 
  model(lm = TSLM(Consumption ~ Income + Production + Savings + Unemployment))
report(fit_mr)
tidy(fit_mr)

fit_both <- us_change |> 
  model(
    slr = TSLM(Consumption ~ Income),
    mlr = TSLM(Consumption ~ Income + Production + Savings + Unemployment)
  )

tidy(fit_both)
report(fit_both) # doesn't work because we have > 1 model

augment(fit_both)
us_change |> 
  autoplot(Consumption, colour = "orange") + 
  autolayer(augment(fit_both), .fitted)

glance(fit_both)

augment(fit_both) |> 
  features(.innov, ljung_box, lag = 16)

fit_both |> select(slr) |> gg_tsresiduals()

augment(fit_both)

# CV from glance()
glance(fit_both)


fit_select <- us_change |> 
  model(
    mlr0 = TSLM(Consumption ~ Income + Production + Savings + Unemployment),
    mlr1 = TSLM(Consumption ~ Income + Production + Unemployment),
    mlr2 = TSLM(log(Consumption) ~ Income + Production + Savings)
  )
glance(fit_select)


fit_select |> 
  select(mlr0) |> 
  forecast(h = "5 years")


fit_lagged <- us_change |> 
  model(
    mlr_lag = TSLM(Consumption ~ lag(Income) + lag(Production) + lag(Savings) + lag(Unemployment))
  )

fit_lagged |> 
  forecast(new_data(us_change) |> mutate(Income = NA, Production = NA, Savings = NA, Unemployment = NA))

fit_lagged |> 
  forecast(h = 1)