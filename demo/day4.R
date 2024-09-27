library(fpp3)


aus_beer <- aus_production |> select(Beer)

# ETS with one series
fit_beer_ets <- aus_beer |> 
  model(exponential_smmothing = ETS(Beer),
        exponential_smmothing1 = ETS(Beer ~ error("A") + trend("N", alpha = 0.1) + season("N")),
        snaive = SNAIVE(Beer),
        average = MEAN(Beer))

report(fit_beer)
glance(fit_beer)
tidy(fit_beer)
components(fit_beer)

fcst_beer_ets <- fit_beer |> forecast(h= 12)

# ETS with many series series
tourism_fit <- tourism |> 
  model(exponential_smmothing = ETS(Trips),
        exponential_smmothing1 = ETS(Trips ~ error("A") + trend("N", alpha = 0.1) + season("N")),
        snaive = SNAIVE(Trips),
        average = MEAN(Trips))

fcst_tourism_ets <- tourism_fit |> forecast(h= 12)

# ARIMA
fit_beer_arima <- aus_beer |> model(arima = ARIMA(Beer),
                                    arima1 = ARIMA(Beer ~ pdq(1,1,0)+PDQ(0,0,0)))


report(fit_beer_arima)
glance(fit_beer_arima)
tidy(fit_beer_arima)

fcst_beer_ets <- fit_beer_arima |> 
  forecast(h= 12)

# Look at ACF and PACF to decide p,q, P,Q
aus_beer |> ACF(Beer) |> autoplot()
aus_beer |> PACF(Beer) |> autoplot()



# include all models covered so far

fit_beer_ets <- aus_beer |> 
  model(exponential_smmothing = ETS(Beer),
        snaive = SNAIVE(Beer),
        average = MEAN(Beer),
        naive = NAIVE(Beer), 
        arima = ARIMA(Beer),
        regression = TSLM(Beer))

fcst_beer <- fit_beer_ets |> forecast(h=12)
report(fit_beer_ets)
glance(fit_beer_ets)
tidy(fit_beer_ets)



