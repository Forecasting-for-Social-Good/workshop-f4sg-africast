library(fpp3)
# Recap from Day 3
aus_tourism_purpose <- tourism|> group_by(Purpose) |> 
  summarise(Trips =sum(Trips))

fit <- aus_tourism_purpose |> 
  model(average = MEAN(Trips),
        naive = NAIVE(Trips),
        seasonal_naive = SNAIVE(Trips),
        Regression = TSLM(Trips ~ trend() + season()))

fit |> glance()
fit |> tidy()
fit |> select(Regression) |> filter(Purpose == "Holiday") |> report()

fcst <- fit |> forecast(h = "2 years")
fcst <- fit |> forecast(h = 24)

fcst |> autoplot(level = NULL)

fcst |> hilo(level = 90) |> unpack_hilo("90%")

library(ggdist)
fcst |> ggplot(aes(x = Quarter, ydist =Trips))+
  stat_halfeye()+
  facet_wrap(vars(Purpose))+
  labs(x = "Quarter", y="Trips", title = " Forecast distributions of trips")+
  ggthemes::theme_clean()

#------------------------------- ETS---------------------------------------------------

fit <- global_economy |> filter(Country == "Australia") |> 
  model(ses = ETS(Exports ~ error("A") + trend("N") + season("N")))

fit |> glance()
fit |> tidy()
fit |> report()

fit |> forecast(h=10) |> autoplot()

##

aus_economy <- global_economy |> filter(Country == "Australia") |>
  mutate(Pop = Population / 1e6)
autoplot(aus_economy)
fit <- aus_economy |> model(ETS(Pop))

report(fit)
fit |> components() |> autoplot()

fit |> forecast(h=10) |> autoplot(aus_economy)

## Holidays
holiday <- tourism |> filter(Purpose == "Holiday")

fit <- holiday |> model(ets = ETS(Trips))

fit |> filter(Region == "Snowy Mountains") |> report()

fit |> filter(Region == "Snowy Mountains") |> components() |> autoplot()

##
cafe_vic <- tsibbledata::aus_retail |> 
  filter(State == "Victoria",
         Industry == "Cafes, restaurants and catering services"
         ) |> select(Month,Turnover)

#how to forecast with transformations
cafe_vic |> autoplot(box_cox(Turnover, lambda = .2))

#fit <- cafe_vic |> model(ets = ETS(Turnover))

fit <- cafe_vic |> model(ets = ETS(box_cox(Turnover, lambda = .2)))
fct <- fit |> forecast(h = 12)
fct |> autoplot(filter_index(cafe_vic, "2016 Jan" ~ .))

# Generate function to produce forecasts using bootstrapping

simulation <- fit |> generate(h= 12, times = 1000, bootstrap = TRUE)

cafe_vic |> filter_index("2016 Jan" ~ .) |> 
  ggplot(aes(x = Month))+
  geom_line(aes(y =Turnover))+
  geom_line(aes( y = .sim, colour = as.factor(.rep)), data = simulation)+
  guides(col = FALSE)

fct_boot <- fit |> forecast(h= 12, bootstrap = TRUE)

fct_boot |> hilo(level = 90)

fct_boot |> 
ggplot(aes(x = Month, ydist =Turnover))+
  stat_halfeye()+
  autolayer(cafe_vic |> filter_index("2018 Jan" ~ .))

## ------------------------ARIMA -----------------------------------
egy_economy <- global_economy |> filter(Code == "EGY") 
egy_economy |> autoplot(Exports)

fit <- egy_economy |> model(arima = ARIMA(Exports))

report(fit)

fit |> forecast(h=5) |> autoplot(egy_economy)

###

h02 <- PBS |> filter(ATC2 == "H02") |> 
  summarise(Cost = sum(Cost)/ 1e6)
h02 |> autoplot(Cost)

h02 |> autoplot(log(Cost) |> difference(12) |> 
                  difference(1))

#stationairy
h02 |> features(Cost, unitroot_kpss)

h02 |> features(Cost, unitroot_nsdiffs)

##
h02 |> ACF(Cost) |> autoplot()
h02 |> PACF(Cost) |> autoplot()

fit <- h02 |> model(arima= ARIMA(log(Cost)))
report(fit)
fit|> 
  forecast(h= "3 years")


## ------------------------ ensemble ----------------------------------
aus_tourism <- tourism |> index_by(Quarter) |> summarise(Trips = sum(Trips))

fit <- aus_tourism |> model(
  snaive = SNAIVE(Trips),
  ets= ETS(Trips),
  arima = ARIMA(Trips),
  regression = TSLM(Trips ~ trend() + season()),
) |> 
  mutate(combination = (regression+snaive+ets+arima)/4)

fcst <- fit |> forecast(h=12) 

fcst|> autoplot(aus_tourism)
