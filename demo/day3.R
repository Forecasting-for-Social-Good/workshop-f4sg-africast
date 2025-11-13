library(fpp3) # for forecast and tsibble
library(ggdist)# for visualizing distributions
library(distributional)# for working with distributions

#---------------use one model----------

# Select beer production data from 2000 to 2009
aus_beer <- aus_production |> select(Beer) |>
  filter_index("2000 Q1" ~ "2009 Q1")

# Fit a seasonal naive model to the data up to 2009 Q1
fit <- aus_beer |> filter_index(. ~ "2009 Q1") |>
  model(snaive = SNAIVE(Beer))
# Generate forecasts for the next 5 quarters
fcst <- fit |> forecast(h = 5)
# Get fitted values from the model
fitted_ets <- fit |> augment()

# Plot the forecast distribution along with fitted values and actual data
# Plot the forecast distribution along with fitted values and actual data for multiple models
ggplot(data = fcst, mapping = aes(x = Quarter, ydist = Beer))+ # Plot for multiple models
  ggdist::stat_halfeye(alpha = .4)+# Plot the forecast distribution
  geom_line(aes(y=.mean, colour ="Point Forecast"))+# Plot the point forecast line
  geom_line(aes(y = .fitted, colour ="Fitted"), data = filter_index(fitted_ets, "2005 Q1" ~ .))+# Plot the fitted values line
  geom_point(aes(y = .fitted, colour ="Fitted"), data = filter_index(fitted_ets, "2005 Q1" ~ .))+# Plot the fitted values points
  geom_line(aes(y = Beer, colour ="Data"),data = filter_index(aus_beer, "2005 Q1" ~ .))+# Plot the actual data line
  geom_point(aes(y = Beer, colour ="Data"),data = filter_index(aus_beer, "2005 Q1" ~ .))+# Plot the actual data points
  scale_color_manual(name=NULL,
                     breaks=c('Fitted', 'Data',"Point Forecast"),
                     values=c('Fitted'='#E69F00', 'Data'='#0072B2',"Point Forecast"="#000000"))+# Customize colors

  #---------------use multiple models------------------------
# Fit multiple models to the data up to 2009 Q1
fit <- aus_beer |> filter_index(. ~ "2009 Q1") |>
  model(snaive = SNAIVE(Beer),
        naive = NAIVE(Beer),
        average = MEAN(Beer),
        drift = RW(Beer ~ drift()))
# Generate forecasts for the next 5 quarters
fcst <- fit |> forecast(h = 5)
# Get fitted values from the model
fitted_ets <- fit |> augment()

# Plot the forecast distribution along with fitted values and actual data for multiple models
ggplot(data = fcst, mapping = aes(x = Quarter, ydist = Beer))+ # Plot for multiple models
  ggdist::stat_halfeye(alpha = .4)+# Plot the forecast distribution
  geom_line(aes(y=.mean, colour ="Point Forecast"))+# Plot the point forecast line
  geom_line(aes(y = .fitted, colour ="Fitted"), data = filter_index(fitted_ets, "2005 Q1" ~ .))+# Plot the fitted values line
  geom_point(aes(y = .fitted, colour ="Fitted"), data = filter_index(fitted_ets, "2005 Q1" ~ .))+# Plot the fitted values points
  geom_line(aes(y = Beer, colour ="Data"),data = filter_index(aus_beer, "2005 Q1" ~ .))+# Plot the actual data line
  geom_point(aes(y = Beer, colour ="Data"),data = filter_index(aus_beer, "2005 Q1" ~ .))+# Plot the actual data points
  scale_color_manual(name=NULL,
                     breaks=c('Fitted', 'Data',"Point Forecast"),
                     values=c('Fitted'='#E69F00', 'Data'='#0072B2',"Point Forecast"="#000000"))+# Customize colors
  facet_wrap(~.model, ncol = 1) # Facet the plot by model


# Extract prediction interval
fcst |> hilo() |> unpack_hilo() |> View()
# Extract 95% prediction interval
fcst |> hilo() |> unpack_hilo("95%") |> View()
# Extract 50% and 95% prediction intervals
fcst |> hilo(c(50,95)) |> unpack_hilo(c("95%", "50%")) |> View()
# Calculate quantiles from the forecast distribution
fcst |> mutate(q10 = quantile(Beer, 0.10),
               q50 = quantile(Beer, 0.50),
               q90 = quantile(Beer, 0.90))
# Extract variance and standard deviation from the forecast distribution
fcst |> mutate(
  var  = distributional::variance(Beer),
  sd   = sqrt(var)
)

# ---------- Try Bootstrapping -----------

# Fit a seasonal naive model to the data up to 2009 Q1
fit1 <- aus_beer |> filter_index(. ~ "2009 Q1") |>
  model(snaive = SNAIVE(Beer))

# Fit a seasonal naive model to the data up to 2009 Q1
sim <- fit1 |> generate(h = 5, times = 5, bootstrap = TRUE)
sim

# Plot the simulated paths along with actual data
aus_beer |>
  filter_index("2005 Q1" ~ .) |>
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Beer)) +
  geom_line(aes(y = .sim,
                colour = as.factor(.rep)),
            data = sim) +
  geom_point(aes(y = .sim,
                 colour = as.factor(.rep)),
             data = sim) +
  labs(title = "Beer production", y = "Beer") +
  guides(colour = "none")


# Generate bootstrap forecasts for the next 5 quarters
fc <- fit1 |> forecast(h = 5, bootstrap = TRUE)
# Plot the bootstrap forecast distribution along with actual data
autoplot(fc, aus_beer) +
  labs(title="Beer production", y="Beer" )

# Plot the bootstrap forecast paths along with actual data
fc |> hilo() |> unpack_hilo("95%") |> View()
# Calculate quantiles from the bootstrap forecast distribution
fc |> mutate(q10 = quantile(Beer, 0.10),
             q50 = quantile(Beer, 0.50),
             q90 = quantile(Beer, 0.90))


### SESSION 2 START

fit_cons <- us_change %>%
  model(lm = TSLM(Consumption ~ Income))
report(fit_cons)





# Is if Income were to increase by 1%, we expect (on average) that Consumption
# would also increase by 0.27%.

# Is if Income were to increase by 2%, we expect (on average) that Consumption
# would also increase by 0.54%.

fit <- aus_beer |>
  model(
    snaive = SNAIVE(Beer),
    mlr = TSLM(Beer ~ trend() + season())
  )

fit
aus_beer |>
  autoplot(Beer)

fit |>
  forecast(h = "2 years") |>
  autoplot(aus_beer)



fit_cons <- us_change %>%
  model(lm = TSLM(Consumption ~ Income))
fit_cons |>
  forecast(h = "2 years")

the_future_changes <- new_data(us_change, n = 16) |>
  mutate(Income = c(2, 1, 0, 3, -10, -20, rnorm(10)))

fit_cons |>
  forecast(the_future_changes) |>
  autoplot(us_change)

ansett |>
  filter(Airports == "MEL-SYD") |>
  autoplot(Passengers)
