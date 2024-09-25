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


