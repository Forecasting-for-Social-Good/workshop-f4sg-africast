library(fpp3)
food <- aus_retail |>
  filter(Industry == "Food retailing") |>
  summarise(Turnover = sum(Turnover))

food |> 
  autoplot(Turnover)

food |> 
  autoplot(sqrt(Turnover))
# need stronger transformation

food |> 
  autoplot(log(Turnover))

?log

food |> 
  autoplot(log10(Turnover))

food |> 
  autoplot(box_cox(Turnover, 0.5))

# lambda < 0.5 (stronger transformation needed)
# lambda = 0.5 (just right - keep it)
# lambda > 0.5 (transformation was too strong)

food |> 
  autoplot(box_cox(Turnover, 0.05))

food |> 
  features(Turnover, guerrero)


food |> 
  autoplot(box_cox(Turnover, 0.0895))

food |> 
  autoplot(box_cox(Turnover, guerrero(Turnover))) + 
  ylab("Box-Cox transformed Turnover (lambda = 0.895")

food |> 
  autoplot(box_cox(Turnover, 0.09))

food |> 
  mutate(
    Turnover_bc = box_cox(Turnover, guerrero(Turnover))
  )

food |> 
  autoplot(log(Turnover))
food |> 
  model(stl = STL(log(Turnover))) |> 
  components() |> 
  autoplot()

food |> 
  gg_season(Turnover)

food |> 
  model(stl = STL(log(Turnover))) |> 
  components() |> 
  gg_season(season_year)

food |> 
  model(stl = STL(log(Turnover))) |> 
  components() |> 
  as_tsibble() |> 
  autoplot(season_adjust)


food |> 
  model(stl = STL(log(Turnover) ~ trend(window = 9) + season(window = 13))) |> 
  components() |> 
  autoplot()

lm(y ~ x)