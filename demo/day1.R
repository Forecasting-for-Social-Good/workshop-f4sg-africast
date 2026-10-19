# ==============================================================================
# Day 1 demo: tidy time series data, graphics and autocorrelation
# ==============================================================================
# How to run this script:
#   - Ctrl + Enter runs the current line (or the selected code) in the console.
#   - Run the sections in order: later sections reuse objects created earlier.
#
# Contents:
#   1. Setup
#   2. Exploring tsibble data (filter)
#   3. Creating a tsibble from a CSV file
#   4. Summarising tsibbles over keys and time
#   5. Time plots
#   6. Seasonal plots
#   7. Seasonal subseries plots
#   8. Autocorrelation
# ==============================================================================


# 1. Setup ---------------------------------------------------------------------

# install.packages("fpp3") # Only needed once, to install the packages.

# fpp3 is a "meta-package" that loads everything needed for tidy forecasting:
#   - tsibble: the tidy time series data structure (tsibble) and its tools
#   - feasts:  time series features and graphics, e.g. gg_season() and ACF()
#   - fable:   forecasting models
#   - tidyverse packages such as dplyr (filter, mutate, ...) and ggplot2
#   It also provides example datasets such as global_economy, PBS and tourism.
library(fpp3)

# janitor provides helpers for cleaning data; here we use tabyl() to count values.
library(janitor)


# 2. Exploring tsibble data ----------------------------------------------------

# Printing a dataset shows its first rows. The header above the table describes
# the tsibble: the number of rows, the time interval (e.g. [1Y] is yearly), and
# the keys (e.g. Country) that identify each separate time series.
global_economy

# filter() keeps only the rows where the condition is TRUE.
# `==` tests for equality (a single `=` would be an assignment!).
# The pipe `|>` passes the result on the left as the first input of the function
# on the right, so we can read the code top to bottom as a sequence of steps.
# Look only at Australia with filter():
global_economy |>
  filter(Country == "Australia")


# 3. Creating a tsibble from a CSV file ----------------------------------------

# Time series data usually starts out as a plain table, e.g. a CSV file.
# To use it with fpp3 it must be converted into a tsibble. Here we:
#   1. read the file with read_csv() (`readr::` says to use the readr package),
#   2. create a quarterly time column with mutate() and yearquarter(),
#   3. drop the original `date` column with select(-date) (the minus removes it),
#   4. declare the data to be a tsibble with as_tsibble():
#        - `index` is the column that holds the time,
#        - `key` is the set of columns that together identify each time series.
# (%>% is the older magrittr pipe, and works just like |>.)
readr::read_csv("materials/data/prison_population.csv") %>%
  mutate(quarter = yearquarter(date)) |>
  select(-date) |>
  as_tsibble(
    index = quarter,
    key = c(state, gender, legal, indigenous)
  )


# 4. Summarising tsibbles over keys and time -----------------------------------

# PBS is monthly Australian Pharmaceutical Benefits Scheme data (scripts and
# costs of subsidised medicines), with many keys (Concession, Type, ATC1, ATC2).
PBS

# Keep only the ATC2 group "A10" (anti-diabetic drugs).
PBS |>
  filter(ATC2 == "A10")

# summarise() on a tsibble adds up across all keys, for each point in time.
# Here this gives total monthly cost of all A10 scripts as a single series.
PBS |>
  filter(ATC2 == "A10") |>
  summarise(Cost = sum(Cost))

# group_by() + summarise() on a tsibble keeps the groups as separate series.
# Here we get one monthly cost series per Concession type, stored as `a10`
# (`<-` assigns the result to a named object we can reuse below).
a10 <- PBS |>
  filter(ATC2 == "A10") |>
  group_by(Concession) |>
  summarise(Cost = sum(Cost))

# as_tibble() drops the time series structure, giving an ordinary table.
# Now summarise() adds up over *all* months too, so we only get one total cost
# per Concession rather than a time series.
PBS |>
  as_tibble() |>
  filter(ATC2 == "A10") |>
  group_by(Concession) |>
  summarise(Cost = sum(Cost))

# index_by() groups the rows by a coarser time period, so that summarise() can
# aggregate over time (here from monthly to quarterly data).
a10 |>
  index_by(Quarter = yearquarter(Month)) |>
  group_by(Concession) |>
  summarise(Cost = sum(Cost))

# A better approach: first create the Quarter column with mutate(), then
# index_by() it, and keep grouping by the existing Concession key.
# The result is total quarterly cost for each Concession type.
a10 |>
  mutate(Quarter = yearquarter(Month)) |>
  index_by(Quarter) |>
  group_by(Concession) |>
  summarise(Cost = sum(Cost))

# The original monthly a10 data is unchanged, since we never assigned the above.
a10


# 5. Time plots ----------------------------------------------------------------

# ansett contains weekly passenger numbers on Ansett Airlines, with keys for
# the airport pair (Airports) and the travel Class.
ansett

# autoplot() draws a time plot (a line chart of the values over time), picking
# a suitable plot for the type of object it is given.
# We first filter to a single route (Melbourne-Sydney) and class (Economy),
# then plot the Passengers column.
ansett |>
  filter(Airports == "MEL-SYD", Class == "Economy") |>
  autoplot(Passengers)

# The same route for Business class.
ansett |>
  filter(Airports == "MEL-SYD", Class == "Business") |>
  autoplot(Passengers)


# 6. Seasonal plots ------------------------------------------------------------

PBS # The dataset from earlier.

# glimpse() gives a compact overview of each column: its name, type and values.
PBS %>% glimpse()

# tabyl() counts the number of rows for each value of ATC2.
PBS %>% tabyl(ATC2)

# Total A10 cost as a time plot. Cost is divided by 1e6 (1,000,000) to show
# millions of dollars. `+ labs()` adds ggplot2 labels for the axes and title.
PBS |>
  filter(ATC2 == "A10") |>
  summarise(Cost = sum(Cost) / 1e6) |>
  autoplot(Cost) +
  labs(x = "Year", y = "$ million", title = "Antidiabetic drug sales")

# gg_season() overlays each season (e.g. each year) on the same axis, with one
# line per year, to show how the pattern within the year repeats.
# vic_elec is half-hourly electricity demand in Victoria, so it has multiple
# seasonal periods. By default the longest (a year) is shown.
vic_elec |> gg_season(Demand)

# `period` chooses which seasonal period to overlay: weekly...
vic_elec |> gg_season(Demand, period = "week")

# ...or daily.
vic_elec |> gg_season(Demand, period = "day")


# 7. Seasonal subseries plots --------------------------------------------------

# tourism is quarterly Australian domestic overnight trips, with keys for
# Region, State, Purpose (of the trip) and others.
tourism

# Total holiday trips for each State: filter to the Holiday purpose, then add
# up all regions within each state (group_by + summarise).
holidays <- tourism |>
  filter(Purpose == "Holiday") |>
  group_by(State) |>
  summarise(Trips = sum(Trips))

holidays

# A time plot of the trips for each state.
holidays |>
  autoplot(Trips) +
  labs(y = "thousands of trips", title = "Australian domestic holiday trips")

# A seasonal plot: each year is overlaid, so we can see which quarters are
# busiest for each state.
holidays |>
  gg_season(Trips) +
  labs(y = "thousands of trips", title = "Australian domestic holiday trips")

# A seasonal subseries plot: one panel per season (quarter), each showing how
# that quarter changes over the years. The blue line marks the mean of each
# season. This makes changes in the seasonal pattern easier to see.
holidays |>
  gg_subseries(Trips) +
  labs(y = "thousands of trips", title = "Australian domestic holiday trips")


# 8. Autocorrelation -----------------------------------------------------------

# Autocorrelation measures how related a time series is to its own past values
# (its "lags"). For example, lag 4 compares each quarter with the same quarter
# one year earlier.

# year() extracts the year from the Quarter column, so this keeps data from
# 1992 onwards. aus_production is quarterly Australian production of beer,
# bricks, electricity and more.
new_production <- aus_production |>
  filter(year(Quarter) >= 1992)
new_production

# ACF() computes the autocorrelation function of Beer production, for lags
# 1 to 9 (`lag_max` sets the largest lag).
new_production |>
  ACF(Beer, lag_max = 9)

# autoplot() draws the ACF as a correlogram: one bar per lag. Bars that extend
# beyond the dashed blue lines are significantly different from zero.
# Peaks at lags 4 and 8 show the yearly seasonality (the data is quarterly).
new_production |>
  ACF(Beer, lag_max = 9) |>
  autoplot()

# Without `lag_max`, a default number of lags is used, showing more of the
# seasonal pattern.
new_production |>
  ACF(Beer) |>
  autoplot()

# Trend and seasonality in ACF plots:
# A trend makes the ACF decay slowly (autocorrelations stay positive and large
# for many lags), while seasonality creates peaks at multiples of the seasonal
# period. Holiday trips have both, so we see both features here.
holidays |>
  ACF(Trips) |>
  autoplot()
