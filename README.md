
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Tidy time series & forecasting in R

This two full-day workshop provides the basics of time series analysis
and forecasting in R. This workshop is part of the [NYR
2023](https://rstats.ai/nyr/) event, and will run in-person at [Columbia
University](Columbia%20University) on the 11-12th July 2023.

# Registration

Registration is available at <https://rstats.ai/nyr>.

Tickets to the workshop are sold separately from the conference.

## Learning objectives

Attendees will learn:

1.  How to wrangle time series data with familiar tidy tools.
2.  How to compute time series features and visualize large collections
    of time series.
3.  How to select a good forecasting algorithm for your time series.
4.  How to ensure forecasts of a large collection of time series are
    coherent.

# Preparation

The workshop will provide a quick-start overview of exploring time
series data and producing forecasts. There is no need for prior
experience in time series to get the most out of this workshop.

It is expected that you are comfortable with writing R cod and using
tidyverse packages including dplyr and ggplot2. If you are unfamiliar
with writing R code or using the tidyverse, consider working through the
learnr materials here: <https://learnr.numbat.space/>.

Some familiarity with statistical concepts such as the mean, variance,
quantiles, normal distribution, and regression would be helpful to
better understand the forecasts, although this is not strictly
necessary.

## Required equipment

Please bring your own laptop capable of running R.

## Required software

To be able to complete the exercises of this workshop, please install a
suitable IDE (such as RStudio), a recent version of R (4.1+) and the
following packages.

- **Time series packages and extensions**
  - fpp3, sugrrants
- **tidyverse packages and friends**
  - tidyverse, fpp3

The following code will install the main packages needed for the
workshop.

``` r
install.packages(c("tidyverse","fpp3", "GGally", "sugrrants"))
```

Please have the required software installed and pre-work completed
before attending the workshop.
