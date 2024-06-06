
<!-- README.md is generated from README.Rmd. Please edit that file -->

# OpeNoise: noise pollution data analysis <img src="man/figures/hex-OpeNoise.png" height="200" style="float:right; height:100px;"/>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/OpeNoise)](https://cran.r-project.org/package=OpeNoise)![](http://cranlogs.r-pkg.org/badges/grand-total/OpeNoise)
<!-- badges: end -->

### Introdution

The tutorial explains how to use the OpeNoise library. It works on
acoustic data acquired with sound level meters instrument. Input dataset
format is showed in internal examples that you can access them with the
*data()* function.

``` r
library(OpeNoise)

data("PTFA")

head(PTFA)[1:3, 1:6]
#>                  date LAeq LZFmin.6.3 LZFmin.8.0 LZFmin.10.0 LZFmin.12.5
#> 2 2022-03-07 10:12:16 43.9       31.6       34.7        59.9        74.1
#> 3 2022-03-07 10:12:17 44.6       33.7       43.9        59.6        58.9
#> 4 2022-03-07 10:12:18 44.5       46.0       52.2        51.3        57.2
```

### Acoustic summary calculation:

#### Energetic average

Function calculate energetic average of vector of values in dB.
*RoundTo* function round value at 0.5.

``` r
energetic.mean(PTFA$LAeq)

x <- energetic.mean(PTFA$LAeq)

RoundTo(x, 0.5)
```

#### Energetic average weighted

Function calculate energetic average weighted of vector’s values in dB
respect to vector’s time like string in format “HH:MM:SS”

``` r
energetic_w.mean(c(55.2, 88.6), c("03:22:52", "08:55:33"))
```

#### Acoustic percentile

Function return reverse percentile of un vector’s values.

``` r
AcuPercentile(PTFA$LAeq)

RoundTo(AcuPercentile(PTFA$LAeq), 0.5)
```

#### Day and night acoustic percentiles calculate

``` r
data("exampleHourlyData")

AcuDNPercentile(df = exampleHourlyData,
                parameter = "leq",
                from = "5",
                to = "22",
                period = "night")
```

#### Energetic hourly average

Function return energetic average with hourly aggregation.

``` r
HourlyEmean(PTFA, "LAeq", timeZone = "Europe/Rome")
```

#### Time decomposition

function retun seconds from hour, minutes and seconds.

``` r
hour <- 5
minute <- 25
second <- 50
deco.time(hour, minute, second)
```

#### holidays date (Gregorian calendar)

This is simple function using Gauss’algorithm to return holiday date
respect Gregorian calendar.

``` r
HolidaysDate(2022)
```

#### Average day/night period (06:00/21:00 - 22:00/05:00)

Function return energetic average or simple average with aggregation day
(06:00/21:00) or night (22:00/05:00).

``` r
data("exampleHourlyData")

df_night <- avr.day.night(exampleHourlyData, variable = "leq", period = "night", 
              stat = "e_mean")

head(df_night, 5)

df_day <- avr.day.night(exampleHourlyData, variable = "leq", period = "day", 
              stat = "e_mean")

head(df_day, 5)
```

#### Lden calculation

This function return energetic average aggregate:

- *D_acu* (day 06:00/21:00)
- *D* (day 06:00/20:00)
- *E* (Evening 20:00/22:00)
- *N* (Night 22:00/06:00)
- *Lden* (is the level of noise day-evening-night and is an indicator
  correlated with the global nuisance produced by noise over the overall
  24 hours)

``` r
data("exampleHourlyData")

LdenCalculator(dataframe = exampleHourlyData, variable = "leq", type = "daily")

LdenCalculator(dataframe = exampleHourlyData, variable = "leq", type = "total")
```

#### dbsum

Function calculate energetic sum or difference of values

``` r
dbsum(x = 55, y = 33, operator = 1)
dbsum(x = c(55 , 66), y = c(45, 50), operator = 1)

dbsum(x = c(55 , 66), y = c(70, 68), operator = -1)
```

#### SELcalc

Function calculate SEL (single envent level)

``` r
SELcalc(x = 66.8, t = 938)
```

### Plot functions (time history and Running Leq, spectrogram, quantile plot)

``` r
PlotNoiseTimeHistory(df = PTFA, mp = "PTFA", y_lim = c(40, 60))
```

<img src="man/figures/plot_image/README-TH-1.png" width="100%" />

*PlotNoiseTHcompare* function shows Leq’s time history with frequency
components

``` r
PlotNoiseTHcompare(df = PTFA, 
                   variable = "LAeq", 
                   listvar = c("LZFmin.100",
                               "LZFmin.40.0"),
                   mp = "PTFA", 
                   runleq = FALSE)
```

<img src="man/figures/plot_image/README-TH_compare-1.png" width="100%" />

``` r
PlotSpectrogram(PTFA, coLs = c(3:38), plot_title = "Spectrogram")
```

<img src="man/figures/plot_image/README-spectrogram-1.png" width="100%" />

*AcousticQuantilePlot* function plot acoustic quantile aggregate by hour

``` r
library(lubridate)

datasetI <- dataset_impulsive1
datasetH <- dfImpulsiveTrasform(datasetI)
datasetH$date <- ymd_hms(as.character(datasetH$date))

AcousticQuantilePlot(df = datasetH, Cols =c(3:38), Quantile =0.95,
                     TimeZone = "UTC")
```

<img src="man/figures/plot_image/README-AcousticQuantilePlot-1.png" width="100%" />

### Search tone

This function search tonal components in acoustic measure

``` r
search.tone(PTFA[, c(3:38)], statistic = energetic.mean, plot.tone = T)
```

<img src="man/figures/plot_image/README-search_tone-1.png" width="100%" />

### Impulsive finder

This function search impulsive events in acoustic measure

``` r
data("dataset_impulsive2")
results <- searchImpulse(dataset_impulsive2)
results$dfPeaks
#>    ymax xmax startPeak stopPeak                date cri1 cri2
#> 1  97.2 2795      2794     2818 2022-05-06 14:30:54    y    5
#> 2  96.3 2595      2594     2622 2022-05-06 14:30:34    y    5
#> 3  96.1 1570      1564     1591 2022-05-06 14:28:51    y    4
#> 4  91.1 1978      1977     2000 2022-05-06 14:29:32    y    5
#> 5  91.0 1773      1771     1794 2022-05-06 14:29:11    y    5
#> 6  90.9 2401      2397     2420 2022-05-06 14:30:14    y    4
#> 7  90.2 1159      1158     1183 2022-05-06 14:28:10    y    5
#> 8  90.2 1389      1387     1410 2022-05-06 14:28:33    y    5
#> 9  88.8 2198      2197     2218 2022-05-06 14:29:54    y    4
#> 10 86.7  939       936      959 2022-05-06 14:27:48    y    5
```

#### Transform dataset from 100 ms data acquisition to 1 s data acquisition

``` r
data("dataset_impulsive2")
head(dataset_impulsive2, 3)[, 1:5]
#>                      date LAeq LASmax  LAF LAFmax
#> 2 2022-05-06 14:26:14.600 34.8   40.4 34.5   34.8
#> 3 2022-05-06 14:26:14.700 35.0   40.1 34.6   35.1
#> 4 2022-05-06 14:26:14.800 37.0   39.8 35.7   36.2
```

``` r
dfT <- dfImpulsiveTrasform(dfImpulsive = dataset_impulsive2, 
                           statistic = energetic.mean)
head(dfT, 3)[, 1:5]
#>                    date LAeq LZeq.6.3 LZeq.8.0 LZeq.10.0
#> 1 2022-05-06 14:26:14.6 34.8     29.4     37.5      43.7
#> 2 2022-05-06 14:26:14.7 35.0     31.5     34.9      38.5
#> 3 2022-05-06 14:26:14.8 37.0     28.2     37.1      35.2
```

### Calculation of the intrusiveness index

``` r
library(OpeNoise)
library(lubridate)

data("dataset_impulsive1")
data("dfBW")

# dataset handling
df_Imp_sec <- dfImpulsiveTrasform(dataset_impulsive1,
                                  statistic = energetic.mean)
df_Imp_sec$date <- ymd_hms(df_Imp_sec$date, tz = "Europe/Rome")

# extraction of frequency bands from the dataset
freqDF <- df_Imp_sec[, grep("LZeq\\.", names(df_Imp_sec))]

################################################################################

#                  INTRUSIVENESS INDEX CALCULATION FUNCTION

################################################################################
dfa <- freqDF # Environmental dataset simulation
dfr <- freqDF

# Residual dataset simulation by subtracting 4 from dfa
dfr[c(5,8,12,15), ] <- dfr[c(5,8,12,15), ] - 4

BW <- dfBW$BW # bandwidth

# application of the function
IntrusiveIndex(dfa, dfr, BW)
```
