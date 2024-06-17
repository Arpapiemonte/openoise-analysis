## In the same source file (to remind you that you did it) add:
if(getRversion() >= "2.15.1")  utils::globalVariables(
  c("value", "name", "periodo", "ora", "freq", "llmin", "iso", "Hz", "LLfmin",
    "Lf", "date2", "LAFmax", "Component (Hz)")
  )

#' Noise data of misure in house open window condition
#'
#' @name PTFA
#' @docType data
#' @author Pasquale Scordino \email{p.scordino@@arpa.piemonte.it}
#' @author Simone Sperotto \email{s.sperotto@@arpa.piemonte.it}
#' @keywords data
NULL

#' Noise data of misure in house close window condition
#'
#' @name PTFC
#' @docType data
#' @author Pasquale Scordino \email{p.scordino@@arpa.piemonte.it}
#' @author Simone Sperotto \email{s.sperotto@@arpa.piemonte.it}
#' @keywords data
NULL

#' Noise data of misure in house open window condition
#'
#' @name P1FA
#' @docType data
#' @author Pasquale Scordino \email{p.scordino@@arpa.piemonte.it}
#' @author Simone Sperotto \email{s.sperotto@@arpa.piemonte.it}
#' @keywords data
NULL

#' Noise data of misure in house close window condition
#'
#' @name P1FC
#' @docType data
#' @author Pasquale Scordino \email{p.scordino@@arpa.piemonte.it}
#' @author Simone Sperotto \email{s.sperotto@@arpa.piemonte.it}
#' @keywords data
NULL

#' Noise hourly data of misure in environmental open space
#'
#' @name exampleHourlyData
#' @docType data
#' @author Pasquale Scordino \email{p.scordino@@arpa.piemonte.it}
#' @author Simone Sperotto \email{s.sperotto@@arpa.piemonte.it}
#' @keywords data
NULL

#' Dataset with markers
#'
#' @name markers
#' @docType data
#' @author Pasquale Scordino \email{p.scordino@@arpa.piemonte.it}
#' @author Simone Sperotto \email{s.sperotto@@arpa.piemonte.it}
#' @keywords data
NULL

#' Parameters table of equal loudness curve A (ISO 226:1987 “Acoustics -- Normal equal-loudness-level contours”)
#'
#' @name iso
#' @docType data
#' @author Pasquale Scordino \email{p.scordino@@arpa.piemonte.it}
#' @author Simone Sperotto \email{s.sperotto@@arpa.piemonte.it}
#' @keywords data
NULL

#' Noise dataset of impulsive event (100 ms acquisition time)
#'
#' @name dataset_impulsive1
#' @docType data
#' @author Pasquale Scordino \email{p.scordino@@arpa.piemonte.it}
#' @author Simone Sperotto \email{s.sperotto@@arpa.piemonte.it}
#' @keywords data
NULL

#' Noise dataset of impulsive event (100 ms acquisition time)
#'
#' @name dataset_impulsive2
#' @docType data
#' @author Pasquale Scordino \email{p.scordino@@arpa.piemonte.it}
#' @author Simone Sperotto \email{s.sperotto@@arpa.piemonte.it}
#' @keywords data
NULL

#' Table's 1/3 octave bandwidth
#'
#' @name dfBW
#' @docType data
#' @author Pasquale Scordino \email{p.scordino@@arpa.piemonte.it}
#' @author Simone Sperotto \email{s.sperotto@@arpa.piemonte.it}
#' @keywords data
NULL

#' Weighting acoustic table
#'
#' @name AcousticWeightingTable
#' @docType data
#' @author Pasquale Scordino \email{p.scordino@@arpa.piemonte.it}
#' @author Simone Sperotto \email{s.sperotto@@arpa.piemonte.it}
#' @keywords data
NULL

#' Function that calculate min value
#'
#' calculate min value
#' @param y is a numeric vector
#' @author Pasquale Scordino \email{p.scordino@@arpa.piemonte.it}
#' @author Simone Sperotto \email{s.sperotto@@arpa.piemonte.it}
#' @export
energetic.min <- function(y) {
  r <- min(y, na.rm = TRUE)
  return(r)
}

#' Function research pure tone
#'
#' research pure tone
#' @param x is a dataframe with llfmin...
#' @param statistic is statistic used default is energetic.mean
#' @param plot.tone is logic argument default is false don't plot result
#' @return plot of 1/3 octave frequency and isofonic curve A (ISO 226:1987)
#' @author Pasquale Scordino \email{p.scordino@@arpa.piemonte.it}
#' @author Simone Sperotto \email{s.sperotto@@arpa.piemonte.it}
#' @example inst/examples/search.tone_ex.R
#' @import ggplot2
#' @export
search.tone <- function(x, statistic = energetic.mean, plot.tone = FALSE) {

  if (length(names(x)[1:dim(x)[2]]) == 31) {
    names(x)[1:dim(x)[2]] <- c(
      "20",
      "25",
      "31.5",
      "40",
      "50",
      "63",
      "80",
      "100",
      "125",
      "160",
      "200",
      "250",
      "315",
      "400",
      "500",
      "630",
      "800",
      "1k",
      "1.25k",
      "1.6k",
      "2k",
      "2.5k",
      "3.15k",
      "4k",
      "5k",
      "6.3k",
      "8k",
      "10k",
      "12.5k",
      "16k",
      "20k"
    )
  } else {
    names(x)[1:dim(x)[2]] <- c(
      "6.3",
      "8",
      "10",
      "12.5",
      "16",
      "20",
      "25",
      "31.5",
      "40",
      "50",
      "63",
      "80",
      "100",
      "125",
      "160",
      "200",
      "250",
      "315",
      "400",
      "500",
      "630",
      "800",
      "1k",
      "1.25k",
      "1.6k",
      "2k",
      "2.5k",
      "3.15k",
      "4k",
      "5k",
      "6.3k",
      "8k",
      "10k",
      "12.5k",
      "16k",
      "20k"
    )
  }

  # I select the dataframe without the date column
  x <- x[, 1:dim(x)[2]]

  # I use the gather function to reverse the table from wide to long
  xlong  <- tidyr::gather(x, freq, llmin)
  Tab.min <- as.data.frame(as.table(tapply(
    xlong$llmin,
    xlong$freq, statistic
  )))
  # I name the columns of the new table
  names(Tab.min) <- c("Hz", "LLfmin")
  Tab.min$Hz  <- as.factor(Tab.min$Hz)

  if (length(names(x)[1:dim(x)[2]]) == 31) {
    Tab.min$Hz <- factor(Tab.min$Hz, levels = c(
      "20",
      "25",
      "31.5",
      "40",
      "50",
      "63",
      "80",
      "100",
      "125",
      "160",
      "200",
      "250",
      "315",
      "400",
      "500",
      "630",
      "800",
      "1k",
      "1.25k",
      "1.6k",
      "2k",
      "2.5k",
      "3.15k",
      "4k",
      "5k",
      "6.3k",
      "8k",
      "10k",
      "12.5k",
      "16k",
      "20k"
    ))
  } else {
    Tab.min$Hz <- factor(Tab.min$Hz, levels = c(
      "6.3",
      "8",
      "10",
      "12.5",
      "16",
      "20",
      "25",
      "31.5",
      "40",
      "50",
      "63",
      "80",
      "100",
      "125",
      "160",
      "200",
      "250",
      "315",
      "400",
      "500",
      "630",
      "800",
      "1k",
      "1.25k",
      "1.6k",
      "2k",
      "2.5k",
      "3.15k",
      "4k",
      "5k",
      "6.3k",
      "8k",
      "10k",
      "12.5k",
      "16k",
      "20k"
    ))
  }

  Tab.min <- with(Tab.min, Tab.min[order(Hz, LLfmin),])

  # Dataset manipulation
  Tab.min$LLfmin <-
    suppressWarnings(as.numeric(as.character(Tab.min$LLfmin)))

  if (length(names(x)[1:dim(x)[2]]) == 31) {
    isof <- iso[-c(1:5), ]
  } else {
    isof <- iso
  }

  isof$Hz <- Tab.min$Hz
  isof$LLfmin <- signif(x = Tab.min$LLfmin, digits = 3)
  isof$afC <- (isof$af) * (isof$LLfmin - isof$Tf)
  isof$bfC <- (isof$bf) * (isof$LLfmin - isof$Tf)
  isof$Ln <- (isof$afC) / (1 + isof$bfC) + 4.2
  isof$Ln1 <- max(isof$Ln, na.rm = TRUE)
  isof$A <- (isof$Ln1) - 4.2
  isof$Lf <-
    signif(x = (isof$A) / (isof$af - (isof$A) * (isof$bf)) + isof$Tf,
           digits = 3)
  isof <- as.data.frame(isof)

  # Pure Tone Search
  A <- (length(isof$Hz[(which(isof$LLfmin >= isof$Lf))]) == 1)
  B <-
    (isof$Hz[(which(isof$LLfmin >= isof$Lf))] == isof$Hz[(which(diff(isof$LLfmin) >=
                                                                  5) + 1)])
  C <-
    (isof$Hz[(which(isof$LLfmin >= isof$Lf))] == isof$Hz[(which(diff(isof$LLfmin) <=
                                                                  -5))])

  if (A & A %in% B & A %in% C) {
    tonHz <-
      paste(isof$Hz[(which(isof$LLfmin >= isof$Lf))], "Hz", sep = "")
  } else {
    tonHz <- "NA"
  }

  # LLfmin Level of Tone
  if (tonHz == "NA") {
    LevHz <- "NA"
  } else {
    LevHz <- paste(isof$LLfmin[(which(isof$LLfmin >= isof$Lf))],
                   "dB", sep = " ")
  }

  # Isophonic value
  if (tonHz == "NA") {
    LevIsof <- "NA"
  } else {
    LevIsof <- paste(signif(max(isof$Ln, na.rm = TRUE), digits = 3),
                     "phon", sep = " ")
  }

  # Graph: copy to workbook and show on device
  if (plot.tone == TRUE) {
    sp <- ggplot(data = isof) +
      geom_bar(stat="identity", aes(x = Hz, y = LLfmin),
               colour = "darkcyan",  fill = "lightblue") +
      geom_line(aes(x = as.numeric(Hz), y = Lf),  colour = "darkorange4") +
      ylab("Sound Level (dB)") +
      xlab("Frequency (Hz)") +
      labs(title = "High Isophonic Chart vs LLFmin Spectrum - Free Field",
           subtitle = paste("Tone frequency:", tonHz,"| Tone level:", LevHz,
                            "| Isophonic:", LevIsof, sep = " "),
           caption = "") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90))

    return(sp)

  } else {
    if (plot.tone == FALSE) {
      string.result <- c(
        paste("Tone frequency:", tonHz, sep = " "),
        paste("Tone level:", LevHz, sep = " "),
        paste("Isophonic:", LevIsof, sep = " ")
      )
      return(string.result)
    }
  }
}

#' Time decomposition
#'
#' Trasform time from hours, minutes and seconds to seconds
#' @param x  are hours
#' @param y  are minutes
#' @param z  are seconds
#' @return time decomposition in seconds
#' @author Pasquale Scordino \email{p.scordino@@arpa.piemonte.it}
#' @author Simone Sperotto \email{s.sperotto@@arpa.piemonte.it}
#' @example inst/examples/deco.time_ex.R
#' @export
deco.time <-function(x,y,z) {
  H <- x*60*60
  M <- y*60
  S <- z
  print("Time decomposition from hours, minutes and seconds to seconds:")
  res <- H+M+S
  return(res)
}

#' Logarithmic  mean
#'
#' Calculate logarithmic mean
#' @param x  is a vector of value in decibel (dB)
#' @return logarithmic mean
#' @author Pasquale Scordino \email{p.scordino@@arpa.piemonte.it}
#' @author Simone Sperotto \email{s.sperotto@@arpa.piemonte.it}
#' @example inst/examples/energetic.mean_ex.R
#' @export
energetic.mean <- function(x) {
  x <- x[!is.na(x)]
  li <- (1 / length(x))
  s <- sum(10 ^ (x / 10))
  return(round(10 * (log10(li * s)), digits = 1))
}

#' Weigth logaritmic  mean
#'
#' Calculate weigth logarithmic mean respect to time
#' @param x  is a vector of value in decibel (dB)
#' @param t  is a vector of time string "HH:MM:SS"
#' @return weigth logarithmic mean  respect to time
#' @author Pasquale Scordino \email{p.scordino@@arpa.piemonte.it}
#' @author Simone Sperotto \email{s.sperotto@@arpa.piemonte.it}
#' @example inst/examples/energetic_w.mean_ex.R
#' @export
energetic_w.mean <- function(x, t) {
  Index.non.accoppiati <- which((is.na(x) == is.na(t)) == FALSE)
  x[Index.non.accoppiati] <- NA
  t[Index.non.accoppiati] <- NA
  x <- x[!is.na(x)]
  t <- t[!is.na(t)]
  Res <- as.POSIXlt(paste(Sys.Date(), t))
  tm  <- (((Res$hour * 60) + Res$min) * 60) + Res$sec
  s <- sum((10 ^ (x / 10)) * tm)
  return(round(10 * (log10(s / sum(tm))), digits = 1))
}

#' Round to Multiple
#'
#' Returns a number rounded to the nearest specified multiple.
#' @param x  is a vector of value in decibel (dB)
#' @param multiple  is a vector of time in seconds
#' @param FUN the rounding function as character or as expression. Can be one out of trunc, ceiling, round (default) or floor.
#' @author Andri Signorell \email{andri@signorell.net}
#' @example inst/examples/RoundTo_ex.R
#' @export
RoundTo <- function (x, multiple = 1, FUN = round)
{
  if (is.function(FUN)) {
    fct <- FUN
    FUN <- "fct"
    FUN <- gettextf("%s", FUN)
  }
  return(eval(parse(text = gettextf("%s(x/multiple) * multiple",
                                    FUN))))
}

#' Calculate running Leq
#'
#' Returns a vector of energetic mean of Leq......
#' @param x  is a vector of value in decibel (dB)
#' @author Pasquale Scordino \email{p.scordino@@arpa.piemonte.it}
#' @author Simone Sperotto \email{s.sperotto@@arpa.piemonte.it}
#' @example inst/examples/runningLeq_ex.R
#' @export
runningLeq <- function(x) {
  id <- NULL
  n <- 1
  while(n <= length(x)){
    id[n] <- energetic.mean(x[1:n])
    n <- n + 1
  }
  return(id)
}

#' Extract index and name of markers
#'
#' Returns a list of index and name
#' @param filemarks  is a dataframe with date and markers
#' @param mp  is a name of misure point
#' @param dataset is dataframe in analysis
#' @author Pasquale Scordino \email{p.scordino@@arpa.piemonte.it}
#' @author Simone Sperotto \email{s.sperotto@@arpa.piemonte.it}
#' @example inst/examples/ExtractIndexMark_ex.R
#' @export
ExtractIndexMark <- function(filemarks, dataset, mp) {
  listaIndiciMarcatori <- list()
  marcatori_pm <- subset(filemarks, filemarks[, 1] %in% mp)

  for (i in 1:length(marcatori_pm$inizio)) {
    listaIndiciMarcatori[[i]] <- which(dataset$date %in%
                                         seq.POSIXt(marcatori_pm[, c(2:4)][i,1],
                                                    marcatori_pm[, c(2:4)][i,2],
                                                    by = "sec"))
  }

  listaNomiMarcatori <- list()
  for (j in 1:length(marcatori_pm[, 4])) {
    listaNomiMarcatori[[j]] <- rep(marcatori_pm[, 4][j],
                                   length(listaIndiciMarcatori[[j]]))
  }

  result <- do.call("c", list(index = listaIndiciMarcatori,
                              name = listaNomiMarcatori))
  return(result)
}

#' Add index and name of markers in misure dataframe
#'
#' Returns a dataframe
#' @param filemarks  is a dataframe with date and markers
#' @param mp  is a name of misure point
#' @param dataset is dataframe in analysis
#' @author Pasquale Scordino \email{p.scordino@@arpa.piemonte.it}
#' @author Simone Sperotto \email{s.sperotto@@arpa.piemonte.it}
#' @example inst/examples/Maskapply_ex.R
#' @export
Maskapply <- function(filemarks, dataset, mp) {
  df <- dataset
  IndexNameMark <- ExtractIndexMark(filemarks, dataset, mp)
  df$marker <- NA
  for (n in 1:((length(IndexNameMark))/2)) {
    df$marker[IndexNameMark[[n]]] <-
      IndexNameMark[[((length(IndexNameMark))/2) + n]]
  }
  return(df)
}

#' Plot time history of noise misure with marker and running Leq
#'
#' Returns a time history plot
#' @param df  is a dataframe with date, leq and markers
#' @param variable is a string name of column you want plot
#' @param filemarks  is a dataframe with date and markers
#' @param escl_marks is mark that you want esclude in plot
#' @param mp  is a name of misure point
#' @param y_lim y axes range
#' @author Pasquale Scordino \email{p.scordino@@arpa.piemonte.it}
#' @author Simone Sperotto \email{s.sperotto@@arpa.piemonte.it}
#' @example inst/examples/PlotNoiseTimeHistory_ex.R
#' @import ggplot2
#' @export
PlotNoiseTimeHistory <- function (df = NULL, variable = NULL, filemarks = NULL,
                                  escl_marks = NULL, mp,  y_lim = c(20, 80))
{
  if (!missing(filemarks)) {
    IndexNameMark <- ExtractIndexMark(filemarks, df, mp)
    a <- Maskapply(filemarks, df, mp)
    start <- NULL
    stop <- NULL
    marker <- NULL
    for (s in 1:(length(names(IndexNameMark))/2)) {
      start[s] <- IndexNameMark[[s]][1]
      stop[s] <- IndexNameMark[[s]][length(IndexNameMark[[s]])]
      marker[s] <- IndexNameMark[[(length(names(IndexNameMark))/2) +
                                    s]][1]
    }
    dateRanges <- data.frame(start = a$date[start], stop = a$date[stop],
                             marker = marker)
    dateRanges$marker <- factor(dateRanges$marker,
                                levels = unique(dateRanges$marker[1:length(dateRanges$marker)]))

    p <- ggplot(a, aes(x = date)) +
      geom_line(aes(y = .data[[variable]])) +
      geom_line(aes(y = runningLeq(.data[[variable]])), col = "blue") +
      geom_rect(data = dateRanges, aes(xmin = start, xmax = stop,
                                       ymin = -Inf, ymax = Inf, colour = marker),
                inherit.aes = FALSE,
                alpha = 0.08, fill = c("lightgreen")) +
      ylim(y_lim) +
      ylab("dB(A)") +
      ggtitle(paste("Time history and running Leq - ",
                    mp)) +
      theme_minimal()
    return(p)
  } else {
    if (!missing(escl_marks) & !missing(df)) {
      df[df$marker %in% escl_marks,
         2:(length(names(df)) - 1)] <- NA

      p <- ggplot(df, aes(x = date)) +
        geom_line(aes(y = .data[[variable]])) +
        geom_line(aes(y = runningLeq(.data[[variable]])), col = "blue") +
        ylim(y_lim) +
        ylab("dB(A)") +
        ggtitle(paste("Time history and running Leq - ", mp)) +
        theme_minimal()
      return(p)
    } else {
      p <- ggplot(df, aes(x = date)) +
        geom_line(aes(y = .data[[variable]])) +
        geom_line(aes(y = runningLeq(.data[[variable]])), col = "blue") +
        ylim(y_lim) +
        ylab("dB(A)") +
        ggtitle(paste("Time history and running Leq - ", mp)) +
        theme_minimal()
      return(p)
    }
  }
}

#' Calculate reverse Percentile
#'
#' Returns a vector of acoustic percetile
#' @param x  is a vector with Leq data
#' @author Pasquale Scordino \email{p.scordino@@arpa.piemonte.it}
#' @author Simone Sperotto \email{s.sperotto@@arpa.piemonte.it}
#' @example inst/examples/AcuPercentile_ex.R
#' @import stats
#' @export
AcuPercentile  <- function(x) {
  perc <- rev(quantile(x,
                       probs = c(0.01, 0.05, 0.10, 0.50, 0.90, 0.95, 0.99),
                       na.rm = T))
  names(perc) <- c("L1","L5","L10","L50","L90","L95","L99" )
  return(perc)
}

#' Calculate reverse Percentile for period
#'
#' Returns a vector of acoustic percetile
#' @param df  is a dataframe with Leq data
#' @param parameter is a parameter, example "LAeq"
#' @param from is start hour
#' @param to is end hour
#' @param period is a period night or day
#' @author Pasquale Scordino \email{p.scordino@@arpa.piemonte.it}
#' @author Simone Sperotto \email{s.sperotto@@arpa.piemonte.it}
#' @example inst/examples/AcuDNPercentile_ex.R
#' @import lubridate
#' @export
AcuDNPercentile <- function(df, parameter, from, to, period) {

  listaDF <- list()
  listaQuantile <- list()

  if (period == "night") {

    for (i in unique(day(df$date))) {
      listaDF[[i]] <- df[which((hour(df$date) >= from & day(df$date) == i)
                               | (hour(df$date) < to & day(df$date) == i + 1)), ]

      listaQuantile[[i]] <- (AcuPercentile(listaDF[[i]][[parameter]]))
    }

  } else {

    if (period == "day") {

      for (i in unique(day(df$date))) {
        listaDF[[i]] <- df[which((hour(df$date) >= from & day(df$date) == i)
                                 | (hour(df$date) < to & day(df$date) == i)), ]

        listaQuantile[[i]] <- (AcuPercentile(listaDF[[i]][[parameter]]))
      }

    }
  }
  return(listaQuantile)
}

#' Calculate hourly energetic mean
#'
#' Returns a dataframe with hourly energetic mean
#' @param df  is a dataframe with date (Y-m-d H:M:S) and variables
#' @param variable  is a variable name
#' @param timeZone is time zone defoult is Europe/Rome
#' @author Pasquale Scordino \email{p.scordino@@arpa.piemonte.it}
#' @author Simone Sperotto \email{s.sperotto@@arpa.piemonte.it}
#' @example inst/examples/HourlyEmean_ex.R
#' @import lubridate
#' @export
HourlyEmean <- function(df, variable, timeZone = "Europe/Rome") {

  df_hour <- as.data.frame(as.table(tapply(df[[variable]],
                                           list(as.Date(df$date, tz = timeZone),
                                                hour(df$date)),
                                           energetic.mean)))
  names(df_hour) <- c("date", "hour", variable)
  df_hour$date <- paste(df_hour$date, df_hour$hour, sep = " ")
  df_hour <- df_hour[, -2]
  df_hour <- na.omit(df_hour)
  return(df_hour)
}

#' Calculate Holidays date (Gregorian calendar)
#'
#' Returns a vector of holiday dates (Gregorian calendar)
#' @param year_holiday  is year example "2022" like character
#' @author Pasquale Scordino \email{p.scordino@@arpa.piemonte.it}
#' @author Simone Sperotto \email{s.sperotto@@arpa.piemonte.it}
#' @example inst/examples/HolidaysDate_ex.R
#' @import lubridate
#' @export
HolidaysDate <- function(year_holiday){

  holiday_fixed_dm <- c("1-1", "6-1", "25-4", "1-5", "2-6", "15-08",
                       "1-11")

  holiday_fixed <- dmy(paste(holiday_fixed_dm, year_holiday, sep = "-"))

  # GregorianCalendar_MNindex
  if (year_holiday >= 1583 & year_holiday <= 1699) {
    M <- 22
    N <- 2
  } else if(year_holiday >= 1700 & year_holiday <= 1799){
    M <- 23
    N <- 3
  } else if(year_holiday >= 1800 & year_holiday <= 1899){
    M <- 23
    N <- 4
  } else if(year_holiday >= 1900 & year_holiday <= 2099){
    M <- 24
    N <- 5
  } else if(year_holiday >= 2100 & year_holiday <= 2199){
    M <- 24
    N <- 6
  } else if(year_holiday >= 2200 & year_holiday <= 2299){
    M <- 25
    N <- 0
  } else if(year_holiday >= 2300 & year_holiday <= 2399){
    M <- 26
    N <- 1
  } else if(year_holiday >= 2400 & year_holiday <= 2499){
    M <- 25
    N <- 1
  }

  a <- as.numeric(year_holiday) %% 19
  b <- as.numeric(year_holiday) %% 4
  c <- as.numeric(year_holiday) %% 7
  d <- ((19*a) + M) %% 30
  e <- ((2*b) + (4*c) + (6*d) + N) %% 7

  if ((d + e) < 10) {
    easter_day <- (d + e + 22)
    easter_date <- paste(as.character(easter_day), "3", year_holiday, sep = "-")
  } else {
    easter_day <- (d + e - 9)
    easter_date <- paste(as.character(easter_day), "4", year_holiday, sep = "-")
  }

  if (easter_date %in% paste0("26-4-", year_holiday)) {
    easter_date <- paste0("19-4-", year_holiday)
  } else if(easter_date %in% paste0("25-4-", year_holiday) &
            d == 28 & e == 6 & a > 10){
    easter_date <- paste0("18-4-", year_holiday)
  }

  easter_date <- dmy(easter_date)
  after_easter_day <- easter_date + 1

  holiday_date <- c(holiday_fixed, easter_date, after_easter_day)
  return(holiday_date)
}

#' Calculate daily and nightly energetic mean period
#'
#' Returns a dataframe with energetic mean
#' @param x  is a data frame
#' @param variable is variable to apply function
#' @param period is "day" or "night"
#' @param stat is "n_mean" or "e_mean" like mean and energetic mean
#' @param ... another arguments
#' @author Pasquale Scordino \email{p.scordino@@arpa.piemonte.it}
#' @author Simone Sperotto \email{s.sperotto@@arpa.piemonte.it}
#' @example inst/examples/avr.day.night_ex.R
#' @import lubridate
#' @export
avr.day.night <- function(x,
                          variable,
                          period = "day",
                          stat = "n_mean",
                          ...) {

  # I enter the “day” (6-22) and “night” (22-6) flags.
  # Date and time manipulation
  if (is.character(x$date)) {
    x$date <- ymd_h(x$date)
  }
  x$data <- date(x$date)
  x$ora <- hour(x$date)
  x$giorno <- day(x$date)
  x$mese <- month(x$date)
  x$anno <- year(x$date)

  # Insert flags in the new variable Period
  x$periodo1 <- ifelse(x$ora <= 5, "N", "G")
  x$periodo2 <- ifelse(x$ora >= 22, "N", "G")
  x$periodo <-
    ifelse(x$periodo1 == "N" | x$periodo2 == "N", "N", "G")
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # subsetting df with respect to the night “N” and day “G” flags.
  x_N <- subset(x, periodo == "N")
  x_G <- subset(x, periodo == "G")
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Change label to dataset from “x_G” to “x”
  x <- x_G

  if (period == "day" & stat == "n_mean") {
    # Calculation of mean, min, max, and standard deviation Diurnal period
    t_G <- tapply(x[[variable]], list(x$giorno, x$mese, x$anno),
                  function(x) {
                    c(
                      MEAN = mean(x, na.rm = T),
                      MIN = min(x),
                      MAX = (max(x)),
                      SD = sd(x)
                    )
                  })
    # I assemble the contents of the list “t_G” and transform it into dataframe
    x_G_Day <- as.data.frame(do.call(rbind, t_G))

    # I apply the “round” function
    x_G_Day <- round(x_G_Day[ , 1:4], 2)

    # I insert the date vector into the dataframe
    x_G$data <- as.POSIXct(x_G$data)
    x_G_Day$DATA <- seq.Date(date(x_G$data[1]),
                             date(x_G$data[dim(x_G)[1]]), by = "day")

    # Variable position reordering
    x_G_Day <- x_G_Day[, c(5, 1:4)]
    return(x_G_Day)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  } else {
    if (period == "night" & stat == "n_mean") {
      # code for calculation of night statistics straddling two days

      # Input flags at night hours divided by >= 22 “N1” the rest “N2”
      x_N$PeriodoAcu <- ifelse(x_N$ora >= 22, "N1", "N2")
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      x_N1 <- x_N[!(x_N$data == unique((x_N$data))[1] & x_N$ora < 22), ]

      end <- length(unique(x_N1$data))
      x_N2 <- x_N1[!(x_N1$data == unique((x_N1$data))[end] &
                       x_N1$ora >= 22), ]
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Loop that lists the dataframes subsetted according to the criteria
      # 22-6 of two consecutive days
      a <- list()
      for (i in 1:length(unique(x_N2$data))) {
        a[[i]] <- subset(x_N2,
                         ora %in% c(23, 22, 0, 1, 2, 3, 4, 5) &
                           x_N2$data %in% c(unique((x_N2$data))[i],
                                            unique((x_N2$data))[i + 1]))
        a[[i]] <- a[[i]][!(a[[i]]$data == unique((x_N2$data))[i] &
                             a[[i]]$ora %in% c(0, 1, 2, 3, 4, 5)), ]

        a[[i]] <- a[[i]][!(a[[i]]$data == unique((x_N2$data))[i + 1] &
                             a[[i]]$ora %in% c(23, 22)), ]
      }

      # Custom function for calculating basic statistics
      summary_night <- function(x) {
        c(
          MEAN = mean(x, na.rm = T),
          MIN = min(x),
          MAX = max(x),
          SD = sd(x)
        )
      }

      # Application function on the list containing dataframes 22-6
      b <- list()
      suppressWarnings(for (j in 1:length(a)) {
        b[[j]] <- summary_night(a[[j]][[variable]])
      })

      # Creation of the final dataframe with the results
      x_nigthACU <- as.data.frame(do.call(rbind, b))

      # I apply the “round” function
      x_nigthACU <- round(x_nigthACU[ ,1:4], 2)

      # DATE column entry
      x_nigthACU$DATA <- unique(x_N2$data)

      # Dataframe column rearrangement
      x_nigthACU <- x_nigthACU[, c(5, 1, 2, 3, 4)]

      return(x_nigthACU)

    } else {
      if (period == "day" & stat == "e_mean") {
        # Calculation of mean, min, max, and standard deviation Diurnal period
        t_G <- tapply(x[[variable]], list(x$giorno, x$mese, x$anno),
                      function(y) {
                        c(MEAN = energetic.mean(y),
                          MIN = min(y),
                          MAX = max(y)
                        )
                      })

        # I assemble the contents of the list “t_G” and transform it into dataframe
        x_G_Day <- as.data.frame(do.call(rbind, t_G))

        # I apply the “round” function
        x_G_Day <- round(x_G_Day[, 1:3], 2)

        # I insert the date vector into the dataframe
        x_G$data <- as.POSIXct(x_G$data)
        x_G_Day$DATA <- seq.Date(date(x_G$data[1]),
                                 date(x_G$data[dim(x_G)[1]]), by = "day")

        # Variable position reordering
        x_G_Day <- x_G_Day[, c(4, 1:3)]

        return(x_G_Day)

      } else {
        if (period == "night" & stat == "e_mean") {
          #  code for calculation of night statistics straddling two days

          # Input flags at night hours divided by >= 22 “N1”
          # the rest “N2”
          x_N$PeriodoAcu <- ifelse(x_N$ora >= 22, "N1", "N2")
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          # subsetting for the elimination of the first and last hours that do not
          # complete the time range to be analyzed
          x_N1 <- x_N[!(x_N$data == unique((x_N$data))[1] & x_N$ora < 22), ]

          end <- length(unique(x_N1$data))
          x_N2 <- x_N1[!(x_N1$data == unique((x_N1$data))[end] &
                           x_N1$ora >= 22), ]
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          # Loop that lists the dataframes subsetted according to the criteria
          # 22-6 of two consecutive days
          a <- list()
          for (i in 1:length(unique(x_N2$data))) {
            a[[i]] <- subset(x_N2,
                             ora %in% c(23, 22, 0, 1, 2, 3, 4, 5) &
                               x_N2$data %in% c(unique((
                                 x_N2$data
                               ))[i],
                               unique((
                                 x_N2$data
                               ))[i + 1]))
            a[[i]] <- a[[i]][!(a[[i]]$data == unique((x_N2$data))[i] &
                                 a[[i]]$ora %in% c(0, 1, 2, 3, 4, 5)), ]

            a[[i]] <- a[[i]][!(a[[i]]$data == unique((x_N2$data))[i + 1] &
                                 a[[i]]$ora %in% c(23, 22)), ]
          }

          # Custom function for calculating basic statistics
          summary_night <- function(x) {
            c(MEAN = energetic.mean(x),
              MIN = min(x),
              MAX = max(x)
            )
          }

          # Application function on the list containing dataframes 22-6
          b <- list()
          suppressWarnings(for (j in 1:length(a)) {
            b[[j]] <- summary_night(a[[j]][[variable]])
          })

          # Creation of the final dataframe with the results
          x_nigthACU <- as.data.frame(do.call(rbind, b))

          # I apply the “round” function
          x_nigthACU <- round(x_nigthACU[,1:3], 2)

          # DATE column entry
          x_nigthACU$DATA <- unique(x_N2$data)

          # Dataframe column rearrangement
          x_nigthACU <- x_nigthACU[, c(4, 1, 2, 3)]

          return(x_nigthACU)
        }
      }
    }
  }
}

#' Calculate daily or total Lden
#'
#' Returns a dataframe with Lden
#' @param dataframe  is a dataframe
#' @param variable is name of variable
#' @param type is "daily" or "total"
#' @param ... is another arguments
#' @author Pasquale Scordino \email{p.scordino@@arpa.piemonte.it}
#' @author Simone Sperotto \email{s.sperotto@@arpa.piemonte.it}
#' @example inst/examples/LdenCalculator_ex.R
#' @import lubridate
#' @import tidyr
#' @export
LdenCalculator <-
  function(dataframe, variable, type = "daily", ...) {

    df <- dataframe
    df$day <- day(df$date)
    df$hour <- hour(df$date)
    df$month <- month(df$date)
    df$year <- year(df$date)

    df$date3 <- as.Date(df$date)
    for (i in seq_along(df$hour)) {
      if (df$hour[i] %in% 0) {
        df$date3[i] <- df$date3[i] + 1
      }
    }

    # insertion of periods D, E, N and D_acu
    for (i in 1:length(df$hour)) {
      if (df$hour[i] >= 6 & df$hour[i] <= 19) {
        df$period1[i] <- "D"
      } else {
        if (df$hour[i] >= 20 & df$hour[i] <= 21) {
          df$period1[i] <- "E"
        } else {
          df$period1[i] <- "N"
        }
      }
    }

    df$period3 <- ifelse(df$hour >= 6 & df$hour <= 21, "D_acu", "")

    # Creation of a column following the frequency of occurrence
    df$count <- sequence(rle(as.character(df$period1))$lengths)

    # marking periods night straddling two days
    for (i in seq_along(df$count)) {
      if (df$count[i] == 8 & df$period1[i] %in% "N") {
        for (n in 7:1) {
          df$period2[i] <- "N"
          df$period2[i - n] <- "N"
        }
      } else {
        df$period2[i] <- ""
      }
    }


    # Lden calculation function
    LdenCalc <- function(x) {
      result <-
        round(10 * log10(((14 / 24) * (10 ^ (
          x$D / 10
        ))) + ((2 / 24) * (10 ^ ((x$E + 5) / 10
        ))) +
          ((8 / 24) * (10 ^ ((x$N + 10) / 10
          )))), digits = 1)
      return(result)
    }


    if (type == "daily") {
      # Calculation of energy averages as a function of periods
      # And creation of a unique dataset
      #-------------------------------------------------------------------------

      # Management of data straddling two days for the overnight period
      df$date2 <- as.Date(df$date)
      for (i in seq_along(df$hour)) {
        if (df$hour[i] %in% c(22, 23, 0)) {
          df$date2[i] <- df$date2[i] + 1
        }
      }

      df.N <- as.data.frame(as.table(tapply(
        df[[variable]],
        list(df$date2, df$period2),
        energetic.mean
      )))
      #-------------------------------------------------------------------------

      df.DE <- as.data.frame(as.table(tapply(
        df[[variable]],
        list(df$date3, df$period1),
        energetic.mean
      )))


      df.D_acu <- as.data.frame(as.table(tapply(
        df[[variable]],
        list(df$date3, df$period3),
        energetic.mean
      )))

      df.DE <- (df.DE[df.DE$Var2 %in% c("D", "E"),])
      names(df.DE) <- c("date", "period", "value")

      df.D_acu <- (df.D_acu[df.D_acu$Var2 %in% "D_acu",])
      names(df.D_acu) <- c("date", "period", "value")

      df.N <- (df.N[df.N$Var2 %in% "N",])
      names(df.N) <- c("date", "period", "value")

      df.DEN <- rbind(df.D_acu, df.DE, df.N)

      df.DEN_wider <- pivot_wider(df.DEN,
                                  names_from = "period",
                                  values_from = "value")
      df.DEN_wider$Lden <- NA
      #-------------------------------------------------------------------------

      # application of the function
      df.DEN_wider$Lden <- LdenCalc(df.DEN_wider)
      return(df.DEN_wider)
    } else {
      if (type == "total") {
        # total
        df.DEN_total <- as.data.frame(as.table(tapply(
          df[[variable]],
          list(df$period1),
          energetic.mean
        )))

        names(df.DEN_total) <- c("period", "value")

        df.DEN_total_wider <- pivot_wider(df.DEN_total,
                                          names_from = "period",
                                          values_from = "value")

        df.DEN_total_wider$Lden <- NA

        # applicazione della funzione
        df.DEN_total_wider$Lden <- LdenCalc(df.DEN_total_wider)
        return(df.DEN_total_wider)
      }
    }
  }

#' Plot spectrogram
#'
#' Returns a spectrogram
#' @param df  is a dataframe
#' @param coLs is cols index to plot
#' @param plot_title is title of plot
#' @author Pasquale Scordino \email{p.scordino@@arpa.piemonte.it}
#' @author Simone Sperotto \email{s.sperotto@@arpa.piemonte.it}
#' @example inst/examples/PlotSpectrogram_ex.R
#' @import ggplot2
#' @import tidyr
#' @export
PlotSpectrogram <- function (df, coLs, plot_title = NULL)
{
  if (length(names(df)[coLs]) == 31) {
    names(df)[coLs] <- as.numeric(c(
      "20",
      "25",
      "31.5",
      "40",
      "50",
      "63",
      "80",
      "100",
      "125",
      "160",
      "200",
      "250",
      "315",
      "400",
      "500",
      "630",
      "800",
      "1000",
      "1250",
      "1600",
      "2000",
      "2500",
      "3150",
      "4000",
      "5000",
      "6300",
      "8000",
      "10000",
      "12500",
      "16000",
      "20000"))
  } else {
    names(df)[coLs] <- as.numeric(c(
      "6.3",
      "8",
      "10",
      "12.5",
      "16",
      "20",
      "25",
      "31.5",
      "40",
      "50",
      "63",
      "80",
      "100",
      "125",
      "160",
      "200",
      "250",
      "315",
      "400",
      "500",
      "630",
      "800",
      "1000",
      "1250",
      "1600",
      "2000",
      "2500",
      "3150",
      "4000",
      "5000",
      "6300",
      "8000",
      "10000",
      "12500",
      "16000",
      "20000"))
  }

  df_long <- pivot_longer(df, cols = coLs)

  namecol <- (length(names(df_long)) - 1)
  valuecol <- (length(names(df_long)))
  df_long$name <- factor(df_long$name, levels = c(unique(df_long$name)))

  spec <- ggplot(df_long, aes(date, name)) +
    geom_raster(aes(fill = value), interpolate = TRUE) +
    scale_y_discrete("frequency (Hz)") +
    scale_fill_viridis_c(direction = -1, option = "plasma") +
    labs(x = "time", fill = "dB") +
    ggtitle(paste(plot_title, "\n")) +
    theme_classic()+
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank())
  return(spec)
}

#' Plot time histrory and compare frequency components
#'
#' Returns a plot
#' @param df  is a dataframe
#' @param variable is Leq or another variable to plot first
#' @param listvar are names of frequency component you want compare
#' @param mp is a misure point
#' @param runleq is logical value that plot running leq line
#' @param y_lim y axe range
#' @author Pasquale Scordino \email{p.scordino@@arpa.piemonte.it}
#' @author Simone Sperotto \email{s.sperotto@@arpa.piemonte.it}
#' @example inst/examples/PlotNoiseTHcompare_ex.R
#' @import ggplot2
#' @import tidyr
#' @export
PlotNoiseTHcompare <- function (df, variable, listvar = NULL, mp, runleq = TRUE, y_lim = c(20, 80))
{

  if (is.null(listvar)) {
    if (runleq == TRUE) {
      p <- ggplot(df, aes(x = date)) +
        geom_line(aes_string(y = variable)) +
        geom_line(aes_string(y = runningLeq(df[[variable]])), col = "blue") +
        ylim(y_lim) +
        ylab("dB") +
        ggtitle(paste("Time history and running ", variable, " - " ,  mp)) +
        theme_minimal()
    } else {
      p <- ggplot(df, aes(x = date)) +
        geom_line(aes_string(y = variable)) +
        ylim(y_lim) +
        ylab("dB") +
        ggtitle(paste("Time history ", variable, " - ",  mp)) +
        theme_minimal()
    }

  } else {

    CoLs <- which(names(df) %in% listvar)
    df_long <- pivot_longer(df,
                            cols = all_of(CoLs))
    names(df_long)[length(df_long) - 1] <- "Component (Hz)"

    if (runleq == TRUE) {
      p <- ggplot(df, aes(x = date)) +
        geom_line(aes_string(y = variable)) +
        geom_line(aes_string(y = runningLeq(df[[variable]])), col = "blue") +
        geom_line(data = df_long, aes(y = value, colour = `Component (Hz)`)) +
        ylim(y_lim) +
        ylab("dB") +
        ggtitle(paste("Time history and running ", variable, " - " ,  mp)) +
        theme_minimal() +
        theme()
    } else{
      p <- ggplot(df, aes(x = date)) +
        geom_line(aes_string(y = variable)) +
        geom_line(data = df_long, aes(y = value, colour = `Component (Hz)`)) +
        ylim(y_lim) +
        ylab("dB") +
        ggtitle(paste("Time history ", variable, " - ",  mp)) +
        theme_minimal() +
        theme()
    }

  }
  return(p)
}

#' Search impulsive event
#'
#' Returns a list with dataframe of peaks impulsive and a plot
#' @param df  is a impulse dataframe, samples of 100 ms
#' @param cri1 is first criteria 6dB (LAImax - LASmax > 6dB)
#' @param cri2 is second criteria -10dB ((LAFmax - 10dB) < 1s)
#' @param Threshold is minimun level for detect peaks
#' @author Pasquale Scordino \email{p.scordino@@arpa.piemonte.it}
#' @author Simone Sperotto \email{s.sperotto@@arpa.piemonte.it}
#' @example inst/examples/searchImpulse_ex.R
#' @import lubridate
#' @import ggplot2
#' @import tidyr
#' @importFrom  pracma findpeaks
#' @export
searchImpulse <- function(df, cri1 = 6, cri2 = -10, Threshold = 30) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # CRITERI di IMPULSIVITA'
  id_1criterio <- which((df$LAImax - df$LASmax) > cri1)
  df$cri1 <- NA
  df$cri1[id_1criterio] <- "y"

  df$id <- c(1:length(df$date))
  df$date2 <- ymd_hms(df$date, tz = "Europe/Rome")

  x <- findpeaks(df$LAFmax, threshold = Threshold, sortstr=TRUE)

  df_peakList <- list()
  for (i in seq_along(x[,1])) {
    df_peakList[[i]] <- df[c(x[, 3][i]:x[, 4][i]), ]
  }

  xmax <- x[, 2]
  ymax <- x[, 1]
  x1 <- NA
  x2 <- NA
  FWHM <- NA
  for (i in seq_along(df_peakList)) {
    x1[i] <- df_peakList[[i]]$id[df_peakList[[i]]$id <
                                   xmax[i]][which.min(abs(df_peakList[[i]]$LAFmax[df_peakList[[i]]$id <
                                                                                    xmax[i]]- (max(df_peakList[[i]]$LAFmax) + cri2)))]
    x2[i] <- df_peakList[[i]]$id[df_peakList[[i]]$id >
                                   xmax[i]][which.min(abs(df_peakList[[i]]$LAFmax[df_peakList[[i]]$id >
                                                                                    xmax[i]]- (max(df_peakList[[i]]$LAFmax) + cri2)))]
    FWHM[i] <- x2[i]-x1[i]
  }

  xx <- as.data.frame(x)
  xx$V5 <- df$date2[xx$V2]
  xx$V6 <- df$cri1[xx$V2]
  xx$V7 <- FWHM
  names(xx) <- c("ymax", "xmax", "startPeak", "stopPeak", "date", "cri1", "cri2")
  ImpPlot <- ggplot() +
    geom_line(data = df, aes(x = date2, y = LAFmax), colour = "navy") +
    geom_point(data = xx, aes(x = date, y = ymax), colour = "maroon") +
    ggtitle("LAFmax Time Serie - Search Impulsive Event",
            subtitle = paste0("Impulsive peaks: ", length(FWHM))) +
    labs(x = "date", y = "LAFmax (dB)") +
    theme_bw()

  listaRisultatiFunzione <- list(Plot = ImpPlot, dfPeaks = xx)
  return(listaRisultatiFunzione)
}

#' Trasform impulsive dataframe (100 ms samples) in dataframe (1s samples)
#'
#' Returns a dataframe (1s samples)
#' @param dfImpulsive  is a dataframe for impulse
#' @param statistic is energetic mean (default)
#' @author Pasquale Scordino \email{p.scordino@@arpa.piemonte.it}
#' @author Simone Sperotto \email{s.sperotto@@arpa.piemonte.it}
#' @example inst/examples/dfImpulsiveTrasform_ex.R
#' @import lubridate
#' @export
dfImpulsiveTrasform <- function(dfImpulsive, statistic = energetic.mean) {
  df <- dfImpulsive
  df <- df[, -c(3:6)]
  df$date <- ymd_hms(df$date, tz = "Europe/Rome")
  df_min <- as.data.frame(as.table(tapply(df$LAeq, df$date, statistic)))
  names(df_min) <- c("date", "LAeq")
  for (i in names(df)[3:length(names(df))]) {
    df_min[[i]] <- as.data.frame(as.table(tapply(df[[i]], df$date, statistic)))$Freq
  }
  return(df_min)
}


#' Calculate Intrusive Index (UNI/TS 11844 march 2022)
#'
#' Returns a number
#' @param dfa  is a dataframe Lfa (enviromental sound levels) 1/3 octave specta data
#' @param dfr  is a dataframe Lfr (residual sound levels) 1/3 octave specta data
#' @param BW   a vector of 1/3 octave bandwidth data
#' @author Pasquale Scordino \email{p.scordino@@arpa.piemonte.it}
#' @author Simone Sperotto \email{s.sperotto@@arpa.piemonte.it}
#' @example inst/examples/IntrusiveIndex_ex.R
#' @export
IntrusiveIndex <- function(dfa, dfr, BW) {
  Tspect <-     as.data.frame(apply(dfa, 2, energetic.mean))
  Tspect$Lfa <- as.data.frame(apply(dfa,
                                    2, energetic.mean))$'apply(dfa, 2, energetic.mean)'
  Tspect$Lfr <- as.data.frame(apply(dfr,
                                    2, energetic.mean))$'apply(dfr, 2, energetic.mean)'
  Tspect$`apply(dfa, 2, energetic.mean)`<- row.names(Tspect)
  row.names(Tspect) <- NULL
  names(Tspect)[1] <- "frequency"

  for (i in seq_along(Tspect$frequency)) {
    if ((Tspect$Lfa[i] - Tspect$Lfr[i]) >= 3) {
      Tspect$Lfs[i] <- 10*log10((10^(Tspect$Lfa[i]/10)) - (10^(Tspect$Lfr[i]/10)))
    } else {
      Tspect$Lfs[i] <- Tspect$Lfa[i] - 3
    }
  }

  Tspect$bw <- BW

  Tspect$d <- 0.4*sqrt(Tspect$bw)*((10^(Tspect$Lfs/10))/(10^(Tspect$Lfr/10)))
  dc <- sqrt(sum(Tspect$d))

  DL <- round(10*log10(dc), digits = 0)

  if (DL < 13) {
    StringIndex <- " Intrusivity Index is negligible"
  } else {
    if (DL >= 13 | DL < 18) {
      StringIndex <- " Intrusivity Index is very low"
    } else {
      if (DL >= 18 | DL < 23) {
        StringIndex <- " Intrusivity Index is low"
      } else {
        if (DL >= 23 | DL < 33) {
          StringIndex <- " Intrusivity Index is medium"
        } else {
          if (DL >= 33 | DL < 43) {
            StringIndex <- " Intrusivity Index is height"
          } else {
            if (DL >= 43) {
              StringIndex <- " Intrusivity Index is very height"
            }
          }
        }
      }
    }
  }
  result <- paste0(DL, StringIndex)
  return(result)
}

#' Plot acoustic quantile
#'
#' Returns a plot of acoustic qualtile of 1/3 band frequency
#' @param df  is a dataframe
#' @param Cols vector of index cols (1/3 band frequency)
#' @param Quantile quantile, for example 0.95
#' @param TimeZone Time zone dataset (default is UTC)
#' @author Pasquale Scordino \email{p.scordino@@arpa.piemonte.it}
#' @author Simone Sperotto \email{s.sperotto@@arpa.piemonte.it}
#' @example inst/examples/AcousticQuantilePlot_ex.R
#' @import lubridate
#' @import ggplot2
#' @export
AcousticQuantilePlot <- function(df, Cols, Quantile, TimeZone = "UTC") {

  AcousticQuantile <- function(x, Quantile) {
    res <- as.vector(rev(quantile(x, probs = as.numeric(Quantile), na.rm = T)))
    return(res)
  }

  Quant <- list()
  for (i in Cols) {
    Quant[[i]] <- as.data.frame(as.table(tapply(df[, i],
                                                list(as.Date(df$date, tz = TimeZone),
                                                     hour(df$date)),
                                                AcousticQuantile, Quantile)))
  }
  names(Quant) <- names(df[, c(1:(Cols[1] - 1), Cols)])

  dd <- na.omit(as.data.frame(do.call(rbind, Quant)))
  dd$frequency <- row.names(dd)
  names(dd) <- c("date", "hour", "value", "frequency")
  rownames(dd) <- NULL

  if (length(unique(dd$hour)) > 1) {
     dd$frequency <- gsub("\\.\\d+$", "", dd$frequency, perl = T)
     dd$frequency <- gsub("LZeq\\.", "", dd$frequency, perl = T)
  } else {
     dd$frequency <- gsub("LZeq\\.", "", dd$frequency, perl = T)
  }

  dd$frequency <- factor(dd$frequency, levels = unique(dd$frequency))

  p <- ggplot(dd, aes(hour, frequency)) +
    geom_tile(aes(fill = value), colour = "white") +
    scale_fill_gradient2(low = "blue", mid = "green", high = "red",
                         midpoint = mean(dd$value, na.rm = T)) +
    ggtitle(paste0("AcusticQuantilePlot ", "L", Quantile*100)) +
    labs(y = "frequency (Hz)", fill = "dB") +
    theme_classic()

  return(p)
}

#' Calculate energetic sum or difference of values
#'
#' Returns energetic sum or difference of values
#' @param x is first value or vector
#' @param y is second value or vector
#' @param operator is 1 for sum and -1 for difference
#' @author Pasquale Scordino \email{p.scordino@@arpa.piemonte.it}
#' @author Simone Sperotto \email{s.sperotto@@arpa.piemonte.it}
#' @example inst/examples/dbsum_ex.R
#' @import lubridate
#' @import tidyr
#' @export
dbsum <- function(x, y, operator) {
  r = 10*log10(10^(x/10) + operator * (10^(y/10)))
  return(r)
}

#' Calculate SEL (Single Event Level)
#'
#' Returns SEL
#' @param x is value in dB
#' @param t is period in second
#' @author Pasquale Scordino \email{p.scordino@@arpa.piemonte.it}
#' @author Simone Sperotto \email{s.sperotto@@arpa.piemonte.it}
#' @example inst/examples/SELcalc_ex.R
#' @export
SELcalc <- function(x, t) {
  SEL <- 10*(log10(10^(x/10)*t))
  return(SEL)
}
