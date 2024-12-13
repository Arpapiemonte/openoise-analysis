% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/OpeNoise.R
\name{LdenCalculator}
\alias{LdenCalculator}
\title{Calculate daily or total Lden (Day-evening-night level)}
\usage{
LdenCalculator(dataframe, variable, type = "daily", ...)
}
\arguments{
\item{dataframe}{is a dataframe}

\item{variable}{is name of variable}

\item{type}{is "daily" or "total"}

\item{...}{is another arguments}
}
\value{
dataframe with Lden values
}
\description{
Returns a dataframe with Lden
}
\examples{
#data("exampleHourlyData")

LdenCalculator(dataframe = exampleHourlyData, variable = "leq",
               type = "daily")

LdenCalculator(dataframe = exampleHourlyData, variable = "leq",
               type = "total")
}
\author{
Pasquale Scordino \email{p.scordino@arpa.piemonte.it}

Simone Sperotto \email{s.sperotto@arpa.piemonte.it}
}
