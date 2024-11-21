# Plot reverse quantile of 1/3 band frequency

library(lubridate)

datasetI <- dataset_impulsive1
datasetH <- dfImpulsiveTrasform(datasetI)

AcousticQuantilePlot(df = datasetH, Cols =c(3:38), Quantile = 0.95,
                     TimeZone = "UTC")
