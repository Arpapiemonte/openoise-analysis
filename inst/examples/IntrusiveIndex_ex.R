# Calculation of the intrusiveness index

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
