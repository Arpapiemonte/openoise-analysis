# Plot reverse quantile of 1/3 band frequency

#data(dataset_impulsive1)

AcousticQuantilePlot(df = dataset_impulsive1, Cols =c(7:42), Quantile =0.95,
                     TimeZone = "UTC")
