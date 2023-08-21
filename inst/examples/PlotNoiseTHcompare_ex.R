#data(P1FA)

PlotNoiseTHcompare(df = P1FA ,
                   variable = "LAeq",
                   listvar = c("LZFmin.100",
                               "LZFmin.250"), mp = "P1FA",
                   runleq = TRUE,
                   y_lim = c(30, 70))
