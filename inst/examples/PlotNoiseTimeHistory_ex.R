#data(P1FA)
#data(markers)

PlotNoiseTimeHistory(df = P1FA, variable = "LAeq", mp = "P1FA", y_lim = c(40, 65))

PlotNoiseTimeHistory(df = P1FA, variable = "LAeq", mp = "P1FA",
                     filemarks = markers, y_lim = c(40, 65))

PlotNoiseTimeHistory(df = P1FA, variable = "LAeq", mp = "P1FA", escl_marks = "escludi",
                     y_lim = c(40, 65))

