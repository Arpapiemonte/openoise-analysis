# Calculate energetic mean in nightly period (22-06)

#data(exampleHourlyData)

avr.day.night(exampleHourlyData, "leq", period = "night",
              stat = "e_mean")[1:5, ]

# Calculate energetic mean in daily period (06-22)
avr.day.night(exampleHourlyData, "leq", period = "day",
              stat = "e_mean")[1:5, ]

# Calculate mean in daily period (06-22)
avr.day.night(exampleHourlyData, "leq", period = "day",
              stat = "n_mean")[1:5, ]

