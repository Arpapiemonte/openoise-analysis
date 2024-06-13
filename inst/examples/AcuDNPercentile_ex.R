# Calculate reverse quantile of a dataframe by period

data("exampleHourlyData")

AcuDNPercentile(df = exampleHourlyData,
                parameter = "leq",
                from = "5",
                to = "22",
                period = "night")[1:5]

