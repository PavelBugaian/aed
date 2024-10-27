library(dplyr)

data_gdp <- GDP[, -1]
plot(
  as.vector(data_gdp[1, ]),
  as.vector(data_gdp[2, ]),
  type = "b", pch = 19, col = "red", xlab = "year", ylab = "millions of euro",
  main = "GDP 1975 - 2023"
)
data_gdp_2012 <- unlist(as.vector(data_gdp[2, ])[38:50])
data_years_2012 <- unlist(as.vector(data_gdp[1, ])[38:50])
plot(
  data_years_2012,
  data_gdp_2012,
  type = "b", pch = 19, col = "red", xlab = "year", ylab = "millions of euro",
  main = "GDP 2012 - 2023"
)
plot(col_means$unmpMeans, data_gdp_2012, 
     xlab = "Unemployment rate (.%)", 
     ylab = "GDP, millions of €"
     )
cor(data_gdp_2012, unlist(col_means$unmpMeans))
