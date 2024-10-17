library(dplyr)

data <- unemployment[, -1]
unmp_rate <- lapply(data[2, ], function(x) {as.numeric(gsub(",", ".", x))})
plot(
  as.vector(data[1, ]), 
  as.vector(unmp_rate), 
  type = "b", pch = 19, col = "red", xlab = "year", ylab = "rate"
)


lapply(unemployment_by_land[-1, c(2:13)], function(x) {
  as.numeric(gsub(",", ".", x))
})
data_by_land <- unemployment_by_land[-1, c(2:13)]
hist(
  some_df$V2, 
  col="red", 
  main="Unemployment in 2012 by region",
  xlab="% of unemployed"
)

hist(
  some_df$V10,
  col="red",
  main="Unemployment in 2020 by region",
  xlab="% of unemployed"
)

density_object <- density(some_df$V2)
plot(density_object)