library(dplyr)

hist(
  some_df$V13, 
  breaks = seq(1, 6, by=1),
  col="red", 
  main="Unemployment in 2023 by region in Germany",
  xlab="% of unemployed",
)
col_means = data.frame(unmpMeans = colMeans(some_df, na.rm = TRUE))
unpm_years <- as.vector(unemployment_by_land[1, -1])
plot(
  hpi_years[8:19],
  as.vector(col_means$unmpMeans),
  type = "b", pch = 19, col = "red", 
  xlab = "year", 
  ylab = "% of unemployment",
  main = "Unemployment rates in Germany from 2012 to 2023",
)

unemp_y <- as.vector(some_df$V13)
y_mean <- mean(unemp_y, na.rm = TRUE)
y_sd <- sd(unemp_y, na.rm = TRUE)
y_min <- min(unemp_y, na.rm = TRUE)
y_max <- max(unemp_y, na.rm = TRUE)
