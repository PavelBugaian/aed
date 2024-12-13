library(dplyr)
hpi_years <- as.vector(HPI_2[1, ])
hpi_y <- as.vector(HPI_2[2, ])
plot(
  hpi_years, 
  hpi_y,
  type = "b", pch = 19, col = "red", 
  xlab = "year", 
  ylab = "HPI",
  main = "Housing Price Index from 2005 to 2023"
)
y_mean <- mean(as.numeric(hpi_y))
abline(v = 2016, col="blue", lwd = 2, lty = 3, ylab="mean")
text(x = 2017.2, y = 160, 'mean')

y_median <- median(as.numeric(hpi_y))
abline(v = 2014, col="darkgreen", lwd = 2, lty = 3, ylab="mean")
text(x = 2012.5, y = 160, 'median')

y_sd <- sd(as.numeric(hpi_y))
y_min <- min(as.numeric(hpi_y))
y_max <- max(as.numeric(hpi_y))

lines(
  as.vector(HPI_3[1, -1]), 
  as.vector(HPI_3[3, -1]), 
  type = "b", pch = 19, col = "darkblue"
)

legend(2005, 160, legend=c("Germany", "France"),
       col=c("red", "blue"), lty=1:1, cex=0.8)
