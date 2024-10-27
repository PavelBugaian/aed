library(dlypr)

plot(
  as.vector(inflation2[1, ]), 
  as.vector(inflation2[2, ]),
  type = "b", pch = 19, col = "red", xlab = "year", ylab = "Inflation rate (.%)",
  main = "Inflation rates in Germany from 1995 to 2023",
)

plot(hpi_y, inflation2[2, ][10:28])
inflation_clean <- unlist(inflation2[2, ][10:28])
cor(unlist(hpi_y), inflation_clean, method = "pearson")
