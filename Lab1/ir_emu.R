library(dplyr)

data <- interest_rates_emu[-1, -1]
plot(
  as.vector(interest_rates_emu[1, -1]),
  as.vector(data[1, ]),
  type = "b", pch = 19, col = "red", xlab = "year", ylab = "Interest rates"
)
