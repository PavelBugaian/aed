library(dplyr)
library(ggplot2) # For plotting
library(ggrepel) # For better label placement
library(tibble)
library(ggfortify)

years <- as.vector(gdp_score[1,-1][, -2])
gdp_rate_de <- as.vector(gdp_score[2,-1][, -2])
gdp_rate_de_reversed <- rev(gdp_rate_de)
gdp_rate_fr <- as.vector(gdp_score[3,-1][, -2])
gdp_rate_de_num <- as.numeric(gdp_score[2,-1][, -2])

hist(gdp_rate_de_num, col = "red", 
     main = "GDP Rate in Germany", xlab = "GDP Rate, %")

de_scaled <- scale(gdp_rate_de_num)

plot(
  years, 
  de_scaled,
  type = "b",
  xlab = "year", 
  ylab = "GDP Rate, %",
  pch = 19, col = "red", 
  main = "GDP Rate in Germany",
)

hicp_de <- as.vector(hicp[2,-1])
hicp_de_num <- as.numeric(hicp[2,-1])

plot(
  as.vector(hicp[1,-1]), 
  hicp_de,
  type = "b",
  xlab = "year", 
  ylab = "GDP Rate, %",
  pch = 19, col = "red", 
  main = "HICP",
)

gdp_pca <- as.numeric(gdp_rate_de_reversed[-c(1:3)])
  
pca_df <- data.frame(
  hicp = hicp_de_num,
  gdp_date = gdp_pca
)

# PCA
pca <- prcomp(pca_df, center = TRUE, scale. = TRUE)
plot(pca$x)
biplot(pca)
screeplot(pca)

autoplot(pca, loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3)
