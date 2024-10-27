library(dplyr)
library(ggplot2) # For plotting
library(ggrepel) # For better label placement
library(tibble)
library(ggfortify)
library(psych)

gdp_score_tf <- t(gdp_rate_2)
years <- as.vector(gdp_rate_2[1,-1])
colnames(gdp_score_tf) <- gdp_score_tf[1,]
gdp_score_tf <- as.data.frame(gdp_score_tf[-1,])

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

hicp_tf <- hicp[, c(-1, -2, -3, -4, -5)]
hicp_de <- as.vector(hicp_tf[2,-1])
hicp_de_num <- as.numeric(hicp[2,-1])

plot(
  as.vector(hicp_tf[1,-1]), 
  hicp_de,
  type = "b",
  xlab = "year", 
  ylab = "GDP Rate, %",
  pch = 19, col = "red", 
  main = "HICP",
)

gdp_pca <- as.numeric(gdp_rate_de_reversed[-c(1:3)])

pca <- principal(pca_df, nfactors = 2, rotate = "varimax")
pairs.panels(pca_df, gap=0, pch=21)

hpi_normal <- t(hpi)
colnames(hpi_normal) <- hpi_normal[1,]
hpi_normal <- as.data.frame(hpi_normal[-1,])

hpi_normal_scaled <- scale(as.numeric(hpi_normal$Germany))

length(hicp_de_num)
length(gdp_pca)
length(hpi_normal_scaled)

pca_df <- data.frame(
  hicp = hicp_de_num,
  gdp_date = gdp_pca,
  hpi = hpi_normal_scaled
)
