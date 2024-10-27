library(dplyr)
library(ggplot2) # For plotting
library(ggrepel) # For better label placement
library(tibble)
library(ggfortify)
library(psych)

gdp_score_tf <- t(gdp_rate_2)
colnames(gdp_score_tf) <- gdp_score_tf[1,]
gdp_score_df <- as.data.frame(gdp_score_tf[-1,])
gdp_score_scaled <- scale(as.numeric(gdp_score_tf$Germany))

hicp_tf <- hicp[, c(-1, -2, -3, -4, -5)]
hicp_de_num <- as.numeric(hicp[2,-1])[c(-1,-2,-3,-4)]

hpi_normal <- t(HPI)
colnames(hpi_normal) <- hpi_normal[1,]
hpi_normal <- as.data.frame(hpi_normal[-1,])
hpi_normal_scaled <- scale(as.numeric(hpi_normal$Germany))


# Balance of payments by country
bop_tf <- t(bop2)
colnames(bop_tf) <- bop_tf[1,]
bop_tf_df <- as.data.frame(bop_tf[-1,])[c(-1,-2,-3,-4),]
bop_tf_scaled <- scale(as.numeric(bop_tf_df$Germany))

# Export market shares (BPM6)




length(de_scaled)
length(gdp_score_scaled)
length(hpi_normal_scaled)
length(bop_tf_scaled)

pca_df <- data.frame(
  hicp = hicp_de_num,
  gdp_rate = gdp_score_scaled,
  hpi = hpi_normal_scaled,
  bop = bop_tf_scaled
)

pca <- principal(data.matrix(pca_df), nfactors = 2)
pairs.panels(pca_df, gap=0, pch=21)
