library(dplyr)
library(psych)
library(factoextra)

gdp_score_tf <- t(gdp_rate_2)
colnames(gdp_score_tf) <- gdp_score_tf[1,]
gdp_score_df <- as.data.frame(gdp_score_tf[-1,])
gdp_score_scaled <- scale(as.numeric(gdp_score_df$Germany))


hicp_tf <- t(hicp[, c(-1, -2, -3, -4, -5)])
colnames(hicp_tf) <- hicp[, 1]
hicp_df <- as.data.frame(hicp_tf)
hicp_scaled <- scale(as.numeric(hicp_df$Germany))

hpi_normal <- t(hpi)
colnames(hpi_normal) <- hpi_normal[1,]
hpi_normal <- as.data.frame(hpi_normal[-1,])
hpi_normal_scaled <- scale(as.numeric(hpi_normal$Germany))

# Balance of payments by country
bop_tf <- t(bop2)
colnames(bop_tf) <- bop_tf[1,]
bop_tf_df <- as.data.frame(bop_tf[-1,])[c(-1,-2,-3,-4),]
bop_tf_scaled <- scale(as.numeric(bop_tf_df$Germany))

# Export market shares (BPM6)
bpm6_tf <- t(bpm6)
colnames(bpm6_tf) <- bpm6_tf[1,]
bpm6_df <- as.data.frame(bpm6_tf[-1,])
bpm6_df_scaled <- scale(as.numeric(bpm6_df$Germany))

length(hicp_scaled)
length(gdp_score_scaled)
length(hpi_normal_scaled)
length(bop_tf_scaled)
length(bpm6_df_scaled)

pca_df <- data.frame(
  hicp = hicp_scaled,
  gdp_rate = gdp_score_scaled,
  hpi = hpi_normal_scaled,
  bop = bop_tf_scaled,
  bpm6 = bpm6_df_scaled
)

pca <- principal(data.matrix(pca_df), nfactors = 5)
pairs.panels(pca_df, gap=0, pch=21)

pca <- prcomp(pca_df)
fviz_eig(pca$x, addlabels = TRUE, ylim = c(0, 70))
