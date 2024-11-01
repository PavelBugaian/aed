# SUBINDEX 1

library(dplyr)
library(psych)
library(factoextra)
library(tidyr)

gdp_score_tf <- t(gdp_rate_2)
colnames(gdp_score_tf) <- gdp_score_tf[1,]
gdp_score_df <- as.data.frame(gdp_score_tf[-1,])
gdp_score_scaled <- scale(as.numeric(gdp_score_df$Germany))

hicp_tf <- t(hicp[, c(-1, -2, -3, -4, -5)])
colnames(hicp_tf) <- hicp[, 1]
hicp_df <- as.data.frame(hicp_tf)
hicp_scaled <- scale(hicp_df$Germany)

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

plot(gdp_score_scaled, type="l", col="darkblue")
lines(hicp_scaled, type="l", col="darkred")
lines(hpi_normal_scaled, type="l", col="darkgreen")
lines(bop_tf_scaled, type="l", col="lightblue")
lines(bpm6_df_scaled, type="l", col="purple")
lines(rep(0, length(bpm6_df_scaled)), type="l", col="black")

length(hicp_scaled)
length(gdp_score_scaled)
length(hpi_normal_scaled)
length(bop_tf_scaled)
length(bpm6_df_scaled)

df <- data.frame(
  hicp = hicp_scaled[1:10],
  gdp_rate = gdp_score_scaled[1:10],
  hpi = hpi_normal_scaled[1:10],
  bop = bop_tf_scaled[1:10],
  bpm6 = bpm6_df_scaled[1:10]
)

# SUBINDEX 2

gaz_prices <- rev(gaz_prices)
gaz_prices_t <- t(gaz_prices[,-11])

gaz_consumption_t<- gaz_consumption[,-1]
gaz_consumption_t <- t(gaz_consumption_t)

electricity_prices <- rev(electricity_prices)
electricity_prices_t <- t(electricity_prices[,-11])

electricity_consumption <- rev(electricity_consumption)
electricity_consumption_t <- t(electricity_consumption[,-11])

poverty_status <- rev(poverty_status)
poverty_status_t <- t(poverty_status[,-11])

colnames(gaz_prices_t) <- gaz_prices[,11]
gaz_prices_t <- as.data.frame(gaz_prices_t)

colnames(gaz_consumption_t) <- gaz_consumption[,1]
gaz_consumption_t <- as.data.frame(gaz_consumption_t)

colnames(electricity_prices_t) <- electricity_prices[,11]
electricity_prices_t <- as.data.frame(electricity_prices_t)

colnames(electricity_consumption_t) <- electricity_consumption[,11]
electricity_consumption_t <- as.data.frame(electricity_consumption_t)

colnames(poverty_status_t) <- poverty_status[,11]
poverty_status_t <- as.data.frame(poverty_status_t)

df <- mutate(df,
  gas_prices=gaz_prices_t$Germany,
  electricity_prices=electricity_prices_t$Germany,
  gaz_consumpt=gaz_consumption_t$Germany,
  electricity_consumpt=electricity_consumption_t$Germany,
  poverty_status=poverty_status_t$Germany
)

si2_df_scaled <- scale(si2_df)

weights <- si2_df %>%
  summarise(across(everything(), ~ var(.)))

