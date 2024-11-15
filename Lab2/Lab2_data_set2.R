library(dplyr)
library(psych)
library(tidyr)
library(zoo)
library(ggplot2)
library(pROC)

# DATASET IMPORT

## CATEGORY 2

value_of_export <- read.csv("data/value_of_export.csv", header=FALSE, sep=";")
value_of_import <- read.csv("data/value_of_import.csv", header=FALSE, sep=";")
volume_of_export <- read.csv("data/volume_of_export.csv", header=FALSE, sep=";")
volume_of_import <- read.csv("data/volume_of_import.csv", header=FALSE, sep=";")



## CATEGORY 2

gdp_rate_2000 <- read.delim("data/gdp_rate_2000.tsv", header=FALSE)
hicp <- read.delim("data/hicp.tsv", header=FALSE)
HPI <- read.delim("data/HPI.tsv", header=FALSE)
bop2 <- read.delim("data/bop2.tsv", header=FALSE)
bpm6 <- read.delim("data/bpm6.tsv", header=FALSE)



## CATEGORY 3
gaz_prices <- read.csv("data/gaz_prices.csv", header=FALSE, sep=";")
gaz_consumption <- read.csv("data/gaz_consumption.csv", header=FALSE, sep=";")
electricity_prices <- read.csv("data/electricity_prices.csv", header=FALSE, sep=";")
electricity_consumption <- read.csv("data/electricity_consumption.csv", header=FALSE, sep=";")
poverty_status <- read.csv("data/poverty_status.csv", header=FALSE, sep=";")

# REGION: DATA CLEANING

value_of_export<- t(value_of_export) 
colnames(value_of_export)<- c("year", "dollars")
value_of_export <- as.data.frame(value_of_export)

value_of_import <- t(value_of_import)
colnames(value_of_import) <- c("year", "dollar")
value_of_import <- as.data.frame(value_of_import)

volume_of_export <- t(volume_of_export)
colnames(volume_of_export) <- c("year", "index")
volume_of_export <- as.data.frame(volume_of_export)

volume_of_import <- t(volume_of_import)
colnames(volume_of_import) <- c("year", "index")
volume_of_import <- as.data.frame(volume_of_import)

gdp_score_tf <- t(gdp_rate_2000)
colnames(gdp_score_tf) <- gdp_score_tf[1,]
gdp_score_df <- as.data.frame(gdp_score_tf[-1,])

hicp_tf <- t(hicp[, c(-1, -2, -3, -4, -5)])
colnames(hicp_tf) <- hicp[, 1]
hicp_df <- as.data.frame(hicp_tf)

hpi_normal <- t(HPI)
colnames(hpi_normal) <- hpi_normal[1,]
hpi_normal <- as.data.frame(hpi_normal[-1,])

# Balance of payments by country
bop_tf <- t(bop2)
colnames(bop_tf) <- bop_tf[1,]
bop_tf_df <- as.data.frame(bop_tf[-1,])[c(-1,-2,-3,-4),]

# Export market shares (BPM6)
bpm6_tf <- t(bpm6)
colnames(bpm6_tf) <- bpm6_tf[1,]
bpm6_df <- as.data.frame(bpm6_tf[-1,])

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

# ENDREGION



sub1_df <- data.frame(
  gas_prices=gaz_prices_t$Germany,
  electricity_prices=electricity_prices_t$Germany,
  gaz_consumpt=gaz_consumption_t$Germany,
  electricity_consumpt=electricity_consumption_t$Germany,
  poverty_status=poverty_status_t$Germany
)

sub2_df <- data.frame(
  value_of_export=value_of_export$dollars[10:19],
  value_of_import=value_of_import$dollar[10:19],
  volume_of_export=volume_of_export$index[10:19],
  volume_of_import=volume_of_import$index[10:19]
)

sub3_df <- data.frame(
  hicp = hicp_scaled[1:10],
  gdp_rate = gdp_score_scaled[1:10],
  hpi = hpi_normal_scaled[1:10],
  bop = bop_tf_scaled[1:10],
  bpm6 = bpm6_df_scaled[1:10]
)

sub3_df_scaled <- as.data.frame(scale(sub3_df))
sub_index1 <- (
  sub3_df_scaled$hicp + 
    sub3_df_scaled$gdp_rate + 
    sub3_df_scaled$hpi + 
    sub3_df_scaled$bop +
    sub3_df_scaled$bpm6
  ) / 5

sub2_df_scaled <- as.data.frame(scale(sub2_df))
sub2_df_scaled$Subindex <- (1/4) * sub2_df$value_of_export  + 
  (1/4) * sub2_df$value_of_import + 
  (1/4) * sub2_df$volume_of_export +
  (1/4) * sub2_df$volume_of_import

sub1_df_scaled <- as.data.frame(scale(sub1_df))

sub_index2 <- rowMeans(sub1_df_scaled[c("gas_prices", "gaz_consumpt", 
                                   "electricity_prices","electricity_consumpt", "poverty_status")])

sub_index3 <- rowMeans(sub2_df_scaled[c("value_of_export", "value_of_import", 
                                    "volume_of_export","volume_of_import")])
composite_index_df <- data.frame(sub_index1, sub_index2, sub_index3)

plot(c(2013:2022), composite_index_df$sub_index1, col="red", type = "l")
lines(c(2013:2022), composite_index_df$sub_index2, col = "darkblue")     
lines(c(2013:2022), composite_index_df$sub_index3, col = "darkgreen")     

pca <- principal(composite_index_df[1:3], nfactors=2)
composite_index_df$pca_scores <- pca$scores

pca1_variance <- 0.55
pca2_variance <- 0.40
total_variance <- pca1_variance + pca2_variance
weight_pca1 <- pca1_variance / total_variance
weight_pca2 <- pca2_variance / total_variance
composite_index_df$risk_index <- (weight_pca1 * pca$scores[,1] ) + (weight_pca2 *pca$scores[,2])

model <- lm(gdp_score_scaled[14:23] ~ risk_index)
summary(model)
plot(c(2013:2022), y, main = "Actual vs Predicted GDP Growth Rate", xlab = "Year", ylab = "GDP Growth Rate", type = "l", col="darkblue")
lines(c(2013:2022), predict(model), col = "red")     
legend("topleft", legend = c("Actual", "Predicted"), col = c("darkblue", "red"), pch=19)

residuals <- resid(model)

plot(fitted(model), residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 20, col = "blue")
abline(h = 0, col = "red", lwd = 2)

pca <- principal(composite_index_df[1:3], nfactors=2)
composite_index_df$crisis_indicator <- c(0, 0, 1, 0, 0, 0, 0, 1, 0, 1)
composite_index_df$pca_scores <- pca$scores

r_squared <- summary(model)$r.squared
cat("R-squared:", r_squared, "\n")
predictions <- predict(model, risk_index)

plot(predict(model), gdp_score_scaled[14:23], xlab = "Predicted Composite Index", ylab = "Actual Composite Index")
abline(0, 1, col = "red")

plot(predict(model), type="l", col="red")
lines(gdp_score_scaled[14:23], type="l", col="darkblue")

ggplot(composite_index_df, aes(x = risk_index, y = gdp_score_scaled[14:23])) + 
  geom_point() + 
  geom_smooth(method = "lm", col = "blue") +
  labs(x = "Risk index", y = "GDP scores(scaled)")

new_data <- matrix(rexp(10, rate=1), ncol=2)
new_data_df <- data.frame(pca_scores=new_data)
predicted <- predict(model, new_data_df)

ggplot(composite_index_df, aes(x = pca_scores, y = gdp_score_scaled[14:23])) + 
  geom_point(color = "blue", alpha = 0.5) +
  geom_point(data = new_data_df, aes(predicted), color="red", shape=1) + 
  labs(title="Predicted values", x = "pca_scores", y = "GDP rates") + 
  theme_minimal()

years <- c(2013:2022)
plot(years, composite_index_df$sub_index1, type="l", col = "red")
lines(years, composite_index_df$sub_index2, col = "darkblue")
lines(years, composite_index_df$sub_index3, col = "darkgreen")
lines(years, risk_index, col = "orange")

plot(years, composite_index_df$sub_index1, type = "l", col = "blue", xlab = "Year", ylab = "Sub-Index Value")
lines(years, composite_index_df$sub_index2, col = "red")
lines(years, composite_index_df$sub_index3, col = "green")

text(2014, composite_index_df$sub_index1[2], labels = "Start of Crisis", pos = 3, col = "blue")
text(2020, composite_index_df$sub_index2[8], labels = "COVID-19", pos = 3, col = "red")

composite_index_df$year <- years

subindex <- data.frame(
  subindex1 = composite_index_df$sub_index1,
  subindex2 = composite_index_df$sub_index2,
  subindex3 = composite_index_df$sub_index3,
  risk_index = risk_index
)

ggplot(composite_index_df, aes(x = year, y = sub_index1, fill = risk_index)) +
  geom_area(alpha = 0.7) +
  labs(title = "Decomposition Projection of Composite Index",
       x = "Year",
       y = "Composite Index Level",
       fill = "Sub-Index") +
  theme_minimal()

ggplot(composite_index_df, aes(x = year, y = sub_index2, fill = risk_index)) +
  geom_area(alpha = 0.7) +
  labs(title = "Decomposition Projection of Composite Index",
       x = "Year",
       y = "Composite Index Level",
       fill = "Sub-Index") +
  theme_minimal()

ggplot(composite_index_df, aes(x = year, y = sub_index3, fill = risk_index)) +
  geom_area(alpha = 0.7) +
  labs(title = "Decomposition Projection of Composite Index",
       x = "Year",
       y = "Composite Index Level",
       fill = "Sub-Index") +
  theme_minimal()

subindex_ts <- ts(subindex, start = 2013, end = 2022, frequency = 1)

subindeces_decompose <- decompose(subindex)

ggplot(subindex, aes(x = years)) + 
  geom_line(aes(y=risk_index, color="Composite index"), linewidth=1) +
  geom_line(aes(y=sub_index1, color="Subindex 1"), linewidth=1) +
  geom_line(aes(y=sub_index2, color="Subindex 2"), linewidth=1) +
  geom_line(aes(y=sub_index3, color="Subindex 3"), linewidth=1) +
  scale_color_manual(values = c("Subindex 1" = "blue", "Subindex 2" = "red", "Subindex 3" = "green", "Composite index" = "purple")) +
  theme_minimal()

crisis_years <- c(2015, 2020, 2022)
df$Crisis <- ifelse(electricity_prices_t$TIME %in% crisis_years, 1, 0)
logistic_model <- glm(Crisis ~ Composite_Index, data = df, family = binomial)
summary(logistic_model)
predicted_probs <- predict(logistic_model, type = "response")
