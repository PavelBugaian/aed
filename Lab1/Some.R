x <- unemployment$V1
data <- read.csv(text = x, sep=",")
filtered_data <- filter(data, grepl("DE", geo))
filtered_dataset <- data.frame(geo = filtered_data$geo)

plot(de_dataset$V3[-1], pch=20)

de_dataset <- de_dataset[-1, ]
ds_years <- de_dataset[-1]

summarise(ds_years, avg = mean(V2))
  