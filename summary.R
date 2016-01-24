data <- read.csv(file="data.txt", header=F, sep=" ")
colnames(data) <- c("method", "sequenceCount", "itemCount", "itemsPerItemset", "itemsetsPerSequence", "minSupport", "memoryLimit", "dummy", "time")
data$dummy <- NULL

library(dplyr)
library(ggplot2)

data %>%
  group_by(method, sequenceCount) %>%
  summarise(
    mean = mean(time, na.rm = TRUE)
  ) %>%
  select(method, sequenceCount, mean) %>%
  ggplot(aes(x=sequenceCount, y=mean, color=method)) +
    labs(x="Długość sekwencji", y = "Średni czas [s]") +
    geom_point(shape=1) +
    scale_x_log10() +
    scale_colour_hue(l=50) + # Use a slightly darker palette than normal
    geom_smooth(method=lm,   # Add linear regression lines
                se=FALSE,    # Don't add shaded confidence region
                fullrange=TRUE) # Extend regression lines

data %>%
  filter(!is.na(time)) %>%
  mutate(time = round(time)) %>%
  group_by(method, minSupport, time) %>%
  summarise(
    count = n()
  ) %>%
  select(method, minSupport, time, count) %>%
  ggplot(aes(x=minSupport, y=time, color=method)) +
  labs(x="Minimalna wartość wsparcia", y = "Średni czas [s]") +
  geom_point(aes(size = count), shape=1) +
  scale_x_log10() +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) # Extend regression lines
