data <- read.csv(
  file="data.txt",
  header=F,
  sep="",
  na.strings = c("NA")
  #colClasses = c("factor", "character", "character", "character", "character", "double", "factor", "double")
  )
colnames(data) <- c("method", "sequenceCount", "itemCount", "itemsPerItemset", "itemsetsPerSequence", "minSupport", "memoryLimit", "time")
data$method <- as.factor(data$method)
data$memoryLimit <- as.factor(data$memoryLimit)

library(dplyr)
library(ggplot2)

data %>%
  group_by(method, sequenceCount) %>%
  summarise(
    mean = mean(time)
  ) %>%
  mutate(mean = ifelse(is.na(mean), -1, mean)) %>%
  select(method, sequenceCount, mean) %>%
  ggplot(aes(x=sequenceCount, y=mean, color=method)) +
    labs(x="Długość sekwencji", y = "Średni czas [s]") +
    geom_point(shape=1) +
    scale_x_log10() +
    scale_colour_hue(l=50) +
    geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

data %>%
  mutate(time = round(time)) %>%
  group_by(method, minSupport, time) %>%
  summarise(
    count = n()
  ) %>%
  select(method, minSupport, time, count) %>%
  mutate(time = ifelse(is.na(time), -1, time)) %>%
  ggplot(aes(x=minSupport, y=time, color=method)) +
  labs(x="Minimalna wartość wsparcia", y = "Średni czas [s]") +
  geom_point(aes(size = count), shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

data %>%
  mutate(time = round(time)) %>%
  group_by(method, minSupport, sequenceCount) %>%
  summarise(
    time = mean(time)
  ) %>%
  mutate(time = ifelse(is.na(time), -1, time)) %>%
  select(method, minSupport, sequenceCount, time) %>%
  ggplot(aes(x=minSupport, y=time, color=method)) +
  labs(x="Minimalna wartość wsparcia", y = "Średni czas [s]") +
  geom_point(aes(size = sequenceCount), shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

max_time <- ceiling(max(data$time, na.rm = T))

plot_vars <- function(...) {
  data %>%
    mutate(time = round(time)) %>%
    group_by(...) %>%
    summarise(
      time = max(time, na.rm = T)
    ) %>%
    ggplot(aes(x, y)) +
    geom_tile(aes(fill = time)) +
    scale_fill_gradient(limits=c(0, max_time), low = "red", high = "green", na.value = "black", guide = "colourbar")
}

plot_vars(x = method, y = memoryLimit)
plot_vars(x = method, y = minSupport)

plot_vars(x = minSupport, y = memoryLimit)
