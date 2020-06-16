library(readr)
forestfires <- read_csv("data/forestfires.csv")

names(forestfires)
View(forestfires)

library(dplyr)
library(ggplot2)

#sort the months!
forestfires_by_month <- forestfires %>%
  group_by(month) %>%
  summarize(nfires = n())

forestfires_by_month$month <- factor(forestfires_by_month$month,
                                     levels = tolower(month.abb))
ggplot(data = forestfires_by_month) + 
  aes(x = month, y = nfires) + 
  geom_bar(stat = "identity")

forestfires_by_day <- forestfires %>%
  group_by(day) %>%
  summarize(nfires = n())

weekday <- c("mon", "tue", "wed", "thu", "fri", "sat", "sun")
forestfires_by_day$day <- factor(forestfires_by_day$day,
                                     levels = weekday)
ggplot(data = forestfires_by_day) + 
  aes(x = day, y = nfires) + 
  geom_bar(stat = "identity")

#or change it in the main database
forestfires <- forestfires %>%
  mutate(month = factor(month, levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")))

forestfires_by_month <- forestfires %>%
  group_by(month) %>%
  summarize(nfires = n())

ggplot(data = forestfires_by_month) + 
  aes(x = month, y = nfires) + 
  geom_bar(stat = "identity")

forestfires <- forestfires %>%
  mutate(day = factor(day, levels = c("mon", "tue", "wed", "thu", "fri", "sat", "sun")))

forestfires_by_day <- forestfires %>%
  group_by(day) %>%
  summarize(nfires = n())

ggplot(data = forestfires_by_day) + 
  aes(x = day, y = nfires) + 
  geom_bar(stat = "identity")

ggplot(data = forestfires) + 
  aes(x = FFMC) + 
  geom_histogram() + 
  facet_wrap(~month)

library(purrr)
plot_by_month <- function(vname){
  
  ggplot(data = forestfires) + 
    aes_string(x = "month", y = vname) + 
    geom_boxplot()
}

v_to_plot <- c("FFMC", "DMC", "DC", "ISI", "temp", "RH", "wind", "rain")
map(v_to_plot, plot_by_month)

plot_by_day <- function(vname){
  
  ggplot(data = forestfires) + 
    aes_string(x = "day", y = vname) + 
    geom_boxplot()
}

v_to_plot <- c("FFMC", "DMC", "DC", "ISI", "temp", "RH", "wind", "rain")
map(v_to_plot, plot_by_day)

forestfires_area <- forestfires %>%
  filter(area > 0 & area < mean(area)*3)

plot_scatter <- function(vname) {
  ggplot(data = forestfires_area) + 
    aes_string(x = vname, y = "area") + 
    geom_point(alpha= 0.3)
}
map(v_to_plot, plot_scatter)



