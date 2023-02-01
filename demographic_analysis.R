data <- read.csv("demographic_data.csv")

# Age
mean(data$Age)
sd(data$Age)
min(data$Age)
max(data$Age)

# Sex
table(data$sex)

# Time spent outdoors
mean(data$Weekday.average.hours.spent.outdoors)
sd(data$Weekday.average.hours.spent.outdoors)
min(data$Weekday.average.hours.spent.outdoors)
max(data$Weekday.average.hours.spent.outdoors)

mean(data$Weekend.average.hours.spent.outdoors)
sd(data$Weekend.average.hours.spent.outdoors)
min(data$Weekend.average.hours.spent.outdoors)
max(data$Weekend.average.hours.spent.outdoors)

wilcox.test(data$Weekend.average.hours.spent.outdoors, data$Weekday.average.hours.spent.outdoors, paired=TRUE)