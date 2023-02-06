library(ggplot2)
library(HH)
library(tidyverse)
data <- read.csv("survey_data.csv")

## Test
data_WEAR = dplyr::select(data,starts_with("WEAR"))
data_WEAR_sum = table(gather(data_WEAR))
data_WEAR_sum <- data_WEAR_sum[, c(7, 2, 5, 3, 4, 1, 6)]
likert(data_WEAR_sum, ReferenceZero=4, as.percent=TRUE, positive.order=TRUE)

## Test
data_IMI = dplyr::select(data,starts_with("IMI"))
data_IMI_sum = table(gather(data_IMI))
data_IMI_sum <- data_IMI_sum[, c(7, 2, 5, 3, 4, 1, 6)]
likert(data_IMI_sum, ReferenceZero=4, as.percent=TRUE, positive.order=TRUE)

## Test # select some questions
data_ACCEPTANCE = dplyr::select(data,starts_with("ACCEPTANCE"))
data_ACCEPTANCE_sum = table(gather(data_ACCEPTANCE))
data_ACCEPTANCE_sum <- data_ACCEPTANCE_sum[, c(7, 2, 5, 3, 4, 1, 6)]
likert(data_ACCEPTANCE_sum, ReferenceZero=4, as.percent=TRUE, positive.order=TRUE)

## Test # select some questions
data_SUS = dplyr::select(data,starts_with("SUS"))
data_SUS_sum = table(gather(data_SUS))
data_SUS_sum <- data_SUS_sum[, c(7, 2, 5, 3, 4, 1, 6)]


## Test # select some questionsd
data_ALL = data[, -1]
data_ALL_sum = table(gather(data_ALL))
data_ALL_sum <- data_ALL_sum[, c(7, 2, 5, 3, 4, 1, 6)]
data_ALL_sum <- as.data.frame.matrix(data_ALL_sum)
#

##
# Revisit row names
row.names(data_ALL_sum)
data_ALL_sum <- data_ALL_sum %>% add_column(type = NA)
data_ALL_sum[startsWith(row.names(data_ALL_sum), "WEAR"), ]$type = "WEAR"
data_ALL_sum[startsWith(row.names(data_ALL_sum), "IMI"), ]$type = "IMI"
data_ALL_sum[startsWith(row.names(data_ALL_sum), "SUS"), ]$type = "SUS"
data_ALL_sum[startsWith(row.names(data_ALL_sum), "ACCEPTANCE"), ]$type = "Acceptance scale"

# Clean up row names
row.names(data_ALL_sum) <- str_remove(row.names(data_ALL_sum), "WEAR..")
row.names(data_ALL_sum) <- str_remove(row.names(data_ALL_sum), "IMI..")
row.names(data_ALL_sum) <- str_remove(row.names(data_ALL_sum), "SUS..")
row.names(data_ALL_sum) <- str_remove(row.names(data_ALL_sum), "ACCEPTANCE..")
row.names(data_ALL_sum) <- str_replace_all(row.names(data_ALL_sum), fixed("."), " ")
row.names(data_ALL_sum) <- str_trim(row.names(data_ALL_sum))

##
p1 <- likert(~ .| type, data_ALL_sum, ReferenceZero=4, as.percent=TRUE, positive.order=TRUE, layout=c(1, 4), scales=list(y=list(relation="free")), ylab="", main="Rating data")
pdf("survey_plot.pdf", width=1.5*11.69 , height=1.5*8.27)
p1 
dev.off()
