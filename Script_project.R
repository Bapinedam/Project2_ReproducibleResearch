library(readr)
library(dplyr)

data <- read_csv("repdata_data_StormData.csv.bz2")

# Abstract

# Data processing

  # Across the United States, which types of events 
  # (as indicated in the (EVTYPE) are most harmful 
  # with respect to population health?

#evtype <- data %>% group_by(EVTYPE) %>% summarise(n = n()) %>% mutate(Percentage = n / sum(n))

evtype <- data %>% group_by(EVTYPE) %>% summarise(INJURIES = sum(INJURIES))

evtype <- evtype[order(evtype$INJURIES, decreasing = TRUE),]  

head(evtype)

library(wordcloud2) 
set.seed(1234)
wordcloud2(data = head(evtype, 20), size=1.6)

library(ggplot2)
head(evtype, 20) %>%
  arrange(INJURIES) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(name = factor(EVTYPE, levels = EVTYPE)) %>%   # This trick update the factor levels
  ggplot( aes(x = name, y = INJURIES)) +
  geom_segment( aes(xend = name, yend=0)) +
  geom_point( size=4, color="orange") +
  coord_flip() +
  theme_bw() +
  xlab("")

  # Across the United States, which types of events have
  # the greatest economic consequences?

data$PROPDMGEXP[data$PROPDMGEXP == ""] <- NA
data$PROPDMGEXP <- gsub("1", "10", data$PROPDMGEXP)
data$PROPDMGEXP <- gsub("NA", "1", data$PROPDMGEXP)
data$PROPDMGEXP <- gsub("\\-", "1", data$PROPDMGEXP)
data$PROPDMGEXP <- gsub("\\?", "1", data$PROPDMGEXP)
data$PROPDMGEXP <- gsub("\\+", "1", data$PROPDMGEXP)
data$PROPDMGEXP <- gsub("^0$", "1", data$PROPDMGEXP)
data$PROPDMGEXP <- gsub("h|H", "100", data$PROPDMGEXP)
data$PROPDMGEXP <- gsub("K", "1000", data$PROPDMGEXP)
data$PROPDMGEXP <- gsub("m|M", "1000000", data$PROPDMGEXP)
data$PROPDMGEXP <- gsub("B", "1000000000", data$PROPDMGEXP)
data$PROPDMGEXP <- gsub("2", "100", data$PROPDMGEXP)
data$PROPDMGEXP <- gsub("3", "1000", data$PROPDMGEXP)
data$PROPDMGEXP <- gsub("4", "10000", data$PROPDMGEXP)
data$PROPDMGEXP <- gsub("5", "100000", data$PROPDMGEXP)
data$PROPDMGEXP <- gsub("6", "1000000", data$PROPDMGEXP)
data$PROPDMGEXP <- gsub("7", "10000000", data$PROPDMGEXP)
data$PROPDMGEXP <- gsub("8", "100000000", data$PROPDMGEXP)


data$PROPDMGEXP <- as.numeric(data$PROPDMGEXP)

propdmgexp <- data %>% group_by(EVTYPE) %>% summarise("PROPERTY DAMAGED" = sum(PROPDMGEXP, na.rm = TRUE))
propdmgexp <- propdmgexp[order(propdmgexp$`PROPERTY DAMAGED`, decreasing = TRUE), ]



head(propdmgexp[propdmgexp$EVTYPE != "?", ], 20) %>%
  arrange(`PROPERTY DAMAGED`) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(name = factor(EVTYPE, levels = EVTYPE)) %>%   # This trick update the factor levels
  ggplot( aes(x = name, y = `PROPERTY DAMAGED`)) +
  geom_segment( aes(xend = name, yend=0)) +
  geom_point( size=4, color="orange") +
  coord_flip() +
  theme_bw() +
  xlab("")
# Results
