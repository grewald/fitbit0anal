library(dplyr)
library(tidyverse)

activity <- read_csv("activity.csv")

TotalSteps <- activity %>%  group_by(date) %>% 
  summarise(TotalSteps= sum(steps, na.rm = TRUE)) 

TotalSteps %>%  ggplot(aes(x= TotalSteps)) + 
  geom_histogram(bins = 60, fill='lightgreen')+
  labs(x= "Total Steps On a Day" , y= "Number of Days")+
  theme_classic()

summSteps <- activity %>%  group_by(date) %>% 
  summarise(TotalSteps= sum(steps, na.rm = TRUE)) %>% ungroup() %>% 
  mutate(allSteps = sum(TotalSteps),meanPerDay= mean(TotalSteps), medianPerDay= median(TotalSteps))

summSteps$meanPerDay[1]
summSteps$medianPerDay[1]
