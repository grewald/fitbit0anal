library(dplyr)
library(tidyverse)

activity <- read_csv("activity.csv")

# calculate total steps on each day
TotalSteps <- activity %>%  group_by(date) %>% 
  summarise(TotalSteps= sum(steps, na.rm = TRUE)) 

# use histogram to display steps
TotalSteps %>%  ggplot(aes(x= TotalSteps)) + 
  geom_histogram(bins = 60, fill='lightgreen')+
  labs(x= "Total Steps On a Day" , y= "Number of Days")+
  theme_classic()

# derive mean and median steps per day 
summSteps <- activity %>%  group_by(date) %>% 
  summarise(TotalSteps= sum(steps, na.rm = TRUE)) %>% ungroup() %>% 
  mutate(allSteps = sum(TotalSteps),meanPerDay= mean(TotalSteps), medianPerDay= median(TotalSteps))

summSteps$meanPerDay[1]
summSteps$medianPerDay[1]

# derive mean and median steps per 5 minute interval 
summStepsInt <- activity %>%  group_by(interval) %>% 
  summarise(TotalSteps= sum(steps, na.rm = TRUE),meanPerInt= mean(steps, na.rm = TRUE)) 

#plot average steps across the time points
summStepsInt %>%  ggplot(aes(x= interval, y= meanPerInt)) + geom_line(color='blue') +
  theme_classic()

# get the maximum averge step time interval

maxPerInt <- summStepsInt %>% filter(meanPerInt== max(meanPerInt))

maxPerInt$interval

#get total records missing records
missing_rec <- activity %>% filter(is.na(steps)) %>% count()
