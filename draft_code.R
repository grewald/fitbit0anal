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

# get the maximum average step time interval

maxPerInt <- summStepsInt %>% filter(meanPerInt== max(meanPerInt))

maxPerInt$interval

#get total records missing records
missing_rec <- activity %>% filter(is.na(steps) | is.na(date) | is.na(interval) ) %>% count()
missing_rec$n

# replace the missing steps for the interval by median steps for that interval
#1- calculate the median steps per interval
summStepsInt2 <- activity %>%  group_by(interval) %>% 
  summarise(medianPerInt= median(steps, na.rm = TRUE)) 
#2- replace missing with median value

activityImputed <- activity %>% left_join(summStepsInt2 , by= c("interval")) %>% 
  mutate(steps=if_else(is.na(steps),medianPerInt, steps )) %>% select(-c(medianPerInt))

###############
# repeat above steps using the imputed data

# calculate total steps on each day
TotalSteps <- activityImputed %>%  group_by(date) %>% 
  summarise(TotalSteps= sum(steps, na.rm = TRUE)) 

# use histogram to display steps
TotalSteps %>%  ggplot(aes(x= TotalSteps)) + 
  geom_histogram(bins = 60, fill='lightgreen')+
  labs(x= "Total Steps On a Day" , y= "Number of Days")+
  theme_classic()

# derive mean and median steps per day 
summSteps <- activityImputed %>%  group_by(date) %>% 
  summarise(TotalSteps= sum(steps, na.rm = TRUE)) %>% ungroup() %>% 
  mutate(allSteps = sum(TotalSteps),meanPerDay= mean(TotalSteps), medianPerDay= median(TotalSteps))

summSteps$meanPerDay[1]
summSteps$medianPerDay[1]

######

# add forcat

activityDay <- activity %>% mutate(day=as.factor(if_else(
  weekdays(date) %in% c("Monday" ,   "Tuesday"  , "Wednesday", "Thursday" , "Friday"), "Weekday", "Weekend" )))

activityDay2 <- activityDay %>%  mutate(day=factor(day, levels=c("Weekend", "Weekday")))
str(activityDay)
str(activityDay2)

# derive mean and median steps per 5 minute interval 
summStepsInt <- activityDay %>%  group_by(interval, day) %>% 
  summarise(TotalSteps= sum(steps, na.rm = TRUE),meanPerInt= mean(steps, na.rm = TRUE)) 

#plot average steps across the time points
summStepsInt %>%  ggplot(aes(x= interval, y= meanPerInt, color=day)) + geom_line() +
  theme_classic()

