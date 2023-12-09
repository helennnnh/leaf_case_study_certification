# leaf_case_study_certification
Case study showcasing what I have learned about the data analytics process from Google on Coursera. All computations are made using R.
---
title: "Google Data Analytics Cert_Case Study 2"
author: "Helen"
date: "2023-11-21"
output: html_document
---

#Looking at Fitness Tracker Data Trends for 'Leaf'.

```{r Packages Installed and Loaded}
#For Data Referencing
#install.packages("here")
library("here")

#For Data Cleaning
#install.packages("janitor")
library("janitor")

#For Data Filterization
#install.packages("dplyr")
library("dplyr")

#load tidyverse for sorting and filtering
#install.packages("tidyverse")
library("tidyverse")

#load lubridate to work with dates and times in R
install.packages("lubridate")
library("lubridate")

#loading stringr to create leading zeros in R
#install.packages("stringr")
library(stringr)

#loading a skimr package for data summarization
#install.packages("skimr")
library("skimr")

#loading anytime package to help remove date from timestamp
#install.packages("anytime")
library(anytime)

```



##FitBit Fitness Tracker Data
```{r Import Datasets}

dailyActivity_merged <- read.csv("~/Google Data Analytics Certification/Week 34 to 37/Kaggle - 2016_FitBit_Fitness_Tracker_Data/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
heartrate_seconds_merged <- read.csv("~/Google Data Analytics Certification/Week 34 to 37/Kaggle - 2016_FitBit_Fitness_Tracker_Data/Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged_2.csv")
sleepDay_merged <- read.csv("~/Google Data Analytics Certification/Week 34 to 37/Kaggle - 2016_FitBit_Fitness_Tracker_Data/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")

```


##Clean Datasets
```{r Fixing camelCase}
daily_activity <- clean_names(dailyActivity_merged)
heartrate_secs <- clean_names(heartrate_seconds_merged)
daily_sleep <- clean_names(sleepDay_merged)
```



```{r  Check data format}
#Reviewing whether there are any incorrect data formats.


str(daily_activity)
str(daily_sleep)
str(heartrate_secs)

#daily_activity$activity_date is chr instead of date format.
#daily_activity$calories should be removed since it is not relevant to activity performed.
#daily_activity would benefit from a total_active_hours column.
#daily_sleep$sleep_day is chr instead of date format (note 'AM' time).
#daily_sleep$total_minutes_asleep would be easier to read in hours.
#daily_sleep$total_time_in_bed would be easier to read in hours.
#heartrate_secs$time is chr instead of date format (note 'AM' time).
#Adding growth opp by reviewing heart rates during sleep hours to reveal quality of sleep of customers
#[N/A]Could add growth opp by reviewing if heart rates are too high or low during sleep hours to "diagnose" sleep conditions such as sleep apnea or dangerous heart conditions.

```



```{r }
#Fixing All Date Format Issues
#!Code appears to randomly keep breaking here
daily_activity$activity_date <- ymd(daily_activity$activity_date)
daily_sleep$sleep_day <- ymd_hms(daily_sleep$sleep_day)
heartrate_secs$time <- ymd_hms(heartrate_secs$time)

```



```{r}
# Removing 'calories' from daily_activity
daily_activity <- daily_activity %>%
  select(-c(calories))

```



##Adding New Columns for ease of viewing and tracking trends
```{r}
#Adding 'total_active_hours' column to 'daily_activity'
daily_activity <- daily_activity %>%
  mutate(daily_activity, total_active_hours=(very_active_minutes + fairly_active_minutes + lightly_active_minutes)/60)

```


```{r}
#Adding 'total_hours_asleep' and 'total_hours_in_bed' to 'daily_sleep'
daily_sleep <- daily_sleep %>%
  mutate(daily_sleep, total_hours_asleep=total_minutes_asleep/60, total_hours_in_bed=total_time_in_bed/60)



```




```{r Create column for Wake-Up Time}
daily_sleep <- daily_sleep %>%
  mutate(daily_sleep, hours_asleep=total_minutes_asleep%/%60, ":", remaining_mins_asleep=total_minutes_asleep%%60)
daily_sleep$remaining_mins_asleep<-stringr::str_pad(daily_sleep$remaining_mins_asleep, width=2, pad="0")

daily_sleep$wake_up_time <- paste(daily_sleep$sleep_day, " ", daily_sleep$hours_asleep,daily_sleep$`":"`,daily_sleep$remaining_mins_asleep, ":00", sep="") 
daily_sleep$wake_up_time <- ymd_hms(daily_sleep$wake_up_time)


```



```{r}
#Adding stages of sleep 'light', 'deep', and 'REM' to 'heartrate_secs'.
#Data assumes the resting heart rate of all individuals is 60 since this data is not stored in the datasets used.
#We would replace the numbers with variables in the event the product started tracking resting heart rates of individuals

heartrate_secs <- heartrate_secs %>%
  mutate(sleep_stage=case_when(
    heartrate_secs$time<daily_sleep$wake_up_time & heartrate_secs$value<60 & heartrate_secs$time<daily_sleep$wake_up_time & heartrate_secs$value>=50 ~ "Light",
    heartrate_secs$time<daily_sleep$wake_up_time & heartrate_secs$value<=60 ~ "REM",
    heartrate_secs$time<daily_sleep$wake_up_time & heartrate_secs$value<50 & heartrate_secs$time<daily_sleep$wake_up_time & heartrate_secs$value>=50 ~ "Deep",
    heartrate_secs$time>=daily_sleep$wake_up_time ~ "Awake"
  ))



```



```{r Add No Date Time to heartrate_secs}

heartrate_secs$no_date_time <- as.POSIXct(heartrate_secs$time, format = "%H:%M:%S")
heartrate_secs$no_date_time <- anytime(heartrate_secs$no_date_time)
heartrate_secs$no_date_time <- format(heartrate_secs$no_date_time, "%H:%M:%S")

```



#Removing Impossible Datapoints
```{r Removing null value in total steps}
#Assuming a person must get up, out of bed, at least once a day, any value at 0 indicates the value is incorrect or that the individual did not wear their fitness tracking device that day.

daily_activity_2 <- daily_activity[daily_activity$total_steps!=0,]
```


```{r Add Day of the Week to daily_sleep and daily_activity_2}
daily_sleep$day_of_week <-wday(daily_sleep$sleep_day, label=T, abbr=T)
daily_activity_2$day_of_week <- wday(daily_activity_2$activity_date, label=T, abbr=T)

```



#Analyzing Trends from Dataset
```{r Some thoughts}
#The business requirements are: 
# (1)What are some trends in smart device usage?; 
# (2)How could these trends apply to Bellabeat customers?; 
# (3)How could these trends help influence Bellabeat marketing strategy?
#Averages
# (1) Steps taken per day
# (2) Distance per day
# (3) Active hours per day
# (4) Hours of Sleep
#     (1.1 to 4.1) Customers can keep track of their weekly progress towards their health goals.
# (5) Stages of sleep
#     (5.1) Marketing can introduce tracking of sleep quality as a new product feature.
#     (5.2) Customers want to know if they are achieving the optimal benefits out of their sleep.


#See averages in steps, distance, active hours,and hours of sleep
summary(daily_activity_2$total_steps)
summary(daily_activity_2$total_distance)
summary(daily_activity_2$total_active_hours)
summary(daily_sleep$total_hours_asleep)


#Average steps: 8319
#Average distance:5.98
#Average active hours: 247.8
#Average total hours of sleep: 6.99

```


```{r Boxplots of Average values for steps, distance, active hours, and hours of sleep}
#Let's draw a boxplot to visually show what quartile customers are falling under for total steps
boxplot(daily_activity_2$total_steps,
        main = "Distribution of Daily Steps by Customers",
        xlab = "Steps",
        horizontal = TRUE,
        col = "blue",
        border = "brown",
        notch = TRUE
)
ggsave('Distribution_Total_Steps_by_Day.pdf')

#Let's draw a boxplot to visually show what quartile customers are falling under for total distance
boxplot(daily_activity_2$total_distance,
        main = "Distribution of Daily Distance by Customers",
        xlab = "Distance",
        horizontal = TRUE,
        col = "green",
        border = "brown",
        notch = TRUE
)
ggsave('Distribution_Total_Distance_by_Day.pdf')

#Let's draw a boxplot to visually show what quartile customers are falling under for total active hours
boxplot(daily_activity_2$total_active_hours,
        main = "Distribution of Total Active Hours by Customers",
        xlab = "Hours",
        horizontal = TRUE,
        col = "orange",
        border = "brown",
        notch = TRUE
)
ggsave('Distribution_Total_Active_Hours_by_Day.pdf')

#Let's draw a boxplot to visually show what quartile customers are falling under for hours of sleep
boxplot(daily_sleep$total_hours_asleep,
        main = "Distribution of Sleep Per Day by Customers",
        xlab = "Hours",
        horizontal = TRUE,
        col = "pink",
        border = "brown",
        notch = TRUE
)
ggsave('Distribution_Sleep_Per_Day_by_Day.pdf')

#Let's make a boxplot of how long customers are sleeping in the REM cycle using sample data
#We will create a sample boxplot for our stakeholders to pitch the value of this data going to market.
#Experts recommend that people should get at least 90 minutes of REM sleep per day.
#We can market this as a way to encourage users to go to bed earlier so they can get enough REM.
#We can also do the same to other sleep cycles to show the quality of sleep customers are getting throughout the day.
#We can also use tracking data we collect about customer's locations to benchmark users' sleep based on whether they live in an urban, suburban, or rural environment
#We can also benchmark how other customers are doing on their sleep quality and give this data to customers to track their progress
sample_REM <- c(20,0,0,0,0,0,0,20,40,30,50,60,50,40,70,80,90,100,95,105,90,120,91,92,115,110,125, 135, 120, 112)
boxplot(sample_REM,
        main = "[SAMPLE] Distribution of REM Sleep Per Day by Customers",
        xlab = "Minutes",
        horizontal = TRUE,
        col = "lavender",
        border = "brown",
        notch = TRUE
)
ggsave('[SAMPLE]_Distribution_REM_Sleep_Per_Day.pdf')

#More than half of the time, customers are not completing the daily recommendation of at least 10,000 steps.
#Median daily distance is just over 5 (not sure if it's in km or mi)
#The median number of active hours appears to be 4, but the actual range 25% to 75% varies between 2hrs-6hrs.
#The median number of hours of sleep is 7, but 25%-75% of the time, customers are getting 6hrs-8hrs. Meaning most customers fall behind the recommended 8hrs of sleep a day.
#If we store customer's resting heart rates and it is reached during their sleep cycle, we can identify when the customer reaches REM sleep. From the time they reach REM sleep until the time they wake up, we can determine how much time the customer spent in REM. This information would inform customers of the quality of their sleep each night and whether it is affecting their energy levels throughout the day or their overall health. This would require a technological change in the way we are storing data, but does not require any new technology upgrades or metrics that we do not already record for the product.


```




```{r Bar Graph of Hours of Sleep During the Week}
ggplot(data=daily_sleep)+
  aes(x=hours_asleep, y=day_of_week)+
  geom_col(color='lavender')+
  labs(x='Total Hours Asleep', y='Day of the Week', title= 'Hours of Sleep During the Week')
ggsave('Hours_asleep_during_day_of_the_week.pdf')


#We can see which days of the week customers are getting the most and least amount of sleep
```



```{r Bar Graph of Active Hours During the Week}
ggplot(data=daily_activity_2)+
  aes(x=total_active_hours, y=day_of_week)+
  geom_col(color='orange')+
  labs(x='Total Hours Active', y='Day of the Week', title= 'Hours of Physical Activity During the Week')
ggsave('Hours_physically_active_during_day_of_the_week.pdf')


#We can see which days of the week customers are getting the most and least amount of sleep
```



```{r Bar Graph of Heart Rate Times Per Day}
#Show distribution of where customer heart rates fall in the day and potential outliers that might be of concern
ggplot(data=heartrate_secs)+
  aes(x=value, y=no_date_time)+
  geom_point()+
  geom_col(color='green')+
  labs(x='Heart Rate', y='Times Per Day', title= 'Frequency of Various Heart Rates')
ggsave('Heart_Rate_Frequency.pdf')

#A number of customers sometimes have alarmingly low or alarmingly high heart rates.
```

```{r}
#What Does this Mean to Us?
## Here are some deliverables we recommend:
### 1. Highlight customer successes to encourage and maintain long-term engagement with product
### 2. Highlight areas of concern or improvement during months we expect more customers to think about their health including New Years, springtime (in preparation for the perfect beach body), and mid-autumn (right before the holiday season)
### 3. Setting up a system where we have customers input their resting heart rates and begin tracking and monitoring customer's sleep cycles through the use of their heart rates. 
### 4. Add ability to set reminders on Sundays to go to bed earlier and set yourself up for a successful week.
### 5. Add ability to set reminders on days of the week where you need more encouragement to work out

```
