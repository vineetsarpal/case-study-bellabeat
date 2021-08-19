# Load R packages
library(tidyverse)
library(ggplot2)
library(scales)
library(reshape2)

#============================
#STEP 1 COLLECT AND READ DATA
#============================

# Reading csv files
# Ignoring 3 files about daily - Calories, Intensities and Steps respectively, since same data is already in the dailyActivity data frame

df_dailyActivity <- read_csv("/VINEET/R/Data/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
df_heartrate_seconds <- read_csv("/VINEET/R/Data/Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv")
df_hourlyCalories <- read_csv("/VINEET/R/Data/Fitabase Data 4.12.16-5.12.16/hourlyCalories_merged.csv")
df_hourlyIntensities <- read_csv("/VINEET/R/Data/Fitabase Data 4.12.16-5.12.16/hourlyIntensities_merged.csv")
df_hourlySteps <- read_csv("/VINEET/R/Data/Fitabase Data 4.12.16-5.12.16/hourlySteps_merged.csv")
df_minuteCalories <- read_csv("/VINEET/R/Data/Fitabase Data 4.12.16-5.12.16/minuteCaloriesNarrow_merged.csv")
df_minuteIntensities <- read_csv("/VINEET/R/Data/Fitabase Data 4.12.16-5.12.16/minuteIntensitiesNarrow_merged.csv")
df_minuteMETs <- read_csv("/VINEET/R/Data/Fitabase Data 4.12.16-5.12.16/minuteMETsNarrow_merged.csv")
df_minuteSteps <- read_csv("/VINEET/R/Data/Fitabase Data 4.12.16-5.12.16/minuteStepsNarrow_merged.csv")
df_minuteSleep <- read_csv("/VINEET/R/Data/Fitabase Data 4.12.16-5.12.16/minuteSleep_merged.csv")
df_dailySleep <- read_csv("/VINEET/R/Data/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
df_weight <- read_csv("/VINEET/R/Data/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")


#=================================
#STEP 2 - WRANGLE AND COMBINE DATA
#=================================

# Joining hourly data into a single data frame
key_hourly <- c("Id","ActivityHour")
df_hourlyActivity <- full_join(df_hourlyCalories,df_hourlyIntensities, by = key_hourly) %>% 
  full_join(df_hourlySteps, by = key_hourly)

# Joining minute-wise data into a single data frame
key_minute <- c("Id","ActivityMinute")
df_minutewiseActivity <- full_join(df_minuteCalories,df_minuteIntensities, by = key_minute) %>% 
  full_join(df_minuteMETs, by = key_minute) %>%
  full_join(df_minuteSteps, by = key_minute)

# Looking at the data
head(df_dailyActivity)
head(df_hourlyActivity)
head(df_minutewiseActivity)
head(df_heartrate_seconds)
head(df_minuteSleep)
head(df_dailySleep)
head(df_weight)

# Checking the length of Id column
length(unique(df_dailyActivity$Id))
length(unique(df_hourlyActivity$Id))
length(unique(df_minutewiseActivity$Id))
length(unique(df_heartrate_seconds$Id))
length(unique(df_minuteSleep$Id))
length(unique(df_dailySleep$Id))
length(unique(df_weight$Id))


#=================================================
# STEP 3: CLEAN DATA AND PREPARE DATA FOR ANALYSIS
#=================================================

# Fixing data type issues with Dates and date-time data
df_dailyActivity$ActivityDate <- as.POSIXct(df_dailyActivity$ActivityDate, format="%m/%d/%Y")
df_hourlyActivity$ActivityHour <- as.POSIXct(df_hourlyActivity$ActivityHour, format="%m/%d/%Y %I:%M:%S %p")
df_minutewiseActivity$ActivityMinute <- as.POSIXct(df_minutewiseActivity$ActivityMinute, format="%m/%d/%Y %I:%M:%S %p")
df_dailySleep$SleepDay <- as.POSIXct(df_dailySleep$SleepDay, format="%m/%d/%Y")
df_minuteSleep$date <- as.POSIXct(df_minuteSleep$date, format="%m/%d/%Y %I:%M:%S %p")

# Adding new columns
df_dailyActivity$DayOfWeek <- weekdays(df_dailyActivity$ActivityDate)

# Creating Activity Levels
df_dailyActivity <- df_dailyActivity %>% 
  mutate(ActivityLevel = case_when(
    TotalSteps < 10 ~ "N/A",
    TotalSteps >= 10 & TotalSteps <5000 ~ "Sedentary",
    TotalSteps >= 5000 & TotalSteps <7500 ~ "Low Active",
    TotalSteps >= 7500 & TotalSteps <10000 ~ "Somewhat Active",
    TotalSteps >= 10000 & TotalSteps <12500 ~ "Active",
    TotalSteps >= 12500 ~ "Highly Active"))

head(df_dailyActivity)
table(df_dailyActivity$ActivityLevel)

# Filtering out where Activity Level is N/A
df_dailyActivity <- df_dailyActivity %>% 
  filter(ActivityLevel != "N/A")

# Adding new columns and categorizing data
df_hourlyActivity <- df_hourlyActivity %>% 
  mutate(HourValue = as.numeric(format(ActivityHour, format="%H")),
  TimeOfDay = case_when(
    HourValue >= 5 & HourValue < 12 ~ "Morning",
    HourValue >= 12 & HourValue < 17 ~ "Afternoon",
    HourValue >= 17 & HourValue < 22 ~ "Evening",
    HourValue >= 22 | HourValue < 5 ~ "Night"))
head(df_hourlyActivity)

#Looking at sleep data
head(df_dailySleep)
head(df_minuteSleep)

# Summarizing df_minuteSleep data
df_minuteSleep_summary <-df_minuteSleep %>% 
  group_by(Id, logId) %>% 
  summarise(SleepStart = min(date), SleepEnd = max(date), SleepDuration = difftime(SleepEnd,SleepStart,units = "hours"), SleepValue=sum(value), .groups = "drop")

# Addding new columns
df_minuteSleep_summary$SleepStartDay <- weekdays(df_minuteSleep_summary$SleepStart)

# Creating Sleep Category
df_minuteSleep_summary <- df_minuteSleep_summary %>% 
  mutate(SleepCategory = case_when(
    SleepDuration < 4 ~ "N/A",
    SleepDuration >=4 & SleepDuration < 7 ~ "Insufficient",
    SleepDuration >= 7 & SleepDuration <= 9 ~ "Appropriate",
    SleepDuration > 9 ~ "Oversleeping"))
table(df_minuteSleep_summary$SleepCategory)

# Filtering out N/A Sleep Category data
df_minuteSleep_summary <- df_minuteSleep_summary %>% 
  filter(SleepCategory != "N/A")
head(df_minuteSleep_summary)

# Adding new columns and categorizing data
df_minuteSleep_summary <- df_minuteSleep_summary %>% 
  mutate(IdealSleepStartDay = if_else(
    as.numeric(format(SleepEnd, format="%d")) - as.numeric(format(SleepStart, format="%d")) == 0,
    weekdays(as.Date(SleepStart)-1),
    weekdays(SleepStart)
    ),
    SleptWhen = if_else(
      SleepStartDay == IdealSleepStartDay,
      "Before Midnight", "After Midnight")
    )
head(df_minuteSleep_summary)

# Ordering columns in a meaningful manner
df_dailyActivity$DayOfWeek <- ordered(df_dailyActivity$DayOfWeek, levels=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))
df_dailyActivity$ActivityLevel <- ordered(df_dailyActivity$ActivityLevel, levels=c('Sedentary','Low Active','Somewhat Active','Active','Highly Active'))
df_hourlyActivity$TimeOfDay <- ordered(df_hourlyActivity$TimeOfDay, levels=c("Morning","Afternoon","Evening","Night"))
df_minuteSleep_summary$IdealSleepStartDay <- ordered(df_minuteSleep_summary$IdealSleepStartDay, levels=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))
df_minuteSleep_summary$SleepCategory <- ordered(df_minuteSleep_summary$SleepCategory, levels=c("Insufficient","Appropriate","Oversleeping"))

#===================================
# STEP 4: ANALYZE AND VISUALIZE DATA
#===================================

# Reduce tendency to display axis values in scientific notation
options(scipen = 100)

# 1) Plotting steps v/s calories
ggp1 <- df_dailyActivity %>% 
  ggplot(aes(x=TotalSteps, y=Calories)) + geom_point() + geom_smooth(method = "lm", formula = y~x)
ggp1 + labs(title = "Figure 1") + scale_x_continuous(labels = comma) + scale_y_continuous(labels = comma)


# 2A) Plotting no of entries by Activity Level
ggp2a <- df_dailyActivity %>% 
  ggplot(aes(x=ActivityLevel, fill=ActivityLevel)) + geom_bar()
ggp2a + labs(title = "Figure 2A")

# 2B) Plotting no of entries by Activity Level per User 
ggp2b <- df_dailyActivity %>% 
  group_by(Id, ActivityLevel) %>% 
  summarise(NoOfRows = n(), .groups = "drop") %>% 
  ggplot(aes(x=ActivityLevel, y=NoOfRows, fill=ActivityLevel)) + geom_col() + facet_wrap(~Id)
ggp2b + ggtitle("Figure 2B")

# 3AB) Plotting Avg Steps and Avg Calories by Day of Week
ggp3a <- df_dailyActivity %>%
  group_by(DayOfWeek) %>% 
  summarise(AvgSteps = mean(TotalSteps), .groups = "drop") %>% 
  ggplot(aes(x=DayOfWeek, y=AvgSteps, fill=AvgSteps)) + geom_col()
ggp3a + labs(title = "Figure 3A")

ggp3b <- df_dailyActivity %>% 
  group_by(DayOfWeek) %>% 
  summarise(AvgCalories = mean(Calories), .groups = "drop") %>% 
  ggplot(aes(x=DayOfWeek, y=AvgCalories, fill=AvgCalories)) + geom_col()
ggp3b + labs(title = "Figure 3B")

# 3C) Plotting Avg Hours spent in various activity levels by day of week
df_melt_dailyActivity <- select(df_dailyActivity, 11:16,-15) %>% 
  melt("DayOfWeek")
ggp3c <- df_melt_dailyActivity %>% 
  group_by(DayOfWeek,variable) %>% 
  summarise(AvgHours = mean(value)/60, .groups = "drop") %>% 
  ggplot(aes(x=DayOfWeek, y=AvgHours, fill=variable)) + geom_col()
ggp3c + labs(title = "Figure 3C")

# 3D) Plotting No of steps taken by day of week, categorized by Activity Level                 
ggp3d <- df_dailyActivity %>% 
  group_by(DayOfWeek) %>% 
  ggplot(aes(x=DayOfWeek, y=TotalSteps, fill=TotalSteps)) + geom_col() + facet_grid(~ActivityLevel)
ggp3d + labs(title = "Figure 3D") + scale_x_discrete(label=function(x) strtrim(x,3)) + scale_y_continuous(labels = comma)


# 4AB) Plotting Avg values of Calories, Steps and Intensity through the Day
df_melt1_hourlyActivity <- select(df_hourlyActivity, 3:8,-7) %>% 
  melt("TimeOfDay")
ggp4a <- df_melt1_hourlyActivity %>% 
  group_by(TimeOfDay, variable) %>% 
  summarise(AvgValue = mean(value), .groups = "drop") %>% 
  ggplot(aes(x=TimeOfDay, y=AvgValue, fill=variable)) + geom_col(position = "dodge") + facet_wrap(~variable, scales = "free_y")
ggp4a + labs(title = "Figure 4A")

df_melt2_hourlyActivity <- select(df_hourlyActivity, 3:7) %>% 
  melt("HourValue")
ggp4b <- df_melt2_hourlyActivity %>% 
  group_by(HourValue, variable) %>% 
  summarise(AvgValue = mean(value), .groups = "drop") %>% 
  ggplot(aes(x=HourValue, y=AvgValue, fill=variable)) + geom_line() + facet_wrap(~variable, scales = "free_y")
ggp4b + labs(title = "Figure 4B")


# 5A) Plotting no of records by Sleep Category
ggp5a <- df_minuteSleep_summary %>% 
  ggplot(aes(x=SleepCategory, fill=SleepCategory)) + geom_bar()
ggp5a + labs(title = "Figure 5A")


# 5B) Plotting Sleep Category records by Day of Week
ggp5b <- df_minuteSleep_summary %>% 
  group_by(IdealSleepStartDay, SleepCategory) %>% 
  summarise(NoOfRows = n(), .groups = "drop") %>% 
  ggplot(aes(x=SleepCategory, y=NoOfRows, fill=SleepCategory)) + geom_col(position = "dodge") + facet_wrap(~IdealSleepStartDay)
ggp5b + labs(title = "Figure 5B")

# 5C) Plotting Sleep Category records by when the person slept   
ggp5c <- df_minuteSleep_summary %>% 
  group_by(SleptWhen, SleepCategory) %>% 
  summarise(NoOfRows = n(), .groups = "drop") %>% 
  ggplot(aes(x=SleepCategory, y=NoOfRows, fill=SleepCategory)) + geom_col() + facet_grid(~SleptWhen)
ggp5c + labs(title = "Figure 5C")
