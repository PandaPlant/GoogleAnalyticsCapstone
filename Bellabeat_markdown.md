Bellabeat data analytics project
================
Alejandro Puerta
2022-08-08

## Project statement

Bellabeat is a smart device company specialized in women users. The
company is looking for data-driven decisions regarding the general usage
of smart devices and its relation to one of their products. In this
project we will:

-   Find trends in a public dataset regarding the general usage of smart
    devices, using sleep duration, total distance per day, activity
    intensity and total calories burnt.
-   Relate these trends to one of the products from Bellabeat.
-   Propose marketing strategies in order to broaden Bellabeat sales of
    a specific product.

**Stakeholders:** Founder of Bellabeat, Urška Sršen, cofounder and Chief
Creative Officer, Sando Mur, and the Bellabeat marketing team.

## Preparing phase

For this analysis, I decided to consider, initially four tables:
dailyActivity, sleepDay, hourlySteps and weightLogInfo. These will help
us find trends between the types of users based on they activity and
steps data, and find trends based on their sleep and weight. But first,
we want to know if there are enough records and users in each table to
find significant trends.

### Importing packages

First, we import the relevant packages, define working directory and
import the tables to be used.

``` r
#Importing packages
library(tidyverse) 
library(lubridate) #simplifies date work
library(dplyr) #filtering and selecting fields in datasets
library(ggplot2) #plotting
library(tidyr) #manipulating datasets

#defining working dir and reading csv files

setwd("C:/Users/a.puerta/Documents/Proyectos/Bellabeat/Fitabase Data 4.12.16-5.12.16/")
activity = read.csv("dailyActivity_merged.csv")
sleep = read.csv("sleepDay_merged.csv")
steps = read.csv("hourlySteps_merged.csv")
weight = read.csv("weightLogInfo_merged.csv")
```

### Checking amount of records and participants

``` r
#checking amount of records
glimpse(activity)
```

    ## Rows: 940
    ## Columns: 15
    ## $ Id                       <dbl> 1503960366, 1503960366, 1503960366, 150396036…
    ## $ ActivityDate             <chr> "4/12/2016", "4/13/2016", "4/14/2016", "4/15/…
    ## $ TotalSteps               <int> 13162, 10735, 10460, 9762, 12669, 9705, 13019…
    ## $ TotalDistance            <dbl> 8.50, 6.97, 6.74, 6.28, 8.16, 6.48, 8.59, 9.8…
    ## $ TrackerDistance          <dbl> 8.50, 6.97, 6.74, 6.28, 8.16, 6.48, 8.59, 9.8…
    ## $ LoggedActivitiesDistance <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ VeryActiveDistance       <dbl> 1.88, 1.57, 2.44, 2.14, 2.71, 3.19, 3.25, 3.5…
    ## $ ModeratelyActiveDistance <dbl> 0.55, 0.69, 0.40, 1.26, 0.41, 0.78, 0.64, 1.3…
    ## $ LightActiveDistance      <dbl> 6.06, 4.71, 3.91, 2.83, 5.04, 2.51, 4.71, 5.0…
    ## $ SedentaryActiveDistance  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ VeryActiveMinutes        <int> 25, 21, 30, 29, 36, 38, 42, 50, 28, 19, 66, 4…
    ## $ FairlyActiveMinutes      <int> 13, 19, 11, 34, 10, 20, 16, 31, 12, 8, 27, 21…
    ## $ LightlyActiveMinutes     <int> 328, 217, 181, 209, 221, 164, 233, 264, 205, …
    ## $ SedentaryMinutes         <int> 728, 776, 1218, 726, 773, 539, 1149, 775, 818…
    ## $ Calories                 <int> 1985, 1797, 1776, 1745, 1863, 1728, 1921, 203…

``` r
glimpse(sleep)
```

    ## Rows: 413
    ## Columns: 5
    ## $ Id                 <dbl> 1503960366, 1503960366, 1503960366, 1503960366, 150…
    ## $ SleepDay           <chr> "4/12/2016 12:00:00 AM", "4/13/2016 12:00:00 AM", "…
    ## $ TotalSleepRecords  <int> 1, 2, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
    ## $ TotalMinutesAsleep <int> 327, 384, 412, 340, 700, 304, 360, 325, 361, 430, 2…
    ## $ TotalTimeInBed     <int> 346, 407, 442, 367, 712, 320, 377, 364, 384, 449, 3…

``` r
glimpse(steps)
```

    ## Rows: 22,099
    ## Columns: 3
    ## $ Id           <dbl> 1503960366, 1503960366, 1503960366, 1503960366, 150396036…
    ## $ ActivityHour <chr> "4/12/2016 12:00:00 AM", "4/12/2016 1:00:00 AM", "4/12/20…
    ## $ StepTotal    <int> 373, 160, 151, 0, 0, 0, 0, 0, 250, 1864, 676, 360, 253, 2…

``` r
glimpse(weight)
```

    ## Rows: 67
    ## Columns: 8
    ## $ Id             <dbl> 1503960366, 1503960366, 1927972279, 2873212765, 2873212…
    ## $ Date           <chr> "5/2/2016 11:59:59 PM", "5/3/2016 11:59:59 PM", "4/13/2…
    ## $ WeightKg       <dbl> 52.6, 52.6, 133.5, 56.7, 57.3, 72.4, 72.3, 69.7, 70.3, …
    ## $ WeightPounds   <dbl> 115.9631, 115.9631, 294.3171, 125.0021, 126.3249, 159.6…
    ## $ Fat            <int> 22, NA, NA, NA, NA, 25, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ BMI            <dbl> 22.65, 22.65, 47.54, 21.45, 21.69, 27.45, 27.38, 27.25,…
    ## $ IsManualReport <chr> "True", "True", "False", "True", "True", "True", "True"…
    ## $ LogId          <dbl> 1.462234e+12, 1.462320e+12, 1.460510e+12, 1.461283e+12,…

From the amount of records, we can see that activity, sleep and steps
have a significant amount of records taking into account that there is a
total of 33 participants. On the other hand, the weight table only has
67 observations. This might be too little in order to draw significant
conclusions. However, for now we will see if trends can be found.

Now lets find the amount of participants in each table:

``` r
#checking amount of participants in each table
n_distinct(activity$Id)
```

    ## [1] 33

``` r
n_distinct(sleep$Id)
```

    ## [1] 24

``` r
n_distinct(steps$Id)
```

    ## [1] 33

``` r
n_distinct(weight$Id)
```

    ## [1] 8

From these results, we can see that again, weight table might have few
participants in order to draw conclusions about the smart devices global
consumer population. However, there seems to be enough observations per
participant in order to find how they have progressed though time in
terms of their weight. This point of view needs to be done carefully
since we don’t know which is the weight objective of each participant.
For now, we will keep on finding trends with these four tables.

## Process phase

First we will see how we can clean the data. We will be searching for:
formatting issues (date in particular needs to be changed into a global
format), and duplicate observations. Typos are avoided taking into
consideration that most fields are numerical or date, and they have been
formatted correctly.

### Formatting dates

We will change the format taking into account

``` r
#fixing chr format to date in ActivityDate field
activity$ActivityDate = as.POSIXct(activity$ActivityDate,format = "%m/%d/%Y")
sleep$SleepDay = as.POSIXct(sleep$SleepDay ,format="%m/%d/%Y %I:%M:%S %p")
steps$ActivityHour = as.POSIXct(steps$ActivityHour,format="%m/%d/%Y %I:%M:%S %p")
weight$Date = as.POSIXct(weight$Date,format = "%m/%d/%Y %I:%M:%S %p")

#Checking for global format
head(activity$ActivityDate)
```

    ## [1] "2016-04-12 -05" "2016-04-13 -05" "2016-04-14 -05" "2016-04-15 -05"
    ## [5] "2016-04-16 -05" "2016-04-17 -05"

``` r
head(sleep$SleepDay)
```

    ## [1] "2016-04-12 -05" "2016-04-13 -05" "2016-04-15 -05" "2016-04-16 -05"
    ## [5] "2016-04-17 -05" "2016-04-19 -05"

``` r
head(steps$ActivityHour)
```

    ## [1] "2016-04-12 00:00:00 -05" "2016-04-12 01:00:00 -05"
    ## [3] "2016-04-12 02:00:00 -05" "2016-04-12 03:00:00 -05"
    ## [5] "2016-04-12 04:00:00 -05" "2016-04-12 05:00:00 -05"

``` r
head(weight$Date)
```

    ## [1] "2016-05-02 23:59:59 -05" "2016-05-03 23:59:59 -05"
    ## [3] "2016-04-13 01:08:52 -05" "2016-04-21 23:59:59 -05"
    ## [5] "2016-05-12 23:59:59 -05" "2016-04-17 23:59:59 -05"

Everything seems fine with the format. Now, we want to separate the
columns that contain both date and hour from the hourly steps and weight
tables. In order to do that we use the following code:

``` r
steps = separate(steps,ActivityHour, c("Date","Hour"),sep = " ")
weight = separate(weight,Date,c("Date", "Hour"), sep = " ")
```

### Checking duplicates

To check for duplicates we created conditionals to check if there were
duplicates on the first place, and then remove them if there were. Here
we found that only the Sleep table contained duplicates.

``` r
#Checking if there are duplicates and removing them
print("Checking Activity duplicates")
```

    ## [1] "Checking Activity duplicates"

``` r
{
  if(sum(duplicated(activity))>0){
    cat("Initial duplicates = ", sum(duplicated(activity)))
    activity = activity %>% 
      distinct() %>% 
      drop_na()
    print("Duplicates and NA have been removed")
    cat("Duplicates now = ", sum(duplicated(activity)))
  } else {
    print("There are no duplicates")
    cat("Duplicates =", sum(duplicated(activity)))
  }
}
```

    ## [1] "There are no duplicates"
    ## Duplicates = 0

``` r
print("Checking sleep duplicates")
```

    ## [1] "Checking sleep duplicates"

``` r
{
  if(sum(duplicated(sleep))>0){
    cat("Initial duplicates = ", sum(duplicated(sleep)))
    sleep = sleep %>% 
      distinct() %>% 
      drop_na()
    print("Duplicates and NA have been removed")
    cat("Duplicates now = ", sum(duplicated(sleep)))
  } else {
    print("There are no duplicates")
    cat("Duplicates =", sum(duplicated(sleep)))
  }
}
```

    ## Initial duplicates =  3[1] "Duplicates and NA have been removed"
    ## Duplicates now =  0

``` r
print("Checking Hourly steps duplicates")
```

    ## [1] "Checking Hourly steps duplicates"

``` r
{
  if(sum(duplicated(steps))>0){
    cat("Initial duplicates = ", sum(duplicated(steps)))
    steps = steps %>% 
      distinct() %>% 
      drop_na()
    print("Duplicates and NA have been removed")
    cat("Duplicates now = ", sum(duplicated(steps)))
  } else {
    print("There are no duplicates")
    cat("Duplicates =", sum(duplicated(steps)))
  }
}
```

    ## [1] "There are no duplicates"
    ## Duplicates = 0

``` r
print("Checking Hourly weight duplicates")
```

    ## [1] "Checking Hourly weight duplicates"

``` r
{
  if(sum(duplicated(weight))>0){
    cat("Initial duplicates = ", sum(duplicated(weight)))
    weight = weight %>% 
      distinct() %>% 
      drop_na()
    print("Duplicates and NA have been removed")
    cat("Duplicates now = ", sum(duplicated(weight)))
  } else {
    print("There are no duplicates")
    cat("Duplicates =", sum(duplicated(weight)))
  }
}
```

    ## [1] "There are no duplicates"
    ## Duplicates = 0

### Summarizing and understanding the data

#### Activity distribution depending on type of activity

Now we want to see how the important variables are distributed in each
of the tables selected. First, in the activity table, we are interested
in how sedentary or active are the participants. We use a pipe to select
relevant columns (in this case the minutes spent in each type of
activity), and then we use the summary() function:

``` r
activity %>% 
  select(SedentaryMinutes,LightlyActiveMinutes,FairlyActiveMinutes,VeryActiveMinutes) %>% 
  summary()
```

    ##  SedentaryMinutes LightlyActiveMinutes FairlyActiveMinutes VeryActiveMinutes
    ##  Min.   :   0.0   Min.   :  0.0        Min.   :  0.00      Min.   :  0.00   
    ##  1st Qu.: 729.8   1st Qu.:127.0        1st Qu.:  0.00      1st Qu.:  0.00   
    ##  Median :1057.5   Median :199.0        Median :  6.00      Median :  4.00   
    ##  Mean   : 991.2   Mean   :192.8        Mean   : 13.56      Mean   : 21.16   
    ##  3rd Qu.:1229.5   3rd Qu.:264.0        3rd Qu.: 19.00      3rd Qu.: 32.00   
    ##  Max.   :1440.0   Max.   :518.0        Max.   :143.00      Max.   :210.00

There are important preliminar results in this output. In particular,
the median and mean calculations of the SedentaryMinutes tells us that
most of the people are above the mean, but there are few partipants that
have lower sedentary minutes significant enough to draw the mean to a
lower than median result. A similar trend occurs with the
veryActiveMinutes column. In this case most of the participants are
below the mean, but there are very active participants that make the
mean higher than the median. In other words, the majority of people have
the tendency to have longer sedentary periods, and shorter very active
periods.

## Analysis

### Ranking the type of activity of each day for each participant

Now we want to see how each day activity could have been influenced by
other variables such as sleep time or weight. In order to do that we did
the following: 1) rank the overall day activity into four categories.
These categories were defined by:

-   *A day was very active if:* your very active period was for 60 or
    more minutes, or if your fairly active period was equal or longer
    than 180 minutes.
-   *A day was fairlt active if:* your very active period was greater or
    equal than 30 minutes, or if your fairly active period was longer
    than 120 minutes.
-   *A day was lightly active if:* your very active period was greater
    or equal than 15 minutes, or if your fairly active period was longer
    than 60 minutes.
-   *Otherwise, you had a sedentary day.*

Then, we created another column for the activity table in which the rank
of the day was calculated with the formulation above.

### Calculating mean participant type of activity

We want to further understand the sample of this dataset. We can
calculate the mean type of activity of each user if we can associate
with a number (1,2,3 or 4), calculate the mean of their days and finally
round the number in order to associate to a given rank. Then, an
appropiate pie chart can be created working around the ggplot2 bar chart
with polar coordinates.

``` r
#Creating category (string) and rank (interger) columns.
activity$category = c("") 
activity$rank = c(0)


#determining type of activity and rank of each day in every participant
for(i in 1:nrow(activity)){
  {
    if(activity[i,11] >= 60 || activity[i,12] >= 180){
      activity[i,16] = "Very active"
      activity[i,17] = 4
    } else if ( activity[i,11]>= 30 || activity[i,12]>= 120){
      activity[i,16] = "Fairly active"
      activity[i,17] = 3
    } else if ( activity[i,11] >= 15 || activity[i,12] >= 60){
      activity[i,16] = "Lightly active"
      activity[i,17] = 2
    } else {
      activity[i,16] = "Sedentary"
      activity[i,17] = 1
    }
  }
}

#creating a table with the mean rank of each participant
rankTable = activity %>% 
  select(Id, rank) %>% 
  group_by(Id) %>% 
  summarise(rank_mean = mean(rank))

#rounding the results of the mean in another column
rankTable$round = round(rankTable$rank_mean)

#finally creating a table that has the percentage of each type of activity
pieTable = data.frame(user_type=c("Sedentary","Lightly active","Fairly active","Very active"),
                      rank=c(sum(rankTable$round == 1), sum(rankTable$round == 2),
                          sum(rankTable$round == 3),sum(rankTable$round == 4)))

pieTable$labels = scales::percent(pieTable$rank/33)
```

``` r
## ggplot piechart
{
  ggplot(pieTable, aes(x="",y=rank, fill=user_type)) + 
  geom_bar(stat = "identity",width = 1) + 
  coord_polar("y", start=0) +
  theme_minimal()+
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size=14, face = "bold")) +
  scale_fill_manual(values = c("#85e085","#e6e600", "#ffd480", "#ff8080")) +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5))+
  labs(title="User type distribution")
}
```

![](Bellabeat_markdown_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

This information is telling us that most participants have, in general,
a sedentary lifestyle (more than half of the sample). Therefore we can
ask ourselves, what could be causing this sedentary lifestyle? Is there
something that the company can do in order to make their life more
active? Therefore, I decided to explore the possibility that sleep and
weight could be important variables associated to this lifestyle.

### How is sleep related to activity?

Now, lets see how the sleep time influences the activity of each day.
For that we used ggplot distributions in which the counted variable is
TotalMinutesAsleep, and the distributions were painted in four colors,
one for wach rank calculated.

In order to do that we followed the following steps:

-   Merging the sleep table and the activity table by two columns (Id
    and date) using the left_join function.
-   Using the ggplot histogram function using TotalTimeAsleep as the
    counted variable, and category as the fill variable. The code below
    shows the process and result.

``` r
#defining the table in whcich we will pair up Id and Date with minutes asleep.
a = select(activity,Id,ActivityDate,category)
b = select(sleep,Id,SleepDay,TotalMinutesAsleep)


cor2 = merge(x = a, y = b, by.x = c("Id", "ActivityDate"),
             by.y = c("Id","SleepDay"))

#plotting the distributions
ggplot(cor2, aes(x=TotalMinutesAsleep,fill = as_factor(category),color = I("black"))) + 
  geom_histogram(alpha = 0.6, position = "identity") + 
  labs(fill = "Activity rank") +
  guides(color = "none") +
  scale_fill_brewer(palette="Dark2") +
  theme_minimal()+theme_classic() +
  ggtitle("Asleep time distributions for each day activity rank") +  
  theme(plot.title=element_text(hjust = 0.5))
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Bellabeat_markdown_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

This distributions highlight some important features of the daily
activity of the participants:

-   Fairly, lightly and very active days have asleep time around the
    same distribution, centered around 370 and 460 minutes (around 6 and
    7 hours).
-   However, sedentary days imply an asleep time with a distribution
    centered in around 500 minutes (around 8 to 9 hours of sleep).
-   Besides, the amount of sedentary days is much higher compared to the
    other type of days (given the height of the sedentary distribution).
-   There are few people that had few to no sleep, which led to a
    sedentary day. This would be an outlier given that participants most
    likely don’t have the energy in these days in order to be physically
    active.
-   It is important to avoid causation in this situation. More data is
    needed in order to draw this conclusion, however it seems that this
    might be an important factor to take into account if one wants to
    have an active day.

In other words, there are a lot of days in which these participants
stayed asleep, and that lead to a lot of days ranked as sedentary. One
possible strategy to encourage more active days, is to give feedback on
asleep time. If they get up from bed earlier, they will have more active
days!

### How is wight related to activity?

Taking the same steps as before, we can build a similar visualization of
weight distribution taking by activity of each day.

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Bellabeat_markdown_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

However in this case the analysis lack richness:

-   First, we can see there are not enough observations in order to draw
    useful conclusions about trends.
-   We could try to make a different analysis taking into account the
    progress made by each participant and its relation to the type of
    activity they have done in the past days. However, this analysis
    doesn’t make sense if we don’t know the goal of each participant.
-   It would be nice to have enough data, however it is important to
    avoid bias in this analysis: people with higher weight are not
    necessarily sedentary.

### Steps by hour and their relation to day activity rank

We will now see how the mean of the steps is distributed along the day.
However, we want to know how the category given to that day affects this
distribution. Since we have that information in the activity table, we
first merge it with the steps table.

``` r
Total_by_hour = merge(select(activity,Id,ActivityDate,category),steps,
                      by.x=c("Id","ActivityDate"),
                      by.y=c("Id","Date"))
```

Now we summarize the data so that we have a table with the mean steps
per hour per category. Therefore, we should have 4x24 = 96
observations.Then we will plot a point graph where the color indicated
the activity rank, the independent variable are the hours, and the
dependend variable is the mean of the steps taken at that hour.

``` r
#grouping by hour and summarizing:


Total2 = Total_by_hour %>% 
  select(Hour,category,StepTotal) %>% 
  group_by(Hour,category) %>% 
  summarise(meanSteps = mean(StepTotal))
```

    ## `summarise()` has grouped output by 'Hour'. You can override using the
    ## `.groups` argument.

``` r
ggplot(Total2, aes(x=Hour,y=meanSteps,color=category)) + 
  geom_point() +
  labs(color = "Activity rank") +
  scale_fill_brewer(palette="Dark2") +
  theme_minimal()+theme_classic() +
  ggtitle("Mean steps per hour depending on activity rank") + 
  theme(axis.text.x = element_text(angle = 90), plot.title=element_text(hjust = 0.5))
```

![](Bellabeat_markdown_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

We observe the following trends in this data:

-   Sedentary steps are at all times below the rest of the categories,
    followed by lightly, fairly and very active as expected.
-   The most active hours for very active days are between 11AM and 7PM.
-   Some active days have a significant peak of steps at 8AM.
-   In overall activity, the biggest differences between sedentary
    activity and the rest of the categories occurs between 9AM to 7PM.
-   We could predict that a day would be sedentary if the amount of
    steps taken by 9am is still below 500.

This findings could be helpful in the following way: now we know that
the participants are more likely to be active in certain time intervals
of the day. Besides, we could predict when a day is most likely to be
sedentary. Therefore, appropriate motivational messages at certain hours
could motivate the participants to turn the sedentary predicted day,
into a more active day!

## Key findings

We found really interesting findings in this dataset!

-   Most customers tend to be sedentary (more than 50%). Two actions
    must be taken given that information: 1) attracting non-sedentary
    customers must be key in order to have more sales and 2) sedentary
    customers must have a retribution in their lifestyle after they
    bought our product. Therefore, we will have to answer the following
    question: what is causing this sedentary lifestyle, and 2) how can
    we help them change it?

-   After exploring both weight and sleep, we found that only sleep
    could be considered as an important influence over the activity done
    each day. In particular, we found that sedentary days meant more
    time asleep. Therefore, one possible way to tackle this sedentary
    lifestyle is to motivate customers and help them get out of bed more
    quickly. However, it is important that they don’t sleep less than
    what is recommended by specialists.

-   Finally, after getting to know what type of day each participant
    had, we could visualize how the mean of the steps by hour changed
    depending on the type of day. It was clear that sedentary days could
    be predicted in the morining, and that motivating customers to
    perform physical activity after 11AM and before 7PM could be a great
    strategy in order to turn around the prediction done in the
    morining.
