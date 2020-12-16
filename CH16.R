library("tidyverse")
library("lubridate")
library("nycflights13")

#16.2.1-------------------------------------------------------------------

#What happens if you parse a string that contains invalid dates?
ret <- ymd(c("2010-10-10", "bananas"))
#Warning message:
#1 failed to parse.
print(class(ret))
#[1] "Date"
ret
#[1] "2010-10-10" NA
  
#What does the tzone argument to today() do? Why is it important?
#It determines the time-zone of the date. Since different time-zones can have different dates.

#Use the appropriate lubridate function to parse each of the following dates:
d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14" # Dec 30, 2014
#Answer:
mdy(d1)
#[1] "2010-01-01"
ymd(d2)
#[1] "2015-03-07"
dmy(d3)
#[1] "2017-06-06"
mdy(d4)
#[1] "2015-08-19" "2015-07-01"
mdy(d5)
#[1] "2014-12-30"

#16.3.4----------------------------------------------------------------------

#This code is needed by exerciswses:
make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights %>%
  filter(!is.na(dep_time), !is.na(arr_time)) %>%
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>%
  select(origin, dest, ends_with("delay"), ends_with("time"))

#How does the distribution of flight times within a day change over the course of the year?
flights_dt %>%
  filter(!is.na(dep_time)) %>%
  mutate(dep_hour = update(dep_time, yday = 1)) %>%
  mutate(month = factor(month(dep_time))) %>%
  ggplot(aes(dep_hour, color = month)) +
  geom_freqpoly(aes(y = ..density..), binwidth = 60 * 60)

#Compare dep_time, sched_dep_time and dep_delay.
flights_dt %>%
  mutate(dep_time_ = sched_dep_time + dep_delay * 60) %>%
  filter(dep_time_ != dep_time) %>%
  select(dep_time_, dep_time, sched_dep_time, dep_delay)

#Compare air_time with the duration between the departure and arrival. 
flights_dt %>%
  mutate(
    flight_duration = as.numeric(arr_time - dep_time),
    air_time_mins = air_time,
    diff = flight_duration - air_time_mins
  ) %>%
  select(origin, dest, flight_duration, air_time_mins, diff)

#How does the average delay time change over the course of a day? Should you use dep_time or sched_dep_time? Why?
#Use sched_dep_time because that is the relevant metric for someone scheduling a flight. 

#On what day of the week should you leave if you want to minimise the chance of a delay?
#Saturday has the lowest average departure delay time and the lowest average arrival delay time.

#What makes the distribution of diamonds$carat and flights$sched_dep_time similar?
#In sched_dep_time it is at 00 and 30 minutes. In carats, it is at 0, 1/3, 1/2, 2/3 .

#Confirm my hypothesis that the early departures of flights in minutes 20-30 and 50-60 are caused by scheduled flights that leave early. 
flights_dt %>% 
  mutate(minute = minute(dep_time),
         early = dep_delay < 0) %>% 
  group_by(minute) %>% 
  summarise(
    early = mean(early, na.rm = TRUE),
    n = n()) %>% 
  ggplot(aes(minute, early)) +
  geom_line()

#16.4.5-----------------------------------------------------------------

#Why is there months() but no dmonths()?
#Because the months have differing numbers of days.

#Explain days(overnight * 1) to someone who has just started learning R. How does it work?
#The overnight is a variable equal to TRUE or FALSE. If it is an overnight flight, this becomes 1 day, and if not, then overnight = 0, and no days are added to the date.

#Create a vector of dates giving the first day of every month in 2015.
ymd("2015-01-01") + months(0:11)
#Create a vector of dates giving the first day of every month in the current year.
floor_date(today(), unit = "year") + months(0:11)

#Write a function that given your birthday (as a date), returns how old you are in years.
age <- function(bday) {
(bday %--% today()) %/% years(1)
}
age(ymd("1998-2-19"))

#Why can’t (today() %--% (today() + years(1))) / months(1) work?
#The numerator of the expression, (today() %--% (today() + years(1)), is an interval, which includes both a duration of time and a starting point. The interval has an exact number of seconds. 

