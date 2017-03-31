# Load libraries ----------------------------------------------------------
library(dplyr)
library(readr)
library(purrr)
library(ggplot2)

# Import Data -------------------------------------------------------------
df <- read_csv(file = 'flights.csv',
               col_names = T)
# skip = 3,
# n_max = 20)

# Investigate data --------------------------------------------------------
dim(df)

head(df)

str(df)

table(complete.cases(df))
prop.table(table(complete.cases(df)))
ccases <- complete.cases(df)

# !ADVANCED!
map_int(.x = df, .f = ~ sum(is.na(.x)))

# Cleaning dirty data -----------------------------------------------------

df.clean <- df %>%
  filter(ccases)

dim(df.clean)

# Subsetting data - by row ------------------------------------------------

#What are all the flights on Jan 2nd?
df.clean %>%
  filter(month == 1, day == 2) #And, by default

#What are all the flights in Jan and Feb?
df.clean %>%
  filter(month == 1 | month == 2)

#Select rows by position
df.clean %>%
  slice(2:4)

# Subsetting data - by column ---------------------------------------------

df.clean %>%
  select(carrier, flight, tailnum)

df.clean %>%
  select(carrier:dest)

df.clean %>%
  select(carrier:distance, -tailnum)

# Randomly sample rows ----------------------------------------------------

df.clean %>%
  sample_n(10)

df.clean %>%
  sample_frac(0.01)

# Sort rows ---------------------------------------------------------------

df.clean %>%
  arrange(-year, -month, distance)

# Unique rows -------------------------------------------------------------

df.clean %>%
  distinct(tailnum)

df.clean %>%
  distinct(carrier)

df.clean %>%
  distinct(month)

# Making new variables ----------------------------------------------------

df.clean %>%
  mutate(
    gain = arr_delay - dep_delay,
    speed = distance / air_time * 60,
    gain_per_hour = gain / (air_time / 60)
  )

df.clean %>%
  transmute(
    gain = arr_delay - dep_delay,
    speed = distance / air_time * 60,
    gain_per_hour = gain / (air_time / 60)
  )

# Summarizing data --------------------------------------------------------

df.clean %>%
  summarise(
    dep_mean = mean(dep_delay),
    dep_sd = sd(dep_delay),
    arr_mean = mean(arr_delay),
    arr_sd = sd(arr_delay),
    maxmonth = max(month)
  )

df.clean %>%
  summarise(
    count_rows=n(),
    no_of_months=n_distinct(month),
    largest_delay=max(dep_delay)
  )


# Grouping data -----------------------------------------------------------

df.clean %>%
  group_by(carrier)

df.clean %>%
  group_by(month)

# Chaining! -----------------------------------------------------------------

# How many flights have departed from each destination?
df.clean %>%
  group_by(dest) %>%
  summarise(flights=n())

# How many unique planes have departed from each destination?
df.clean %>%
  group_by(dest) %>%
  summarise(planes=n_distinct(tailnum))

# How many flights on each day?
df.clean %>%
  group_by(year,month,day) %>%
  summarise(flights=n())

# How many flights total per month?
df.clean %>%
  group_by(year,month) %>%
  summarise(totals=n())

# How about something more complex?
# Which flights had an average arrival delay  and average departure delay > 30 min?
df.clean %>%
  group_by(year, month, day) %>%
  select(arr_delay, dep_delay) %>%
  summarise(
    arr = mean(arr_delay),
    dep = mean(dep_delay)
  ) %>%
  filter(arr > 30 | dep > 30)

# What's the average distance traveled by each carrier?
df.clean %>%
  group_by(carrier) %>%
  summarize(
    avg_dist = mean(distance)
  )

# How about the top 5 largest distances?
df.clean %>%
  group_by(carrier) %>%
  summarize(
    avg_dist = mean(distance)
  ) %>%
  arrange(-avg_dist) %>%
  slice(1:5)

# What is the average arrival delay for flights with
# at least 20 flights flying, but for distances < 2000 km?
df.clean %>%
  group_by(tailnum) %>%
  summarise(
    count=n(),
    dist=mean(distance),
    delay=mean(arr_delay)
  ) %>%
  filter(count>20, dist<2000)

# Visualizing -------------------------------------------------------------
