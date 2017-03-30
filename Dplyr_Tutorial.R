# Load libraries
library(dplyr)
library(purrr)
library(ggplot2)
library(nycflights13)

# Import data
df <- flights

# Investigate data
dim(df)

str(df)

table(complete.cases(df))
prop.table(table(complete.cases(df)))
ccases <- complete.cases(df)

# !ADVANCED!
map_int(.x = df,.f = ~sum(is.na(.x)))

# Cleaning dirty data
df.clean <- df %>%
  filter(ccases)

# Reshaping data

# Long form to wide form

# Wide form to long form

# Subsetting data - by row

# Subsetting data - by column

# Making new variables

# Summarizing data

# Grouping data

# Visualizing
