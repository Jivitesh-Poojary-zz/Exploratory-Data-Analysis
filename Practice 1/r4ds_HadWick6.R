library(nycflights13)
library(dplyr)

# filter() -    Pick observations by their values.
# arrange() -   Reorder the rows.
# select() -    Pick variables by their names.
# mutate() -    Create new variables with functions of existing variables.
# summarise() - Collapse many values down to a single summary.
# between() - 

# Using Filter
jan1 <- filter(flights, month == 1, day == 1)

# Using Subset
jan_1 <- subset(flights, flights$month ==1 & flights$day ==1)

nov_dec <- filter(flights, month %in% c(11, 12))

arrange(flights, year, month, day)
arrange(flights, desc(arr_delay))

# DUMMY DATA
df <- tibble(x = c(3, 5, 2, NA))
arrange(df, x)
arrange(df, desc(is.na(x)))

flights$speed = flights$distance/ flights$air_time
er <- arrange(flights, desc(speed))
(er$speed)

select(flights, year:day)
select(flights, -(year:day))

rename(flights, tail_num = tailnum)

summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))



# Combining multiple operations with the pipe
by_dest <- group_by(flights, dest)
delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
)
delay <- filter(delay, count > 20, dest != "HNL")

ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)

# Pipe %>% ---> 'then'
delays <- flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL")


flights %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay, na.rm = TRUE))


