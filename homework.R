# 259 Homework - exploratory data analysis + integrating skills
# For full credit, answer at least 8/10 questions
# List students working with below:

#Christian

library(tidyverse)
library(lubridate)
library(DataExplorer)

#> These data are drawn from the fivethirtyeight article:
#> http://fivethirtyeight.com/features/what-12-months-of-record-setting-temperatures-looks-like-across-the-u-s/
#> The directory us-weather-history contains a data file for each of 10 cities, labelled by their station name
#> Each data file contains:
#> `date` | The date of the weather record, formatted YYYY-M-D
#> `actual_mean_temp` | The measured average temperature for that day
#> `actual_min_temp` | The measured minimum temperature for that day
#> `actual_max_temp` | The measured maximum temperature for that day
#> `average_min_temp` | The average minimum temperature on that day since 1880
#> `average_max_temp` | The average maximum temperature on that day since 1880
#> `record_min_temp` | The lowest ever temperature on that day since 1880
#> `record_max_temp` | The highest ever temperature on that day since 1880
#> `record_min_temp_year` | The year that the lowest ever temperature occurred
#> `record_max_temp_year` | The year that the highest ever temperature occurred
#> `actual_precipitation` | The measured amount of rain or snow for that day
#> `average_precipitation` | The average amount of rain or snow on that day since 1880
#> `record_precipitation` | The highest amount of rain or snow on that day since 1880

stations <- c("KCLT", "KCQT", "KHOU", "KIND", "KJAX", "KMDW", "KNYC", "KPHL", "KPHX", "KSEA")
cities <- c("Charlotte", "Los Angeles", "Houston", "Indianapolis", "Jacksonville", 
            "Chicago", "New York City", "Philadelphia", "Phoenix", "Seattle")


# QUESTION 1
#> The data files are in the directory 'us-weather-history'
#> Write a function that takes each station abbreviation and reads
#> the data file and adds the station name in a column
#> Make sure the date column is a date
#> The function should return a tibble
#> Call the function "read_weather" 
#> Check by reading/glimpsing a single station's file

KCLT <- read_csv("us-weather-history/KCLT.csv")

#attempt 1
read_weather <- function(data) {
  df <- read_csv(str_glue("us-weather-history/{data}.csv"))
  df <- df %>% mutate(station = data)
  df$date <- as.Date(df$date, format = "%Y-%m-%d")
  return(df)
}

KCQT <- read_weather("KCQT","KCQT")

glimpse(KCQT)

#I think I got it??? Ignore below lol
#df$date <- as.Date(df$date, format = "%Y-%m-%d") this seems to mess up my code

# QUESTION 2
#> Use map() and your new function to read in all 10 stations
#> Note that because map_dfr() has been superseded, and map() does not automatically bind rows, you will need to do so in the code.
#> Save the resulting dataset to "ds"

ds <- map(stations,read_weather) %>% 
  bind_rows()

# QUESTION 3
#> Make a factor called "city" based on the station variable
#> (station should be the level and city should be the label)
#> Use fct_count to check that there are 365 days of data for each city 

ds <- ds %>% mutate(city =factor(station, levels = station, labels = cities))

# QUESTION 4
#> Since we're scientists, let's convert all the temperatures to C
#> Write a function to convert F to C, and then use mutate across to 
#> convert all of the temperatures, rounded to a tenth of a degree


### CHECK YOUR WORK
#> At this point, your data should look like the "compiled_data.csv" file
#> in data-clean. If it isn't, read in that file to use for the remaining
#> questions so that you have the right data to work with.

ds2 <- read_csv("data-clean/compiled_data.csv")

# QUESTION 5
#> Write a function that counts the number of extreme temperature days,
#> where the actual min or max was equal to the (i.e., set the) record min/max
#> A piped function starting with '.' is a good strategy here.
#> Group the dataset by city to see how many extreme days each city experienced,
#> and sort in descending order to show which city had the most:
#> (Seattle, 20, Charlotte 12, Phoenix 12, etc...)
#> Don't save this summary over the original dataset!

#attempt 1 ignore lol
#count_ex_temp <- . %>% 
  #select(height, mass) %>% 
  #summarize(mean_ht = mean_na(height), mean_ms = mean_na(mass))

#ds2 %>% count_ex_temp
#ds2 %>% group_by(city) %>% count_ex_temp

#attempt 2 with madison and friends
ex_temp_days <- 
  . %>%
        mutate(ext_day = actual_min_temp == record_min_temp | 
                 actual_max_temp == record_max_temp,
        ext_day = as.numeric(ext_day)) %>%
        summarize(ext_day = sum(ext_day, na.rm = TRUE))

ds2 %>% 
  group_by(city) %>%
  ex_temp_days %>% 
  arrange(desc(ext_day))

#Madison CODE:
#extreme_days <- . %>% 
  #mutate(is_extreme_day = actual_min_temp == record_min_temp | actual_max_temp == record_max_temp,
         #is_extreme_day = as.numeric(is_extreme_day)) %>% 
  #summarize(n_extreme = sum(is_extreme_day))


# QUESTION 6
#> Pull out the month from the date and make "month" a factor
#> Split the tibble by month into a list of tibbles 

#attempt 1
#ds2 <- ds2 %>% mutate(month = factor(date, labels = c("01","02","03","04","05","06","07","08","09","10","11","12")))
#df_split <- ds2 %>% group_by(month) %>% group_split %>% set_names(levels(ds2$month))
#df_split

#attempt 2
ds2 <- ds2 %>% mutate(month = month(date, label = TRUE))

#Madison Code
#ds_by_month <- ds2 %>% group_by(month) %>% group_split %>% set_names(levels(month))


# QUESTION 7
#> For each month, determine the correlation between the actual_precipitation
#> and the average_precipitation (across all cities), and between the actual and average mins/maxes
#> Use a for loop, and print the month along with the resulting correlation
#> Look at the documentation for the ?cor function if you've never used it before

ds2 %>% select(city, actual_precipitation:average_precipitation) %>% plot_correlation()

ds2 %>% select(city, actual_min_temp, average_min_temp) %>% plot_correlation()

ds2 %>% select(city, actual_max_temp, average_max_temp) %>% plot_correlation()

cor_output <- vector()
for (p in preds) {
  cor_output[p] <- cor(mtcars['mpg'], mtcars[p])
}
cor_output

# QUESTION 8
#> Use the Data Explorer package to plot boxplots of all of the numeric variables in the dataset
#> grouped by city, then do the same thing grouped by month. 
#> Finally, use plot_correlation to investigate correlations between the continuous variables only
#> Check the documentation for plot_correlation for an easy way to do this

#atempt 1:

ds2 %>% 
  plot_boxplot(by = "city")

ds2 %>% 
  plot_boxplot(by = "month")

#Madison code:

plot_boxplot(ds2, by = "city")

# QUESTION 9
#> Create a scatterplot of actual_mean_temp (y axis) by date (x axis)
#> Use facet_wrap to make a separate plot for each city (3 columns)
#> Make the points different colors according to month




# QUESTION 10
#> Write a function that takes the dataset and the abbreviate month as arguments
#> and creates a scatter and line plot of actual temperature (y axis) by date (x axis)
#> Note, just add geom_line() to your ggplot call to get the lines
#> use the ggtitle() function to add the month as a title
#> The function should save the plot as "eda/month_name.png"
#> The eda folder has an example of what each plot should look like
#> Call the function in a map or loop to generate graphs for each month












