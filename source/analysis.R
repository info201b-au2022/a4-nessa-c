library(tidyverse)
library(dplyr)
library(ggplot2)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
incarceration_df <- get_data()
View(incarceration_df)
no_jail_states <- states_with_no_jail_pop(incarceration_df)
highest_total_pop <- incarceration_df %>%
  slice(which.max(total_pop)) %>%
  pull(state, county_name)
highest_jail_pop_overall <- incarceration_df %>%
  slice(which.max(total_jail_pop)) %>%
  summarize(year, state, county_name, total_pop, total_jail_pop)
highest_jail_pop_21st_century <- incarceration_df %>%
  filter(year >= 2000) %>%
  slice(which.max(total_jail_pop)) %>%
  summarize(year, state, county_name, total_pop, total_jail_pop)
ratio_jail_pop_total_2018 <- total_jail_pop_country_2018 / total_pop_country_2018
CA_jail_pop_years <- incarceration_df %>%
  filter(state == "CA") %>%
  group_by(year) %>%
  mutate(total_jail_pop_state = sum(total_jail_pop, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(year, total_jail_pop_state) %>%
  mutate(change_in_jail_pop = total_jail_pop_state - lag(total_jail_pop_state))
LA_jail_pop_years <- incarceration_df %>%
  filter(county_name == "Los Angeles County") %>%
  distinct(year, total_jail_pop) %>%
  mutate(change_in_jail_pop = total_jail_pop - lag(total_jail_pop))
average_change_LA_jail_pop <- mean(LA_jail_pop_years$change_in_jail_pop, na.rm = TRUE)
average_change_CA_jail_pop <- mean(CA_jail_pop_years$change_in_jail_pop, na.rm = TRUE)

#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function returns a data frame of the total jail population per year
get_year_jail_pop <- function() {
  country_totals <- incarceration_df %>%
    group_by(year) %>%
    mutate(total_country_jail_pop = sum(total_jail_pop, na.rm = TRUE)) %>%
    ungroup() %>%
    distinct(year, total_country_jail_pop)
  return(country_totals)   
}

# This function returns a bar chart of the US prison population over the years
plot_jail_pop_for_us <- function()  {
  country_totals <- get_year_jail_pop()
  jail_over_years_chart <- 
    ggplot(country_totals, 
           aes(x = year, y = total_country_jail_pop)) + 
    geom_col(fill = "#FFBCD1") +
    labs(y = "Jailed Population", x = "Years") +
    ggtitle("US Prison Population from 1970 to 2018")
  return(jail_over_years_chart)   
} 

chart <- plot_jail_pop_for_us()
## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
# This function returns a data frame of the total 
# jail population per state each year
get_jail_pop_by_states <- function(states) {
  jail_pop_states <- incarceration_df %>%
    distinct(year)
  for (item in states) {
    new_column <- incarceration_df %>%
      filter(state == item) %>%
      group_by(year) %>%
      mutate(temp = sum(total_jail_pop, na.rm = TRUE)) %>%
      ungroup() %>%
      distinct(year, temp)
    jail_pop_states <- jail_pop_states %>%
      add_column(!!item := new_column$temp)
  }
  jail_pop_states <- jail_pop_states %>%
    select(states, year) %>%
    gather(key = "State", value = "jail_pop", -year)
  return(jail_pop_states)   
}

# This function returns a line chart of the total 
# jail population per state each year
plot_jail_pop_by_states <- function(states) {
  df <- get_jail_pop_by_states(c("WA", "CT", "NY"))
  state_chart <-
    ggplot(df, aes(x = year, y = jail_pop)) +
    ggtitle("US Prison Population from 1970 to 2018 (by State)") +
    geom_line(aes(color = State)) +
    labs(x = "Year", y = "Jail Population")

  return(state_chart)
}
test_chart <- plot_jail_pop_by_states(c("WA", "CT", "NY"))
## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
get_jail_pop_by_race <- function(state_focus, races) {
  jail_pop_races <- incarceration_df %>%
    select(year, state, races, total_jail_pop) %>%
    filter(state == state_focus) %>%
    group_by(year) %>%
    summarize_if(is.numeric, sum, na.rm=TRUE) %>%
    ungroup() %>%
    distinct(year, .keep_all = TRUE) %>%
    filter(year > 1984) %>%
    select(total_jail_pop, races, year) %>%
    gather(key = "Races", value = "jail_pop", -year) %>%
    mutate(Races = recode(Races, 
                          aapi_jail_pop = 'AAPI',
                          black_jail_pop = 'Black',
                          latinx_jail_pop =  'Latinx',
                          native_jail_pop = "Native",
                          white_jail_pop = "White",
                          total_jail_pop = "Total Population"))
  return(jail_pop_races)
}

# This function returns a line chart of the total state
# jail population per race (must be specified in a vector) each year
plot_jail_pop_by_race <- function(state_focus) {
  races <- c("aapi_jail_pop", 
             "black_jail_pop", 
             "latinx_jail_pop", 
             "native_jail_pop", 
             "white_jail_pop")
  df <- get_jail_pop_by_race(state_focus, races)
  chart <-
    ggplot(df, aes(x = year, y = jail_pop)) +
    ggtitle(paste(state_focus, "Prison Population from 1984 to 2018 (by Race)")) +
    geom_line(aes(color = Races)) +
    labs(x = "Year", y = "Jail Population")
  
  return(chart)
}

test <- plot_jail_pop_by_race("WA")
## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


