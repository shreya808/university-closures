rm(list= ls())
library(tidyverse)
library(janitor)
library(usmap)
library(ggplot2)

degrees_by_year <- read_csv("degrees_by_year.csv") %>% 
  filter(!is.na(institution_name)) 

 
#majors removed/deleted from 2020 and/or 2010 cip codes
# deleted_majors <- c(51.16, 23.1001, 05.9999, 51.1699, 23.0401, 29.0101,
# )

#summarize number of schools, number of degrees awarded by year in each state
state_stats <- degrees_by_year %>% 
  #because data is arranged at the 6-digit code level
  summarize(degrees_state_2007 = sum(total_6_2007), 
             degrees_state_2013 = sum(total_6_2013),
             degrees_state_2018 = sum(total_6_2018),
             degrees_state_2023 = sum(total_6_2023),
            # only count number of schools that awarded degrees in each year, i.e. total_degrees (each school) > 0
             number_of_schools_2007 = sum(total_degrees_2007 > 0), 
             number_of_schools_2013 = sum(total_degrees_2013 > 0),
             number_of_schools_2018 = sum(total_degrees_2018 > 0),
             number_of_schools_2023 = sum(total_degrees_2023 > 0),
             .by = state)
            
#average number of schools that discontinued majors in each state


#number of schools with major by year and state
majors_discontinued_state <- degrees_by_year %>% 
  select(-(starts_with("total_2"))) %>% 
  #summarize at 4-digit code level
  distinct(unitid, cip_4, total_4_2007, total_4_2013, total_4_2018, total_4_2023, 
           institution_name, state, .keep_all = T) %>% 
  summarize(
  #number of schools that didn't award degrees for 4-digit cip code in 2023 and 2018 given that they awarded degrees in 2007
    major_discontinued_2023_count = sum((total_4_2023 == 0 & total_4_2018 == 0 & total_4_2007 > 0)|
                                     (total_4_2023 == 0 & total_4_2018 > 0 & total_4_2007 > 0)),
    new_schools_offering = sum(total_4_2023 > 0 & total_4_2007 == 0),
    prev_schools_offering = sum(total_4_2007 > 0 & total_4_2023 == 0),
    # number of institutions in each state that offer this major based on if they awarded degree for major in that year (4-digit))
    number_schools_major_2007 = sum(total_4_2007 > 0),
    number_schools_major_2013 = sum(total_4_2013 > 0),
    number_schools_major_2018 = sum(total_4_2018 > 0),
    number_schools_major_2023 = sum(total_4_2023 > 0),
    .by = c(state, cip_4, total_4_2013))
  

majors_discontinued_tot <- majors_discontinued_state %>% 
  # count number of schools that have discontinued this major in all states
  summarize(us_major_discontinued_2023_count = sum(major_discontinued_2023_count),
            us_schools_major_2007 = sum(number_schools_major_2007), 
            us_schools_major_2013 = sum(number_schools_major_2013),
            us_schools_major_2018 = sum(number_schools_major_2018),
            us_schools_major_2023 = sum(number_schools_major_2023),
            .by = c(cip_4))
  # percentage of schools that have discontinued major out of all schools that offered major
  # mutate(percent_discontinued = total_major_discontinued_count/total_number_of_schools_major)

percentage_closures <- majors_discontinued_state %>% 
  left_join(state_stats, by = join_by(state)) %>% 
  summarize(percent_closed = mean(major_discontinued_2023_count, na.rm = T),
                                             .by = c(state))
  
            



plot_closures <- plot_usmap(data = percentage_closures, values = "percent_closed") +
  scale_fill_gradient(name = "Percentage of Schools that Discontinued Major", label = scales::percent, low = "#f8c9c1", high = "#e24b03")
plot_closures

#13.12 cip in pennsylvania moved to not sure what