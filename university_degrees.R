rm(list= ls())
library(tidyverse)
library(janitor)

ipeds_2007 <- read_csv("C2007_A/c2007_a_revised.csv") %>% 
  clean_names() %>% # convert column names to snake case
  mutate(awlevel = as.numeric(awlevel)) # convert awlevel to numeric
  
cleaned_2007 <- ipeds_2007 %>% 
  filter(awlevel == 5) %>% # filtering only bachelor's degrees
  # selecting institution id, cip code (2000 classification, first or 
  # second major, award level, grand total, and imputation variable for grand total
  select(unitid, cipcode, majornum, awlevel, crace24, xcrace24)

ipeds_2013 <- read_csv("C2013_A/c2013_a_revised.csv") %>% 
  clean_names() %>% # convert column names to snake case
  mutate(awlevel = as.numeric(awlevel)) # convert awlevel to numeric

cleaned_2013 <- ipeds_2013 %>%
  filter(awlevel == 5) %>% # filtering only bachelor's degrees
  # selecting institution id, cip code (2010 classification, first or 
  # second major, award level, grand total, and imputation variable for grand total
  select(unitid, cipcode, majornum, awlevel, ctotalt, xctotalt)

ipeds_2018 <- read_csv("C2018_A/c2018_a_revised.csv") %>% 
  clean_names() %>% # convert column names to snake case
  mutate(awlevel = as.numeric(awlevel)) # convert awlevel to numeric


cleaned_2018 <- ipeds_2018 %>%
  filter(awlevel == 5) %>% # filtering only bachelor's degrees
  # selecting institution id, cip code (2010 classification, first or 
  # second major, award level, grand total, and imputation variable for grand total
  select(unitid, cipcode, majornum, awlevel, ctotalt, xctotalt)

ipeds_2023 <- read_csv("c2023_a.csv") %>% 
  clean_names() %>% # convert column names to snake case
  mutate(awlevel = as.numeric(awlevel)) # convert awlevel to numeric

cleaned_2023 <- ipeds_2023 %>%
  filter(awlevel == 5) %>% # filtering only bachelor's degrees
  # selecting institution id, cip code (2020 classification, first or 
  # second major, award level, grand total, and imputation variable for grand total
  select(unitid, cipcode, majornum, awlevel, ctotalt, xctotalt)

rm(ipeds_2007, ipeds_2013, ipeds_2018, ipeds_2023)


totals_2007 <- cleaned_2007 %>% 
  # add totals for first and second major
  summarize(total_6 = sum(crace24), .by = c(unitid, cipcode)) %>% 
  mutate(year = 2007,
         cip_4 = substr(cipcode, 1, 5),
         cip_2 = substr(cipcode, 1, 2)) %>% # extract 4 digit and 2 digit cip code
  mutate(total_2 = sum(total_6), .by = c(unitid, cip_2)) %>% # total 2 digit
  mutate(total_4 = sum(total_6), .by = c(unitid, cip_4))

totals_2013 <- cleaned_2013 %>% 
  # add totals for first and second major
  summarize(total_6 = sum(ctotalt), .by = c(unitid, cipcode)) %>% 
  mutate(year = 2013,
         cip_4 = substr(cipcode, 1, 5),
         cip_2 = substr(cipcode, 1, 2)) %>% # extract 4 digit and 2 digit cip code
  mutate(total_2 = sum(total_6), .by = c(unitid, cip_2)) %>% # total 2 digit 
  mutate(total_4 = sum(total_6), .by = c(unitid, cip_4))

totals_2018 <- cleaned_2018 %>%
  # add totals for first and second major
  summarize(total_6 = sum(ctotalt), .by = c(unitid, cipcode)) %>% 
  mutate(year = 2018,
         cip_4 = substr(cipcode, 1, 5),
         cip_2 = substr(cipcode, 1, 2)) %>% # extract 4 digit and 2 digit cip code
  mutate(total_2 = sum(total_6), .by = c(unitid, cip_2)) %>% # total 2 digit
  mutate(total_4 = sum(total_6), .by = c(unitid, cip_4))

totals_2023 <- cleaned_2023 %>%
  # add totals for first and second major
  summarize(total_6 = sum(ctotalt), .by = c(unitid, cipcode)) %>% 
  mutate(year = 2023,
         cip_4 = substr(cipcode, 1, 5),
         cip_2 = substr(cipcode, 1, 2)) %>% # extract 4 digit and 2 digit cip code
  mutate(total_2 = sum(total_6), .by = c(unitid, cip_2)) %>% # total 2 digit
  mutate(total_4 = sum(total_6), .by = c(unitid, cip_4))

degrees_by_year <- totals_2007 %>% 
  bind_rows(totals_2013) %>% 
  bind_rows(totals_2018) %>% 
  bind_rows(totals_2023) %>% 
  pivot_wider(names_from = c(year), values_from = c(total_6, total_2, total_4),
               values_fill = 0)  # use 0 values for missing values in next years


institution_names <- read_csv("institution_names.csv") %>% #list of institutions by unit id
  distinct()

degrees_by_year <- degrees_by_year %>% 
  left_join(institution_names, by = join_by(unitid == UnitID),
            relationship = "many-to-one") %>% 
  clean_names()

majors_discontinued <- degrees_by_year %>% 
  # look only at programs that don't have any people in 2-digit cip major in 2023
  filter(total_2_2023 == 0) 

# first 140 are unknown
institutions_missing <- degrees_by_year %>% filter(is.na(institution_name)) %>% distinct(unitid)
