rm(list= ls())
library(tidyverse)
library(janitor)

ipeds_2010 <- read_csv("datasets/C2010_A/c2010_a_rv.csv") %>% 
  clean_names() %>% # convert column names to snake case
  mutate(awlevel = as.numeric(awlevel)) # convert awlevel to numeric

cleaned_2010 <- ipeds_2010 %>%
  filter(awlevel == 5) %>% # filtering only bachelor's degrees
  # selecting institution id, cip code (2010 classification, first or 
  # second major, award level, grand total, and imputation variable for grand total
  select(unitid, cipcode, majornum, awlevel, ctotalt, xctotalt) %>% 
  mutate(year = 2010)

ipeds_2011 <- read_csv("datasets/C2011_A/c2011_a_rv.csv") %>% 
  clean_names() %>% # convert column names to snake case
  mutate(awlevel = as.numeric(awlevel)) # convert awlevel to numeric

cleaned_2011 <- ipeds_2011 %>%
  filter(awlevel == 5) %>% # filtering only bachelor's degrees
  # selecting institution id, cip code (2010 classification, first or 
  # second major, award level, grand total, and imputation variable for grand total
  select(unitid, cipcode, majornum, awlevel, ctotalt, xctotalt) %>% 
  mutate(year = 2011)
  
ipeds_2012 <- read_csv("datasets/C2012_A/c2012_a_rv.csv") %>% 
  clean_names() %>% # convert column names to snake case
  mutate(awlevel = as.numeric(awlevel)) # convert awlevel to numeric

cleaned_2012 <- ipeds_2012 %>%
  filter(awlevel == 5) %>% # filtering only bachelor's degrees
  # selecting institution id, cip code (2010 classification, first or 
  # second major, award level, grand total, and imputation variable for grand total
  select(unitid, cipcode, majornum, awlevel, ctotalt, xctotalt) %>% 
  mutate(year = 2012)


ipeds_2013 <- read_csv("datasets/C2013_A/c2013_a_revised.csv") %>%
  clean_names() %>% # convert column names to snake case
  mutate(awlevel = as.numeric(awlevel)) # convert awlevel to numeric

cleaned_2013 <- ipeds_2013 %>%
  filter(awlevel == 5) %>% # filtering only bachelor's degrees
  # selecting institution id, cip code (2010 classification, first or
  # second major, award level, grand total, and imputation variable for grand total
  select(unitid, cipcode, majornum, awlevel, ctotalt, xctotalt) %>% 
  mutate(year = 2013)


ipeds_2014 <- read_csv("datasets/C2014_A/c2014_a_rv.csv") %>% 
  clean_names() %>% # convert column names to snake case
  mutate(awlevel = as.numeric(awlevel)) # convert awlevel to numeric

cleaned_2014 <- ipeds_2014 %>%
  filter(awlevel == 5) %>% # filtering only bachelor's degrees
  # selecting institution id, cip code (2010 classification, first or 
  # second major, award level, grand total, and imputation variable for grand total
  select(unitid, cipcode, majornum, awlevel, ctotalt, xctotalt) %>% 
  mutate(year = 2014)

ipeds_2015 <- read_csv("datasets/C2015_A/c2015_a_rv.csv") %>%
  clean_names() %>% # convert column names to snake case
  mutate(awlevel = as.numeric(awlevel)) # convert awlevel to numeric

cleaned_2015 <- ipeds_2015 %>%
  filter(awlevel == 5) %>% # filtering only bachelor's degrees
  # selecting institution id, cip code (2010 classification, first or 
  # second major, award level, grand total, and imputation variable for grand total
  select(unitid, cipcode, majornum, awlevel, ctotalt, xctotalt) %>% 
  mutate(year = 2015)

ipeds_2016 <- read_csv ("datasets/C2016_A/c2016_a_rv.csv") %>% 
  clean_names() %>% # convert column names to snake case
  mutate(awlevel = as.numeric(awlevel)) # convert awlevel to numeric

cleaned_2016 <- ipeds_2016 %>%
  filter(awlevel == 5) %>% # filtering only bachelor's degrees
  # selecting institution id, cip code (2010 classification, first or 
  # second major, award level, grand total, and imputation variable for grand total
  select(unitid, cipcode, majornum, awlevel, ctotalt, xctotalt) %>% 
  mutate(year = 2016)

ipeds_2017 <- read_csv("datasets/C2017_A/c2017_a_rv.csv") %>%
  clean_names() %>% # convert column names to snake case
  mutate(awlevel = as.numeric(awlevel)) # convert awlevel to numeric

cleaned_2017 <- ipeds_2017 %>%
  filter(awlevel == 5) %>% # filtering only bachelor's degrees
  # selecting institution id, cip code (2010 classification, first or 
  # second major, award level, grand total, and imputation variable for grand total
  select(unitid, cipcode, majornum, awlevel, ctotalt, xctotalt) %>% 
  mutate(year = 2017)


ipeds_2018 <- read_csv("datasets/C2018_A/c2018_a_revised.csv") %>% 
  clean_names() %>% # convert column names to snake case
  mutate(awlevel = as.numeric(awlevel)) # convert awlevel to numeric


cleaned_2018 <- ipeds_2018 %>%
  filter(awlevel == 5) %>% # filtering only bachelor's degrees
  # selecting institution id, cip code (2010 classification, first or 
  # second major, award level, grand total, and imputation variable for grand total
  select(unitid, cipcode, majornum, awlevel, ctotalt, xctotalt) %>% 
  mutate(year = 2018)

ipeds_2019 <- read_csv("datasets/C2019_A/c2019_a_rv.csv") %>%
  clean_names() %>% # convert column names to snake case
  mutate(awlevel = as.numeric(awlevel)) # convert awlevel to numeric

cleaned_2019 <- ipeds_2019 %>%
  filter(awlevel == 5) %>% # filtering only bachelor's degrees
  # selecting institution id, cip code (2010 classification, first or 
  # second major, award level, grand total, and imputation variable for grand total
  select(unitid, cipcode, majornum, awlevel, ctotalt, xctotalt) %>% 
  mutate(year = 2019)


ipeds_2020 <- read_csv("datasets/C2020_A/c2020_a_rv.csv") %>% 
  clean_names() %>% # convert column names to snake case
  mutate(awlevel = as.numeric(awlevel)) # convert awlevel to numeric

cleaned_2020 <- ipeds_2020 %>%
  filter(awlevel == 5) %>% # filtering only bachelor's degrees
  # selecting institution id, cip code (2010 classification, first or 
  # second major, award level, grand total, and imputation variable for grand total
  select(unitid, cipcode, majornum, awlevel, ctotalt, xctotalt) %>% 
  mutate(year = 2020)

ipeds_2021 <- read_csv("datasets/C2021_A/c2021_a.csv") %>%
  clean_names() %>% # convert column names to snake case
  mutate(awlevel = as.numeric(awlevel)) # convert awlevel to numeric

cleaned_2021 <- ipeds_2021 %>%
  filter(awlevel == 5) %>% # filtering only bachelor's degrees
  # selecting institution id, cip code (2020 classification, first or 
  # second major, award level, grand total, and imputation variable for grand total
  select(unitid, cipcode, majornum, awlevel, ctotalt, xctotalt) %>% 
  mutate(year = 2021)


ipeds_2022 <- read_csv("datasets/C2022_A/c2022_a.csv") %>% 
  clean_names() %>% # convert column names to snake case
  mutate(awlevel = as.numeric(awlevel)) # convert awlevel to numeric

cleaned_2022 <- ipeds_2022 %>%
  filter(awlevel == 5) %>% # filtering only bachelor's degrees
  # selecting institution id, cip code (2020 classification, first or 
  # second major, award level, grand total, and imputation variable for grand total
  select(unitid, cipcode, majornum, awlevel, ctotalt, xctotalt) %>% 
  mutate(year = 2022)

ipeds_2023 <- read_csv("datasets/c2023_a.csv") %>% 
  clean_names() %>% # convert column names to snake case
  mutate(awlevel = as.numeric(awlevel)) # convert awlevel to numeric

cleaned_2023 <- ipeds_2023 %>%
  filter(awlevel == 5) %>% # filtering only bachelor's degrees
  # selecting institution id, cip code (2020 classification, first or 
  # second major, award level, grand total, and imputation variable for grand total
  select(unitid, cipcode, majornum, awlevel, ctotalt, xctotalt) %>% 
  mutate(year = 2023)

rm(ipeds_2010, ipeds_2011, ipeds_2012, ipeds_2013, ipeds_2014, ipeds_2015, 
   ipeds_2016, ipeds_2017, ipeds_2018, ipeds_2019, ipeds_2020, ipeds_2021, ipeds_2022, ipeds_2023)


# cip codes that changed from 2010 classification to 2020 classification
crosswalk <- read_csv("info_files/Crosswalk2010_2020.csv") %>% 
  clean_names() %>% 
  mutate(original_code = str_extract(original_code, "\\d{2}\\.\\d{4}"),
         current_code = str_extract(current_code, "\\d{2}\\.\\d{4}"))


crosswalk_2020 <- cleaned_2010 %>% 
  bind_rows(cleaned_2011) %>%
  bind_rows(cleaned_2012) %>%
  bind_rows(cleaned_2013) %>%
  bind_rows(cleaned_2014) %>%
  bind_rows(cleaned_2015) %>%
  bind_rows(cleaned_2016) %>%
  bind_rows(cleaned_2017) %>%
  bind_rows(cleaned_2018) %>%
  bind_rows(cleaned_2019) %>%
  left_join(crosswalk, by = join_by(cipcode == original_code),
            relationship = "many-to-one") %>% 
  select(-original_title, -current_title) %>% 
# cip codes that changed from 2010 classification to 2020 classification
# will be changed to 2020 codes
  mutate(cipcode = case_when(
    action == "Moved to" ~ current_code,
    .default = cipcode)) %>% 
  select(-current_code, -action, -text_change) 

totals <- crosswalk_2020 %>% 
  bind_rows(cleaned_2020) %>% 
  bind_rows(cleaned_2021) %>%
  bind_rows(cleaned_2022) %>% 
  bind_rows(cleaned_2023) %>% 
  summarize(total_6 = sum(ctotalt, na.rm = T),
            .by= c(unitid,cipcode, year)) %>% 
  mutate(cip_4 = substr(cipcode, 1, 5),
         cip_2 = substr(cipcode, 1, 2)) %>%  # extract 4 digit and 2 digit cip code
  mutate(total_4 = sum(total_6, na.rm = T), .by = c(year, unitid, cip_4)) %>% 
  mutate(total_2 = sum(total_6, na.rm = T), .by = c(year, unitid, cip_2)) %>% 
  distinct(unitid, cip_4, year, total_4, total_2) 



# institution directory for 2022-2023
directory <- read_csv("info_files/hd2022.csv") %>% 
  clean_names() %>% 
  select(instnm, unitid, stabbr, city, newid, control) %>% 
  rename(institution_name = instnm,
         state = stabbr) 

# 51 Theological seminaries and other specialized faith-related institutions:
# These institutions primarily offer religious instruction or train members of the clergy.
# exclude religious seminaries
exclude_religious <- read_csv("info_files/hd2022.csv") %>% 
  clean_names() %>% 
  # classified as religious based on 2019-2020 data
  filter(c21basic == 24) %>% 
  select(instnm, unitid, newid, carnegie, ccbasic, c15basic, c18basic, c21basic)


# do not include for-profit or fully remote instruction institutions in degrees_by_year
not_included <- read_csv("info_files/IC2022/ic2022_rv.csv") %>% 
  clean_names() %>% 
  # 2 = private for-profit, distanced = 1 = fully distance education
  filter(cntlaffi == 2 | distnced == 1) %>% 
  select(unitid, cntlaffi, distnced) %>% 
  left_join(directory, by = join_by(unitid))


  
degrees_by_year <- totals %>% 
  left_join(directory, by = join_by(unitid),
            relationship = "many-to-one") %>% 
  # control == 3 is for-profit
  filter(!unitid %in% not_included$unitid | !unitid %in% exclude_religious$unitid | control != 3) %>% 
  mutate(unitid = if_else(str_detect(newid, "^\\d{2,}"), newid, unitid)) %>% 
  select(-newid) %>% 
  #filter out any observations that don't have data for any year
  # filter(if_any(starts_with("total_6_"), ~ . > 0)) %>% 
  write_csv("degrees_by_year.csv") 


# institutions_missing <- degrees_by_year %>% filter(is.na(institution_name)) %>%
#   distinct(unitid)