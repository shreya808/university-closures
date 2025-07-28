rm(list= ls())
library(tidyverse)
library(janitor)
library(usmap)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(readxl)
library(scales)


# cip codes and descriptions
cip_codes <- read_csv("info_files/CIPCode2020.csv") %>% 
  clean_names() %>% 
  # take out punctuation from codes
  mutate(cip_family = str_extract(cip_family, "\\d{2}"),
         cip_code = gsub('="', '', cip_code)) %>% 
  mutate(cip_code = gsub('"', '', cip_code))


# map CIP codes to disciplines from Carnegie classification
cip_discipline <- read_excel("info_files/CCIHE2021-CIPMAP.xlsx") %>% 
  clean_names() %>% 
  rename(cip_code = cipcd,
         discipline = grad_cat) %>% 
  mutate(cip_code = as.character(cip_code)) %>% 
  mutate(cip_code = case_when(
    # make cip codes in standard format
    str_detect(cip_code, "^\\d{1}$") ~ paste0("0", cip_code, ".00"),
    str_detect(cip_code, "^\\d{1}\\.") ~ paste0("0", cip_code),
    str_detect(cip_code, "^\\d{2}$") ~ paste0(cip_code, ".00"),
    .default = cip_code)) %>% 
  # extract 4 digit cip codes
  mutate(cip_code = str_extract(cip_code, "^\\d{2}\\.\\d{2}")) %>% 
  distinct(cip_code, .keep_all = TRUE) %>% 
  select(-c(bacc_cat, assoc_cat)) %>% 
  mutate(discipline = case_match(discipline,
                                 1 ~ "Humanities",
                                 2 ~ "Social Sciences",
                                 3 ~ "STEM",
                                 4 ~ "All other disciplines",
                                 .default = "All other disciplines"))
         

# only 4 digit descriptions
cip_codes_4 <- cip_codes %>% 
  filter(str_length(cip_code) == 5) %>% 
  left_join(cip_discipline, by = c("cip_code" = "cip_code"))

# only 2 digit descriptions
cip_codes_2 <- cip_codes %>% 
  filter(str_length(cip_code) == 2)

# majors that are cross-referenced by different 2-digit cip
cross_ref_cip <- cip_codes %>% 
  filter(!is.na(cross_references)) %>% 
  mutate(cross_ref = str_extract_all(cross_references, "\\d{2}\\.\\d{2}"),
         cip_code = str_extract(cip_code, "\\d{2}\\.\\d{2}")) %>% 
  mutate(cross_ref = str_remove_all(cross_ref, 'c\\(\"|\"\\,|\"\\)|\"')) %>% 
  separate_wider_regex(cross_ref,
                       c(ref_1 = "\\d{2}\\.\\d{2}", "\\s*",
                         ref_2 = "\\d{2}\\.\\d{2}", "\\s*",
                         ref_3 = "\\d{2}\\.\\d{2}", "\\s*",
                         ref_4 = "\\d{2}\\.\\d{2}", "\\s*",
                         ref_5 = "\\d{2}\\.\\d{2}", "\\s*",
                         ref_6 = "\\d{2}\\.\\d{2}"),
                         too_few = "align_start") %>% 
  pivot_longer(cols = starts_with("ref_"), names_to = NULL, values_to = "ref_cip", values_drop_na = TRUE) %>% 
  distinct(cip_code, ref_cip, .keep_all = TRUE) %>% 
  select(-c(action, text_change, examples))

degrees_by_year_short <- read_csv("degrees_by_year.csv") %>% 
  filter(!is.na(institution_name)) %>% 
  left_join(cip_codes_4, by = c("cip_4" = "cip_code"))

degrees_by_year_long <- degrees_by_year_short %>% 
  # use to find if major is discontinued
  # total__ will show NA if the major was not reported in the original data for that year
  pivot_wider(names_from = year, values_from = c(total_4, total_2),
            names_glue = "{year}_{.value}",
            names_sort = T) 
  
# number of 4-digit major degrees each year by college
num_majors_college <- degrees_by_year_short %>% 
  distinct(cip_4, unitid, institution_name, state, year) %>% 
  filter(cip_4 != 99) %>% 
  summarize(majors_offered = n(), .by = c(unitid, institution_name, state, year))

# number of schools per year

num_schools_year <- degrees_by_year_short %>% 
  distinct(unitid, institution_name, state, year) %>% 
  summarize(n = n(), .by = c(year)) %>% 
  arrange(year)

# total number of degrees awarded each year by college
# number of 4-digit majors offered by college each year
year_total_degrees_1 <- degrees_by_year_short %>% 
  filter(cip_4 == 99) %>% 
  rename(total_degrees = total_4) %>%
  select(unitid, year, institution_name, state, total_degrees) %>% 
  left_join(num_majors_college, by = c("unitid", "institution_name", "state", "year"))

rm(num_majors_college)

national_total_degrees <- year_total_degrees_1 %>% 
  summarize(total_degrees = sum(total_degrees), 
            .by = c(year)) 

# institutions percentage of degrees awarded out of total US degrees awarded by year
year_total_degrees <- year_total_degrees_1 %>% 
  left_join(national_total_degrees, by = join_by(year)) %>% 
  rename(total_degrees_national = total_degrees.y,
         total_degrees_inst = total_degrees.x) %>% 
  mutate(percent_of_national = 100* total_degrees_inst / total_degrees_national) %>% 
  arrange(desc(percent_of_national))

rm(year_total_degrees_1)

# total degrees awarded each year by major nationally
major_total_by_year <- degrees_by_year_short %>% 
  filter(cip_4 != 99) %>% 
  left_join(year_total_degrees, by = c("unitid", "year", "institution_name", "state")) %>% 
  select(-c(action, text_change, cip_definition,cross_references, examples)) %>% 
  summarize(n_schools = n(),
            # find number of schools offering but weighted by the proportion of 
            # students that were awarded degrees at institution
            n_schools_weighted = sum(total_degrees_inst/total_degrees_national),
            total_4 = sum(total_4), 
            .by = c(year, cip_4, cip_family, discipline, cip_title, total_degrees_national)) %>% 
  mutate(n_schools_weighted = n_schools_weighted * n_schools) %>% 
  # percentage of schools weighted
  left_join(num_schools_year, by = join_by(year)) %>% 
  rename(total_schools_national = n) %>% 
  mutate(percent_schools = 100* n_schools/total_schools_national,
         percent_of_national_degrees = 100 * total_4 /total_degrees_national)

missing_majors <- degrees_by_year_long %>% 
  filter(cip_4 != 99) %>% 
  # look for 4-digit majors that are missing for either 2010 or 2023
  filter(is.na(`2010_total_4`) | is.na(`2023_total_4`)) %>% 
  distinct(cip_4, institution_name, state, unitid, .keep_all = T)

# majors missing in 2023 (most recent year)
discontinued_majors <- missing_majors %>% 
  select(-text_change) %>% 
  filter(is.na(`2023_total_4`)) %>% 
  # find year and number of degrees awarded (for discontinued major) in last year major is offered
  mutate(last_year_awarded = case_when(
    !is.na(`2022_total_4`) ~ 2022,
    !is.na(`2021_total_4`) ~ 2021,
    !is.na(`2020_total_4`) ~ 2020,
    !is.na(`2019_total_4`) ~ 2019,
    !is.na(`2018_total_4`) ~ 2018,
    !is.na(`2017_total_4`) ~ 2017,
    !is.na(`2016_total_4`) ~ 2016,
    !is.na(`2015_total_4`) ~ 2015,
    !is.na(`2014_total_4`) ~ 2014,
    !is.na(`2013_total_4`) ~ 2013,
    !is.na(`2012_total_4`) ~ 2012,
    !is.na(`2011_total_4`) ~ 2011,
    !is.na(`2010_total_4`) ~ 2010
  )) %>% 
  mutate(degrees_in_last_year = case_when(
    last_year_awarded == 2022 ~ `2022_total_4`,
    last_year_awarded == 2021 ~ `2021_total_4`,
    last_year_awarded == 2020 ~ `2020_total_4`,
    last_year_awarded == 2019 ~ `2019_total_4`,
    last_year_awarded == 2018 ~ `2018_total_4`,
    last_year_awarded == 2017 ~ `2017_total_4`,
    last_year_awarded == 2016 ~ `2016_total_4`,
    last_year_awarded == 2015 ~ `2015_total_4`,
    last_year_awarded == 2014 ~ `2014_total_4`,
    last_year_awarded == 2013 ~ `2013_total_4`,
    last_year_awarded == 2012 ~ `2012_total_4`,
    last_year_awarded == 2011 ~ `2011_total_4`,
    last_year_awarded == 2010 ~ `2010_total_4`
  )) %>% 
  select(-cross_references, -examples) %>% 
  # find total number of degrees awarded across college for year that major is discontinued
  left_join(year_total_degrees, by = c("unitid", "institution_name", "state", "last_year_awarded" = "year")) %>% 
  rename(majors_offered_in_last_year = majors_offered) %>% 
  select(-percent_of_national) %>% 
  mutate(percent_inst_discontinued = 100* degrees_in_last_year/total_degrees_inst) %>% 
  # join table with total number of degrees awarded for major nationally
  left_join(major_total_by_year, by = c("cip_4", "cip_family", "cip_title", "last_year_awarded" = "year", "total_degrees_national", "discipline")) %>% 
  rename(total_4_national = total_4) %>% 
  # find percent of major degrees in 4-digit code among national total 4
  mutate(percent_of_national_4 = 100* degrees_in_last_year/total_4_national) %>% 
  relocate(percent_inst_discontinued, .after = total_degrees_inst) %>% 
  select(-ends_with('total_2'))
# 
# discontinued_majors_filtered <- discontinued_majors %>% 
#   # filter out majors that were discontinued due to lack of people in major
#   filter(percent_discontinued > 0) %>% 
#   # filter out 'other' majors (cip code is 99)
#   filter(cip_4 != 99) %>% 
#   # filter over 100 degrees awarded in last year major was offered
#   filter(total_degrees_in_last_year > 100) %>% 
#   arrange(desc(degrees_in_last_year))
# 

# # percentage of majors discontinued that were offered for each college
# percent_discontinued_college <- number_discontinued_college %>% 
#   left_join(num_majors_college, by = c("unitid", "institution_name", "state")) %>% 
#   mutate(percent_discontinued = discontinued/offered) %>% 
#   arrange(desc(percent_discontinued))


# 
# # number of colleges that discontinued major by state
# discontinued_majors_count_state <- discontinued_majors %>% 
#   summarize(n = n(), .by = c(cip_4, state, cip_title, action)) %>% 
#   arrange(desc(n))

# number of colleges that discontinued major nationally

# for info on teaching majors going down
# https://www.k12dive.com/news/harvard-dissolves-undergrad-teacher-education-program/621976/

absorbing_majors <- degrees_by_year_long %>% 
  filter(cip_4 != 99) %>% 
  filter(!is.na(`2010_total_4`) & !is.na(`2011_total_4`) &
           !is.na(`2012_total_4`) & !is.na(`2013_total_4`) &
           !is.na(`2014_total_4`) & !is.na(`2015_total_4`) &
           !is.na(`2016_total_4`) & !is.na(`2017_total_4`) &
           !is.na(`2018_total_4`) & !is.na(`2019_total_4`) &
           !is.na(`2020_total_4`) & !is.na(`2021_total_4`) &
           !is.na(`2022_total_4`) & !is.na(`2023_total_4`)) %>% 
  distinct(cip_4, institution_name, state, unitid, .keep_all = T) %>% 
  select(-text_change, -cross_references, -examples) %>% 
  mutate(first_year_awarded = 2010,
         degrees_in_first_year = `2010_total_4`) %>% 
  left_join(year_total_degrees, by = c("unitid", "institution_name", "state", "first_year_awarded" = "year")) %>% 
  select(-total_degrees_national, -percent_of_national) %>% 
  rename(majors_offered_in_first_year = majors_offered) %>% 
  select(-c(`2010_total_2`, `2011_total_2`, `2012_total_2`, `2013_total_2`,
           `2014_total_2`, `2015_total_2`, `2016_total_2`, `2017_total_2`,
           `2018_total_2`, `2019_total_2`, `2020_total_2`, `2021_total_2`,
           `2022_total_2`, `2023_total_2`))

added_majors <- missing_majors %>% 
  #filtering out majors that were added and discontinued within 2010-2023
  filter(is.na(`2010_total_4`) & !is.na(`2023_total_4`)) %>% 
  mutate(first_year_awarded = case_when(
    !is.na(`2011_total_4`) ~ 2011,
    !is.na(`2012_total_4`) ~ 2012,
    !is.na(`2013_total_4`) ~ 2013,
    !is.na(`2014_total_4`) ~ 2014,
    !is.na(`2015_total_4`) ~ 2015,
    !is.na(`2016_total_4`) ~ 2016,
    !is.na(`2017_total_4`) ~ 2017,
    !is.na(`2018_total_4`) ~ 2018,
    !is.na(`2019_total_4`) ~ 2019,
    !is.na(`2020_total_4`) ~ 2020,
    !is.na(`2021_total_4`) ~ 2021,
    !is.na(`2022_total_4`) ~ 2022,
    !is.na(`2023_total_4`) ~ 2023
  )) %>% 
  mutate(degrees_in_first_year = case_when(
    first_year_awarded == 2023 ~ `2023_total_4`,
    first_year_awarded == 2022 ~ `2022_total_4`,
    first_year_awarded == 2021 ~ `2021_total_4`,
    first_year_awarded == 2020 ~ `2020_total_4`,
    first_year_awarded == 2019 ~ `2019_total_4`,
    first_year_awarded == 2018 ~ `2018_total_4`,
    first_year_awarded == 2017 ~ `2017_total_4`,
    first_year_awarded == 2016 ~ `2016_total_4`,
    first_year_awarded == 2015 ~ `2015_total_4`,
    first_year_awarded == 2014 ~ `2014_total_4`,
    first_year_awarded == 2013 ~ `2013_total_4`,
    first_year_awarded == 2012 ~ `2012_total_4`,
    first_year_awarded == 2011 ~ `2011_total_4`,
    first_year_awarded == 2010 ~ `2010_total_4`
  )) %>% 
  select(-cross_references, -examples, -text_change) %>% 
  # find total number of degrees awarded across college for year that major is discontinued
  left_join(year_total_degrees, by = c("unitid", "institution_name", "state", "first_year_awarded" = "year")) %>% 
  select(-total_degrees_national, -percent_of_national) %>% 
  rename(majors_offered_in_first_year = majors_offered) %>% 
  select(-c(`2010_total_2`, `2011_total_2`,
            `2012_total_2`, `2013_total_2`,`2014_total_2`,
           `2015_total_2`, `2016_total_2`, `2017_total_2`,
           `2018_total_2`, `2019_total_2`, `2020_total_2`,
           `2021_total_2`, `2022_total_2`, `2023_total_2`))
  
# combining absorbing majors and added majors
absorbing_added <- added_majors %>% 
  rows_insert(absorbing_majors, 
             by = c("unitid", "cip_4", "institution_name", "city", "state", "cip_family",
                 "cip_title", "2010_total_4", "2012_total_4", "2014_total_4",
                 "2016_total_4", "2018_total_4", "2020_total_4",
                 "2022_total_4", "2023_total_4", "first_year_awarded",
                 "degrees_in_first_year", "total_degrees_inst",
                 "majors_offered_in_first_year"
                 ))

rm(absorbing_majors)

# Step 1: Join for original cip_code match
originals <- degrees_by_year_long %>%
  select(-cross_references, -examples, -action, -text_change, -ends_with("total_2")) %>% 
  inner_join(cross_ref_cip, by = c("cip_4" = "cip_code", "cip_family"), relationship = "many-to-many") %>% 
  rename(cip_code = cip_4)

# Step 2: Join for cross-reference match
cross_refs <- degrees_by_year_long %>%
  select(-cross_references, -examples, -action, -text_change, -ends_with("total_2")) %>% 
  inner_join(cross_ref_cip %>% select(ref_cip), by = c("cip_4" = "ref_cip"), relationship = "many-to-many") %>% 
  rename(ref_cip = cip_4) %>% 
  select(unitid, ref_cip)

# Keep only unitid/cip_code pairs where both original and cross-ref exist
valid_pairs <- inner_join(
  originals, cross_refs, 
  by = c("unitid", "ref_cip")) %>% 
  select(unitid, cip_code, ref_cip) %>% 
  distinct()

# filter discontinued majors such that if college has the cross referenced major still then it should be removed

#joining absorbing, added and cross referenced majors
absorbing_added_cross <- degrees_by_year_long %>% 
  semi_join(valid_pairs, by = c("unitid", "cip_4" = "ref_cip")) %>% 
  filter(!is.na(`2023_total_4`)) %>% 
  mutate(first_year_awarded = case_when(
    !is.na(`2011_total_4`) ~ 2011,
    !is.na(`2012_total_4`) ~ 2012,
    !is.na(`2013_total_4`) ~ 2013,
    !is.na(`2014_total_4`) ~ 2014,
    !is.na(`2015_total_4`) ~ 2015,
    !is.na(`2016_total_4`) ~ 2016,
    !is.na(`2017_total_4`) ~ 2017,
    !is.na(`2018_total_4`) ~ 2018,
    !is.na(`2019_total_4`) ~ 2019,
    !is.na(`2020_total_4`) ~ 2020,
    !is.na(`2021_total_4`) ~ 2021,
    !is.na(`2022_total_4`) ~ 2022,
    !is.na(`2023_total_4`) ~ 2023
  )) %>% 
  mutate(degrees_in_first_year = case_when(
    first_year_awarded == 2023 ~ `2023_total_4`,
    first_year_awarded == 2022 ~ `2022_total_4`,
    first_year_awarded == 2021 ~ `2021_total_4`,
    first_year_awarded == 2020 ~ `2020_total_4`,
    first_year_awarded == 2019 ~ `2019_total_4`,
    first_year_awarded == 2018 ~ `2018_total_4`,
    first_year_awarded == 2017 ~ `2017_total_4`,
    first_year_awarded == 2016 ~ `2016_total_4`,
    first_year_awarded == 2015 ~ `2015_total_4`,
    first_year_awarded == 2014 ~ `2014_total_4`,
    first_year_awarded == 2013 ~ `2013_total_4`,
    first_year_awarded == 2012 ~ `2012_total_4`,
    first_year_awarded == 2011 ~ `2011_total_4`,
    first_year_awarded == 2010 ~ `2010_total_4`
  )) %>% 
  select(-cross_references, -examples, -text_change) %>% 
  # find total number of degrees awarded across college for year that major is discontinued
  left_join(year_total_degrees, by = c("unitid", "institution_name", "state", "first_year_awarded" = "year")) %>% 
  select(-total_degrees_national, -percent_of_national) %>% 
  rename(majors_offered_in_first_year = majors_offered) %>% 
  select(-c(ends_with("total_2"))) %>% 
  rows_insert(absorbing_added, by = c("unitid", "cip_4", "institution_name", "city", "state", "cip_family",
                                      "cip_title", "2010_total_4", "2012_total_4", "2014_total_4",
                                      "2016_total_4", "2018_total_4", "2020_total_4",
                                      "2022_total_4", "2023_total_4", "first_year_awarded",
                                      "degrees_in_first_year", "total_degrees_inst",
                                      "majors_offered_in_first_year"),
              conflict = "ignore")
  

majors_replaced <- discontinued_majors %>% 
  select(-(ends_with('total_2'))) %>% 
  inner_join(absorbing_added_cross,
            by = join_by(unitid, institution_name, city, state), relationship = "many-to-many") %>% 
  #filtering majors that were added and discontinued within 2010-2023
  filter(cip_4.x != cip_4.y) %>% 
  select(-action.x, -action.y, -cip_definition.x, -cip_definition.y) %>% 
  relocate(c(first_year_awarded, degrees_in_first_year),
           .after = degrees_in_last_year) %>% 
  relocate(cip_title.y, .after = cip_title.x) %>% 
  relocate(cip_4.y, .after = cip_4.x) %>% 
  mutate(cip_title_x = str_remove_all(cip_title.x , "General|Other|\\sand"),
         cip_title_y = str_remove_all(cip_title.y, "General|Other|\\sand")) %>%
  mutate(cip_title_x = str_replace_all(cip_title_x, "[[:punct:]]", " "),
         cip_title_y = str_replace_all(cip_title_y, "[[:punct:]]", " "))


majors_replaced_filtered <- majors_replaced %>%
  # making sure majors that are similar match if they don't share words
  mutate(cip_title_x = str_replace_all(cip_title_x, "Behavioral Sciences", "Behavioral Sciences Psychology Cognitive"),
         cip_title_y = str_replace_all(cip_title_y, "Behavioral Sciences", "Behavioral Sciences Psychology Cognitive")) %>%
  mutate(cip_title_x = str_replace_all(cip_title_x, "Biopsychology", "Biopsychology Psychology"),
         cip_title_y = str_replace_all(cip_title_y,"Biopsychology", "Biopsychology Psychology"),
         cip_title_x = str_replace_all(cip_title_x , "Curriculum Instruction", "Curriculum Instruction Education"),
         cip_title_y = str_replace_all(cip_title_y, "Curriculum Instruction", "Curriculum Instruction Education"),
         cip_title_x = str_replace_all(cip_title_x, "Criminal Justice Corrections", "Criminal Justice Corrections Enforcement"),
         cip_title_y = str_replace_all(cip_title_y, "Criminal Justice Corrections", "Criminal Justice Corrections Enforcement"),
         cip_title_x = str_replace_all(cip_title_x, "Marketing", "Marketing Business Commerce"),
         cip_title_y = str_replace_all(cip_title_y, "Marketing", "Marketing Business Commerce"),
         cip_title_x = str_replace_all(cip_title_x, "Textiles", "Textiles Textile"),
         cip_title_y = str_replace_all(cip_title_y, "Textiles", "Textiles Textile"),
         cip_title_x = str_replace_all(cip_title_x, "Religious", "Religion Religious"),
         cip_title_y = str_replace_all(cip_title_y, "Religious", "Religion Religious"),
         cip_title_x = str_replace_all(cip_title_x, "Pastoral Counseling", "Pastoral"),
         cip_title_y = str_replace_all(cip_title_y, "Pastoral Counseling", "Pastoral"),
         cip_title_x = str_remove_all(cip_title_x, "Specialized"),
         cip_title_y = str_remove_all(cip_title_y, "Specialized")
         ) %>%
  #curriculum and instruction match with teaching
  rowwise() %>%
  # only include rows that have matching words for discontinued and added major title
  filter(any(str_extract_all(cip_title_x, "\\b[[:alpha:]]+\\b")[[1]] %in% str_extract_all(cip_title_y, "\\b[[:alpha:]]+\\b")[[1]])) %>%
  ungroup() %>%
  # remove words such as "science", "arts", "education" that are common in many majors
  mutate(cip_title_x = str_replace_all(cip_title_x, "Managerial", "Management")) %>%
  mutate(cip_title_x = str_remove_all(cip_title_x,
"Sciences|Arts|Science|Studies|Allied|Support|Professions|Services|Related|Teaching|Applied|Administration|Asian|Clinical|Research")) %>%
  mutate(cip_title_x = str_remove_all(cip_title_x, "Languages  Literatures  Linguistics"),
         cip_title_y = str_remove_all(cip_title_y, "Languages  Literatures  Linguistics")) %>%
  mutate(cip_title_x = if_else(!str_detect(`cip_title_x`, "Special Education"),
                               str_remove(`cip_title_x`, "Education"), cip_title_x),
         cip_title_y = if_else(!str_detect(`cip_title_y`, "Special Education"),
                               str_remove(`cip_title_y`, "Education"), cip_title_y)) %>% 
  rowwise() %>%
  filter(any(str_extract_all(cip_title_x, "\\b[[:alpha:]]+\\b")[[1]] %in% str_extract_all(cip_title_y, "\\b[[:alpha:]]+\\b")[[1]]))


majors_title_match <- majors_replaced_filtered %>% 
  distinct(cip_title.x, cip_title.y)

# only majors that were fully discontinued, not replaced
discontinued_not_replaced <- discontinued_majors %>% 
  filter(!(cip_4 %in% majors_replaced_filtered$cip_4.x &
           unitid %in% majors_replaced_filtered$unitid &
           last_year_awarded %in% majors_replaced_filtered$last_year_awarded),
         .by = c(unitid, cip_4, institution_name, city, state, cip_family,
                 cip_title)) %>% 
  # filter out majors that were discontinued due to lack of people in major
  filter(percent_inst_discontinued > 0) %>% 
  # filter out 'other' majors (cip code is __.99)
  filter(cip_4 != 30.99 & cip_family != 24 & cip_4 != 45.01 & !str_detect(cip_4, "\\.99$")) %>% 
  # only colleges that are still opened
  arrange(desc(degrees_in_last_year)) 
  # filtering out schools that closed completely
  # filter(total_degrees_inst != degrees_in_last_year)



# # number of majors discontinued for each college by year
# # https://hechingerreport.org/rural-universities-already-few-and-far-between-are-being-stripped-of-majors/
# 
number_discontinued_college <- discontinued_not_replaced %>%
  summarize(discontinued_majors = n(),
            total_degrees_inst = unique(total_degrees_inst),
            total_discontinued_degrees_in_last_year = sum(degrees_in_last_year),
            percent_discontinued_degrees = 100*sum(degrees_in_last_year)/unique(total_degrees_inst),
            .by = c(unitid, institution_name, state, last_year_awarded, majors_offered_in_last_year))



closed_colleges <- number_discontinued_college %>%
  filter(majors_offered_in_last_year == discontinued_majors & total_degrees_inst == total_discontinued_degrees_in_last_year)



degrees_discontinued <- number_discontinued_college %>%
  # filter to make sure colleges did not close
  filter(majors_offered_in_last_year != discontinued_majors & total_degrees_inst != total_discontinued_degrees_in_last_year) %>% 
  ggplot(aes(x= total_degrees_inst, y = percent_discontinued_degrees)) +
  geom_point(alpha = 0.6, color = "red")+
  labs(title = "Percentage of Degrees Granted in Discontinued Majors' \nLast Active Year vs. Total Degrees Awarded at Institution",
       y = "Percent of Degrees in \nDiscontinued Majors (%)",
       x = "Total Degrees Awarded at Institution") +
  theme(title = element_text(face = "bold", size = 15),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(size = 13))

# Penn State University is the outlier on the right side of graph; it had 16675 total degrees in 2021, when 1647 degrees were awarded in majors that were discontinued that year.

ggsave(degrees_discontinued, filename = "plots/degrees_discontinued.jpeg", width = 10, height = 6)
ggsave(degrees_discontinued, filename = "plots/degrees_discontinued.png", width = 10, height = 6)

degrees_discontinued


# scatter plot of number of majors discontinued vs. number of degrees awarded


# FIX PLOTS
# right now the plot isn't what the title is

majors_offered_total_degrees <- year_total_degrees %>% 
  # do not include institution plot points where 0 degrees awarded in a given year
  filter(total_degrees_inst > 0) %>% 
  ggplot(aes(x = total_degrees_inst, y = majors_offered)) +
  geom_point(alpha = 0.4, color = "blue") +
  labs(title = "Number of Majors Offered vs. Total Degrees Awarded at Institution",
       x = "Total Degrees Awarded at Institution",
       y = "Number of Majors Offered") +
  theme(title = element_text(face = "bold", size = 15),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(size = 13))

majors_offered_total_degrees

ggsave(majors_offered_total_degrees, filename = "plots/majors_offered_total_degrees.jpeg", width = 10, height = 6)
ggsave(majors_offered_total_degrees, filename = "plots/majors_offered_total_degrees.png", width = 10, height = 6)

# 
# majors_offered_total_degrees_ctrl <- year_total_degrees %>% 
#   left_join(directory, by = join_by(unitid, institution_name == instnm)) %>% 
#   filter(!is.na(control) & control != 3) %>% 
#   mutate(control = case_match(control,
#                               1 ~ "Public",
#                               2 ~ "Private Not-for-Profit")) %>% 
#   filter(total_degrees_inst > 0) %>% 
#   ggplot(aes(x = total_degrees_inst, y = majors_offered, group = control)) +
#   facet_wrap(~control)+
#   geom_point(alpha = 0.4, color = "blue") +
#   labs(title = "Number of Majors Offered vs. Total Degrees Awarded at Institution",
#        x = "Total Degrees Awarded at Institution",
#        y = "Number of Majors Offered") +
#   theme(title = element_text(face = "bold", size = 15),
#         axis.title = element_text(face = "bold"),
#         axis.text = element_text(size = 13))
# 
# majors_offered_total_degrees_ctrl

# only for 2023
majors_offered_total_degrees_23 <- year_total_degrees %>% 
  filter(year == 2023) %>% 
  # do not include institution plot points where 0 degrees awarded in a given year
  filter(total_degrees_inst > 0) %>% 
  ggplot(aes(x = total_degrees_inst, y = majors_offered)) +
  geom_point(alpha = 0.4, color = "blue") +
  labs(title = "Number of Majors Offered vs. Total Degrees Awarded \nat Institution in 2023",
       x = "Total Degrees Awarded at Institution in 2023",
       y = "Number of Majors Offered in 2023") +
  theme(title = element_text(face = "bold", size = 15),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(size = 9))


majors_offered_total_degrees_23


ggsave(majors_offered_total_degrees_23, filename = "plots/majors_offered_total_degrees_23.jpeg", width = 10, height = 6)
ggsave(majors_offered_total_degrees_23, filename = "plots/majors_offered_total_degrees_23.png", width = 10, height = 6)


log_majors_offered_total_degrees_23 <- year_total_degrees %>% 
  filter(year == 2023) %>% 
  # do not include institutions that did not award any degrees
  filter(total_degrees_inst > 0) %>% 
  ggplot(aes(x = total_degrees_inst, y = majors_offered)) +
  geom_point(alpha = 0.4, color = "blue") +
  labs(title = "Number of Majors Offered vs. Log Total Degrees Awarded \nat Institutions in 2023",
       x = "Log Total Degrees Awarded at Institution in 2023",
       y = "Number of Majors Offered in 2023") +
  theme(title = element_text(face = "bold", size = 15),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(size = 13))+
  scale_x_continuous(trans = 'log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  geom_smooth(method = "lm", se = T, color = "darkorange")

log_majors_offered_total_degrees_23

ggsave(log_majors_offered_total_degrees_23, filename = "plots/log_majors_offered_total_degrees_23.jpeg", width = 10, height = 6)
ggsave(log_majors_offered_total_degrees_23, filename = "plots/log_majors_offered_total_degrees_23.png", width = 10, height = 6)

majors_discontinued_college <- number_discontinued_college %>%
  mutate(percent_majors_discontinued = 100* discontinued_majors/majors_offered_in_last_year) %>% 
   # filter to make sure colleges did not fully close
  filter(majors_offered_in_last_year != discontinued_majors & total_degrees_inst != total_discontinued_degrees_in_last_year) %>%
  ggplot(aes(y= percent_majors_discontinued, x= total_degrees_inst)) +
  geom_point(alpha = 0.6, color = "purple")+
  labs(title = "Percentage of Majors Discontinued vs. Total Degrees Awarded in a Given Year \nat Institution",
       y = "Percentage of Majors Discontinued (%)",
       x = "Total Degrees Awarded") +
  theme(title = element_text(face = "bold", size = 15),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(size = 13))

majors_discontinued_college

ggsave(majors_discontinued_college, filename = "plots/majors_discontinued_college.jpeg", width = 10, height = 6)
ggsave(majors_discontinued_college, filename = "plots/majors_discontinued_college.png", width = 10, height = 6)



percent_enrollment_majors <- number_discontinued_college %>% 
  left_join(year_total_degrees, by = join_by(unitid, institution_name, state, last_year_awarded == year, majors_offered_in_last_year == majors_offered, total_degrees_inst)) %>% 
  # filter to make sure colleges did not fully close
  filter(majors_offered_in_last_year != discontinued_majors & total_degrees_inst != total_discontinued_degrees_in_last_year) %>%  
  ggplot(aes(y = discontinued_majors, x = percent_of_national))+
  geom_point(alpha = 0.7, color = "blue")+
  labs(title = "Number of Majors Discontinued vs. Institution's \nPercentage of National Degrees",
       x = "Percent of National Degrees Awarded (%)",
       y = "Number of Majors Discontinued") +
  theme(title = element_text(face = "bold", size = 15),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(size = 13))

  # outlier on right most is Penn State University in 2021
ggsave("plots/percent_enrollment_majors.png", width = 10, height = 6)
ggsave("plots/percent_enrollment_majors.jpeg", width = 10, height = 6)

percent_enrollment_majors

percent_enrollment_degrees_discontinued <- number_discontinued_college %>% 
  # filter to make sure colleges did not fully close
  filter(majors_offered_in_last_year != discontinued_majors & total_degrees_inst != total_discontinued_degrees_in_last_year) %>%
  left_join(year_total_degrees, by = join_by(unitid, institution_name, state, last_year_awarded == year, majors_offered_in_last_year == majors_offered, total_degrees_inst)) %>% 
  ggplot(aes(y = percent_discontinued_degrees, x = percent_of_national))+
  geom_point(alpha = 0.7, color = "#4b3896")+
  labs(title = "Percent of Degrees Awarded in Discontinued Majors \nvs. Institution's Percentage of National Degrees",
       x = "Percent of National Degrees Awarded (%)",
       y = "Percent of Degrees Awarded \nin Discontinued Majors (%)")+
  theme(title = element_text(face = "bold", size = 15),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(size = 13))
  
percent_enrollment_degrees_discontinued

ggsave(percent_enrollment_degrees_discontinued, filename = "plots/percent_enrollment_degrees_discontinued.png", width = 10, height = 6)
ggsave(percent_enrollment_degrees_discontinued, filename = "plots/percent_enrollment_degrees_discontinued.jpeg", width = 10, height = 6)

# percent_enrollment_majors but facet grid by control of institution (private or public)



directory <- read_csv("info_files/hd2022.csv") %>% 
  clean_names() %>% 
  select(unitid, instnm, control)
# 
# percent_enrollment_majors_ctrl <- number_discontinued_college %>% 
#   left_join(year_total_degrees, by = join_by(unitid, institution_name, state, last_year_awarded == year, majors_offered_in_last_year == majors_offered, total_degrees_inst)) %>% 
#   left_join(directory, by = join_by(unitid, institution_name == instnm)) %>% 
#   filter(!is.na(control) & control != 3) %>% 
#   mutate(control = case_match(control,
#                               1 ~ "Public",
#                               2 ~ "Private Not-for-Profit")) %>% 
#   ggplot(aes(y = discontinued_majors, x = percent_of_national, group = control))+
#   geom_point(alpha = 0.4, color = "blue")+
#   facet_grid(~control)+
#   labs(title = "Number of Majors Discontinued vs. Percentage of National Degrees Awarded \nby Institution",
#        x = "Percent of National Degrees Awarded (%)",
#        y = "Number of Majors Discontinued") +
#   theme(title = element_text(face = "bold", size = 15),
#         axis.title = element_text(face = "bold"),
#         axis.text = element_text(size = 13))
# 
# percent_enrollment_majors_ctrl


total_enrollment_discontinued_majors <- number_discontinued_college %>% 
  ggplot(aes(x = total_degrees_inst, y = discontinued_majors))+
  geom_point(alpha = 0.5, color = "blue")+
  labs(title = "Majors Discontinued vs. Total Degrees Awarded in a Given Year \nat Institution",
       y = "Number of Majors Discontinued",
       x = "Total Degrees Awarded") +
  theme(title = element_text(face = "bold", size = 15),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(size = 13))
total_enrollment_discontinued_majors


ggsave(total_enrollment_discontinued_majors, filename = "plots/total_enrollment_discontinued_majors.png", width = 10, height = 6)
ggsave(total_enrollment_discontinued_majors, filename = "plots/total_enrollment_discontinued_majors.jpeg", width = 10, height = 6)



discontinued_majors_year <- discontinued_not_replaced %>% 
  summarize(n = n(), # number of colleges that discontinued major in given year
            # total number of degrees discontinued in major for given year
            number_degrees = sum(degrees_in_last_year), 
            # sum of degrees in discontinued major across country divided by total number of degrees nationally
            percent_national_degrees = 100* sum(degrees_in_last_year)/unique(total_degrees_national),
            # sum of degrees in discontinued major across country divided by total number of degrees for major across country
            percent_national_4 = 100* sum(degrees_in_last_year)/unique(total_4_national),
            .by = c(last_year_awarded, cip_4, cip_title, cip_family)) %>% 
  # not including missing majors
  arrange(desc(percent_national_4)) 

discontinued_overall <- discontinued_not_replaced %>% 
  summarize(n = n(), # number of colleges
            number_degrees = sum(degrees_in_last_year), 
            .by = c(cip_4, cip_title, cip_family)) %>% 
  # not including missing majors
  filter(cip_4 != 99) %>% 
  arrange(desc(number_degrees))

discontinued_major_state <- discontinued_not_replaced %>% 
    summarize(n = n(), # number of colleges
              number_degrees = sum(degrees_in_last_year), 
              # percentage of degrees in major that were discontinued out of all degrees in major in that year by state
              percent_degrees = 100*sum(degrees_in_last_year)/sum(total_degrees_inst),
              .by = c(cip_4, cip_title, state, last_year_awarded)) %>% 
    # not including missing majors
    filter(cip_4 != 99) %>% 
    arrange(desc(percent_degrees))
#   
# discontinued_majors_national <- discontinued_not_replaced %>% 
#   summarize(n = n(), # number of colleges
#             number_degrees = sum(degrees_in_last_year), 
#             percent_national_degrees = 100* sum(degrees_in_last_year)/(total_degrees_national),
#             percent_degrees = 100*sum(degrees_in_last_year)/sum(total_degrees_in_last_year),
#             .by = c(cip_4, cip_title)) %>% 
#   # interdisciplinary studies
#   filter(cip_4 != 30.99) %>% 
#   arrange(desc(n), desc(number_degrees), desc(percent_degrees))


# 

# number of majors added and dropped
newly_added_dropped <- added_majors %>%
  summarize(newly_added = n(), .by = c(first_year_awarded, cip_4, cip_title, cip_family)) %>%
  left_join(discontinued_majors_year %>% 
              # discontinued year is the year that the major is no longer offered, so last_year_awarded + 1
              mutate(discontinued_year = last_year_awarded + 1), 
            by = join_by(first_year_awarded == discontinued_year, cip_4, cip_title, cip_family)) %>% 
  select(-c(number_degrees, percent_national_degrees, percent_national_4, last_year_awarded)) %>% 
  rename(n_closing = n,
         year = first_year_awarded) %>% 
  mutate(n_closing = if_else(is.na(n_closing), 0, n_closing)) %>% 
  left_join(cip_discipline, by = join_by(cip_4 == cip_code))



# newly added and dropped nationally
# technically, this is the number of schools that added a major in a given year and the number of schools that discontinued a major, as in the year is the first year the major is not offered

national_add_drop <- newly_added_dropped %>% 
  summarize(newly_added = sum(newly_added),
            n_closing = sum(n_closing), .by = year) %>% 
  ggplot(aes(x = year)) +
  geom_line(aes(y = newly_added, colour = "Added")) +
  geom_line(aes(y = n_closing, colour = "Dropped")) +
  labs(title = "Number of Majors Added and Discontinued Nationally",
       x = "Year",
       y = "Number of Majors",
       colour = "Status")+
  theme_minimal()+
  theme(title = element_text(face = "bold", size = 15),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(size = 13))+
  scale_x_continuous(breaks = seq(2010, 2023, by = 2)) 

national_add_drop
ggsave(national_add_drop, filename = "plots/national_add_drop.png", width = 10, height = 6)
ggsave(national_add_drop, filename = "plots/national_add_drop.jpeg", width = 10, height = 6)



# Figure here shows number of majors closed and added each year.
natl_added_dropped_discipline <- newly_added_dropped %>% 
  filter(!is.na(discipline)) %>% 
  summarize(newly_added = sum(newly_added),
            n_closing = sum(n_closing), .by = c(year, discipline)) %>% 
  ggplot(aes(x = year, group = discipline))+
  facet_wrap(~discipline)+
  geom_line(aes(y = newly_added, colour = "Added")) +
  geom_line(aes(y = n_closing, colour = "Dropped")) +
  labs(title = "Number of Majors Added and Discontinued Nationally \nby Discipline",
       x = "Year",
       y = "Number of Majors",
       colour = "Status")+
  theme_minimal()+
  theme(title = element_text(face = "bold", size = 15),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(size = 10))+
  scale_x_continuous(breaks = seq(2010, 2023, by = 2)) 

natl_added_dropped_discipline

ggsave(natl_added_dropped_discipline, filename = "plots/added_dropped_discipline.png", width = 10, height = 6)

ggsave(natl_added_dropped_discipline, filename = "plots/added_dropped_discipline.jpeg", width = 10, height = 6)


# program closures by discipline
# percent change in majors offered from 2010 to 2023


discipline_change <- major_total_by_year %>% 
  summarize(n_schools = sum(n_schools), .by = c(year, discipline)) %>% 
  filter(!is.na(discipline)) %>% 
  filter(year == 2010 | year == 2023) %>% 
  mutate(percent_change = 100 * (n_schools - lag(n_schools, order_by = year))/lag(n_schools, order_by = year), .by = discipline) %>% 
  filter(year == 2023) %>% 
  rename(number_schools_2023 = n_schools)


major_change <- major_total_by_year %>% 
  summarize(n_schools = sum(n_schools), .by = c(year, cip_family)) %>% 
  filter(year == 2010 | year == 2023) %>% 
  mutate(percent_change = 100 * (n_schools - lag(n_schools, order_by = year))/lag(n_schools, order_by = year), .by = cip_family) %>% 
  left_join(cip_codes_2, by = join_by(cip_family)) %>% 
  select(year, cip_family, cip_title, n_schools, percent_change) %>% 
  filter(year == 2023) %>% 
  mutate(cip_title = tolower(cip_title)) %>% 
  select(-year) %>%
  rename(number_schools_2023 = n_schools) %>% 
  arrange(percent_change)
  

major_change

# percentage enrollment change at college 2010-2023 on x-axis and number of majors dropped on y -axis

enrollment_change <- number_discontinued_college %>% 
  summarize(n_majors = sum(discontinued_majors),
            n_degrees_discontinued = sum(total_discontinued_degrees_in_last_year),
            .by = c(unitid, institution_name)) %>%
  left_join(year_total_degrees %>% filter(year == 2010 | year == 2023),
            by = join_by('unitid', 'institution_name')) %>% 
  filter(!is.na(year)) %>% 
  mutate(enroll_change = 100 * (total_degrees_inst - lag(total_degrees_inst, order_by = year))/lag(total_degrees_inst, order_by = year), .by = c(unitid, institution_name)) %>% 
  filter(!is.na(enroll_change)) 


enroll_change_plot <- enrollment_change %>% 
  ggplot(aes(x = enroll_change, y = n_degrees_discontinued))+
  geom_point(alpha = 0.4, color = "blue")+
  labs(title = "Total Number of Degrees in Majors' Last Active Year vs. \nPercentage Enrollment Change by Institution from 2010-2023",
       y = "Number of Degrees Discontinued",
       x = "Percentage Enrollment Change (%)")+
  theme(title = element_text(face = "bold", size = 15),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(size = 13))
  

ggsave(enroll_change_plot, filename = "plots/enroll_change_plot.png", width = 10, height = 6)
ggsave(enroll_change_plot, filename = "plots/enroll_change_plot.jpeg", width = 10, height = 6)

enroll_change_plot

# most common discontinued majors by year by the number of colleges that discontinued
most_common_discontinued_major_national_year <- discontinued_majors_year %>% 
  filter(n == max(n), .by = c(last_year_awarded)) %>%
  arrange(last_year_awarded)


# most common discontinued majors by year by the number of degrees that were discontinued
largest_degrees_discontinued_major_national <- discontinued_majors_year %>% 
  filter(number_degrees == max(number_degrees), .by = c(last_year_awarded)) %>%
  arrange(last_year_awarded)

# most common discontinued majors by year by the percentage of degrees that were discontinued
largest_discontinued_major_national_year_percent <- discontinued_majors_year %>% 
  filter(percent_national_degrees == max(percent_national_degrees), .by = c(last_year_awarded)) %>%
  arrange(last_year_awarded)
# 
# # time trend for majors that were at least 4% of total degrees nationally
# time_trend_large_majors <- closed_offered %>% 
#   filter(any(percent_of_national_degrees >= 4), .by = c(cip_4)) %>% 
#   filter(!str_detect(cip_4, "99$|01$")) %>% 
#   ggplot(aes(x = year, y = percent_of_national_degrees, color = cip_title))+
#   geom_point()+
#   geom_line()+
#   labs(title = "Percent of Degrees Awarded Nationally \nfor Large Majors",
#        subtitle = "2010-2023",
#        color = "CIP Title")+
#   theme(legend.position="bottom")+
#   scale_x_continuous(breaks = seq(2010, 2023, 2))+
#   xlab("Year")+
#   ylab("Percent of Degrees Awarded Nationally (%)")+
#   theme_minimal()
# time_trend_large_majors
# 
# 
# time_trend_weighted_large_majors <- closed_offered %>% 
#   filter(any(percent_of_national_degrees >= 4), .by = c(cip_4)) %>% 
#   filter(!str_detect(cip_4, "99$|01$")) %>% 
#   ggplot(aes(x = year, color = cip_title))+
#   geom_line(aes(y = percent_schools))+
#   geom_line(aes(y = percent_schools_weighted), linetype = "dashed")+
#   labs(title = "Percent of Schools Offering Majors \nfor Large Majors",
#        subtitle = "2010-2023",
#        color = "CIP Title")+
#   theme(legend.position="bottom")+
#   scale_x_continuous(breaks = seq(2010, 2023, 2))+
#   xlab("Year")+
#   ylab("Percent of Schools Offering Majors (%)")+
#   theme_minimal()
# 
# 
# time_trend_weighted_large_majors
 # time trend for majors that were between 1 and 4% of total degrees nationally
time_trend_med_majors <- major_total_by_year %>% 
  filter(any(percent_of_national_degrees >= 1), .by = c(cip_4)) %>% 
  filter(percent_of_national_degrees < 4) %>%
  filter(!str_detect(cip_4, "99$|01$")) %>% 
  ggplot(aes(x = year, y = percent_of_national_degrees,
             color = cip_title))+
  geom_point()+
  geom_line()+
  facet_wrap(~cip_family)+
  labs(title = "Percent of Degrees Awarded Nationally \nfor Medium Size Majors",
       subtitle = "2010-2023",
       color = "CIP Title")+
  scale_x_continuous(breaks = seq(2010, 2023, 4))+
  xlab("Year")+
  ylab("Percent of Degrees Awarded Nationally (%)")+
  theme(legend.position = "bottom") +
  theme_minimal()

# time trend for majors with most number of discontinued degrees

most_degrees <- discontinued_overall %>% 
  arrange(desc(number_degrees)) %>%
  head(10)

most_deg_plot <- major_total_by_year %>% 
  filter(cip_4 %in% most_degrees$cip_4) %>% 
  ggplot(aes(x = year, y = percent_of_national_degrees, color = cip_title))+
  geom_point()+
  geom_line()+
  labs(title = "Percent of Degrees Awarded Nationally \nfor 10 Most Common Majors",
       subtitle = "2010-2023",
       color = "CIP Title")+
  scale_x_continuous(breaks = seq(2010, 2023, 2))+
  theme_minimal()+
  xlab("Year")+
  ylab("Percent of Degrees Awarded Nationally (%)")

most_deg_plot

# time trend for germanic & romance languages degrees
language_ling <- major_total_by_year %>% 
  filter(cip_4 == 16.05 | cip_4 == 16.09) %>% 
  ggplot(aes(x = year, y = percent_of_national_degrees, color = cip_title))+
  geom_point()+
  geom_line()+
  labs(title = "Percent of Degrees Awarded Nationally in Language Majors",
       subtitle = "2010-2023",
       color = "CIP Title")+
  scale_x_continuous(breaks = seq(2010, 2023, 2))+
  xlab("Year")+
  ylab("Percent of Degrees Awarded Nationally (%)") +
  theme_minimal()
  
  
 language_ling
 
 #number of schools offering and number of schools closing lang majors
 
 # 
 # num_lang <- closed_offered %>% 
 #   filter(cip_4 == 16.05 | cip_4 == 16.09) %>% 
 #   ggplot(aes(x = year, color = cip_title))+
 #   geom_point(aes(y = offering_weighted))+
 #   geom_line(aes(y = offering_weighted))+
 #   labs(title = "Number of Schools Nationally Offering Language Majors",
 #        subtitle = "2010-2023",
 #        y = "Percent of Schools Nationally",
 #        color = "CIP Title")+
 #   scale_x_continuous(breaks = seq(2010, 2023, 2))+
 #   xlab("Year")+
 #   ylab("Percentage of Schools (%)")+
 #   theme_minimal()+
 #   theme(title = element_text(face = "bold", size = 15),
 #         axis.title = element_text(face = "bold"),
 #         legend.text = element_text(size = 13),
 #         axis.text = element_text(size = 13))
 # 
 
 # num_lang
 
 writing <- major_total_by_year %>% 
   filter(str_detect(cip_4, "^23")) %>% 
   ggplot(aes(x = year, y = percent_of_national_degrees, color = cip_title))+
   geom_point()+
   geom_line()+
   labs(title = "Percent of Degrees Awarded Nationally in English Majors",
        subtitle = "2010-2023",
        color = "CIP Title")+
   scale_x_continuous(breaks = seq(2010, 2023, 2))+
   xlab("Year")+
   ylab("Percent of Degrees Awarded Nationally (%)") +
   theme_minimal()
 
 writing
 
 

 law <- major_total_by_year %>% 
   filter(cip_family == 43) %>% 
   ggplot(aes(x = year, y = percent_of_national_degrees, color = cip_title))+
   geom_point()+
   geom_line()+
   labs(title = "Percent of Degrees Awarded Nationally in Homeland Security and Protective Services Majors",
        subtitle = "2010-2023",
        color = "CIP Title")+
   scale_x_continuous(breaks = seq(2010, 2023, 2))+
   xlab("Year")+
   ylab("Percent of Degrees Awarded Nationally (%)") +
   theme_minimal()
 
 
 law
 
 
social_science_trend <- major_total_by_year %>% 
  filter(cip_family == 45 & cip_4 != 45.01 & cip_4 != 45.99) %>% 
  ggplot(aes(x = year, y = percent_of_national_degrees, color = cip_title))+
  geom_point()+
  geom_line()+
  labs(title = "Percent of Degrees Awarded Nationally in Social Science Majors",
       subtitle = "2010-2023",
       color = "CIP Title")+
  scale_x_continuous(breaks = seq(2010, 2023, 2))+
  xlab("Year")+
  ylab("Percent of Degrees Awarded Nationally (%)") +
  theme_minimal()

social_science_trend 
# 
# econ_percent_schools <- closed_offered %>% 
#   filter(cip_4 == 45.06) %>% 
#   ggplot(aes(x = year))+
#   geom_point(aes(y = percent_schools))+
#   geom_line(aes(y = percent_schools_weighted), linetype = "dashed", color = "blue")+
#   geom_line(aes(y = percent_schools))+
#   labs(title = "Percentage of Schools Offering Economics Majors",
#        subtitle = "2010-2023",
#        y = "Percentage of Schools (%)")+
#   scale_x_continuous(breaks = seq(2010, 2023, 2))+
#   xlab("Year")+
#   theme_minimal()+
#   theme(title = element_text(face = "bold", size = 15),
#         axis.title = element_text(face = "bold"),
#         legend.text = element_text(size = 13),
#         axis.text = element_text(size = 13))
# 
# econ_percent_schools 




#majors missing in 2010 and 2023 but not in between
missing <- missing_majors %>% 
  filter(is.na(`2010_total_4`) & is.na(`2023_total_4`))

