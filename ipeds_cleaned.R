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
  select(unitid, cipcode, majornum, awlevel, crace24, xcrace24) %>% 
  # replace old cip codes with most updated one if number changed from 2000 classification to 2010 classification
  mutate(cipcode = case_when(
    cipcode == 23.0501 ~ '23.1302',
    cipcode == 51.1601 ~ '51.3801',
    cipcode == 30.2401 ~ '26.1501',
    cipcode == 42.1101 ~ '42.2706',
    cipcode == 23.0801 ~ '23.1404',
    cipcode == 23.1101 ~ '23.1303',
    cipcode == 42.0601 ~ '42.2803',
    cipcode == 42.0901 ~ '42.2804',
    cipcode == 14.3101 ~ '40.1001',
    cipcode == 42.0701 ~ '42.2703',
    cipcode == 42.2101 ~ '42.2808',
    cipcode == 42.2301 ~ '42.2810',
    cipcode == 42.1601 ~ '42.2707',
    cipcode == 23.0701 ~ '23.1402',
    cipcode == 42.0301 ~ '42.2701',
    cipcode == 42.2601 ~ '42.2812',
    cipcode == 42.0401 ~ '42.2802',
    cipcode == 42.1801 ~ '42.2708',
    cipcode == 42.2501 ~ '42.2811',
    cipcode == 42.1001 ~ '42.2705',
    cipcode == 42.0201 ~ '42.2801',
    cipcode == 42.0801 ~ '42.2704',
    cipcode == 51.1616 ~ '51.3813',
    cipcode == 42.1901 ~ '42.2708',
    .default = (cipcode))) 

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
  select(unitid, cipcode, majornum, awlevel, ctotalt, xctotalt) %>% 
  # cip codes that changed from 2010 classification to 2020 classification
  mutate(cipcode = case_when(
    cipcode == 39.0606 ~ '38.0207',
    cipcode == 51.2501 ~ '01.8101',
    cipcode == 45.1401 ~ '45.1103',
    .default = (cipcode)))

rm(ipeds_2007, ipeds_2013, ipeds_2018, ipeds_2023)


totals_2007 <- cleaned_2007 %>% 
  # add totals for first and second major
  summarize(total_6 = sum(crace24, na.rm = T), .by = c(unitid, cipcode)) %>% 
  mutate(year = 2007,
         cip_4 = substr(cipcode, 1, 5),
         cip_2 = substr(cipcode, 1, 2)) # extract 4 digit and 2 digit cip code
 

totals_2013 <- cleaned_2013 %>% 
  # add totals for first and second major
  summarize(total_6 = sum(ctotalt, na.rm = T), .by = c(unitid, cipcode)) %>% 
  mutate(year = 2013,
         cip_4 = substr(cipcode, 1, 5),
         cip_2 = substr(cipcode, 1, 2))  # extract 4 digit and 2 digit cip code
  
totals_2018 <- cleaned_2018 %>%
  # add totals for first and second major
  summarize(total_6 = sum(ctotalt, na.rm = T), .by = c(unitid, cipcode)) %>% 
  mutate(year = 2018,
         cip_4 = substr(cipcode, 1, 5),
         cip_2 = substr(cipcode, 1, 2))  # extract 4 digit and 2 digit cip code

totals_2023 <- cleaned_2023 %>%
  # add totals for first and second major
  summarize(total_6 = sum(ctotalt, na.rm = T), .by = c(unitid, cipcode)) %>% 
  mutate(year = 2023,
         cip_4 = substr(cipcode, 1, 5),
         cip_2 = substr(cipcode, 1, 2)) # extract 4 digit and 2 digit cip code

institution_names <- read_csv("institution_names.csv") %>%  #list of institutions by unit id
  distinct()

degrees_awarded <- totals_2007 %>% 
  bind_rows(totals_2013) %>% 
  bind_rows(totals_2018) %>%  
  bind_rows(totals_2023) %>% 
  pivot_wider(names_from = c(year), values_from = c(total_6), 
              names_glue = "total_6_{year}",
               values_fill = 0) %>% # use 0 values for missing values in next years
  #number of degrees awarded in each year by institution
 mutate(total_degrees_2007 = sum(total_6_2007),
        total_degrees_2013 = sum(total_6_2013),
        total_degrees_2018 = sum(total_6_2018),
        total_degrees_2023 = sum(total_6_2023), .by = unitid) %>% 
  select(unitid, total_degrees_2007, total_degrees_2013, total_degrees_2018, total_degrees_2023) %>% 
  distinct()
  
degrees_by_year <- totals_2007 %>% 
  bind_rows(totals_2013) %>% 
  bind_rows(totals_2018) %>% 
  bind_rows(totals_2023) %>% 
  # use 0 values for missing values in next years
  pivot_wider(names_from = c(year), values_from = c(total_6),
              names_glue = "total_6_{year}",
               values_fill = 0) %>% 
  mutate(total_4_2007 = sum(total_6_2007, na.rm = T),
         total_4_2013 = sum(total_6_2013, na.rm = T),
         total_4_2018 = sum(total_6_2018, na.rm = T),
         total_4_2023 = sum(total_6_2023, na.rm = T),
         .by = c(unitid, cip_4)) %>% 
  mutate(total_2_2007 = sum(total_6_2007, na.rm = T),
         total_2_2013 = sum(total_6_2013, na.rm = T),
         total_2_2018 = sum(total_6_2018, na.rm = T),
         total_2_2023 = sum(total_6_2023, na.rm = T),
         .by = c(unitid, cip_2)) %>% 
  left_join(institution_names, by = join_by(unitid == UnitID),
            relationship = "many-to-one") %>% 
  left_join(degrees_awarded, by = join_by(unitid)) %>% 
  clean_names() %>% 
  #filter out any observations that don't have data for any year
  filter(if_any(starts_with("total_6_"), ~ . > 0)) %>% 
  write_csv("degrees_by_year.csv")

#unitid not found on ipeds database
unlisted_institutions <- c(448284, 448309, 448318, 448345, 448442, 448451, 
                           448488, 448503, 448512, 448521, 448549, 448558, 
                           448567, 448576, 448585, 448628, 244190, 244464,
                           448734, 448752, 448761, 448804, 448822, 448831, 
                           448877, 448886, 449010, 449931, 450049, 450085, 
                           450094, 450164, 450207, 450216, 450225, 450234,
                           450243, 450252, 450261, 450270, 
                           450289, 450429, 450447, 450456, 450474, 450483, 
                           450492, 450526, 450535, 103945, 114390, 118541,
                           122250, 122834, 122852, 130448, 134112, 139287, 
                           151166, 151616, 151944, 152822, 156903, 157465, 
                           157599, 157696, 160940, 164438, 173489, 203313, 
                           204316, 205610, 205647, 206631, 210906, 212452, 
                           220312, 225849, 225858, 230472, 238573, 261472, 
                           363439, 364168, 366580, 366678, 366696, 374972, 
                           375489, 377069, 397270, 406219, 408039, 409829, 
                           417318, 421832, 427663, 430379, 430388, 434052, 
                           436702, 179256, 179450, 181011, 181093, 181242, 
                           181376, 181400, 182111, 182430, 182458, 182661, 
                           182865, 183345, 184269, 187222, 188146, 188465,
                           189264, 189459, 189556, 190248, 190734, 190770, 
                           192244, 192439, 192563, 192855, 193317, 193645, 
                           194107, 194541, 194675, 194709, 195137, 195562, 
                           197610, 197832, 198242, 198747, 199315, 200244, 
                           201007, 201858, 202019, 202541, 202611, 202684, 
                           203818, 206002, 206330, 207689, 208239, 208488, 
                           208965, 209108, 209533, 210076, 210942, 210960, 
                           211158, 211361, 211644, 212160, 213613, 213783, 
                           215099, 215132, 216180, 218089, 219213, 219578, 
                           220589, 220808, 220941, 221014, 221254, 221795, 
                           221856, 223214, 224402, 227243, 228714, 161882, 
                           162681, 163842, 163921, 164207, 164571, 164720, 
                           164933, 166948, 166984, 167251, 167321, 167455,
                           168290, 168838, 170417, 170842, 171298, 172972,
                           173629, 173887, 173984, 174206, 174279, 174385, 
                           174394, 174464, 174561, 174622, 174631, 174932, 
                           175917, 176026, 176460, 176637, 176938, 177153,
                           177162, 177579, 178226, 178305, 178350, 179070,
                           179201, 230056, 230384, 230621, 230630, 230825, 
                           230898, 230931, 230940, 230977, 231077, 231086, 
                           231192, 231828, 231837, 233499, 233912, 234216, 
                           234492, 235510, 235529, 235769, 237154, 237640, 
                           238892, 238935, 239503, 239743, 239929, 242981, 
                           244011, 244154, 245342, 245731, 104531, 104805, 
                           105163, 105172, 105367, 105516, 106102, 108250,
                           109013, 110051, 110185, 110945, 112312, 112446, 
                           113607, 116040, 116466, 116475, 116484, 116712, 
                           117113, 117928, 119544, 119711, 121071, 121381, 
                           121983, 122454, 122843, 126702, 126872, 127024, 
                           127680, 127699, 128425, 128683, 129686, 130271, 
                           131098, 131308, 131788, 132338, 133085, 134149, 
                           134680, 134909, 135063, 135939, 136066, 136206, 
                           136288, 137148, 137810, 138336, 138725, 138789,
                           138983, 139074, 139533, 140164, 140322, 140401, 
                           140669, 141097, 142337, 144759, 145460, 145770,
                           146010, 146393, 146621, 146676, 460473, 460613, 
                           460729, 460738, 460783, 460899, 461005, 461236, 
                           462239, 462363, 462372, 462390, 462406, 465812, 
                           466152, 466161, 466170, 466189, 466514, 467058, 
                           467793, 468015, 468024, 468723, 470092, 470162, 
                           470852, 470861, 470870, 470889, 470898, 470904, 
                           474702, 474890, 474906, 474951, 474960, 474979, 
                           475185, 475291, 475617, 475699, 475866, 475875, 
                           475884, 476319, 477011, 477020, 477950, 477996, 
                           478005, 478014, 478625, 478634, 479567, 479576,
                           479585, 479594, 479600, 479619, 479628, 479637, 
                           479646, 479655, 479770, 480064, 480082, 480134, 
                           480444, 480499, 480754, 481580, 154509, 157234, 
                           157270, 157359, 157632, 161165, 161244, 161837,
                           247700, 247825, 248882, 248943, 251251, 260293, 
                           260804, 260901, 260910, 260947, 260974, 261931, 
                           262013, 262305, 262332, 366650, 366748, 367024, 
                           367194, 367839, 367893, 367909, 367936, 368601, 
                           369084, 369905, 372213, 372222, 372578, 372958, 
                           373827, 378406, 380465, 381617, 381741, 381787, 
                           381909, 382063, 390701, 392840, 398130, 402615, 
                           402776, 404073, 404082, 404648, 404806, 404949, 
                           405979, 405988, 405997, 406006, 406015, 406194,
                           407009, 407063, 407285, 407319, 407665, 409069, 
                           409838, 410502, 413723, 413839, 413848, 413857, 
                           413866, 413884, 414160, 414531, 414568, 414586, 
                           414708, 414823, 416962, 417114, 419457, 419509, 
                           419572, 420006, 420307, 104188, 104364, 146825, 
                           147165, 148177, 148335, 148849, 150561, 151458,
                           151494, 151500, 151519, 151625, 152248, 152266, 
                           152363, 153409, 153418, 420316, 420352, 420510, 
                           420574, 420866, 421009, 421018, 425296, 428268, 
                           428286, 428444, 429076, 429599, 430087, 430102, 
                           430263, 430485, 430810, 432199, 432214, 432223, 
                           432232, 432241, 432533, 434034, 434414, 434548, 
                           434557, 434946, 434955, 434964, 434973, 434982, 
                           434991, 436094, 436191, 436438, 436474, 440448, 
                           440457, 440466, 440484, 440536, 440545, 440590, 
                           440758, 440767, 440925, 441061, 441186, 441399, 
                           441511, 441955, 441964, 441973, 442143, 442152, 
                           442161, 442170, 442189, 442222, 442259, 442365, 
                           442578, 442718, 442824, 442897, 442949, 443456, 
                           443465, 443474, 443526, 443544, 443687, 443872,
                           443881, 443890, 443906, 443915, 443924, 443997, 
                           444033, 444042, 444158, 444167, 444723, 444848, 
                           444927, 444990, 445072, 445081, 445124, 445258, 
                           445276, 445300, 445319, 445355, 445391, 445407, 
                           445416, 445434, 445717, 445920, 445939, 446394, 
                           446613, 446677, 446701, 446710, 446729, 482592, 
                           482608, 482644, 482662, 482671, 482954, 200624, 
                           240055, 376321, 377342, 448354, 454616, 458575, 
                           459161, 460747, 480912, 484640, 484668, 484677, 
                           484686, 484695, 484701, 484729, 484747, 484765, 
                           484783, 484835, 485263, 485281, 485908, 487296, 
                           488101, 488819, 489788, 490063, 490179, 490771, 
                           490850, 491093, 491109, 491491, 492458, 492573, 
                           492582, 451662, 451671, 451699, 451705, 451769, 
                           451796, 451848, 451875, 451884, 451936, 451945, 
                           451954, 451963, 451972, 451981, 451990, 452018, 
                           452027, 452090, 453127, 454591, 454883, 455451, 
                           455585, 455600, 455619, 456010, 456056, 456296, 
                           456348, 456409, 456427, 456436, 456445, 456612, 
                           456630, 456667, 456782, 456791, 456807, 456816, 
                           456834, 457022, 457183, 457688, 458432, 458469, 
                           458502, 458557, 458733, 458742, 458751, 458760, 
                           458779, 458937, 458991, 459091, 459107, 459587, 
                           459596, 459602, 459611, 459620, 459639, 459648, 
                           459657, 459666, 459675, 459684, 459693, 459709, 
                           459718, 459815, 460039, 460048, 460075, 481614, 
                           481623, 482024, 482307, 482316, 482325, 482334, 
                           482343, 482352, 482361, 482370, 482389, 482398, 
                           482486, 482495, 482501, 482510, 482529, 482583,
                           436483, 437042, 437051, 437060, 437219, 437316,
                           437325, 437510, 437848, 437866, 438151, 
                           438382, 438416, 438498, 438601, 438610, 438629, 
                           438902, 438966, 439057, 439163, 439279, 439288, 
                           439297, 439330, 439613, 440147, 440165, 440253, 
                           440341, 440411, 440420, 440439, 101073, 101541,
                           102261, 103644, 103723, 103787, 446738, 446747,
                           446756, 446765, 446792, 446808, 
                           446817, 446899, 446905, 446914, 446969, 447069,
                           447139, 447236, 447458, 447485, 447643, 447661,
                           447670, 439136, 440299, 441478, 442338, 443535, 
                           444574, 446668, 446923, 447272, 447290, 447360,
                           447476, 447652, 448479, 448497, 448530, 448840, 
                           450465, 450544, 450766, 450979, 451060)


# institutions_missing <- degrees_by_year %>% filter(is.na(institution_name)) %>%
#   distinct(unitid) %>% 
#  filter(!unitid %in% unlisted_institutions)
