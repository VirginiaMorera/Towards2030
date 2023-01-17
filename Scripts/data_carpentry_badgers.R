rm(list = ls())
source("Scripts/setup.R")

###############################################
#### Load and clean badger individual data ####
###############################################

# load 
badgers <- read_csv("raw_data/forR_captured_badgers.csv")
badgers_vacc <- read_csv("raw_data/forR_vacc_badgers_names_removed.csv")
ireland <- st_read("Data/ireland_ITM.shp")

# remove unnecessary columns and clean
badgers_clean <- badgers %>% 
  # select columns to retain
  dplyr::select(-AGE, -GIRTH, -LENGTH, -DATE_SETT_LAST_CHECKED, -COST, -TOTAL_COST, -POSTMORTEM_CODE, 
                -INVOICE_NO) %>% 
  # Turn date caught and entered into Date formats
  mutate(DATE_CAUGHT = parse_date(DATE_CAUGHT, "%b %d, %Y %H:%M:%S %p"), 
         DATE_ENTERED = parse_date(DATE_ENTERED, "%b %d, %Y %H:%M:%S %p"), 
         # turn sex into categorical
         SEX = factor(SEX, levels = c("Female", "Male")), 
         # month and year of data entry
         month_entered = month(DATE_ENTERED, label = TRUE), 
         year_entered = year(DATE_ENTERED), 
         monthyear_ent = paste(month_entered, year_entered, sep = "-"), 
         monthyear_ent = parse_date(monthyear_ent, "%b-%Y"), 
         # month and year of capture
         month_captured = month(DATE_CAUGHT, label = TRUE), 
         year_captured = year(DATE_CAUGHT), 
         monthdyear_capt = paste(month_captured, year_captured, sep = "-"), 
         monthyear_capt = parse_date(monthdyear_capt, "%b-%Y"), 
         # add columns to match with the vaccination ds
         BADGER_ACTION = "REMOVED", 
         VACCINATION = "N", 
         PROGRAMME = "Culling", 
         BADGER_STATUS = "Culled",
         # turn weight into NA if it's 0
         WEIGHT = na_if(WEIGHT, 0))

badgers_vacc_clean <- badgers_vacc %>% 
  # select necessary variables
  dplyr::select(BADGER_ID, SETT_ID, SEX, AGE, VACCINATION, WEIGHT,
                ECTOPARASITES, BADGER_STATUS,
                ECTOPARASITE_TYPE, BADGER_ACTION, DATE_ENTERED, 
                DATE_CAUGHT = DATE_CAPTURE) %>% 
  # modify date caught 
  mutate(DATE_CAUGHT = parse_date(DATE_CAUGHT, "%b %d, %Y %H:%M:%S %p"), 
         month_captured = month(DATE_CAUGHT, label = TRUE), 
         year_captured = year(DATE_CAUGHT), 
         monthdyear_capt = paste(month_captured, year_captured, sep = "-"), 
         monthdyear_capt = parse_date(monthdyear_capt, "%b-%Y"),
         # modify date entered
         DATE_ENTERED = parse_date(DATE_ENTERED, "%b %d, %Y %H:%M:%S %p"),
         month_entered = month(DATE_ENTERED, label = TRUE), 
         year_entered = year(DATE_ENTERED), 
         monthyear_ent = paste(month_entered, year_entered, sep = "-"), 
         monthyear_ent = parse_date(monthyear_ent, "%b-%Y"),   
         # clean and recode Sex
         SEX = recode(SEX, 'F' = "Female", 'FEMALE' = "Female", 
                      'M' = "Male", 'MALE' = "Male", 
                      'X' = NA_character_), 
         # clean and recode Age
         AGE = recode(AGE, 'adult' = "Adult", 'ADULT' = "Adult",'CUB' = "Cub", 
                      'JUVENILE' = "Juvenile", 'OLD' = "Old", 
                      '9' = NA_character_), 
         # clean and recode badger status
         BADGER_STATUS = recode(BADGER_STATUS, '2014 badger' = "2014_badger", 
                                'Ann Barber' = "AnnBarber", 'BADGER' = "Badger",
                                'EPI Removed Under Permission' = "Deceased", 
                                'Euthanised by V' = "Deceased", 
                                'Goodger not on' = "Goodger", 
                                'new' = "New",
                                'NEW' = "New",
                                'Old thin' = NA_character_, 
                                'poor condition' = NA_character_,
                                'RECAPTURE' = "Recapture", 
                                'Recapture (14 days)' = "Recapture",
                                'Recapture (30 days)' = "Recapture",
                                'removed' = "Deceased",
                                'road kill' = "Deceased",
                                'roadside injury' = NA_character_), 
         # turn SETT to numeric to match other ds
         SETT_ID = as.numeric(SETT_ID), 
         # id programme
         PROGRAMME = "Vaccination", 
         # clean weight variable
         WEIGHT = na_if(WEIGHT, 0))


badgers_all <- bind_rows(badgers_clean, badgers_vacc_clean)

# saveRDS(badgers_all, file = "Data/badgers_all.RDS")

##########################################
#### Visualise badger individual data ####
##########################################

# Date captured
ggplot(badgers_all) + 
  geom_bar(aes(x = monthyear_capt, fill = month_captured)) + 
  scale_x_date(date_breaks = "1 year",date_labels = "%Y") + 
  labs(x = "Date", y = "Number of badgers", fill = "Month") + 
  scale_fill_discrete(na.value = "gray") + 
  theme_bw() + 
  ggtitle("Date of capture")

# more than half the records in the culling dataset don't have capture date
table(is.na(badgers_clean$DATE_CAUGHT))

# Date entered
ggplot(badgers_all) + 
  geom_bar(aes(x = monthyear_ent, fill = as.factor(month_entered)), 
           stat = "count") + 
  scale_x_date(date_breaks = "1 year",date_labels = "%Y") + 
  scale_fill_discrete(breaks=seq(1, 12, 1), 
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                "Jul", "Aug", 
                                "Sep", "Oct", "Nov", "Dec")) + 
  labs(x = "Date", y = "Number of badgers", fill = "Month") + 
  theme_bw() + 
  ggtitle("Date entered")

# Month of capture
badgers_all %>% 
  filter(!is.na(month_captured)) %>%
  ggplot + 
  geom_bar(aes(x = month_captured, 
               fill = month_captured), 
           stat = "count") + 
  theme_bw() + 
  scale_fill_discrete() + 
  labs(x = "Month", y = "Number of badgers", fill = "Month") +
  ggtitle("Capture month")
#Biased because 56328 capture dates missing. Also, is this biased by fieldwork personnel holidays, farm working days, etc? 

# weight histogram
badgers_all %>% 
  filter(WEIGHT < 20) %>%
  filter(!is.na(month_captured)) %>% 
  ggplot + 
  geom_histogram(aes(x = WEIGHT), binwidth = 0.5)

# weight by month
badgers_all %>% 
  filter(WEIGHT < 20) %>% 
  filter(!is.na(month_captured)) %>% 
  ggplot + 
  geom_boxplot(aes(x = factor(month_captured), y = WEIGHT, fill = factor(month_captured))) + 
  theme_bw() + 
  scale_x_discrete(breaks=seq(1, 12, 1), 
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                "Jul", "Aug", 
                                "Sep", "Oct", "Nov", "Dec")) +
  scale_fill_discrete(breaks=seq(1, 12, 1), 
                   labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                              "Jul", "Aug", 
                              "Sep", "Oct", "Nov", "Dec")) +
  labs(x = "Month of capture", y = "Weight (kg)", fill = "Month of capture") + 
  ggtitle("Weight by month")

# ectoparasites
badgers_all %>% 
  mutate(Parasite_data = if_else(is.na(ECTOPARASITE_TYPE), "No", 
                                 if_else(ECTOPARASITE_TYPE == "N/A", "No", "Yes"))) %>% 
  ggplot + 
  geom_bar(aes(x = year_entered, fill = Parasite_data), stat = "count") + 
  theme_bw() + 
  scale_x_continuous(breaks=seq(2007, 2022, 1)) +
  labs(x = "Year", y = "Number of badgers", fill = "Ectoparasite type collected") + 
  theme(legend.position="bottom") + 
  ggtitle("Collection of ectoparasite data")

#programme
badgers_all %>% 
  ggplot + 
  geom_bar(aes(x = year_entered, fill = PROGRAMME), stat = "count") + 
  theme_bw() + 
  scale_x_continuous(breaks=seq(2007, 2022, 1)) +
  labs(x = "Year", y = "Number of badgers", fill = "Programme") +
  ggtitle("Programme")


# badger destiny
badgers_all %>% 
  filter(!is.na(BADGER_ACTION)) %>% 
  mutate(BADGER_ACTION = factor(BADGER_ACTION, levels = c("REMOVED", "RELEASED"))) %>% 
  ggplot + 
  geom_bar(aes(x = year_entered, fill = BADGER_ACTION), stat = "count") + 
  theme_bw() + 
  scale_x_continuous(breaks=seq(2007, 2022, 1)) +
  labs(x = "Year", y = "Number of badgers", fill = "Badger's destiny") +
  ggtitle("Badger destiny")

# badger destiny (including categories in vacc dataset)
badgers_all %>% 
  filter(!is.na(BADGER_STATUS)) %>% 
  ggplot + 
  geom_bar(aes(x = year_entered, fill = BADGER_STATUS), stat = "count") + 
  theme_bw() + 
  scale_x_continuous(breaks=seq(2007, 2022, 1)) +
  labs(x = "Year", y = "Number of badgers", fill = "Badger's destiny") +
  ggtitle("Badger destiny")


# sex ratio
badgers_all %>% 
  # filter(!is.na(SEX)) %>% 
  ggplot + 
  geom_bar(aes(x = monthday_ent, fill = SEX), stats = "count") + 
  theme_bw() + 
  ggtitle("Sex ratio")

# size by sex
badgers_all %>% 
  filter(!is.na(SEX)) %>%
  filter(WEIGHT < 20) %>% 
  ggplot + 
  geom_boxplot(aes(x = SEX, fill = SEX, y = WEIGHT)) + 
  theme_bw() + 
  ggtitle("Weight by sex")

# age
badgers_all %>% 
  filter(!is.na(AGE)) %>%
  ggplot + 
  geom_bar(aes(x = monthday_ent, fill = AGE), stats = "count") + 
  theme_bw() + 
  ggtitle("Sex ratio")

# size by age
badgers_all %>% 
  filter(!is.na(AGE)) %>%
  mutate(AGE = factor(AGE, levels = c("Old", "Adult", "Juvenile", "Cub"))) %>% 
  filter(WEIGHT < 20) %>% 
  ggplot + 
  geom_boxplot(aes(x = AGE, fill = AGE, y = WEIGHT)) + 
  theme_bw() + 
  ggtitle("Weight by age")

