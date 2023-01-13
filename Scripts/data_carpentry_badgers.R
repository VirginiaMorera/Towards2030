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
  # clean up, date format, etc
  mutate(DATE_CAUGHT = parse_date(DATE_CAUGHT, "%b %d, %Y %H:%M:%S %p"), 
         DATE_ENTERED = parse_date(DATE_ENTERED, "%b %d, %Y %H:%M:%S %p"), 
         SEX = factor(SEX, levels = c("Female", "Male")), 
         month_entered = month(DATE_ENTERED), 
         year_entered = year(DATE_ENTERED), 
         month_captured = month(DATE_CAUGHT), 
         year_captured = year(DATE_CAUGHT), 
         monthday_ent = paste(month_entered, year_entered, sep = "-"), 
         monthday_ent = parse_date(monthday_ent, "%m-%Y"), 
         monthday_capt = paste(month_captured, year_captured, sep = "-"), 
         monthday_capt = parse_date(monthday_capt, "%m-%Y"), 
         BADGER_ACTION = "REMOVED", 
         VACCINATION = "N", 
         PROGRAMME = "Culling", 
         WEIGHT = na_if(WEIGHT, 0))

badgers_vacc_clean <- badgers_vacc %>% 
  dplyr::select(BADGER_ID, SETT_ID, SEX, AGE, VACCINATION, WEIGHT,
                ECTOPARASITES, 
                ECTOPARASITE_TYPE, BADGER_ACTION, DATE_ENTERED, 
                DATE_CAUGHT = DATE_CAPTURE) %>% 
  mutate(DATE_CAUGHT = parse_date(DATE_CAUGHT, "%b %d, %Y %H:%M:%S %p"), 
         DATE_ENTERED = parse_date(DATE_ENTERED, "%b %d, %Y %H:%M:%S %p"),
         month_entered = month(DATE_ENTERED), 
         year_entered = year(DATE_ENTERED), 
         month_captured = month(DATE_CAUGHT), 
         year_captured = year(DATE_CAUGHT), 
         monthday_ent = paste(month_entered, year_entered, sep = "-"), 
         monthday_ent = parse_date(monthday_ent, "%m-%Y"), 
         monthday_capt = paste(month_captured, year_captured, sep = "-"), 
         monthday_capt = parse_date(monthday_capt, "%m-%Y"),  
         SEX = recode(SEX, 'F' = "Female", 'FEMALE' = "Female", 
                      'M' = "Male", 'MALE' = "Male", 
                      'X' = NA_character_), 
         AGE = recode(AGE, 'adult' = "Adult", 'ADULT' = "Adult",'CUB' = "Cub", 
                      'JUVENILE' = "Juvenile", 'OLD' = "Old", 
                      '9' = NA_character_), 
         SETT_ID = as.numeric(SETT_ID), 
         PROGRAMME = "Vaccination", 
         WEIGHT = na_if(WEIGHT, 0))


badgers_all <- bind_rows(badgers_clean, badgers_vacc_clean)

saveRDS(badgers_all, file = "Data/badgers_all.RDS")

##########################################
#### Visualise badger individual data ####
##########################################

# Date captured
ggplot(badgers_all) + 
  geom_bar(aes(x = monthday_capt, fill = as.factor(month_captured)), 
           stat = "count") + 
  scale_x_date(date_breaks = "1 year",date_labels = "%Y") + 
  labs(x = "Date", y = "Number of badgers", fill = "Month") + 
  theme_bw() + 
  ggtitle("Date of capture")

# more than half the records in the culling dataset don't have capture date
table(is.na(badgers_clean$DATE_CAUGHT))

# Date entered
ggplot(badgers_all) + 
  geom_bar(aes(x = monthday_ent, fill = as.factor(month_entered)), 
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
ggplot(badgers_all) + 
  geom_bar(aes(x = month_captured, fill = as.factor(month_captured)), 
           stat = "count") + 
  theme_bw() + 
  scale_x_continuous(breaks=seq(1, 12, 1), 
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                "Jul", "Aug", 
                                "Sep", "Oct", "Nov", "Dec")) + 
  labs(x = "Month", y = "Number of badgers", fill = "Month") +
  ggtitle("Capture month")
#Biased because 56328 capture dates missing. Also, is this biased by fieldwork personnel holidays, farm working days, etc? 

# weight histogram
badgers_all %>% 
  filter(WEIGHT < 20) %>%
  filter(!is.na(month_captured)) %>% 
  ggplot + 
  geom_histogram(aes(x = WEIGHT))

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
