rm(list = ls())
source("Scripts/setup.R")

ireland <- st_read("Data/Other/ireland_ITM.shp")

# Load and clean badger individual data ####

## Culling programme badgers ####

### Load and clean badger data ####
badgers <- read_xlsx("Data/Raw/TBL_CAPTURED_BADGERS_2025.xlsx")

# remove unnecessary columns and clean
badgers_clean <- badgers %>% 
  # select columns to retain
  dplyr::select(-AGE, -GIRTH, -LENGTH, -DATE_SETT_LAST_CHECKED, -COST, 
                -TOTAL_COST, -POSTMORTEM_CODE, -INVOICE_NO, -DATE_ENTERED) %>% 
  separate(DATE_CAUGHT, into = c("DATE_CAUGHT_B", NA), sep = " ") %>% 
  mutate(DATE_CAUGHT_B = dmy(DATE_CAUGHT_B),  
         # remove weird ' from badger names
         across('BADGER_ID', str_replace, "`", ""),
         # replace  \ with / in capture block event
         across('CAPTURE_BLOCK_EVENT', str_replace, "\\\\", "/"),
         # turn sex into categorical
         SEX = recode(SEX, D = NA_character_, Female = "Female", Male = "Male", 
                      Unknown = NA_character_, Unknownm = NA_character_),
         SEX = factor(SEX, levels = c("Female", "Male")), 
         # turn weight into NA if it's 0
         WEIGHT = na_if(WEIGHT, 0))

### Load and clean capture block event data ####
capture_events <- read_xlsx("Data/Raw/TBL_CAPTURE_EVENTS_2025.xlsx")

capture_events %<>%
  select(CAPTURE_BLOCK_EVENT, DATE_COMMENCED, DATE_COMPLETED, TOTAL_BADGERS)

### Add capture dates to badger data and clean mistakes ####
badgers_clean_with_date <- badgers_clean %>% 
  left_join(capture_events) 

# There's 385 badgers where the year of capture in the badger dataset doesn't 
# match the date that appears in the capture event dataset. We'll take the capture
# event dataset as the true date (even if we're not sure it is).

# there's 124 badgers th didn't match with a capture event so there's no capture 
# event date. However, in 47 of those there's a date in the badger dataset, so 
# we'll use that date for those 47 and remove the rest which have no date info at all. 

badgers_clean_with_date <- badgers_clean_with_date %>% 
  mutate(DATE = if_else(is.na(DATE_COMMENCED), DATE_CAUGHT_B, DATE_COMMENCED))

c_badgers <- badgers_clean_with_date %>% 
  select(SETT_ID, BADGER_ID, SEX, WEIGHT, CAPTURE_BLOCK_EVENT, DATE) %>% 
  mutate( 
    # add columns to match with the vaccination ds
    BADGER_ACTION = "REMOVED", 
    VACCINATION = "N", 
    PROGRAMME = "Culling", 
    BADGER_STATUS = "Culled") %>% 
  filter(!is.na(DATE))
  
## Vaccination programme badgers ####

### Load and clean badger data ####

badgers_vacc <- read_xlsx("Data/Raw/TBL_VACC_BADGERS_2025.xlsx")

v_badgers <- badgers_vacc %>% 
  mutate(CAPTURE_BLOCK_EVENT = paste0(QUARTILE, "/", EVENT_NO+1000)) %>% 
  # select necessary variables
  dplyr::select(SETT_ID, BADGER_ID, SEX, AGE, WEIGHT, DATE = DATE_CAPTURE,
                VACCINATION, CAPTURE_BLOCK_EVENT,
                ECTOPARASITES, BADGER_STATUS,
                ECTOPARASITE_TYPE, BADGER_ACTION, DATE_ENTERED, 
                ) %>% 
  # clean data
  mutate(SEX = recode(SEX, 'F' = "Female", 'FEMALE' = "Female", 
                      'M' = "Male", 'MALE' = "Male", 
                      'X' = NA_character_), 
         # clean and recode Age
         AGE = recode(AGE, 'adult' = "Adult", 'ADULT' = "Adult",'CUB' = "Cub", 
                      'JUVENILE' = "Juvenile", 'OLD' = "Old", 
                      '9' = NA_character_), 
         # clean and recode badger status
         BADGER_STATUS = recode(BADGER_STATUS, '2014 badger' = "2014_badger", 
                                '2nd Chip' = NA_character_, 'clinical suspec' = NA_character_,
                                'Ann Barber' = "AnnBarber", 'BADGER' = "Badger",
                                'EPI Removed Under Permission' = "Deceased", 
                                'Euthanised' = "Deceased", 
                                'Euthanised by V' = "Deceased", 
                                'Goodger not on' = "Goodger", 
                                'new' = "New",
                                'NEW' = "New",
                                'Not on system' = NA_character_,
                                'Not on System' = NA_character_,
                                'Old thin' = NA_character_, 
                                'poor condition' = NA_character_,
                                'other' = NA_character_,
                                'RECAPTURE' = "Recapture", 
                                'Recapture (14 days)' = "Recapture",
                                'Recapture (30 days)' = "Recapture",
                                'Recaptured' = 'Recapture',
                                'removed' = "Deceased",
                                'road kill' = "Deceased",
                                'roadside injury' = NA_character_), 
        # id programme
         PROGRAMME = "Vaccination", 
         # clean weight variable
         WEIGHT = na_if(WEIGHT, 0)) %>% 
  filter(!is.na(DATE))


## Put everything together ####
badgers_all <- bind_rows(c_badgers, v_badgers) %>% 
  select(SETT_ID, BADGER_ID, AGE, WEIGHT, SEX, DATE, 
         CAPTURE_BLOCK_EVENT, PROGRAMME, BADGER_ACTION) %>% 
  mutate(
    DATE = as.Date(DATE),
    YEAR = as.factor(year(DATE)), 
    MONTH = as.factor(month(DATE)), 
    MONTHYEAR = ym(paste(YEAR, MONTH, sep = "_"))
  )

culled_total <- badgers_all %>% 
  filter(BADGER_ACTION == "REMOVED")

saveRDS(culled_total, file = "Data/culled_both_programmes.RDS")

## remove repeated observations of goodgers ####

badgers_all <- badgers_all %>%
  group_by(BADGER_ID) %>%
  arrange(DATE) %>% 
  slice(1) %>%
  ungroup()

saveRDS(badgers_all, file = "Data/badgers_all_2025.RDS")

# Visualise badger data ####

# Date captured
ggplot(badgers_all) + 
  geom_bar(aes(x = DATE, fill = MONTH, group = MONTH)) + 
  scale_x_date(date_breaks = "1 year",date_labels = "%Y") + 
  labs(x = "Date", y = "Number of badgers", fill = "Month") + 
  scale_fill_discrete(na.value = "gray") + 
  theme_bw() + 
  ggtitle("Date of capture")

ggplot(badgers_all) + 
  geom_bar(aes(x = MONTHYEAR, fill = MONTH, group = MONTH)) + 
  scale_x_date(date_breaks = "1 year",date_labels = "%Y") + 
  labs(x = "Month", y = "Number of badgers", fill = "Month") + 
  scale_fill_discrete(na.value = "gray") + 
  theme_bw() + 
  ggtitle("Date of capture")


# weight histogram
badgers_all %>% 
  filter(WEIGHT < 20) %>%
  ggplot + 
  geom_histogram(aes(x = WEIGHT), binwidth = 0.5) +
  facet_wrap(~SEX) + 
  theme_bw()

# weight by month
badgers_all %>% 
  filter(WEIGHT < 20) %>% 
  ggplot + 
  geom_boxplot(aes(x = factor(MONTH), y = WEIGHT, fill = factor(MONTH))) + 
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
  facet_wrap(~SEX) + 
  ggtitle("Weight by month")

#programme
badgers_all %>% 
  ggplot + 
  geom_bar(aes(x = YEAR, fill = PROGRAMME), stat = "count") + 
  theme_bw() + 
  labs(x = "Year", y = "Number of badgers", fill = "Programme") +
  ggtitle("Programme")


