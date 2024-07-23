rm(list = ls())
source("Scripts/setup.R")

ireland <- st_read("Data/Other/ireland_ITM.shp")

# Load and clean badger individual data ####

## Culling programme badgers ####

### Load and clean badger data ####
badgers <- read_xlsx("Data/Raw/tbl_captured_badgers_2023.xlsx")

# remove unnecessary columns and clean
badgers_clean <- badgers %>% 
  # select columns to retain
  dplyr::select(-AGE, -GIRTH, -LENGTH, -DATE_SETT_LAST_CHECKED, -COST, 
                -TOTAL_COST, -POSTMORTEM_CODE, -INVOICE_NO, -DATE_ENTERED) %>% 
  mutate(
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
capture_events <- read_xlsx("Data/Raw/tbl_capture_EVENTS_2023.xlsx")

capture_events %<>%
  mutate(event_duration = DATE_COMPLETED - DATE_COMMENCED) %>% 
  select(-diff, -diff2)

### Add capture dates to badger data and clean mistakes ####
badgers_clean_with_date <- badgers_clean %>% 
  left_join(capture_events)

## Events where the number of badgers captured field doesn't match the number of 
#  badgers in the dataset  

unmatched_cap_events <- badgers_clean_with_date %>% 
  group_by(CAPTURE_BLOCK_EVENT) %>% 
  summarise(n_rows = n(),
            badgers_captured = unique(TOTAL_BADGERS), 
            diff = n_rows - badgers_captured) %>% 
  filter(!is.na(badgers_captured)) %>% # remove events where number of badgers wasn't recorded, we'll have to trust the number of rows
  # mutate(diff = replace_na(diff, 0)) %>% 
  filter(diff != 0) %>% # remove events where badgers captured matches no rows
  filter(abs(diff) != 1) %>% # remove events where the difference is just 1 (close enough)
  filter(diff %!in% -2:-5) # if we're missing 2 to 5 rows we might accept it
  
badgers_clean_with_date <- badgers_clean_with_date %>% 
  filter(CAPTURE_BLOCK_EVENT %!in% unmatched_cap_events$CAPTURE_BLOCK_EVENT)

## Events where the capture date doesn't fall within the dates of the capture event
just_date <- badgers_clean_with_date %>% 
  select(BADGER_ID, DATE_CAUGHT, DATE_COMMENCED, DATE_COMPLETED, CAPTURE_BLOCK_EVENT) %>% 
  mutate(diff_time = difftime(DATE_COMPLETED, DATE_COMMENCED, units = "days"), 
         checks = if_else(DATE_CAUGHT >= DATE_COMMENCED & DATE_CAUGHT <= DATE_COMPLETED, 
                          "Yes", "No")) %>% 
  filter(checks == "No")
  
badgers_cleaner_with_date <- badgers_clean_with_date %>% 
  filter(CAPTURE_BLOCK_EVENT %!in% just_date$CAPTURE_BLOCK_EVENT)

c_badgers <- badgers_cleaner_with_date %>% 
  select(SETT_ID:DATE_COMPLETED) %>% 
  mutate( 
    # add columns to match with the vaccination ds
    BADGER_ACTION = "REMOVED", 
    VACCINATION = "N", 
    PROGRAMME = "Culling", 
    BADGER_STATUS = "Culled", 
    DATE_EST =  DATE_COMMENCED + floor(difftime(DATE_COMPLETED, DATE_COMMENCED, units = "days")/2),
    DATE_CAUGHT = if_else(is.na(DATE_CAUGHT), DATE_EST, DATE_CAUGHT))
  
## Vaccination programme badgers ####

### Load and clean badger data ####

badgers_vacc <- read_xlsx("Data/Raw/tbl_vacc_badgers_2023.xlsx")

v_badgers <- badgers_vacc %>% 
  mutate(CAPTURE_BLOCK_EVENT = paste(QUARTILE, EVENT_NO, sep = "_")) %>% 
  # select necessary variables
  dplyr::select(SETT_ID, BADGER_ID, DATE_CAUGHT = DATE_CAPTURE, SEX, AGE, WEIGHT,
                VACCINATION, CAPTURE_BLOCK_EVENT,
                ECTOPARASITES, BADGER_STATUS,
                ECTOPARASITE_TYPE, BADGER_ACTION, DATE_ENTERED, 
                ) %>% 
  # modify date caught 
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
         WEIGHT = na_if(WEIGHT, 0))

## Load and clean vaccination effort data ####
effort <- read_xlsx("Data/Raw/tbl_vaccine_2023.xlsx")

effort <- effort %>% 
  mutate(CAPTURE_BLOCK_EVENT = paste(QUARTILE, EVENT, sep = "_"), 
         diff_time = difftime(DATE_COMPLETED, DATE_COMMENCED, units = "days")) %>% 
  select(CAPTURE_BLOCK_EVENT, diff_time, VACCINE_STATUS, VACC_VISIT)
  

## Put everything together ####
badgers_all <- bind_rows(c_badgers, v_badgers) %>% 
  select(SETT_ID, BADGER_ID, AGE, WEIGHT, SEX, DATE_CAUGHT, 
         CAPTURE_BLOCK_EVENT, PROGRAMME) %>% 
  mutate(
    DATE_CAUGHT = as.Date(DATE_CAUGHT),
    YEAR = as.factor(year(DATE_CAUGHT)), 
    MONTH = as.factor(month(DATE_CAUGHT)), 
    MONTHYEAR = ym(paste(YEAR, MONTH, sep = "_"))
  )
# saveRDS(badgers_all, file = "Data/badgers_all_2023.RDS")

#### Visualise badger individual data ####

badgers_all <- readRDS("Data/badgers_all_2023.RDS")


## remove repeated observations of goodgers 

badgers_all <- badgers_all %>%
  group_by(BADGER_ID) %>%
  arrange(DATE_CAUGHT) %>% 
  slice(1) %>%
  ungroup()

saveRDS(badgers_all, file = "Data/badgers_all_2023.RDS")

# Date captured
ggplot(badgers_all) + 
  geom_bar(aes(x = DATE_CAUGHT, fill = MONTH, group = MONTH)) + 
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
  facet_wrap(~SEX)

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


