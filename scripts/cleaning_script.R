
# Libraries ---------------------------------------------------------------


library(tidyverse)
library(janitor)
library(skimr)



# Load data and clean names -----------------------------------------------

# Lets read in our tables and pass through janitor::clean_names
regional <- read_csv("raw_data/regional_domestic_tourism.csv") %>% clean_names()
accom <- read_csv("raw_data/scottish_accomodation_occupancy.csv") %>% clean_names()
activities <- read_csv("raw_data/tourism_day_visits_activities.csv") %>% clean_names()
demo <- read_csv("raw_data/tourism_day_visits_demographics.csv") %>% clean_names()
location <- read_csv("raw_data/tourism_day_visits_location.csv") %>% clean_names()
transport <- read_csv("raw_data/tourism_day_visits_transport.csv") %>% clean_names()
int_data <- readxl::read_xlsx("raw_data/international_2019.xlsx") %>% clean_names()
region_codes <- read_csv("raw_data/scotland_codes - Sheet1.csv")

# regional cleaning -------------------------------------------------------

summary(regional)
glimpse(regional)
skim(regional) # no missing data at all

# we can lose measurement column, otherwise table can stay
regional <- regional %>% 
  select(-measurement) %>% 
  # join with the region_codes table to get the names of the local authority
  left_join(region_codes, by = c("feature_code" = "local_authority_code"))


# accom cleaning ----------------------------------------------------------

summary(accom)
glimpse(accom)
skim(accom) # no missing data at all

accom <- accom %>% 
  # lets create a new column with the pertinent details of the row in one place
  mutate(accom = case_when(
    weekday_weekend != "All" ~ weekday_weekend,
    size_of_accommodation != "All" ~ size_of_accommodation,
    location != "All" ~ location,
    TRUE ~ "All"
  )) %>% # we can lose these lines as we will be using Room occupancy stats
  filter(accommodation_type_and_occupancy != "Guest House/B&B - Bed Occupancy",
         accommodation_type_and_occupancy != "Hotels - Bed Occupancy") %>% 
  # we can lose these useless columns
  select(-c(measurement, units, feature_code))


# activities cleaning -----------------------------------------------------

summary(activities)
glimpse(activities)
skim(activities) # no missing data at all

activities <- activities %>% 
  # we can lose these useless columns
  select(-c(measurement, feature_code))


# demo cleaning -----------------------------------------------------------

summary(demo)
glimpse(demo)
skim(demo) # no missing data at all

demo <- demo %>% 
  # lets create a new column with data from the pre summarised data - not ideal
  mutate(demo = case_when(
    age != "All" ~ age,
    marital_status != "All" ~ marital_status,
    gender != "All" ~ gender,
    employment_status != "All" ~ employment_status,
    children != "All" ~ children,
    access_to_car != "All" ~ access_to_car,
    social_grade != "All" ~ paste0("Social Grade - " ,social_grade),
    TRUE ~ "All"
  )) %>% 
  select(-c(feature_code, measurement))


# location cleaning -------------------------------------------------------

summary(location)
glimpse(location)
skim(location) # no missing data at all

location <- location %>% 
  # we can lose these useless columns
  select(-c(measurement, feature_code))


# transport cleaning ------------------------------------------------------

summary(transport)
glimpse(transport)
skim(transport) # no missing data at all

transport <- transport %>% 
  # we can lose these useless columns
  select(-c(measurement, feature_code))

# international data cleaning ---------------------------------------------

int_data <- int_data %>% 
  mutate(across(2:7, ~ as.factor(.x)))

# write clean data --------------------------------------------------------

regional %>%  write_csv("clean_data/regional_domestic_tourism.csv") 
accom %>%  write_csv("clean_data/scottish_accomodation_occupancy.csv") 
activities %>% write_csv("clean_data/tourism_day_visits_activities.csv") 
demo %>%  write_csv("clean_data/tourism_day_visits_demographics.csv") 
location %>%  write_csv("clean_data/tourism_day_visits_location.csv")
transport %>%  write_csv("clean_data/tourism_day_visits_transport.csv") 
int_data %>% write_csv("clean_data/int_data")
