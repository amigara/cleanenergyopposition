library(tidyverse)
library(janitor)
library(stringr)

#Read csv files for county data on population, land area, median income, rejected projects and restrictive policies

#Median income
income <- read_csv("ACSST5Y2022.S1901-Data_income.csv") %>% 
  select(GEO_ID, NAME, S1901_C01_012E) %>% 
  mutate(
    geoid_5_digit = str_sub(GEO_ID, -5, -1),
    S1901_C01_012E = as.numeric(S1901_C01_012E)
  )

#Population
population <- read_csv("ACSDT5Y2022.B01003-Data_population.csv") %>% 
  mutate(
    geoid_5_digit= str_sub(GEO_ID, -5, -1),
    B01003_001E = as.numeric(B01003_001E)
  )

#Land area
area <- read_csv("land_area.csv") %>% 
  mutate(
    geoid_5_digit = as.character(GEOID), 
    geoid_5_digit = ifelse(
      str_length(geoid_5_digit) == 4, 
      str_pad(geoid_5_digit, 5, pad = "0"),
      geoid_5_digit
    ))

#Solar and wind technical potential by county
techpot = read_csv("techpot_baseline_county.csv") %>% 
  clean_names() %>% 
  pivot_wider(
    names_from = technology,
    values_from = technical_generation_potential_m_wh
  ) %>% 
  mutate(geoid_5_digit = substr(geography_id, 2, 3) %>% 
           paste0(substr(geography_id, 5, nchar(geography_id) - 1)))

#Solar and wind technical potential by state
techpot_state <- read_csv("techpot_baseline_state_with_abbreviations.csv") %>% 
  clean_names() %>% 
  rename(USPS = state_abbreviation) %>% 
  pivot_wider(
    names_from = technology,
    values_from = technical_generation_potential_m_wh_m_wh
  )

#Restrictions and project rejections by county
restrictions <- read_csv("renewable_restrictions_by_county.csv") %>% 
  rename(geoid_5_digit = GEO_ID) %>% 
  clean_names()
rejections <- read_csv("contested_projects_by_county.csv") %>% 
  rename(geoid_5_digit = GEO_ID) %>% 
  clean_names()

#Merge the county characteristics data
characteristics <- income %>%
  left_join(population, by = "geoid_5_digit") %>%
  left_join(area, by = "geoid_5_digit") %>% 
  left_join(techpot, by = "geoid_5_digit") %>% 
  rename(median_income = S1901_C01_012E,
         population = B01003_001E)

#Calculate population density
characteristics <- characteristics %>% 
  mutate(density = population / ALAND_SQMI)

#Create merged rejections data
rejections_merged <- rejections %>% 
  left_join(characteristics, by ="geoid_5_digit") %>% 
  mutate(across(c(pumped_storage, solar, solar_thermal, transmission_line, wind), ~ replace_na(., 0)))

#Create merged restrictions data
restrictions_merged <- restrictions %>% 
  left_join(characteristics, by ="geoid_5_digit") %>% 
  mutate(across(c(battery_storage, solar, transmission_line, wind), ~ replace_na(., 0)))

#Calculate normalized rejections
rejections_merged <- rejections_merged %>% 
  mutate(solar_normalized = solar / utility_pv,
         wind_normalized = wind / land_based_wind, 
         solar_mw_normalized = mw_solar_contested / utility_pv,
         wind_mw_normalized = mw_wind_contested / land_based_wind,
         solar_and_wind_normalized = solar_normalized + wind_normalized,
         solar_and_wind_mw_normalized = solar_mw_normalized + wind_mw_normalized
  )


#Calculate normalized restrictions
restrictions_merged <- restrictions_merged %>% 
  mutate(solar_normalized = solar / utility_pv,
         wind_normalized = wind / land_based_wind,
         solar_and_wind_normalized = solar_normalized + wind_normalized
  )

#Export merged normalized data
restrictions_merged %>% 
  write_csv("restrictions_merged.csv")

rejections_merged %>% 
  write_csv("rejections_merged.csv")

#Summarize restrictive policies by state and join with technical potential by state
state_restrictions_merged <- restrictions_merged %>% 
  group_by(USPS) %>% 
  summarise(restrictive_policies_total = sum(total_restrictions, na.rm = TRUE),
            solar_restrictive_policies = sum(solar, na.rm = TRUE),
            wind_restrictive_policies = sum(wind, na.rm = TRUE)) %>% 
  left_join(techpot_state, by = "USPS") %>% 
  mutate(solar_restrictions_normalized = solar_restrictive_policies / utility_pv,
         wind_restrictions_normalized = wind_restrictive_policies / land_based_wind,
         solar_and_wind_restrictions_normalized = solar_restrictions_normalized + wind_restrictions_normalized,
         solar_and_wind_restrictions_normalized_per_1000_gw = solar_and_wind_restrictions_normalized*1000000
        )

state_rejections_merged <- rejections_merged %>% 
  group_by(USPS) %>% 
  summarise(rejections_total = sum(contested_project_total, na.rm = TRUE),
            solar_rejections = sum(solar, na.rm = TRUE),
            wind_rejections = sum(wind, na.rm = TRUE)) %>% 
  left_join(techpot_state, by = "USPS") %>% 
  mutate(solar_rejections_normalized = solar_rejections / utility_pv,
         wind_rejections_normalized = wind_rejections / land_based_wind,
         solar_and_wind_rejections_normalized = solar_rejections_normalized + wind_rejections_normalized,
         solar_and_wind_rejections_normalized_per_1000_gw = solar_and_wind_rejections_normalized*1000000
  )

#Export state-level merged normalized data
state_restrictions_merged %>% 
  write_csv("state_restrictions_merged.csv")

state_rejections_merged %>% 
  write_csv("state_rejections_merged.csv")