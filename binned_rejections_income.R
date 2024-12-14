library(tidyverse)
library(janitor)
library(stringr)

#Sum the number of contested projects by county median income group

binned_rejections_income <- rejections_merged %>%
  filter(!is.na(median_income)) %>% 
  group_by(group = cut(median_income, 
                       breaks = seq(0, max(median_income, na.rm = TRUE) + 10000, 10000),
                       labels = paste0(
                          seq(0, max(median_income, na.rm = TRUE), 10000), 
                          "-", 
                          seq(10000, max(median_income, na.rm = TRUE) + 10000, 10000)
                       ))) %>%
  summarise(contested_projects = sum(contested_project_total, na.rm = TRUE)) %>% 
  mutate(percent_rejections = contested_projects / sum(contested_projects)) #Calculate percent of all projects for each income group

#Count the number of U.S. counties within each median income group
binned_income_all_counties <- characteristics %>% 
  filter(!is.na(median_income)) %>%  
  group_by(group = cut(median_income, 
                       breaks = seq(0, max(median_income, na.rm = TRUE) + 10000, 10000),
                       labels = paste0(
                         seq(0, max(median_income, na.rm = TRUE), 10000), 
                         "-", 
                         seq(10000, max(median_income, na.rm = TRUE) + 10000, 10000)
                       ))) %>%
  summarise(count = n()) %>% 
  mutate(percent_all_counties_median_income = count / sum(count)) #Calculate percent of all counties for each income group

#Join all-county counts by median income group with rejected project count per median income group

joined_binned_income <- binned_income_all_counties %>% 
  left_join(binned_rejections_income, by = "group")

joined_binned_income %>% 
  write_csv("binned_income_rejections.csv")

#Sum the number of contested projects by population density group
binned_rejections_density <- rejections_merged %>%
  filter(!is.na(density)) %>% 
  group_by(group = cut(density, 
                       breaks = seq(0, max(density, na.rm = TRUE) + 50, 50),
                       labels = paste0(
                         seq(0, max(density, na.rm = TRUE), 50), 
                         "-", 
                         seq(50, max(density, na.rm = TRUE) + 50, 50)
                       ))) %>%
  summarise(contested_projects = sum(contested_project_total, na.rm = TRUE)) %>% 
  mutate(percent_rejections = contested_projects / sum(contested_projects)) #Calculate percent of all projects for each population density group


#Count the number of U.S. counties within each population density group
binned_density_all_counties <- characteristics %>% 
  filter(!is.na(density)) %>%  
  group_by(group = cut(density, 
                       breaks = seq(0, max(density, na.rm = TRUE) + 50, 50),
                       labels = paste0(
                         seq(0, max(density, na.rm = TRUE), 50), 
                         "-", 
                         seq(50, max(density, na.rm = TRUE) + 50, 50)
                       ))) %>%
  summarise(count = n()) %>% 
  mutate(percent_all_counties_density = count / sum(count)) #Calculate percent of all counties for each population density group

joined_binned_density <- binned_density_all_counties %>% 
  left_join(binned_rejections_density, by = "group")

joined_binned_density <- joined_binned_density %>%
  mutate(
    count = format(count, scientific = FALSE),
    percent_all_counties_density = format(percent_all_counties_density, scientific = FALSE)
  )

joined_binned_density %>% 
  write_csv("binned_density_rejections.csv")

