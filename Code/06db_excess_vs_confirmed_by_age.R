rm(list=ls())
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")
library(tidyverse)
library(readr)
library(lubridate)
library(ggplot2)
library(osfr)

# reading Canada data
#####################

# # OSF Data - Output_10
# osf_retrieve_file("43ucn") %>%
#   osf_download(path = "Data/", conflicts = "overwrite") 
# 
# db_cov <-  read_csv("Data/Output_10.zip",
#                     skip = 3,
#                     col_types = "ccccciiddd")

# # OSF Data - Output_5
# osf_retrieve_file("7tnfh") %>%
#   osf_download(path = "Data/", conflicts = "overwrite")

db_cov <-  read_csv("Data/Output_5.zip",
                    skip = 3,
                    col_types = "ccccciiddd")



# Excess mortality
# ~~~~~~~~~~~~~~~~
db_exc <- read_csv("Output/cumulative_excess_age_2020_2021.csv")

cts <- db_exc %>% 
  select(Country) %>% 
  unique() %>% 
  pull() %>% 
  sort()

db_exc_to_comb <- db_exc %>% 
  select(Country, Date) %>% 
  unique() %>% 
  mutate(in_excess = 1)

# ~~~~~~~~~~~~~~~~~~~

# COVerAGE data
db_cov2 <- db_cov %>% 
  filter(Country %in% cts,
         Region == "All") %>% 
  mutate(Date = dmy(Date)) %>% 
  select(Country, Date, Sex, Age, Deaths) %>% 
  drop_na() %>% 
  group_by(Country, Sex, Date) %>% 
  filter(sum(Deaths) > 0) %>% 
  ungroup()

db_cov_to_comb <- db_cov2 %>% 
  select(Country, Date) %>% 
  unique() %>% 
  mutate(in_cvrg = 1)


# the most recent date in both databases
db_both <- 
  full_join(db_cov_to_comb, db_exc_to_comb) %>% 
  drop_na() %>% 
  group_by(Country) %>% 
  filter(Date == max(Date)) %>% 
  ungroup()
  
# 
db_deaths <- db_cov2 %>% 
  left_join(db_both) %>% 
  drop_na() %>% 
  left_join(db_exc) %>% 
  select(-in_cvrg, -in_excess) %>% 
  rename(Confirmed = Deaths,
         Excess = CumEpi) 


write_rds(db_deaths, "Output/db_confirmed_and_excess_deaths.rds")



