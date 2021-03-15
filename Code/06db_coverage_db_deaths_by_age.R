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

# OSF Data - Output_5
osf_retrieve_file("7tnfh") %>%
  osf_download(path = "Data/", conflicts = "overwrite")

db_cov <-  read_csv("Data/Output_5.zip",
                    skip = 3,
                    col_types = "ccccciiddd")


db_exc <- read_rds("Output/db_for_baseline_monthly_age5.rds")

cts <- db_exc %>% 
  select(Country) %>% 
  unique() %>% 
  pull() %>% 
  sort()

db_cov2 <- db_cov %>% 
  filter(Country %in% cts,
         Region == "All") %>% 
  mutate(Date = dmy(Date)) %>% 
  select(Country, Date, Sex, Age, Deaths) %>% 
  drop_na() %>% 
  group_by(Country, Sex) %>% 
  mutate(last_date = max(Date),
         Deaths_max = max(Deaths)) %>% 
  ungroup() %>% 
  filter(Date == last_date,
         Deaths_max != 0)

unique(db_cov2$Country)

dates <- db_cov2 %>% 
  select(Country, Date) %>% 
  unique()

write_rds(db_cov2, "Output/db_coverage_db_last dates_countries_stmf_age5.rds")
