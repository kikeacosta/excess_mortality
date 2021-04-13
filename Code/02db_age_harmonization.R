library(here)
source(here("Code/00_functions.R"))


# downloading the last version of STMF Mortality input data zip 
download.file("https://www.mortality.org/Public/STMF/Inputs/STMFinput.zip", here("Data/STMFinput.zip"))
zipdf <- unzip(here("Data/STMFinput.zip"), list = TRUE)

# loading all cause deaths from all countries in STMF
db_d <- tibble()
for(i in 1:length(zipdf$Name)){
  csv_file <- zipdf$Name[i]
  print(csv_file)
  temp <- read_csv(unz(here("Data/STMFinput.zip"), csv_file))
  db_d <- db_d %>% 
    bind_rows(temp)
}

test <- db_d %>% 
  mutate(id = 1:n())

# info on country names and codes
ctr_codes <- read_csv(here("Data/country_codes.csv")) %>% 
  select(Country, PopCode)

# loading exposures from the wpp estimates
Offsets <- read_rds(here("Output", "annual_exposures_stmf.rds")) %>% 
  rename(Population = Pop)

unique(Offsets$Country)


# preparing mortality data
# ~~~~~~~~~~~~~~~~~~~~~~~~

# distributing deaths at unknown weeks and ages
db_d2 <- db_d %>% 
  select(-Access, -Type) %>% 
  mutate(Age = ifelse(Age == "Unknown", "UNK", Age),
         Week = ifelse(is.na(Week), "UNK", Week)) %>% 
  group_by(PopCode, Year, Week, Sex) %>% 
  # scale TOT
  do(dist_tot(chunk = .data)) %>% 
  # redistribute UNK Age
  do(dist_unk(chunk = .data)) %>% 
  ungroup() 

# redistribute UNK Week
db_d3 <- db_d2 %>% 
  group_by(PopCode, Year, Sex, Age) %>% 
  do(dist_unk_week(chunk = .data)) %>% 
  ungroup() %>% 
  mutate(Age = as.integer(Age),
         Week = as.integer(Week)) %>% 
  arrange(PopCode, Year, Week, Sex, Age)

db_d4 <- db_d3 %>% 
  group_by(PopCode, Year, Week) %>% 
  # scaling sexes to add total deaths
  do(scale_sexes(chunk = .data)) %>% 
  ungroup() %>% 
  left_join(ctr_codes) %>% 
  group_by(Country, 
           Year,
           Week,
           Sex) %>% 
  mutate(AgeInterval = case_when(
    AgeInterval == "+" ~ as.character(101 - max(Age)),
    TRUE ~ AgeInterval),
    AgeInterval = as.integer(AgeInterval)) %>% 
  rename(AgeInt = AgeInterval) %>% 
  select(Country, PopCode, Year, Week, Sex, Age, AgeInt, Deaths) %>% 
  arrange(Country, Year, Week, Sex, Age) %>% 
  ungroup()
  
unique(db_d4$Week)
unique(db_d4$Age) %>% sort()
unique(db_d4$Sex)
unique(db_d4$Country)

write_rds(db_d4, here("Output", "stmf_std.rds"))

db_d4 <- read_rds(here("Output", "stmf_std.rds"))

# selecting countries that need age harmonization
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# looking at age configurations
db_ages <- db_d4 %>% 
  select(PopCode, Year, Week, Sex, Age, Deaths) %>% 
  filter(Age != "UNK" & Age != "TOT") %>% 
  group_by(PopCode, Year, Week, Sex) %>% 
  mutate(AgeGroups = sum(n())) %>% 
  ungroup()

# countries with at least 19 age groups
db_19 <- db_ages %>%
  filter(Year >= 2010,
         AgeGroups >= 19) %>% 
  select(PopCode, Year, Week, Sex, Age, Deaths) %>% 
  drop_na()

table(db_19$PopCode)

# countries with less than 19 age groups
db_to_ungr <- db_ages %>%
  filter(Year >= 2010,
         AgeGroups < 19) %>% 
  select(PopCode, Year, Week, Sex, Age, Deaths)

table(db_to_ungr$PopCode)

db_to_ungr %>%
  select(PopCode, Year, Week) %>%
  unique() %>% 
  group_by(PopCode) %>% 
  summarise(min_year = min(Year),
            max_year = max(Year),
            weeks = sum(n()))

################################

# grouping ages in 5-year groups for countries with 19+ age intervals
db_d5 <- db_d4 %>% 
  select(Country, PopCode, Year, Week, Sex, Age, Deaths) %>% 
  filter(Age != "UNK" & Age != "TOT") %>% 
  mutate(Age = as.numeric(Age), 
         Age = case_when(Age >= 90 ~ 90, 
                         Age < 5 ~ 0,
                         TRUE ~ Age)) %>% 
  group_by(Country, PopCode, Year, Week, Sex, Age) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  drop_na()

db_d6 <- db_d5 %>% 
  group_by(Country, PopCode, Year, Week, Sex) %>% 
  mutate(AgeGroups = sum(n()),
         last = ifelse(1:n() == AgeGroups, 1, 0),
         AgeInt = case_when(last == 0 ~ lead(Age) - Age, 
                            last == 1 ~ 101 - Age)) %>% 
  ungroup()

# countries that don't need ungrouping of deaths
db_ok <- db_d6 %>%
  filter(Year >= 2010,
         AgeGroups == 19) %>% 
  select(Country, PopCode, Year, Week, Sex, Age, AgeInt, Deaths) %>% 
  drop_na()

# last week of observation by country
last_weeks <- db_ok %>% 
  filter(Year == 2020) %>% 
  group_by(PopCode) %>% 
  summarise(max_week = max(Week))

unique(db_ok$PopCode)

table(db_ok$PopCode)

# series that need ungrouping of deaths into 5-year age intervals
db_to_ungr <- db_d6 %>%
  filter(Year >= 2010,
         AgeGroups < 19) %>% 
  select(Country, PopCode, Year, Week, Sex, Age, AgeInt, Deaths)

unique(db_to_ungr$PopCode)

table(db_to_ungr$PopCode)

db_to_ungr %>%
  select(Country, PopCode, Year, Week) %>%
  unique() %>% 
  group_by(PopCode) %>% 
  summarise(min_year = min(Year),
         max_year = max(Year),
         weeks = sum(n()))

# AUT, NLD, and SWE have wide age groups only during a the last weeks,
# Deaths during this few weeks could be ungrouped based on previous distributions 

exc_ungr <- c("AUT", "NLD", "SWE", "GBR_NIR")

stmf <- db_to_ungr %>% 
  filter(!(PopCode %in% exc_ungr))

# ungrouping all ages for those intervals larger than 5-year
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# # for testing with a feww weeks
# stmf <- stmf %>%
#   filter(Country == "Canada",
#          Sex == "b",
#          Week == 45,
#          Year >= 2020)
# test <- try_harmonize_age_p(stmf, Offsets)

# Using Tim's functions to ungroup age intervals
start_time <- Sys.time()
stmf_out <-
  split(stmf,
        list(stmf$Country,
             stmf$Year,
             stmf$Week,
             stmf$Sex),
        drop = TRUE) %>%
  parallelsugar::mclapply(try_harmonize_age_p, Offsets = Offsets, mc.cores = 20)
end_time <- Sys.time()
end_time - start_time

stmf_out2 <- stmf_out %>% 
  bind_rows()

# Close age groups at 90
stmf_out3 <- stmf_out2 %>% 
  mutate(Age = ifelse(Age > 90, 90, Age)) %>% 
  group_by(Country, PopCode, Year, Week, Sex, Age) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(Source = "Ungrouped")

db_ok2 <- db_ok %>%
  select(Country, PopCode, Year, Week, Sex, Age, Deaths) %>% 
  mutate(Source = "Original")

# appending populations originally in 5 years with those ungrouped
db_deaths <- bind_rows(db_ok2, stmf_out3) %>% 
  arrange(Country, PopCode, Year, Week, Sex, Age) 

unique(db_deaths$PopCode)

write_rds(db_deaths, here("Output/deaths_stmf_age5.rds"))

db_deaths %>% 
  mutate(date = ISOweek::ISOweek2date(paste0(Year, "-W", sprintf("%02d",Week), "-7"))) %>% 
  filter(PopCode == "CAN",
         Sex == "b",
         Age == 50) %>% 
  ggplot()+
  geom_line(aes(date, Deaths))

