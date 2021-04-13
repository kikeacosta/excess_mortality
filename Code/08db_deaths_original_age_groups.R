library(here)
source(here("Code", "00_functions.R"))

# loading Peru and Mexico data
# ============================ #
db_pe_mx <- read_rds(here("Data", "weekly_deaths_peru_and_mexico.rds"))

# STMF data
# ========= #
# downloading the last version of STMF Mortality input data zip 
download.file("https://www.mortality.org/Public/STMF/Inputs/STMFinput.zip", here("Data/STMFinput.zip"))
zipdf <- unzip(here("Data", "STMFinput.zip"), list = TRUE)

# loading all cause deaths from all countries in STMF
db_d <- tibble()
for(i in 1:length(zipdf$Name)){
  csv_file <- zipdf$Name[i]
  print(csv_file)
  temp <- read_csv(unz(here("Data", "STMFinput.zip"), csv_file),
                   col_types = cols(.default = "c"))
  db_d <- db_d %>% 
    bind_rows(temp)
}

test <- db_d %>% 
  mutate(id = 1:n())

# info on country names and codes
ctr_codes <- read_csv(here("Data", "country_codes.csv")) %>% 
  select(Country, PopCode)

# merging STMF and others
db_d2 <- 
  db_d %>% 
  mutate(PopCode = ifelse(PopCode == "AUS2", "AUS", PopCode)) %>% 
  left_join(ctr_codes) %>% 
  select(Country, Year, Week, Sex, Age, Deaths) %>% 
  mutate(Year = as.integer(Year),
         Deaths = as.integer(Deaths)) %>% 
  bind_rows(db_pe_mx %>% select(-year_week)) %>% 
  filter(Year >= 2010)

# preparing mortality data
# ~~~~~~~~~~~~~~~~~~~~~~~~

# distributing deaths at unknown weeks and ages
db_d3 <- db_d2 %>% 
  mutate(Age = ifelse(Age == "Unknown", "UNK", Age),
         Week = ifelse(is.na(Week), "UNK", Week)) %>% 
  group_by(Country, Year, Week, Sex) %>% 
  # scale TOT
  do(dist_tot(chunk = .data)) %>% 
  # redistribute UNK Age
  do(dist_unk(chunk = .data)) %>% 
  ungroup() 

# redistribute UNK Week
db_d4 <- db_d3 %>% 
  group_by(Country, Year, Sex, Age) %>% 
  do(dist_unk_week(chunk = .data)) %>% 
  ungroup() %>% 
  mutate(Age = as.integer(Age),
         Week = as.integer(Week)) %>% 
  arrange(Country, Year, Week, Sex, Age)

db_d5 <- db_d4 %>% 
  filter(Sex != "UNK") %>% 
  group_by(Country, Year, Week, Sex, Age) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Sex,
              values_from = Deaths) %>% 
  mutate(m = m / (m + f) * b,
         f = f / (m + f) * b,
         m = ifelse(is.nan(m),0,m),
         f = ifelse(is.nan(f),0,f)) %>% 
  pivot_longer(m:b, names_to = "Sex", values_to = "Deaths") %>% 
  ungroup() %>% 
  arrange(Country, Year, Week, Sex, Age) %>% 
  drop_na(Deaths) %>% 
  mutate(Country = ifelse(Country == "Northern Irland", "Northern Ireland", Country))
  
unique(db_d5$Week)
unique(db_d5$Age) %>% sort()
unique(db_d5$Sex)
unique(db_d5$Country)

write_rds(db_d5, here("Output", "stmf_std.rds"))
db_d5 <- read_rds(here("Output", "stmf_std.rds"))

# looking at age configurations
# ============================= #

db_ages2 <- db_d5 %>% 
  group_by(Country, Year, Week, Sex) %>% 
  mutate(AgeGroups = sum(n())) %>% 
  ungroup() %>% 
  group_by(Country, AgeGroups, Age, Sex) %>% 
  mutate(periods = sum(n())) %>% 
  ungroup() %>% 
  select(Country, AgeGroups, periods, Age) %>% 
  unique() 

confs <- db_ages2 %>% 
  select(Country, AgeGroups, periods) %>% 
  unique() %>% 
  group_by(Country) %>% 
  mutate(confs = n()) %>% 
  arrange(-confs, Country)

# 8 countries with more than 1 age configuration in the time series:

# 1 AUT             2
# 2 FRATNP          2
# 3 GBR_NIR         2
# 4 GBR_SCO         2
# 5 GBRTENW         2
# 6 ITA             2
# 7 NLD             2
# 8 USA             2

# Decisions:
# Keep only deaths since 2010
# Keep age groups for those with 95+
# Aggregate ages in 5 year age groups


# Aggregate US data into minimum common, that is, groups before 2020,
# and exclude info about sex
usa_ages <- c(0, 25, 45, 65, 75, 85, 110)
labs <- c(0, 25, 45, 65, 75, 85)
usa <- 
  db_d5 %>% 
  filter(Country == "United States",
         Sex == "b") %>% 
  mutate(Age_int = cut(Age, breaks = usa_ages, include.lowest = TRUE, right = FALSE, labels = labs)) %>% 
  group_by(Country, Year, Week, Sex, Age_int) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(Age = as.integer(as.character(Age_int))) %>% 
  select(-Age_int)

# Northern Ireland, Netherlands, and Austria remove last wide groups
nds_aut_nir <- 
  db_d5 %>% 
  filter(Country %in% c("Austria", "Netherlands", "Northern Ireland")) %>% 
  group_by(Country, Year, Week, Sex) %>%
  filter(n() == 19)

# England and Wales, confs before 2020
eng_ages <- c(0, 15, 45, 65, 75, 85, 110)
labs <- c(0, 15, 45, 65, 75, 85)
eng <- 
  db_d5 %>% 
  filter(Country == "England_Wales") %>% 
  mutate(Age_int = cut(Age, breaks = eng_ages, include.lowest = TRUE, right = FALSE, labels = labs)) %>% 
  group_by(Country, Year, Week, Sex, Age_int) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(Age = as.integer(as.character(Age_int))) %>% 
  select(-Age_int)

# Scotland and France close at 90
fra_sco <- 
  db_d5 %>% 
  filter(Country %in% c("France", "Scotland")) %>% 
  mutate(Age = as.double(Age),
         Age = case_when(Age < 5 ~ 0,
                         Age > 90 ~ 90, 
                         TRUE ~ Age)) %>% 
  group_by(Country, Year, Week, Sex, Age) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup()

# Italy 5-year age groups and close at 90
ita_ages <- c(seq(0, 90, 5), 100)
labs <- seq(0, 90, 5)
ita <- 
  db_d5 %>% 
  filter(Country == "Italy") %>% 
  mutate(Age_int = cut(Age, breaks = ita_ages, include.lowest = TRUE, right = FALSE, labels = labs)) %>% 
  group_by(Country, Year, Week, Sex, Age_int) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(Age = as.integer(as.character(Age_int))) %>% 
  select(-Age_int)


fixes <- c("United States",
           "Austria", "Netherlands", "Northern Ireland",
           "England_Wales",
           "France", "Scotland", 
           "Italy")

db_d6 <- 
  db_d5 %>% 
  filter(!Country %in% fixes) %>% 
  bind_rows(usa, 
            nds_aut_nir,
            eng,
            fra_sco,
            ita) %>% 
  arrange(Country, Year, Week, Sex, Age) %>% 
  filter(Country != "Russia") %>% 
  mutate(Sex = recode(Sex,
                      "b" = "t"))

db_ages3 <- db_d6 %>% 
  select(Country, Year, Week, Sex, Age, Deaths) %>% 
  filter(Age != "UNK" & Age != "TOT") %>% 
  group_by(Country, Year, Week, Sex) %>% 
  mutate(AgeGroups = sum(n())) %>% 
  ungroup() %>% 
  group_by(Country, AgeGroups, Age, Sex) %>% 
  mutate(periods = sum(n())) %>% 
  ungroup() %>% 
  select(Country, AgeGroups, periods, Age) %>% 
  unique() 


confs2 <- db_ages3 %>% 
  select(Country, AgeGroups, periods) %>% 
  unique() %>% 
  group_by(Country) %>% 
  mutate(confs = n())

write_rds(db_d6, here("Output", "deaths_stmf_original_ages.rds"))

db_d6 %>% 
  mutate(date = ISOweek::ISOweek2date(paste0(Year, "-W", sprintf("%02d",Week), "-7"))) %>% 
  filter(Country == "United States",
         Sex == "b",
         Age == 85) %>% 
  ggplot()+
  geom_line(aes(date, Deaths))

