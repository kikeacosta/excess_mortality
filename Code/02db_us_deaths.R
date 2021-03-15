library(here)
source(here("Code/00_functions.R"))


# downloading US weekly all cause mortality from CDC
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

usa_cdc <- read_csv("https://data.cdc.gov/api/views/y5bj-9g5w/rows.csv?accessType=DOWNLOAD&bom=true&format=true%20target=")
usa_cdc2 <- usa_cdc %>%
  rename(Date = 2,
         Age = 6,
         Deaths = 7) %>%
  group_by(Date, Age) %>%
  summarise(Deaths = sum(Deaths)) %>%
  ungroup() %>%
  mutate(Date = mdy(Date),
         Age = ifelse(str_detect(Age, "Under"), "0", str_sub(Age, 1, 2)),
         Age = as.integer(Age),
         Week = isoweek(Date),
         Year = format(Date, "%G"),
         PopCode = "USA",
         Sex = "b") %>%
  drop_na() %>% 
  arrange(Date, Age) %>% 
  select(PopCode, Year, Week, Sex, Age, Deaths) %>% 
  group_by(PopCode, Year, Week, Sex) %>% 
  mutate(AgeInt = ifelse(Age == 85, 16, lead(Age) - Age),
         Country = PopCode)


# downloading US from STMF output data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
download.file("https://www.mortality.org/Public/STMF/Outputs/stmf.xlsx", here("Data", "stmf.xlsx"))

usa_stmf <- read_xlsx(here("Data", "stmf.xlsx"),
                  sheet = "USA", 
                  skip = 2)

usa_stmf2 <- usa_stmf %>% 
  select(1:10) %>% 
  gather(-c(Country, Year, Week, Sex), key = "Age", value = "Deaths") %>% 
  mutate(Age = case_when(str_sub(Age, 1, 2) == "0-" ~ "0",
                         str_sub(Age, 1, 2) == "To" ~ "All",
                         TRUE ~ str_sub(Age, 1, 2)),
         Age = as.integer(Age)) %>% 
  drop_na() %>% 
  # arrange(Date, Age) %>% 
  group_by(Country, Year, Week, Sex) %>% 
  mutate(AgeInt = ifelse(Age == 85, 16, lead(Age) - Age),
         PopCode = Country) %>% 
  ungroup()

# loading exposures
offsets <- read_rds(here("Output", "annual_exposures_stmf.rds")) 

usa_offsets <- offsets %>% 
  rename(Population = Pop) %>% 
  filter(PopCode == "USA")

# # for testing with a feww weeks
usa_stmf2 <- usa_stmf2 %>%
  filter(Sex == "f",
         Week >= 20,
         Year == 2020)
test <- harmonize_age_p(usa_stmf2, usa_offsets)

# ungrouping US STMF
# ~~~~~~~~~~~~~~~~~~
usa_out <-
  split(usa_stmf2,
        list(usa_stmf2$Year,
             usa_stmf2$Week,
             usa_stmf2$Sex),
        drop = TRUE) %>%
  parallelsugar::mclapply(try_harmonize_age_p, Offsets = usa_offsets, mc.cores = 6)

usa_out2 <- usa_out %>% 
  bind_rows()

# sex distribution from stmf
usa_stmf_dist <- 
  usa_out2 %>% 
  filter(Sex != "b") %>% 
  group_by(Year, Week, Age) %>% 
  mutate(all_d = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(dist = Deaths / all_d) %>% 
  select(Year, Week, Sex, Age, dist) %>% 
  spread(Sex, dist)

unique(usa_out2$Sex)

# ungrouping US CDC
# ~~~~~~~~~~~~~~~~~
usa_cdc_out <-
  split(usa_cdc2,
        list(usa_cdc2$Year,
             usa_cdc2$Week,
             usa_cdc2$Sex),
        drop = TRUE) %>%
  parallelsugar::mclapply(try_harmonize_age_p, Offsets = usa_offsets, mc.cores = 6)

usa_cdc_out2 <- usa_cdc_out %>% 
  bind_rows()

usa_cdc_out3 <- usa_cdc_out2 %>%
  mutate(Year = as.integer(Year)) %>% 
  left_join(usa_stmf_dist) %>% 
  rename(f_d = f,
         m_d = m) %>% 
  mutate(f = Deaths * f_d,
         m = Deaths * m_d) %>% 
  select(-f_d, -m_d, -Sex) %>% 
  rename(b = Deaths) %>% 
  gather(f, m, b, key = "Sex", value = Deaths)

# Close age groups at 90
usa_cdc_out4 <- usa_cdc_out3 %>% 
  mutate(Age = ifelse(Age > 90, 90, Age)) %>% 
  group_by(Country, PopCode, Year, Week, Sex, Age) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(Source = "Ungrouped")

# Adding the US to the rest of countries
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
db_deaths <- read_rds("Output/deaths_stmf_age5.rds")

db_deaths2 <- db_deaths %>% 
  filter(PopCode!= "USA") %>% 
  bind_rows(usa_cdc_out4)

write_rds(db_deaths2, here("Output/deaths_stmf_age5.rds"))

unique(db_deaths$PopCode) %>% sort
