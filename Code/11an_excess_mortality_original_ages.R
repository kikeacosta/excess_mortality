# Description:
# Summarize EXCESS deaths since week 8, 2020, in all countries by sex and age 
library(here)
source(here("Code/00_functions.R"))

# detach(package:MASS)

# mortality baseline estimates
baseline_files <- fs::dir_ls(here("Output", "baseline_by_country_orig_ages"))
db_all <- vroom(baseline_files)

# country codes and names
ctr_codes <- read_csv(here("Data", "country_codes.csv")) %>% 
  select(Country, PopCode) 
# Note
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Estimates for England and Wales below age 30 must 
# be excluded given the inaccurate partition of age groups
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

db <- 
  db_all %>% 
  select(Country, Year, Week, Date, Sex, Age, Deaths, Baseline, lp, up, Exposure) %>% 
  filter(Year >= 2020)

unique(db$Country)

write_rds(db, here("Output", "baseline_mortality_original_ages.rds"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Three different excess constructions:
# 1) all excess
# 2) positive excess
# 3) epidemic excess
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

db2 <- 
  db %>% 
  mutate(Epi_per = ifelse(Deaths >= up, 1, 0),
         Excess_epi = ifelse(Epi_per == 1, Deaths - Baseline, 0),
         Excess_pos = ifelse(Deaths > Baseline, Deaths - Baseline, 0),
         Excess = Deaths - Baseline,
         Age = as.character(Age))

# Excess mortality since week 8 in 2020 (March 1) 
db_sum <- 
  db2 %>% 
  filter(Date >= "2020-03-01") %>% 
  group_by(Country, Sex, Date) %>% 
  summarise(Baseline = sum(Baseline),
            Excess_epi = sum(Excess_epi),
            Excess_pos = sum(Excess_pos),
            Excess = sum(Excess),
            pscore = (Baseline + Excess_epi) / Baseline) %>% 
  ungroup()

# cumulative excess deaths starting in Week 8
cum_age <- 
  db2 %>% 
  arrange(Date) %>%
  filter(Date >= "2020-03-01") %>% 
  group_by(Country, Sex, Age) %>% 
  mutate(CumEpi = cumsum(Excess_epi),
         CumExc = cumsum(Excess),
         CumPos = cumsum(Excess_pos),
         Exposure = cumsum(Exposure)) %>% 
  arrange(Country, Sex, Age, Date) %>% 
  select(Country, Sex, Age, Date, CumEpi, CumExc, CumPos, Exposure) %>% 
  ungroup()

cum <- 
  cum_age %>% 
  group_by(Country, Date, Sex) %>% 
  summarise(CumEpi = sum(CumEpi),
            CumExc = sum(CumExc),
            CumPos = sum(CumPos),
            Exposure = sum(Exposure)) %>% 
  arrange(Country, Sex, Date) %>% 
  ungroup() %>% 
  mutate(Age = "TOT")

write_csv(cum_age, "Output/cumulative_excess_original_ages_2020_2021.csv")
write_csv(cum, "Output/cumulative_excess_all_ages_2020_2021.csv")

# saving only peru and mexico
cum_age_mxpe <- 
  cum_age %>% 
  filter(Country %in% c("Mexico", "Peru"))

write_csv(cum_age_mxpe, "Output/cumulative_excess_age_2020_2021_pemx.csv")


last_dates <- 
  cum_age %>% 
  group_by(Country) %>% 
  filter(Date == max(Date)) %>% 
  select(Country, Date) %>% 
  unique()

last_dates %>% 
  ggplot()+
  geom_point(aes(Date, reorder(Country, Date)))+
  labs(y = "Country")+
  theme_bw()

ggsave("Figures/last_date_by_country.png")

