library(here)
source(here("Code/00_functions.R"))

# Loading excess deaths
# ~~~~~~~~~~~~~~~~~~~~~
excess <- read_csv("Output/cumulative_excess_all_ages_2020_2021.csv")

last_date_uk <- excess %>% 
  group_by(Country) %>% 
  filter(Date == max(Date)) %>% 
  ungroup() %>% 
  select(Country, Date) %>% 
  unique() %>% 
  filter(Country %in% c("England_Wales",
                        "Northern Irland",
                        "Scotland")) %>% 
  pull(Date) %>% 
  min()

last_dates_exc <- 
  excess %>% 
  group_by(Country) %>% 
  filter(Date == max(Date)) %>% 
  select(Country, Date) %>% 
  unique() %>% 
  ungroup() %>% 
  filter(!Country %in% c("England_Wales",
                          "Northern Irland",
                          "Scotland")) %>% 
  bind_rows(tibble(Country = "UK", Date = last_date_uk))

# Cummulative excess deaths at the last observed date
excess2 <- excess %>%
  filter(Sex == "b",
         !(Country %in% c("England_Wales",
                          "Northern Irland",
                          "Scotland") & 
             Date >= last_date_uk)) %>% 
  # Unifying all UK countries
  mutate(Country = recode(Country,
                          "England_Wales" = "UK",
                          "Northern Irland" = "UK",
                          "Scotland" = "UK")) %>% 
  group_by(Country) %>% 
  filter(Date == max(Date)) %>% 
  summarise(CumEpi = sum(CumEpi),
            CumExc = sum(CumExc),
            CumPos = sum(CumPos),
            Exposure = sum(Exposure)) %>%
  ungroup()

cts <- unique(excess2$Country)

cts_excess <- excess2 %>% 
  select(Country) %>% 
  unique()

last_dates_exc <- 
  excess %>% 
  group_by(Country) %>% 
  filter(Date == max(Date)) %>% 
  select(Country, Date) %>% 
  unique() %>% 
  ungroup() %>% 
  rename(Date_exc = Date)



# Confirmed covid19 deaths from John Hopkins Database
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
covid_deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv") %>%
  gather(date, deaths, 5:ncol(.)) %>%
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  rename(Country = 'Country/Region',
         Date = date,
         Deaths = deaths) %>%  
  group_by(Country, Date) %>%
  summarise(Deaths = sum(Deaths)) %>%
  ungroup() %>% 
  mutate(Country = case_when(Country == "United Kingdom" ~ "UK",
                             Country == "Korea, South" ~ "Republic of Korea",
                             Country == "Taiwan*" ~ "Taiwan",
                             TRUE ~ Country))


covid_deaths2 <- covid_deaths %>% 
  filter(Country %in% cts) %>% 
  left_join(last_dates_exc) %>% 
  filter(Date == Date_exc) %>% 
  select(-Date_exc)


# Merging both sources and estimating rates
db_deaths <- covid_deaths2 %>% 
  left_join(excess2) %>% 
  rename(Confirmed = Deaths) %>% 
  gather(Confirmed, CumEpi, CumExc, CumPos, key = Source, value = Deaths) %>% 
  mutate(Mx = 100000 * Deaths / Exposure,
         Source = recode(Source,
                         "CumEpi" = "Excess (> pred. ints)",
                         "CumExc" = "Excess (all)",
                         "CumPos" = "Excess (> 0)"),
         Source = factor(Source, levels = c("Confirmed", 
                                            "Excess (> pred. ints)", 
                                            "Excess (all)", 
                                            "Excess (> 0)")))

# Plot of rates according to the source
db_deaths %>% 
  mutate(Country2 = paste0(Country, " (", Date, ")")) %>% 
  ggplot()+
  geom_point(aes(Mx, reorder(Country2, Mx), col = Source, shape = Source), alpha = 0.8, size = 2, stroke = 1)+
  scale_shape_manual(values = c(16, 16, 1, 1))+
  labs(y = "Country")+
  theme_bw()
ggsave("Figures/Confirmed_vs_excess_all_ages.png")



