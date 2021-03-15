

excess <- read_csv("Output/cumulative_excess_2020_2021.csv")

#### covid19 deaths
corona_deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv") %>%
  gather(date, deaths, 5:ncol(.)) %>%
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  rename(country = 'Country/Region') %>%  
  group_by(country, date) %>%
  summarise(deaths = sum(deaths)) %>%
  ungroup() %>% 
  mutate(country = case_when(country == "United Kingdom" ~ "UK",
                             country == "Taiwan*" ~ "Taiwan",
                             country == "Korea, South" ~ "South Korea",
                             country == "Taiwan*" ~ "Taiwan",
                             TRUE ~ country))




excess2 <- excess %>%
  filter(Sex == "b", ) %>% 
  group_by(Country) %>% 
  filter(Date == max(Date)) %>% 
  ungroup()
