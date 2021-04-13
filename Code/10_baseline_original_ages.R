library(here)
source(here("Code/00_functions.R"))

# Creating the master database with weekly deaths and exposures
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ctr_codes <- read_csv(here("Data", "country_codes.csv"))

db_d <- read_rds(here("Output", "deaths_stmf_original_ages.rds")) 
db_p <- read_rds(here("Output", "pop_interpol_week_original_ages.rds"))

unique(db_d$PopCode) %>% sort()
unique(db_p$Country) %>% sort()

# Merging mortality data and population estimates
db_dp <- db_d %>% 
  left_join(db_p, by = c("Country", "Year", "Week", "Age", "Sex")) %>% 
  drop_na() 

unique(db_dp$Country) %>% sort()

# definition of flu seasons and heat waves
# For northern countries
flu_season <- c(seq(1, 14, 1), seq(46, 54, 1))
heat_waves <- seq(27, 35, 1)
# For southern countries
south_cts <- c("Australia", "Chile", "New Zealand") 
flu_season_south <- c(seq(21, 42, 1))
heat_waves_south <- c(seq(1, 7, 1), seq(52, 54, 1))
# For equatorial Countries
equat_cts <- c("Peru") 
# no strong seasonality 
flu_season_south <- c()
heat_waves_south <- c()


# Initial year for baseline estimation

# Formating data for baseline estimation
db_de <- db_dp %>% 
  # estimating exposures in person-weeks and rounding deaths to min 1
  mutate(Exposure = Pop / 52,
         Deaths = round(Deaths, 0) + 1) %>% 
  select(Country, Year, Week, Sex, Age, Deaths, Exposure) %>% 
  arrange(Country, Sex, Age, Year, Week) %>% 
  group_by(Country, Age, Sex) %>% 
  mutate(t = 1:n()) %>% 
  ungroup() %>% 
  # adding sinusoidal terms for seasonality
  mutate(sn52 = sin((2*pi*t)/(52)),
         cs52 = cos((2*pi*t)/(52)),
         # excluding winter and summer weeks, as well as 2009 and COVID-19 pandemics
         include = case_when(Country %in% south_cts &
                               !(Week %in% heat_waves_south | Week %in% flu_season_south) & 
                               !((Year == 2020 & Week >7) | Year == 2021) ~ 1,
                             !(Country %in% south_cts) &
                               !(Week %in% heat_waves | Week %in% flu_season) & 
                               Year < 2020 ~ 1,
                             Country %in% equat_cts & 
                               !((Year == 2020 & Week >7) | Year == 2021) ~ 1,
                             TRUE ~ 0),
         include = factor(include)) %>% 
  drop_na()

write_rds(db_de, here("Output", "db_for_baseline_original_ages.rds"))
gc()

# visual inspection of weeks to include and exclude
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
db_de %>% 
  filter(Country == "Peru",
         Age == 80,
         Sex == "t") %>% 
  ggplot()+
  geom_point(aes(t, Deaths, col = include))

db_de %>% 
  filter(Country == "Australia",
         Age == 85,
         Sex == "t") %>% 
  ggplot()+
  geom_point(aes(t, Deaths, col = include))

db_de %>% 
  filter(Country == "Israel",
         Age == 80,
         Sex == "t") %>% 
  ggplot()+
  geom_point(aes(t, Deaths, col = include))

test <- db_de %>% 
  filter(Country == "United States",
         Age == 45,
         Sex == "t")

last_weeks <- db_de %>%
  filter(Year >= 2020) %>%
  group_by(Country) %>%
  summarise(max_week = max(Week))

first_date <- db_de %>% 
  group_by(Country) %>% 
  filter(Year == min(Year)) %>%
  ungroup() %>% 
  select(Country, Year) %>% 
  unique()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# estimating baseline for each country, sex, and age
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# loading packages for baseline estimation
p_load(pkgs_bsl, character.only = TRUE)
select <- dplyr::select
# setting cores use for parallel processing
registerDoParallel(cores = 6)

# creating directories to locally store partial results that do not sync with github
if (!dir.exists(here("Figures"))){
  dir.create(here("Figures"))
}

if (!dir.exists(here("Figures","baseline_by_country_orig_ages"))){
  dir.create(here("Figures","baseline_by_country_orig_ages"))
}

if (!dir.exists(here("Output","baseline_by_country_orig_ages"))){
  dir.create(here("Output","baseline_by_country_orig_ages"))
}

# starting year of observation
cts <- unique(db_de$Country) %>% sort()
ags <- unique(db_de$Age)
cts <- c("Peru")
c <- "United States"
a <- 45
s <- "t"
db_blns_all <- tibble()
for (c in cts) {
  db_blns <- tibble()
  temp <- 
    db_de %>% 
    filter(Country == c)
  sxs <- unique(temp$Sex)
  
  for (s in sxs) {
    temp2 <- 
      temp %>% 
      filter(Sex == s)
    ags <- unique(temp2$Age)
    
    for (a in ags) {
      
      temp3 <- 
        temp2 %>% 
        filter(Age == a) %>% 
        select(Year, Week, t, Deaths, Exposure, sn52, cs52, include)
      
      cat(paste(c, s, a, "\n", sep = "_"))
      
      temp4 <- fit_baseline(temp3) %>% 
        mutate(Country = c,
               Sex = s,
               Age = a,
               Date = ISOweek::ISOweek2date(paste0(Year, "-W", sprintf("%02d",Week), "-7")),
               mx_b = 100000 * Baseline / Exposure,
               mx_b_u = 100000 * up / Exposure,
               mx_b_l = 100000 * lp / Exposure,
               mx_d = 100000 * Deaths / Exposure) 
      
      db_blns <- db_blns %>% 
        bind_rows(temp4)
      
      ## plots of estimates
      ## ~~~~~~~~~~~~~~~~~~
      temp4 %>%
        ggplot()+
        geom_vline(xintercept = ymd("2020-04-03"), col = "#b80c09", alpha = 0.1, size = 5)+
        geom_line(aes(Date, mx_d), size = 0.4)+
        geom_ribbon(aes(Date, ymin = mx_b_l, ymax = mx_b_u), fill = "#01BAEF", alpha = 0.25)+
        geom_line(aes(Date, mx_b), col = "#01BAEF", alpha = 0.9, size = 0.6)+
        scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 year", date_labels = "%Y")+
        labs(title=paste0(c, "_", s, "_", a))+
        theme_bw()+
        theme(
          panel.grid.minor = element_blank(),
          plot.title = element_text(size=11),
          axis.text.x = element_text(size=8),
          axis.text.y = element_text(size=8),
          axis.title.x = element_text(size=10),
          axis.title.y = element_text(size=10))+
        ggsave(paste0("Figures/baseline_by_country_orig_ages/", c, "_", s, "_", a, ".png"), dpi = 300, width = 6, height = 4)
    }
  }
  db_blns <- db_blns %>% 
    mutate(Country = c)
  write_csv(db_blns, path = here("Output", "baseline_by_country_orig_ages", paste0(c, "_baseline.csv")))
  db_blns_all <- bind_rows(db_blns_all, db_blns)
}

write_rds(db_blns, here("Output", "baseline_mortality_orig_ages.rds"))

detach(package:MASS)


