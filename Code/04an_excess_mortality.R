# Description:
# Summarize EXCESS deaths since week 8, 2020, in all countries by sex and age 
library(here)
source(here("Code/00_functions.R"))

# detach(package:MASS)

# mortality baseline estimates
baseline_files <- fs::dir_ls(here("Output", "baseline_by_country"))
db_all <- vroom(baseline_files)

# country codes and names
ctr_codes <- read_csv(here("Data", "country_codes.csv")) %>% 
  select(Country, PopCode) %>% 
  mutate(PopCode = ifelse(PopCode == "AUS2", "AUS", PopCode))

# Note
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Estimates for England and Wales below age 30 must 
# be excluded given the inaccurate partition of age groups
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

db <- 
  db_all %>% 
  select(PopCode, Year, Week, Date, Sex, Age, Deaths, Baseline, lp, up, Exposure) %>% 
  filter(Year >= 2020,
         !(PopCode == "GBRTENW" & Age < 30),
         !(PopCode == "USA"))

unique(db$PopCode)

write_rds(db, here("Output", "baseline_mortality.rds"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Three different excess constructions:
# 1) all excess
# 2) positive excess
# 3) epidemic excess
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

db2 <- 
  db %>% 
  left_join(ctr_codes) %>% 
  mutate(Epi_per = ifelse(Deaths >= up, 1, 0),
         Excess_epi = ifelse(Epi_per == 1, Deaths - Baseline, 0),
         Excess_pos = ifelse(Deaths > Baseline, Deaths - Baseline, 0),
         Excess = Deaths - Baseline,
         Age = as.character(Age))

db_sum <- 
  db_age_sum %>% 
  group_by(Country, Sex, Date) %>% 
  summarise(Baseline = sum(Baseline),
            Excess_epi = sum(Excess_epi),
            Excess_pos = sum(Excess_pos),
            Excess = sum(Excess),
            pscore = (Baseline + Excess_epi) / Baseline) %>% 
  ungroup()
  
# Since week 1 in 2020 
cum_age_wk1 <- 
  db2 %>% 
  arrange(Date) %>% 
  group_by(Country, Sex, Age) %>% 
  mutate(CumEpi = cumsum(Excess_epi),
         CumExc = cumsum(Excess),
         CumPos = cumsum(Excess_pos),
         Exposure = cumsum(Exposure)) %>% 
  arrange(Country, Sex, Age, Date) %>% 
  ungroup()

# Since week 8 in 2020 
cum_age_wk8 <- 
  db2 %>% 
  arrange(Date) %>%
  filter(Date >= "2020-03-01") %>% 
  group_by(Country, Sex, Age) %>% 
  mutate(CumEpi_w8 = cumsum(Excess_epi),
         CumExc_w8 = cumsum(Excess),
         CumPos_w8 = cumsum(Excess_pos),
         Exposure_w8 = cumsum(Exposure)) %>% 
  arrange(Country, Sex, Age, Date) %>% 
  select(Country, Sex, Age, Date, CumEpi_w8, CumExc_w8, CumPos_w8, Exposure_w8) %>% 
  ungroup()

cum_age <- 
  cum_age_wk1 %>% 
  left_join(cum_age_wk8, by = c("Country", "Date", "Sex", "Age")) %>% 
  replace_na(list(CumEpi_w8 = 0,
                  CumExc_w8 = 0,
                  CumPos_w8 = 0,
                  Exposure_w8 = 0))

cum <- 
  cum_age %>% 
  group_by(Country, Date, Sex) %>% 
  summarise(CumEpi = sum(CumEpi),
            CumExc = sum(CumExc),
            CumPos = sum(CumPos),
            Exposure = sum(Exposure),
            CumEpi_w8 = sum(CumEpi_w8),
            CumExc_w8 = sum(CumExc_w8),
            CumPos_w8 = sum(CumPos_w8),
            Exposure_w8 = sum(Exposure_w8)) %>% 
  arrange(Country, Sex, Date) %>% 
  ungroup()

write_csv(cum, "Output/cumulative_excess_2020_2021.csv")

# 
# db4 <- db3 %>% 
#   group_by(Country, Sex) %>% 
#   summarise(Exposure = sum(Exposure),
#             Deaths = sum(Deaths),
#             Baseline = sum(Baseline),
#             Excess = sum(Excess),
#             Excess_lp = sum(Excess_lp),
#             Excess_up = sum(Excess_up), 
#             Baseline_pos = sum(Baseline_pos),
#             Excess_pos = sum(Excess_pos),
#             Excess_pos_lp = sum(Excess_pos_lp),
#             Excess_pos_up = sum(Excess_pos_up),
#             Excess_epi = sum(Excess_epi)) %>% 
#   mutate(Age = "All") %>% 
#   ungroup()
# 
# db5 <- bind_rows(db3, db4) %>% 
#   arrange(Country, Sex, suppressWarnings(as.integer(Age))) 
# 
# all <- c("Excess", "Excess_lp", "Excess_up")
# pos <- c("Excess_pos", "Excess_pos_lp", "Excess_pos_up")
# lvs <- c("Upper", "Central", "Lower")
# 
# db6 <- db5 %>%   
#   select(Country, Sex, Age, Excess, Excess_lp, Excess_up, Excess_pos, Excess_pos_lp, Excess_pos_up, Excess_epi, Exposure) %>% 
#   gather(-c(Country, Sex, Age, Exposure), key = "unit", value = "Excess") %>% 
#   mutate(Including = case_when(unit %in% c("Excess", "Excess_lp", "Excess_up") ~ "All values", 
#                                unit %in% c("Excess_pos", "Excess_pos_lp", "Excess_pos_up") ~ "Positive values", 
#                                unit %in% "Excess_epi" ~ "Epi Periods"),
#          Estimate = case_when(unit %in% c("Excess", "Excess_pos", "Excess_epi") ~ "Central", 
#                               unit %in% c("Excess_lp", "Excess_pos_lp") ~ "Lower", 
#                               unit %in% c("Excess_up", "Excess_pos_up") ~ "Upper"),
#          Estimate = factor(Estimate, levels = lvs),
#          Rate = 100000 * Excess / Exposure)
# 
# unique(db6$Including)
# write_rds(db6, "Output/db_excess_by_age_from_weekly_3measures.rds")
# db6 <- read_rds("Output/db_excess_by_age_from_weekly_3measures.rds")
# 
# db7 <- db6 %>% 
#   filter(Including == "Epi Periods") %>% 
#   select(-unit)
# 
# write_rds(db7, "Output/db_excess_by_age_from_weekly.rds")
# write_csv(db7, "Output/db_excess_by_age_from_weekly.csv")
# 
# 
# db6 %>%
#   filter(Age == "All",
#          Sex == "b") %>% 
#   ggplot()+
#   geom_point(aes(Country, Excess, col = Including, shape = Estimate), alpha = 0.7)+
#   scale_shape_manual(values = c(1, 16, 1))+
#   coord_flip()+
#   labs(title = "Excess deaths by country")+
#   # scale_color_manual(values = c("black", "blue"))+
#   theme_bw()+
#   theme(
#     # legend.position = "bottom",
#     panel.grid.minor = element_blank(),
#     plot.title = element_text(size=11),
#     axis.text.x = element_text(size=8),
#     axis.text.y = element_text(size=8),
#     axis.title.x = element_text(size=10),
#     axis.title.y = element_text(size=10)
#   )
# 
# # Excess deaths all ages 3 measures
# db6 %>%
#   filter(Age == "All",
#          Sex == "b",
#          Rate >= 0,
#          Estimate == "Central") %>% 
#   ggplot()+
#   geom_point(aes(Country, Excess, col = Including), alpha = 0.7)+
#   # scale_y_log10()+
#   coord_flip()+
#   labs(title = "Excess deaths by country, all ages")+
#   # scale_color_manual(values = c("black", "blue"))+
#   theme_bw()+
#   theme(
#     # legend.position = "bottom",
#     panel.grid.minor = element_blank(),
#     plot.title = element_text(size=10),
#     axis.text.x = element_text(size=8),
#     axis.text.y = element_text(size=8),
#     axis.title.x = element_text(size=10),
#     axis.title.y = element_text(size=10)
#   )
# ggsave(paste0("Figures/summary/total_excess_(3_measures)_all_ages.png"), dpi = 300, width = 6, height = 4)
# a <- c("0", "5", "10", "15")
# # Excess rates all ages
# db6 %>%
#   filter(Age == "All",
#          Sex == "b",
#          Rate >= 0,
#          Estimate == "Central") %>% 
#   ggplot()+
#   geom_point(aes(Country, Rate, col = Including), alpha = 0.7)+
#   scale_y_log10()+
#   coord_flip()+
#   labs(title = "Excess deaths rates (100K) by country, all ages")+
#   # scale_color_manual(values = c("black", "blue"))+
#   theme_bw()+
#   theme(
#     # legend.position = "bottom",
#     panel.grid.minor = element_blank(),
#     plot.title = element_text(size=10),
#     axis.text.x = element_text(size=8),
#     axis.text.y = element_text(size=8),
#     axis.title.x = element_text(size=10),
#     axis.title.y = element_text(size=10)
#   )
# ggsave(paste0("Figures/summary/total_excess_rates_(3_measures)_all_ages.png"), dpi = 300, width = 6, height = 4)
# 
# # All ages 1 measure excess
# db6 %>%
#   filter(Age == "All",
#          Sex == "b",
#          Excess > 0,
#          Estimate == "Central",
#          Including == "Epi Periods") %>% 
#   # mutate(Age = factor(Age, levels = a)) %>% 
#   ggplot()+
#   geom_point(aes(reorder(Country, Excess), Excess), alpha = 0.7)+
#   # scale_y_log10()+
#   coord_flip()+
#   labs(title = paste0("Excess deaths by country, March - December, All ages"))+
#   # scale_color_manual(values = c("black", "blue"))+
#   theme_bw()+
#   theme(
#     # legend.position = "bottom",
#     panel.grid.minor = element_blank(),
#     plot.title = element_text(size=10),
#     axis.text.x = element_text(size=6),
#     axis.text.y = element_text(size=8),
#     axis.title.x = element_text(size=10),
#     axis.title.y = element_text(size=10)
#   )
# 
# ggsave(paste0("Figures/summary/total_excess_(epi_periods)_all_ages.png"), dpi = 300, width = 6, height = 5)
# 
# # All ages 1 measure rates
# 
# db6 %>%
#   filter(Age == "All",
#          Sex == "b",
#          Excess > 0,
#          Estimate == "Central",
#          Including == "Epi Periods") %>% 
#   # mutate(Age = factor(Age, levels = a)) %>% 
#   ggplot()+
#   geom_point(aes(reorder(Country, Rate), Rate), alpha = 0.7)+
#   scale_y_log10()+
#   coord_flip()+
#   labs(title = paste0("Excess death rates (/100K) by country, March - December, all ages"))+
#   # scale_color_manual(values = c("black", "blue"))+
#   theme_bw()+
#   theme(
#     # legend.position = "bottom",
#     panel.grid.minor = element_blank(),
#     plot.title = element_text(size=10),
#     axis.text.x = element_text(size=6),
#     axis.text.y = element_text(size=8),
#     axis.title.x = element_text(size=10),
#     axis.title.y = element_text(size=10)
#   )
# ggsave(paste0("Figures/summary/total_excess_rates_(epi_periods)_all_ages.png"), dpi = 300, width = 6, height = 5)
# 
# unique(db6$Country) %>% sort()
# unique(ctr_codes$Country) %>% sort()
