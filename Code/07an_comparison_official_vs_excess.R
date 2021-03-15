rm(list=ls())
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")
library(tidyverse)
library(readr)
library(lubridate)
library(ggplot2)

db_cov <- read_rds("Output/db_coverage_db_last dates_countries_stmf_age5.rds")

db_exc_w <- read_rds("Output/db_excess_by_age_from_weekly.rds")

db_p <- read_rds("Output/pop_interpol_month_age5.rds")

unique(db_exc_w$Age)

db_exc_w2 <- db_exc_w %>% 
  filter(Including == "Epi Periods",
         Sex == "b",
         Estimate == "Central") %>% 
  select(Country, Age, Excess) %>% 
  rename(Deaths = Excess) %>% 
  mutate(Source = "Excess")


db_cov2 <- db_cov %>% 
  filter(Sex == "b") %>% 
  select(Country, Age, Deaths) %>% 
  mutate(Age = ifelse(Age >= 90, 90, Age)) %>% 
  group_by(Country, Age) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(Source = "Official",
         Age = as.character(Age))

db_cov_all <- db_cov2 %>% 
  group_by(Country, Source) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(Age = "All")

db_p2 <- db_p %>% 
  filter(Date == "2020-07-15",
         Sex == "b") %>% 
  select(Country, Age, Pop) %>% 
  mutate(Age = as.character(Age))

db_p_all <- db_p2 %>% 
  group_by(Country) %>% 
  summarise(Pop = sum(Pop)) %>% 
  ungroup() %>% 
  mutate(Age = "All")

db_p3 <- bind_rows(db_p2, db_p_all)

db <- bind_rows(db_exc_w2, db_cov2, db_cov_all) %>% 
  arrange(Country, Source, suppressWarnings(as.numeric(Age))) %>% 
  left_join(db_p3) %>% 
  mutate(Exposure = Pop * 9/12,
         Mx = 100000 * Deaths / Exposure)

db %>% 
  filter(Age == "All") %>% 
  ggplot()+
  geom_point(aes(Mx, Country, col = Source), alpha = 0.7)+
  scale_x_log10()+
  labs(title = paste0("Rates Excess and Reported by country, March - November, all ages"))+
  # scale_color_manual(values = c("black", "blue"))+
  theme_bw()+
  theme(
    # legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size=10),
    axis.text.x = element_text(size=6),
    axis.text.y = element_text(size=8),
    axis.title.x = element_text(size=10),
    axis.title.y = element_text(size=10)
  )+
  ggsave(paste0("Figures/summary/rates_excess_vs_official(epi_periods)_all_ages.png"), dpi = 300, width = 6, height = 4)

db %>% 
  filter(Age == "All") %>% 
  ggplot()+
  geom_point(aes(Deaths, Country, col = Source), alpha = 0.7)+
  # scale_x_log10()+
  labs(title = paste0("Excess and Reported deaths by country, March - November, all ages"))+
  # scale_color_manual(values = c("black", "blue"))+
  theme_bw()+
  theme(
    # legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size=10),
    axis.text.x = element_text(size=6),
    axis.text.y = element_text(size=8),
    axis.title.x = element_text(size=10),
    axis.title.y = element_text(size=10)
  )+
  ggsave(paste0("Figures/summary/deaths_excess_vs_official(epi_periods)_all_ages.png"), dpi = 300, width = 6, height = 4)

ages <- c("0", "5", "10", "15")

db_0_19 <- db %>% 
  select(Country, Age, Source, Deaths, Exposure, Mx) %>% 
  mutate(Age = ifelse(Age %in% ages, "0-19", Age)) %>% 
  group_by(Country, Age, Source) %>% 
  summarise(Deaths = sum(Deaths), 
            Exposure = sum(Exposure)) %>% 
  ungroup() %>% 
  mutate(Mx = 100000 * Deaths / Exposure)

db_0_19 %>% 
  filter(Age == "0-19") %>% 
  ggplot()+
  geom_point(aes(Mx, Country, col = Source), alpha = 0.7)+
  scale_x_log10()+
  labs(title = paste0("Rates Excess and Reported by country, March - November, ages 0-19"))+
  # scale_color_manual(values = c("black", "blue"))+
  theme_bw()+
  theme(
    # legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size=10),
    axis.text.x = element_text(size=6),
    axis.text.y = element_text(size=8),
    axis.title.x = element_text(size=10),
    axis.title.y = element_text(size=10)
  )+
  ggsave(paste0("Figures/summary/rates_excess_vs_official(epi_periods)_ages_0_19.png"), dpi = 300, width = 6, height = 4)

db_0_19 %>% 
  filter(Age == "0-19",
         Country != "Spain") %>% 
  ggplot()+
  geom_point(aes(Deaths, reorder(Country, Deaths), col = Source), alpha = 0.7)+
  # scale_x_log10()+
  labs(title = paste0("Excess and Reported deaths by country, March - November, ages 0-19"))+
  # scale_color_manual(values = c("black", "blue"))+
  theme_bw()+
  theme(
    # legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size=10),
    axis.text.x = element_text(size=6),
    axis.text.y = element_text(size=8),
    axis.title.x = element_text(size=10),
    axis.title.y = element_text(size=10)
  )+
  ggsave(paste0("Figures/summary/deaths_excess_vs_official(epi_periods)_ages_0_19.png"), dpi = 300, width = 6, height = 5)

db_0_19 %>% 
  filter(Age == "0-19",
         Source == "Excess") %>% 
  ggplot()+
  geom_point(aes(Deaths, Country), alpha = 0.7)+
  # scale_x_log10()+
  labs(title = paste0("Excess deaths by country, March - November, ages 0-19"))+
  # scale_color_manual(values = c("black", "blue"))+
  theme_bw()+
  theme(
    # legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size=10),
    axis.text.x = element_text(size=6),
    axis.text.y = element_text(size=8),
    axis.title.x = element_text(size=10),
    axis.title.y = element_text(size=10)
  )+
  ggsave(paste0("Figures/summary/excess_(epi_periods)_ages_0_19.png"), dpi = 300, width = 6, height = 4)

db_0_19 %>% 
  filter(Age == "0-19",
         Source == "Excess") %>% 
  ggplot()+
  geom_point(aes(Mx, Country), alpha = 0.7)+
  # scale_x_log10()+
  labs(title = paste0("Excess death rates (/100K) by country, March - November, ages 0-19"))+
  # scale_color_manual(values = c("black", "blue"))+
  theme_bw()+
  theme(
    # legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size=10),
    axis.text.x = element_text(size=6),
    axis.text.y = element_text(size=8),
    axis.title.x = element_text(size=10),
    axis.title.y = element_text(size=10)
  )+
  ggsave(paste0("Figures/summary/rates_excess_(epi_periods)_ages_0_19.png"), dpi = 300, width = 6, height = 4)

# ratios
#########


db_ratio <- db %>% 
  select(Country, Age, Source, Mx) %>% 
  spread(Source, Mx) %>% 
  drop_na() %>% 
  mutate(Ratio = Excess / Official) %>% 
  filter(Ratio != Inf,
         Ratio != 0)

db_ratio %>% 
  filter(Age == "All") %>% 
  ggplot()+
  geom_point(aes(Ratio, Country))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  scale_x_log10(breaks = c(0.5, 1, 2, 5, 10, 20))+
  labs(title = paste0("Ratio Excess/Reported by country, March - November, all ages"))+
  # scale_color_manual(values = c("black", "blue"))+
  theme_bw()+
  theme(
    # legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size=10),
    axis.text.x = element_text(size=6),
    axis.text.y = element_text(size=8),
    axis.title.x = element_text(size=10),
    axis.title.y = element_text(size=10)
  )+
  ggsave(paste0("Figures/summary/ratio_excess_vs_official(epi_periods)_all_ages.png"), dpi = 300, width = 6, height = 4)

ages <- c("0", "5", "10", "15")
db_ratio %>% 
  filter(Age %in% ages) %>% 
  ggplot()+
  geom_point(aes(Ratio, Country))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  scale_x_log10()+ 
  facet_wrap(~Age, scales = "free")+
  labs(title = paste0("Ratio Excess/Reported by country, March - November, youngest ages"))+
  # scale_color_manual(values = c("black", "blue"))+
  theme_bw()+
  theme(
    # legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size=10),
    axis.text.x = element_text(size=6),
    axis.text.y = element_text(size=8),
    axis.title.x = element_text(size=10),
    axis.title.y = element_text(size=10)
  )+
  ggsave(paste0("Figures/summary/ratio_excess_vs_official(epi_periods)_youngest_ages.png"), dpi = 300, width = 6, height = 4)


# all youngest ages together
db_ratio_0_19 <- db %>% 
  select(Country, Age, Source, Mx) %>% 
  spread(Source, Mx) %>% 
  drop_na() %>%
  mutate(Age = ifelse(Age %in% ages, "0-19", Age)) %>% 
  group_by(Country, Age) %>% 
  summarise(Excess = sum(Excess), 
            Official = sum(Official)) %>% 
  ungroup() %>% 
  mutate(Ratio = Excess / Official) %>% 
  filter(Ratio != Inf,
         Ratio != 0)

db_ratio_0_19 %>% 
  filter(Age == "0-19") %>% 
  ggplot()+
  geom_point(aes(Ratio, reorder(Country, Ratio)))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  scale_x_log10(breaks = c(0.5, 1, 2, 5, 10, 20))+
  labs(title = paste0("Ratio Excess/Reported by country, March - November, ages 0-19"))+
  # scale_color_manual(values = c("black", "blue"))+
  theme_bw()+
  theme(
    # legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size=10),
    axis.text.x = element_text(size=6),
    axis.text.y = element_text(size=8),
    axis.title.x = element_text(size=10),
    axis.title.y = element_text(size=10)
  )+
  ggsave(paste0("Figures/summary/ratio_excess_vs_official(epi_periods)_ages_0_19.png"), dpi = 300, width = 6, height = 5)
