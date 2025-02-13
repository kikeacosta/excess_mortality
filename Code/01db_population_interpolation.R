# Description:
# Weekly exposures interpolation

library(here)
source(here("Code/00_functions.R"))

# countries to be included (STMF codes and names)
ctr_codes <- read_csv(here("Data", "country_codes.csv"))

# If the file with population interpolarion exists, dont run the script
if (!file.exists(here("Output", "pop_interpol_week_age5.rds")) | 
    !file.exists(here("Output", "annual_exposures_stmf.rds"))){
  
  
  # Country WPP names and codes 
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pcodes <- ctr_codes %>% 
    dplyr::pull(PopCode)
  
  ctrs <- ctr_codes %>% 
    dplyr::pull(wpp_name)
  
  
  # loading "Annual and single age data" population estimates 
  # from the  from the WPP, available in:
  # "https://population.un.org/wpp/Download/Standard/Interpolated/
  # A hard copy of these files are stored in the project's OSF repository
  #########################################################################
  
  # female and male estimates stored in the OSF project
  osf_retrieve_file("eqk3r") %>%
    osf_download(conflicts = "overwrite",
                 path = "Data")
  
  osf_retrieve_file("pcnsf") %>%
    osf_download(conflicts = "overwrite",
                 path = "Data") 
  
  # filtering the countries in which mortality is available
  pop_f <- read_xlsx(here("Data", "WPP2019_INT_F03_3_POPULATION_BY_AGE_ANNUAL_FEMALE.xlsx"),
                     skip = 16) %>% 
    select(3, 8:109) %>% 
    rename(Country = 1, 
           Year = 2) %>% 
    filter(Country %in% ctrs, 
           Year >= 2000) %>% 
    gather(-Country, -Year, key = "Age", value = "Pop") %>% 
    mutate(Age = as.integer(Age),
           Sex = "f")
  
  pop_m <- read_xlsx(here("Data", "WPP2019_INT_F03_2_POPULATION_BY_AGE_ANNUAL_MALE.xlsx"),
                     skip = 16) %>% 
    select(3, 8:109) %>% 
    rename(Country = 1, 
           Year = 2) %>% 
    filter(Country %in% ctrs, 
           Year >= 2000) %>% 
    gather(-Country, -Year, key = "Age", value = "Pop") %>% 
    mutate(Age = as.integer(Age),
           Sex = "m")
  
  # original population data in thousands, converting it to counts
  pop_wpp <- bind_rows(pop_m, pop_f) %>% 
    mutate(Pop = as.numeric(Pop) * 1000) 
  
  unique(pop_wpp$Country) %>% sort()
  
  # loading annual and single-year of age population estimates from the HMD
  # for England and Wales, Scotland, and Northern Ireland 
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  HMDcountries <- c("GBR_NIR", "GBR_SCO", "GBRTENW")
  countries <- ctr_codes %>% 
    filter(PopCode %in% HMDcountries) %>% 
    pull(Country)
  names(countries) <- HMDcountries
  
  OffsetsL <- lapply(HMDcountries, function(xyz, us, pw, countries){
    X         <- readHMDweb(xyz, "Exposures_1x1", us, pw)
    X %>% 
      filter(Year >= 2000) %>% 
      pivot_longer(Female:Total, names_to = "Sex", values_to = "Pop") %>% 
      mutate(Country = countries[xyz],
             Sex = case_when(Sex == "Male" ~ "m",
                             Sex == "Female" ~ "f",
                             Sex == "Total" ~ "b"),
             Pop = as.numeric(Pop)) %>% 
      arrange(Year, Sex, Age) %>% 
      select(Country, Year, Sex, Age, Pop)
    
  }, us = us, pw = pw, countries = countries)
  
  names(OffsetsL) <- countries
  
  pop_hmd <- 
    OffsetsL %>% 
    bind_rows() %>% 
    group_by(Country, Sex) %>% 
    do(pad_offsets(chunk = .data)) %>% 
    ungroup() %>% 
    mutate(Age = ifelse(Age >= 100, 100, Age)) %>% 
    group_by(Country, Year, Sex, Age) %>% 
    summarise(Pop = sum(Pop)) %>% 
    ungroup() %>% 
    filter(Sex != "b")
  
  if (!file.exists(here("Output", "pop_interpol_week_age5.rds"))){  
  
    # Appending WPP and HMD data
    # grouping it in 5-year age intervals
    # assigning week 27 to each estimate
    pop_all <- 
      bind_rows(pop_hmd, pop_wpp) %>% 
      mutate(Age = floor(Age / 5) * 5) %>% 
      group_by(Country, Year, Sex, Age) %>% 
      summarise(Pop = sum(Pop),
                Week = 26) %>%
      ungroup()
    
    ###########################################################
    # Interpolating population estimates to weeks using splines
    ###########################################################
    
    # dataframe with weeks by year between 2000 and 2020
    db_w <- expand_grid(Year = 2000:2021, Week = 1:52) %>% 
      bind_rows(tibble(Year = c(2004, 2009, 2015, 2020), Week = 53)) %>% 
      arrange(Year, Week) %>% 
      mutate(t = 1:n())
    
    ages <- unique(pop_all$Age)
    ctrs <- unique(pop_all$Country)
    
    inters_pop <- NULL
    
    for(c in ctrs){
      pop_temp1 <- pop_all %>% 
        filter(Country == c)
      for(s in c("m", "f")){
        pop_temp2 <- pop_temp1 %>% 
          filter(Sex == s)
        for(a in ages){
          
          db_w_temp <- db_w %>% 
            mutate(Country = c,
                   Sex = s,
                   Age = a) %>% 
            left_join(pop_temp2)
          
          db_w_temp2 <- db_w_temp %>% 
            left_join(interpop(db_w_temp)) %>% 
            mutate(Country = c,
                   Age = a,
                   Sex = s)
          
          inters_pop <- inters_pop %>% 
            bind_rows(db_w_temp2)
          
        }
      }
    }
    
    unique(inters_pop$Country)
    
    # Visual test
    #############
    c <- "Peru"
    a <- 0
    s <- "f"
    
    inters_pop %>% 
      filter(Country == c,
             Age == a,
             Sex == s) %>% 
      ggplot()+
      geom_line(aes(t, Pop2), col = "black")+
      geom_point(aes(t, Pop), col = "red")
    
    # closing age at 90
    inters_pop2 <- inters_pop %>% 
      mutate(Age = ifelse(Age > 90, 90, Age)) %>% 
      group_by(Country, Year, Week, t, Sex, Age) %>% 
      summarise(Pop = sum(Pop2)) %>% 
      ungroup()
    
    # population estimates for all sex
    inters_popt <- inters_pop2 %>% 
      group_by(Year, Week, t, Country, Age) %>% 
      summarise(Pop = sum(Pop)) %>% 
      ungroup() %>% 
      mutate(Sex = "b")
    
    inters_pop3 <- bind_rows(inters_pop2,
                             inters_popt) %>% 
      arrange(Country, Sex, Age, t) %>% 
      select(-t)
    
    write_rds(inters_pop3, here("Output", "pop_interpol_week_age5.rds"))
    unique(inters_pop3$Country) %>% sort()
  
  }
  
  ###################################################
  # Extrapolation WPP estimates to 2021 using splines
  ###################################################
  # If the file with population interpolarion exists, dont run the script
  if (!file.exists(here("Output", "annual_exposures_stmf.rds"))){
    
    db_p2 <- 
      inters_pop3 %>% 
      filter(Week == 26) %>% 
      left_join(ctr_codes) %>% 
      select(PopCode, Country, Year, Sex, Age, Pop)
    
    write_rds(db_p2, here("Output", "annual_exposures.rds"))
  }
}
