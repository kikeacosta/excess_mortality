library(here)
source(here("Code/00_functions.R"))

# info on country names and codes
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ctr_codes <- read_csv(here("Data", "country_codes.csv")) %>% 
  select(Country, PopCode)


# Loading STMF data
# ~~~~~~~~~~~~~~~~~

# downloading the last version of STMF Mortality input data zip 
# this version as of 15 March 2021
# download.file("https://www.mortality.org/Public/STMF/Inputs/STMFinput.zip", here("Data/STMFinput.zip"))

# list of country codes in STMF
zipdf <- unzip(here("Data", "STMFinput.zip"), list = TRUE)

# loading all cause deaths from all countries in STMF
db_d <- tibble()
for(i in 1:length(zipdf$Name)){
  csv_file <- zipdf$Name[i]
  print(csv_file)
  temp <- read_csv(unz(here("Data", "STMFinput.zip"), csv_file))
  db_d <- db_d %>% 
    bind_rows(temp)
}

db_stmf <- db_d %>% 
  select(-Access, -Type, -AgeInterval, -Area) %>% 
  mutate(PopCode = recode(PopCode,
                          "AUS2" = "AUS"),
         Week = as.character(Week)) %>% 
  filter(PopCode != "RUS") %>% 
  left_join(ctr_codes)

# Mexico mortality data
# ~~~~~~~~~~~~~~~~~~~~~
# files from OSF as of XXXXX
osf_retrieve_file("hbxkn") %>%
  osf_download(conflicts = "overwrite",
               path = "Data")


mex_files <- unzip(here("Data", "mexico_deaths.zip"), list = TRUE)

# all deaths from years 2020 and 2021
db_mx20 <- 
  read_csv(unz(here("Data", "mexico_deaths.zip"), mex_files[1,1]))

db_mx20_2 <- 
  db_mx20 %>% 
  select(Date = 5,
         Sex = SEXO,
         Age = EDAD) %>% 
  group_by(Date, Sex, Age) %>% 
  summarise(Deaths = n()) %>% 
  ungroup() %>% 
  mutate(Year = year(Date))

# all deaths from years 2016-2019
db_mx12_19 <- tibble()
for(i in 2:9){
  temp <- 
    read_csv(unz(here("Data", "mexico_deaths.zip"), mex_files[i,1]))
  
  temp2 <- 
    temp %>% 
    select(Sex = sexo, 
           Age = edad, 
           Day = dia_ocurr, 
           Month = mes_ocurr, 
           Year = anio_ocur) %>% 
    mutate(Date = make_date(y = Year, m = Month, d = Day),
           Age = case_when(Age < 4000 ~ 0,
                           Age > 4000 & Age < 4130 ~ Age - 4000,
                           TRUE ~ NA_real_)) %>% 
    group_by(Date, Sex, Age, Year, Month, Day) %>% 
    summarise(Deaths = n()) %>% 
    ungroup() %>% 
    mutate(file_orig = mex_files[i,1])
  
  db_mx12_19 <- 
    db_mx12_19 %>% 
    bind_rows(temp2)
}

# all Mexico deaths together
db_mx <- 
  db_mx12_19 %>% 
  select(Date, Year, Sex, Age, Deaths) %>% 
  bind_rows(db_mx20_2) %>% 
  # drop_na(Date) %>%
  filter(Year >= 2016 & Year <= 2021) %>% 
  mutate(IsoWeek = ISOweek::ISOweek(Date)) %>% 
  arrange(Date, Sex, Age) %>% 
  group_by(Year, IsoWeek, Sex, Age) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() 

# grouping ages in 5-year intervals, and adding unknowns
db_mx2 <- 
  db_mx %>% 
  mutate(Age = floor(Age / 5) * 5,
         Age = ifelse(Age >= 95, 95, Age)) %>% 
  group_by(Year, IsoWeek, Sex, Age) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(Year = str_sub(IsoWeek, 1, 4) %>% as.integer(),
         Age = ifelse(is.na(Age), "UNK", as.character(Age)),
         Sex = case_when(Sex == 1 ~ "m",
                         Sex == 2 ~ "f",
                         TRUE ~ "UNK"),
         Week = str_sub(IsoWeek, 7, 8) %>% as.integer(),
         Week = ifelse(is.na(Week), "UNK", as.character(Week)),
         Country = "Mexico",
         PopCode = "MEX") %>% 
  filter(Year >= 2016) %>% 
  select(PopCode, Country, Year, Week, Sex, Age, Deaths)

db_mx3 <- 
  db_mx2 %>% 
  filter(Age != "UNK",
         Sex != "UNK",
         Week != "UNK") %>% 
  mutate(year_week = paste(Year, Week, sep = "_")) %>% 
  select(-Year, -Week) %>% 
  tidyr::complete(year_week, PopCode, Country, Sex, Age, fill = list(Deaths = 0)) %>% 
  separate(year_week, c("Year", "Week"), sep = "_") %>% 
  mutate(Year = as.integer(Year)) %>% 
  bind_rows(db_mx2 %>% 
              filter(Age == "UNK" | Sex == "UNK" | Week == "UNK")) %>% 
  arrange(PopCode, Country, Year, suppressWarnings(as.numeric(Week)), Sex, suppressWarnings(as.numeric(Age)))

# Peru mortality data
# ~~~~~~~~~~~~~~~~~~~
# files from OSF as of 20 March 2021
osf_retrieve_file("wqytu") %>%
  osf_download(conflicts = "overwrite",
               path = "Data")

# load data
db_pe <- 
  read_xlsx(unzip(here("Data", "peru_deaths.zip"),
                  "SINADEF_DATOS_ABIERTOS_19032021.xlsx"), skip = 2)

# remove uncompressed file
file.remove(here("SINADEF_DATOS_ABIERTOS_19032021.xlsx"))

# data wrangling
db_pe2 <- db_pe %>% 
  select(Sex = SEXO,
         Age = EDAD, 
         Date = FECHA,
         Month = MES,
         Year = AÑO,
         unit_age = 'TIEMPO EDAD') %>% 
  mutate(Sex = recode(Sex,
                      "MASCULINO" = "m",
                      "FEMENINO" = "f"))

db_pe3 <- 
  db_pe2 %>%
  mutate(Date = ymd(Date),
         Year = year(Date),
         IsoWeek = ISOweek::ISOweek(Date),
         Sex = case_when(Sex == "m" | Sex == "f" ~ Sex,
                         TRUE ~ "UNK"),
         Age = ifelse(unit_age == "AÑOS", Age, "0"),
         Age = as.integer(Age),
         Age = floor(Age / 5)*5,
         Age = ifelse(Age >= 95, 95, Age),
         Age = ifelse(is.na(Age), "UNK", as.character(Age)),
         Year = str_sub(IsoWeek, 1, 4) %>% as.integer(),
         Week = str_sub(IsoWeek, 7, 8) %>% as.integer(),
         Week = ifelse(is.na(Week), "UNK", as.character(Week))) %>% 
  group_by(Year, Week, Sex, Age) %>% 
  summarise(Deaths = n()) %>% 
  ungroup() %>% 
  filter(Year >= 2017) %>% 
  mutate(Country = "Peru",
         PopCode = "PER") %>% 
  select(PopCode, Country, Year, Week, Sex, Age, Deaths)

# completing missing values 
db_pe4 <- 
  db_pe3 %>% 
  filter(Age != "UNK",
         Sex != "UNK",
         Week != "UNK") %>% 
  mutate(year_week = paste(Year, Week, sep = "_")) %>% 
  select(-Year, -Week) %>% 
  tidyr::complete(year_week, PopCode, Country, Sex, Age, fill = list(Deaths = 0)) %>% 
  separate(year_week, c("Year", "Week"), sep = "_") %>% 
  mutate(Year = as.integer(Year)) %>% 
  bind_rows(db_pe3 %>% 
              filter(Age == "UNK" | Sex == "UNK" | Week == "UNK")) %>% 
  arrange(PopCode, Country, Year, suppressWarnings(as.numeric(Week)), Sex, suppressWarnings(as.numeric(Age)))


# all sex and all age Peru and Mexico
db_pe_mx <- 
  bind_rows(db_pe4,
            db_mx3) %>% 
  arrange(PopCode, Country, Year, Week, Sex, suppressWarnings(as.numeric(Age)))

db_pe_mx_all_ages <- 
  db_pe_mx %>% 
  group_by(PopCode, Country, Year, Week, Sex) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(Age = "TOT") %>% 
  filter(Sex != "UNK")

db_pe_mx_all_sex <- 
  db_pe_mx %>% 
  group_by(PopCode, Country, Year, Week, Age) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(Sex = "b") %>% 
  filter(Age != "UNK")

db_pe_mx_all_sex_age <- 
  db_pe_mx_all_sex %>% 
  group_by(PopCode, Country, Year, Week) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(Sex = "b",
         Age = "TOT") %>% 
  filter(Age != "UNK",
         Sex != "UNK")

db_pe_mx2 <- 
  db_pe_mx %>% 
  filter(Age != "UNK",
         Sex != "UNK") %>% 
  bind_rows(db_pe_mx_all_ages,
            db_pe_mx_all_sex,
            db_pe_mx_all_sex_age) %>% 
  arrange(PopCode, Country, Year, suppressWarnings(as.numeric(Week)), Sex, suppressWarnings(as.numeric(Age)))

# Combining all countries
db_deaths <- 
  bind_rows(db_pe_mx2,
            db_stmf) %>% 
  arrange(PopCode, Country, Year, suppressWarnings(as.numeric(Week)), Sex, suppressWarnings(as.numeric(Age)))

write_rds(db_deaths, here("Output", "input_weekly_deaths.rds"))
