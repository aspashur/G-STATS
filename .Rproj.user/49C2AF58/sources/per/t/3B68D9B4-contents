library(dplyr)
library(readxl)
library(ggplot2)
library(janitor)
library(tensorflow)
library(keras)
library(tidyverse)
library(ggplot2)
library(scales)
library(hrbrthemes)



uno <- read_xlsx("P_Data_Extract_From_World_Development_Indicators.xlsx")

un1 <- read_xlsx("P_Data_Extract_From_World_Development_Indicators_o.xlsx")

unk <- merge(uno,un1)

names(unk)


un <- unk %>% 
  select("Country Name",
         "Time",
         "Forest area (sq. km) [AG.LND.FRST.K2]",
         "CO2 emissions (kg per 2015 US$ of GDP) [EN.ATM.CO2E.KD.GD]",
         "Net migration [SM.POP.NETM]", 
         "Population density (people per sq. km of land area) [EN.POP.DNST]",
         "School enrollment, tertiary (% gross) [SE.TER.ENRR]",
         "Control of Corruption: Estimate [CC.EST]",
         "Mobile cellular subscriptions (per 100 people) [IT.CEL.SETS.P2]",                                                           
         "Regulatory Quality: Estimate [RQ.EST]",
         "Rule of Law: Estimate [RL.EST]",
         "Individuals using the Internet (% of population) [IT.NET.USER.ZS]",
         "GDP per capita (constant 2015 US$) [NY.GDP.PCAP.KD]",
         "GDP growth (annual %) [NY.GDP.MKTP.KD.ZG]",
         "GDP (constant 2015 US$) [NY.GDP.MKTP.KD]",
         "Human capital index (HCI) (scale 0-1) [HD.HCI.OVRL]",
         "Mobile cellular subscriptions [IT.CEL.SETS]",
         "Prevalence of severe wasting, weight for height (% of children under 5) [SH.SVR.WAST.ZS]",             
         "Rural population (% of total population) [SP.RUR.TOTL.ZS]",
         "Agricultural machinery, tractors per 100 sq. km of arable land [AG.LND.TRAC.ZS]",                       
         "Physicians (per 1,000 people) [SH.MED.PHYS.ZS]",
         "Nurses and midwives (per 1,000 people) [SH.MED.NUMW.P3]",                                               
         "Population ages 65 and above (% of total population) [SP.POP.65UP.TO.ZS]",
         "Population ages 0-14 (% of total population) [SP.POP.0014.TO.ZS]",                                      
         "Logistics performance index: Ability to track and trace consignments (1=low to 5=high) [LP.LPI.TRAC.XQ]",
         "Population living in areas where elevation is below 5 meters (% of total population) [EN.POP.EL5M.ZS]", 
         "Access to electricity (% of population) [EG.ELC.ACCS.ZS]",            
         "Individuals using the Internet (% of population) [IT.NET.USER.ZS]",
         "Arable land (hectares per person) [AG.LND.ARBL.HA.PC]") %>% 
  clean_names() %>% 
  rename(year = time, country = country_name) %>% 
  filter(year %in% c(2010:2020)) %>% 
  slice(1:2387) %>% 
  mutate(country = case_when(country == "Gambia, The" ~ "Gambia",
                             TRUE ~ as.character(country)))

# names(un)
# un %>% 
# dplyr::count(`Country Name`)


un_temp <- read.csv("temperature.csv") %>% 
          clean_names() %>% 
  mutate(temp = as.numeric(as.character(temperature_celsius))) %>% 
          group_by(year, country) %>%
          summarise(avg_temp = mean(temp)) %>% 
  filter(year %in% c(2010:2020))

# un_temp %>% 
#   dplyr::count(country)


un_rain <- read.csv("rainfall.csv") %>% 
  clean_names() %>% 
   mutate(rainfall_mm = as.numeric(as.character(rainfall_mm))) %>% 
  group_by(year, country) %>%
  summarise(avg_rainfall = mean(rainfall_mm)) %>% 
  filter(year %in% c(2010:2020))


rain_temp <- un_rain %>% 
  left_join(un_temp, by = c("year", "country")) %>% 
  mutate(country = trimws(country))


df_data <- merge(rain_temp,un)


names(df_data)

write.csv(clean_data,"clean_data.csv")
