# Get Packages
library(here) # Get relative paths
library(tidyverse) # Easier data manipulation
library(gtrendsR) # Get Data from google trends
library(jsonlite) # Import JSON Datasets
library(estimatr) # Easy clustered SE
library(directlabels) # Nice graph labelling
library(stringr)
library(dplyr)
library(data.table)
library(ggplot2)
library(readxl)
library(openxlsx)
library(data.table) ## For some minor data wrangling
library(fixest)  
library(haven)
library(lfe)
library(lubridate)
library(rdd)
library(gtsummary)# Make nice tables V1
library(rstatix) # Make nice tables V2
library(modelsummary) # Make nice tables V3
library(expss) # Variable Labels
library(labelled) # Variable Labels V2
library(kableExtra)

#install.packages("devtools")
#devtools::install_github("nipfpmf/eventstudies", ref="master")
#library(eventstudies)


####################################################################
## 1 ##  Get data


# Answer: yes 


# Get Covid Data Datenplattform: Incidence Data

# Link for seafile
covid_infas <- read.csv("https://seafile.zew.de/d/93b185ddaa7b4008af54/files/?p=%2FA_Data%2Finfektionen.csv&dl=1")

# Incidence rates, corrected, but for the entire period
covid_infas_inz <- covid_infas %>% filter(variable == "kr_inz_rate")
dates=as.character(seq(as.Date("2020-03-01"), as.Date("2021-06-22"), by="days"))
headings= c("nr","ags2","Bundesland", "ags5", "Landkreis" ,"var", dates)
colnames(covid_infas_inz) <- headings
rm(covid_infas)

# Incidence rates, uncorrected, but only from November 2020 onwards (RKI)
covid_infas2 = read.xlsx("https://seafile.zew.de/d/93b185ddaa7b4008af54/files/?p=%2FA_Data%2FFallzahlen_Kum_Tab.xlsx&dl=1", sheet= "LK_7-Tage-Inzidenz (fixiert)")
dates=as.character(seq(as.Date("2020-11-18"), as.Date("2021-08-30"), by="days"))
headings= c("nr", "Landkreis", "ags5", dates)
colnames(covid_infas2) <- headings

eins <- covid_infas_inz$ags5
zwei <- covid_infas2$ags5
setdiff(zwei, eins)
# Berlin is reported differently in different datasets

# Combine both incidence datasets
covid_infas2 = covid_infas2[,!(names(covid_infas2) %in% c("nr", "Landkreis"))]
dates=as.character(seq(as.Date("2020-11-18"), as.Date("2021-08-30"), by="days"))
covid_infas_inz = covid_infas_inz[,!(names(covid_infas_inz) %in% dates)]
covid_infas_inz <-covid_infas_inz %>%
  inner_join(covid_infas2, by="ags5")
# incidence dataset with 2 problems:- Berlin is excluded, - before 2020/11/18 corrected, after that uncorrected




# Get data on hospital characteristics
hospital_stat = read.xlsx("https://seafile.zew.de/d/93b185ddaa7b4008af54/files/?p=%2FA_Data%2FKH_ags_lk.xlsx&dl=1")
# type: 1: oeffentlich(public), 2: privat(private), 3: fg(private not for profit)
colnames(hospital_stat)

# Aggregate Data on the county level

# share_private/ share_pubic: simple share of hospitals in acounty
# private_weighted/public_weighted: share weighted by size of the hospitals (in categories)
hospital_stat <- hospital_stat%>% add_count(ags5, name="kh_county")
hospital_stat <- hospital_stat%>% mutate (private=(Type==2)*1)
hospital_stat <- hospital_stat%>% mutate (public=(Type==1)*1)
hospital_stat <- hospital_stat%>% group_by(ags5) %>% add_tally( private, name="agg_private")
hospital_stat <- hospital_stat%>% group_by(ags5) %>% add_tally( public, name="agg_public")
hospital_stat <- hospital_stat%>% mutate (share_private= agg_private/kh_county)
hospital_stat <- hospital_stat%>% mutate (share_public= agg_public/kh_county)
hospital_stat <- hospital_stat %>% mutate (size= str_replace(Betten, "<", "") ) %>% mutate (size= str_replace(size, "<", "") ) 
hospital_stat <- hospital_stat %>% mutate (size= str_replace(size, "[.]","") )
hospital_stat <- hospital_stat %>% mutate (size= as.numeric(size))
hospital_stat <- hospital_stat%>% group_by(ags5) %>% add_tally( size, name="beds_county")
hospital_stat <- hospital_stat%>% mutate (private_index=(Type==2)*1*size)
hospital_stat <- hospital_stat%>% mutate (public_index=(Type==1)*1*size)
hospital_stat <- hospital_stat%>% group_by(ags5) %>% add_tally( private_index, name="agg_private_index")
hospital_stat <- hospital_stat%>% group_by(ags5) %>% add_tally( public_index, name="agg_public_index")
hospital_stat <- hospital_stat%>% mutate (private_weighted=(agg_private_index/beds_county))
hospital_stat <- hospital_stat%>% mutate (public_weighted=(agg_public_index/beds_county))
# collapse on county level
hospital_stat_agg <- hospital_stat%>%
  group_by(ags5) %>%
  summarize_all(mean) 


# Get Hospital Signal Data
hospitals_availability <- fromJSON("https://www.intensivregister.de/api/public/intensivregister")
hospital_availability_new<- flatten(hospitals_availability$data, recursive = TRUE)
colnames(flatten(hospital_availability_new))
hospital_availability_new$ags5=substr(hospital_availability_new$krankenhausStandort.gemeindeschluessel,1,5)
hospital_availability_new<-hospital_availability_new %>% rename(ags8=krankenhausStandort.gemeindeschluessel)

# aggregate hospitals on county level
hospital_availability_new <- hospital_availability_new %>% add_count(ags5, name="kh_county")

# Get Bed Occupancy Data
beds_county <- read.csv("https://diviexchange.blob.core.windows.net/%24web/zeitreihe-tagesdaten.csv")

# Notfallreserve

emergencybeds = read.csv("https://seafile.zew.de/d/93b185ddaa7b4008af54/files/?p=%2FA_Data%2Fdata-RLWg1.csv&dl=1")

## 2 ## Data preparation -----

# Calculate bed occupancy rates and adjust dataset
beds_county <- beds_county %>% mutate(
  # Get occupancy rate
  occupancy_free_all = betten_frei / (betten_frei + betten_belegt),
  occupancy_free_adult = betten_frei_nur_erwachsen / (betten_frei_nur_erwachsen + betten_belegt_nur_erwachsen),
  # Make date as date
  date = as.Date(date))
names(beds_county)[names(beds_county) == "gemeindeschluessel"] <- "ags5"



# Reshape incidence data
x<-cbind(colnames(covid_infas_inz)[7:length(colnames(covid_infas_inz))])
inz_long<-reshape(covid_infas_inz, 
                  varying = c(x), 
                  v.names= "incidence",
                  timevar = "date", 
                  times = c(x),
                  direction = "long")
inz_long$date <- as.Date(inz_long$date, "%Y-%m-%d")


# Match occupancy statistics and incidence rates in one dataset
bed_inz<- merge(beds_county, inz_long, by = c('ags5', 'date'))

# Restrict time period
bed_inz <- bed_inz %>% filter( date >=as.Date('2020-04-24') & date <= as.Date('2021-08-31'))


saveRDS(bed_inz, file = here("B_TempData","Raw_Data.rds"))


# Creating variable for incidence thresholds for different dates

# Fist condition: Incidence above certain levels
bed_inz<-bed_inz %>% 
  mutate(highinzhelp1 =(incidence  >=70 & date >=as.Date('2020-11-16') & date <= as.Date('2021-04-08')) *1)

bed_inz<-bed_inz %>% 
  mutate(highinzhelp2 =(incidence  >=50 & date >=as.Date('2021-04-09') & date <= as.Date('2021-06-15')) *1)

bed_inz<-bed_inz %>% mutate(highinz=(highinzhelp1==1 |highinzhelp2==1)*1)

# Second condition: incidence below certain threshold
bed_inz<-bed_inz %>%mutate(help1 =(incidence  < 200 & date >=as.Date('2020-12-17') & date <= as.Date('2021-01-14')) *1)

bed_inz<-bed_inz %>%mutate(help2 =(incidence  < 150 & date >=as.Date('2021-01-15') & date <= as.Date('2021-06-15')) *1)

# Creating variable for all conditions
bed_inz<-bed_inz %>% mutate(affectedhelp1=(highinz==1 & help1==1 )*1)
bed_inz<-bed_inz %>% mutate(affectedhelp2=(highinz==1 & help2==1 )*1)  
bed_inz<-bed_inz %>% mutate(affected=(affectedhelp1==1 | affectedhelp2==1 )*1)
# attention in the affected variable also single days between affected days are exclueded when incidence is above 200/150


# variable for the number of days above the threshold
bed_inz<-bed_inz %>% mutate(ags5help=ags5)
bed_inz<-bed_inz %>%
  group_by(ags5help = data.table::rleid(highinz == 1)) %>%
  mutate(Consec_Days = if_else(highinz == 1, row_number(), 0L))

bed_inz<-bed_inz %>%
  mutate(sevendays=(Consec_Days>=7)*1)
# Maximum & minimum capacity per county
bed_inz<- bed_inz %>% group_by(ags5) %>%
  mutate(beds = betten_frei + betten_belegt) %>%
  mutate(max_beds = max(beds))%>%
  mutate(min_beds = min(beds))
bed_inz<- bed_inz %>%
  mutate(capacity = betten_frei + betten_belegt)




# Capacity reported by hospitals
bed_inz<-bed_inz %>% mutate(idhelp=id)
bed_inz<-bed_inz %>%
  group_by(idhelp) %>%
  mutate (max_capacity=max(capacity))



# Creating variable all funding conditions in a dummy
bed_inz<- bed_inz %>% 
  mutate (allconditions=(affected==1 & occupancy_free_all<0.25 |affectedhelp1==1 | affectedhelp2==1)*1)
# allconditions includes all days/counties in which hospitals could receive funding (also those which recieve funding without the 0.25 treshold)

# Creating variable counties with no icu capacity
bed_inz<-bed_inz %>% 
  mutate(noicu=(min_beds==0)*1)

bed_inz %>% group_by(ags5) %>%
  filter(noicu==1)
# 2 counties with no icu capacities 


# Merging hospital statistic information and county data on county level
full_data<- merge(bed_inz, hospital_stat_agg, by = c('ags5')) 


# Variable for share of maximal capacity reported
full_data <- full_data %>% mutate (capacity_ratio= capacity/max_beds)

# variable for event of incidence threshold

full_data <- full_data %>% mutate (event=(Consec_Days==1)*1)

# count days per county in which no beds were available
full_khs <- full_data %>% filter(betten_frei==0) %>% group_by(ags5) %>%add_count( ags5, name="days_full")
full_khs <- full_khs %>% group_by(ags5)  %>% summarise_all(mean)
# I= county, days with no free beds 

hist(full_khs$days_full, breaks=15)






full_data$Consec_Days[c(full_data$event[-1], 0) == 1] <- full_data$Consec_Days[c(full_data$event[-1], 0) == 1] -1
#event<- full_data%>% group_by(Consec_Days) %>% summarize_at(vars(occupancy_free_all, capacity), mean) 


# Save Analysis Dataset
saveRDS(full_data, file = here("B_TempData","Analysis_Data.rds"))
saveRDS(bed_inz, file = here("B_TempData","Analysis_Inc_Data.rds"))
