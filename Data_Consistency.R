library(tidyverse)
Analysis_Data <- readRDS("Analysis_Data.rds") ## load Covid dataset

#Filter only necessary variables
Filtered_Data <- Analysis_Data %>% select(ags2, ags5, date, faelle_covid_aktuell, faelle_covid_aktuell_invasiv_beatmet, betten_frei, betten_frei_nur_erwachsen, betten_belegt, betten_belegt_nur_erwachsen, bundesland.y, kreis)

#Sum variables of interest by Land and date
Aggregated_Data <- Filtered_Data %>% group_by(bundesland.y, date) %>%
                  summarise_at(vars(faelle_covid_aktuell, faelle_covid_aktuell_invasiv_beatmet, betten_frei, betten_frei_nur_erwachsen, betten_belegt, betten_belegt_nur_erwachsen), list(sum))

#Control with ags2
Aggregated_Data_Control <- Filtered_Data %>% group_by(ags2, date) %>%
  summarise_at(vars(faelle_covid_aktuell, faelle_covid_aktuell_invasiv_beatmet, betten_frei, betten_frei_nur_erwachsen, betten_belegt, betten_belegt_nur_erwachsen), list(sum))
