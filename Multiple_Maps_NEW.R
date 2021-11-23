pacman::p_load(sf, tidyverse, hrbrthemes, lwgeom, rnaturalearth, maps, mapdata, spData, tigris, tidycensus, leaflet, mapview, tmap, tmaptools)

Analysis_Data <- readRDS("Analysis_Data.rds") ## load Covid dataset
Map_Data <- readRDS("gadm36_DEU_2_sf.rds") ## load map dataset
st_crs(Map_Data) <- st_crs(Map_Data)

Map_Data <- Map_Data %>% select(ags5=CC_2, everything()) ## rename CC2 in Map_Data to ags5, consistent with Analysis_Data
Map_Data <- Map_Data %>% mutate(ags5=as.numeric(ags5)) ## change ags5 in Map_Data to numeric


joined_Data <- full_join(Analysis_Data, Map_Data, by="ags5") ## join the datasets with a full join, keeping all observations
joined_Data <- st_as_sf(joined_Data)
joined_Data$incBinary <- ifelse(joined_Data$incidence<=70, "0", "1") ## create a binary variable that is 1 if the incidence is above 70
joined_Data$OccupBinary <- ifelse(joined_Data$occupancy_free_all<=0.25, "1", "0") ## create a binary variable that is 1 if the number of free ICU beds is below 25%
joined_Data$Fully_Ocuppied <- ifelse(joined_Data$betten_frei==0, "1", "0") ## create a binary variable that is 1 if there are no free ICU beds
joined_Data$Missing_Data <- ifelse(joined_Data$ags5 %in% c(9473, 9573, 9374, 11000, 3152, 3156, 7336, 7338, 16068), "1", "0") #binary variable used to keep NA values in maps
joined_Data <- joined_Data %>% filter(ags5 != "na") ##dropping map data for the Bodensee
joined_Data <- joined_Data %>% select(ags5, Missing_Data, everything()) ## order variables
joined_Data <- joined_Data %>% mutate(share_Covid = faelle_covid_aktuell / betten_belegt) ## number of Covid ICU patients divided by total number of ICU patients



theme_set(hrbrthemes::theme_ipsum()) ##set a theme


### Now create all maps


## Fully Occupied ICUs Nov 16, 2020:

fully_occupied_plot = ggplot(joined_Data %>% filter(date=="2020-11-16" | Missing_Data=="1")) + geom_sf(aes(fill = Fully_Ocuppied), alpha=0.8, col="white") + scale_fill_discrete(name = "Fully Occupied ICUs", type=c("#24de10", "#ff0000"), na.value="gray50", labels=c("Not fully occupied", "Fully Occupied")) + ggtitle("Fully Occupied ICUs by district (Kreis), 16.11.2020")
fully_occupied_plot
ggsave("Fully_Occupied_Plot_16Nov20.png", scale=3.5, width=1000, height=1500, units="px")


## Share of Covid patients, Nov 16, 2020 and Apr 2, 2021:

# For Nov 16, 2020:
# using viridis: share_plot = ggplot(joined_Data %>% filter(date=="2020-11-16" | Missing_Data=="1")) + geom_sf(aes(fill = share_Covid), alpha=0.8, col="white") + scale_fill_viridis_b(name = "Share of occupied ICU beds with Covid patients", option="C", direction=-1, na.value="gray50", n.breaks=6) + ggtitle("Share of occupied ICU beds with Covid patients by district (Kreis), 16.11.2020")
share_plot = ggplot(joined_Data %>% filter(date=="2020-11-16" | Missing_Data=="1")) + geom_sf(aes(fill = share_Covid), alpha=0.8, col="white") + scale_fill_gradient(name = "Share of occupied ICU beds with Covid patients", low="yellow", high="red", limits=c(0, 1), na.value="gray50", n.breaks=6) + ggtitle("Share of occupied ICU beds with Covid patients by district (Kreis), 16.11.2020")
share_plot
ggsave("Share_Covid_ICU_16Nov20.png", scale=3.5, width=1000, height=1500, units="px")

# For Apr 2, 2021:
# using viridis: share_plot = ggplot(joined_Data %>% filter(date=="2021-04-02" | Missing_Data=="1")) + geom_sf(aes(fill = share_Covid), alpha=0.8, col="white") + scale_fill_viridis_b(name = "Share of occupied ICU beds with Covid patients", option="C", direction=-1, na.value="gray50", n.breaks=6) + ggtitle("Share of occupied ICU beds with Covid patients by district (Kreis), 02.04.2021")
share_plot = ggplot(joined_Data %>% filter(date=="2021-04-02" | Missing_Data=="1")) + geom_sf(aes(fill = share_Covid), alpha=0.8, col="white") + scale_fill_gradient(name = "Share of occupied ICU beds with Covid patients", low="yellow", high="red", limits=c(0, 1), na.value="gray50", n.breaks=6) + ggtitle("Share of occupied ICU beds with Covid patients by district (Kreis), 02.04.2021")
share_plot
ggsave("Share_Covid_ICU_02Apr21.png", scale=3.5, width=1000, height=1500, units="px")



## Incidence and Occupancy Plots Nov 20 - Apr 21: 

# For Nov 16, 2020:
incidence_plot = ggplot(joined_Data %>% filter(date=="2020-11-16" | Missing_Data=="1")) + geom_sf(aes(fill = incBinary), alpha=0.8, col="white") + scale_fill_discrete(name = "Incidence", type=c("#24de10", "#ff0000"), na.value="gray50", labels=c("below 70", "above 70")) + ggtitle("Incidence by district (Kreis), 16.11.2020")
incidence_plot
ggsave("Incidence_Plot_16Nov20.png", scale=3.5, width=1000, height=1500, units="px")

occupancy_plot = ggplot(joined_Data %>% filter(date=="2020-11-16" | Missing_Data=="1")) + geom_sf(aes(fill = OccupBinary), alpha=0.8, col="white") + scale_fill_discrete(name = "Free ICU capacity", type=c("#24de10", "#ff0000"), na.value="gray50", labels=c("above 25% free capacity", "below 25% free capacity")) + ggtitle("Free ICU Capacity by district (Kreis), 16.11.2020")
occupancy_plot
ggsave("Occupancy_Plot_16Nov20.png", scale=3.5, width=1000, height=1500, units="px")


# For Dec 2, 2020:
incidence_plot = ggplot(joined_Data %>% filter(date=="2020-12-02" | Missing_Data=="1")) + geom_sf(aes(fill = incBinary), alpha=0.8, col="white") + scale_fill_discrete(name = "Incidence", type=c("#24de10", "#ff0000"), na.value="gray50", labels=c("below 70", "above 70")) + ggtitle("Incidence by district (Kreis), 02.12.2020")
incidence_plot
ggsave("Incidence_Plot_02Dec20.png", scale=3.5, width=1000, height=1500, units="px")

occupancy_plot = ggplot(joined_Data %>% filter(date=="2020-12-02" | Missing_Data=="1")) + geom_sf(aes(fill = OccupBinary), alpha=0.8, col="white") + scale_fill_discrete(name = "Free ICU capacity", type=c("#24de10", "#ff0000"), na.value="gray50", labels=c("above 25% free capacity", "below 25% free capacity")) + ggtitle("Free ICU Capacity by district (Kreis), 02.12.2020")
occupancy_plot
ggsave("Occupancy_Plot_02Dec20.png", scale=3.5, width=1000, height=1500, units="px")

# For Dec 17, 2020:
incidence_plot = ggplot(joined_Data %>% filter(date=="2020-12-17" | Missing_Data=="1")) + geom_sf(aes(fill = incBinary), alpha=0.8, col="white") + scale_fill_discrete(name = "Incidence", type=c("#24de10", "#ff0000"), na.value="gray50", labels=c("below 70", "above 70")) + ggtitle("Incidence by district (Kreis), 17.12.2020")
incidence_plot
ggsave("Incidence_Plot_17Dec20.png", scale=3.5, width=1000, height=1500, units="px")

occupancy_plot = ggplot(joined_Data %>% filter(date=="2020-12-17" | Missing_Data=="1")) + geom_sf(aes(fill = OccupBinary), alpha=0.8, col="white") + scale_fill_discrete(name = "Free ICU capacity", type=c("#24de10", "#ff0000"), na.value="gray50", labels=c("above 25% free capacity", "below 25% free capacity")) + ggtitle("Free ICU Capacity by district (Kreis), 17.12.2020")
occupancy_plot
ggsave("Occupancy_Plot_17Dec20.png", scale=3.5, width=1000, height=1500, units="px")

# For Jan 5, 2021:
incidence_plot = ggplot(joined_Data %>% filter(date=="2021-01-05" | Missing_Data=="1")) + geom_sf(aes(fill = incBinary), alpha=0.8, col="white") + scale_fill_discrete(name = "Incidence", type=c("#24de10", "#ff0000"), na.value="gray50", labels=c("below 70", "above 70")) + ggtitle("Incidence by district (Kreis), 05.01.2021")
incidence_plot
ggsave("Incidence_Plot_05Jan21.png", scale=3.5, width=1000, height=1500, units="px")

occupancy_plot = ggplot(joined_Data %>% filter(date=="2021-01-05" | Missing_Data=="1")) + geom_sf(aes(fill = OccupBinary), alpha=0.8, col="white") + scale_fill_discrete(name = "Free ICU capacity", type=c("#24de10", "#ff0000"), na.value="gray50", labels=c("above 25% free capacity", "below 25% free capacity")) + ggtitle("Free ICU Capacity by district (Kreis), 05.01.2021")
occupancy_plot
ggsave("Occupancy_Plot_05Jan21.png", scale=3.5, width=1000, height=1500, units="px")

# For Jan 21, 2021:
incidence_plot = ggplot(joined_Data %>% filter(date=="2021-01-21" | Missing_Data=="1")) + geom_sf(aes(fill = incBinary), alpha=0.8, col="white") + scale_fill_discrete(name = "Incidence", type=c("#24de10", "#ff0000"), na.value="gray50", labels=c("below 70", "above 70")) + ggtitle("Incidence by district (Kreis), 21.01.2021")
incidence_plot
ggsave("Incidence_Plot_21Jan21.png", scale=3.5, width=1000, height=1500, units="px")

occupancy_plot = ggplot(joined_Data %>% filter(date=="2021-01-21" | Missing_Data=="1")) + geom_sf(aes(fill = OccupBinary), alpha=0.8, col="white") + scale_fill_discrete(name = "Free ICU capacity", type=c("#24de10", "#ff0000"), na.value="gray50", labels=c("above 25% free capacity", "below 25% free capacity")) + ggtitle("Free ICU Capacity by district (Kreis), 21.01.2021")
occupancy_plot
ggsave("Occupancy_Plot_21Jan21.png", scale=3.5, width=1000, height=1500, units="px")

# For Feb 4, 2021:
incidence_plot = ggplot(joined_Data %>% filter(date=="2021-02-04" | Missing_Data=="1")) + geom_sf(aes(fill = incBinary), alpha=0.8, col="white") + scale_fill_discrete(name = "Incidence", type=c("#24de10", "#ff0000"), na.value="gray50", labels=c("below 70", "above 70")) + ggtitle("Incidence by district (Kreis), 04.02.2021")
incidence_plot
ggsave("Incidence_Plot_04Feb21.png", scale=3.5, width=1000, height=1500, units="px")

occupancy_plot = ggplot(joined_Data %>% filter(date=="2021-02-04" | Missing_Data=="1")) + geom_sf(aes(fill = OccupBinary), alpha=0.8, col="white") + scale_fill_discrete(name = "Free ICU capacity", type=c("#24de10", "#ff0000"), na.value="gray50", labels=c("above 25% free capacity", "below 25% free capacity")) + ggtitle("Free ICU Capacity by district (Kreis), 04.02.2021")
occupancy_plot
ggsave("Occupancy_Plot_04Feb21.png", scale=3.5, width=1000, height=1500, units="px")

# For Feb 17, 2021:
incidence_plot = ggplot(joined_Data %>% filter(date=="2021-02-17" | Missing_Data=="1")) + geom_sf(aes(fill = incBinary), alpha=0.8, col="white") + scale_fill_discrete(name = "Incidence", type=c("#24de10", "#ff0000"), na.value="gray50", labels=c("below 70", "above 70")) + ggtitle("Incidence by district (Kreis), 17.02.2021")
incidence_plot
ggsave("Incidence_Plot_17Feb21.png", scale=3.5, width=1000, height=1500, units="px")

occupancy_plot = ggplot(joined_Data %>% filter(date=="2021-02-17" | Missing_Data=="1")) + geom_sf(aes(fill = OccupBinary), alpha=0.8, col="white") + scale_fill_discrete(name = "Free ICU capacity", type=c("#24de10", "#ff0000"), na.value="gray50", labels=c("above 25% free capacity", "below 25% free capacity")) + ggtitle("Free ICU Capacity by district (Kreis), 17.02.2021")
occupancy_plot
ggsave("Occupancy_Plot_17Feb21.png", scale=3.5, width=1000, height=1500, units="px")

# For Mar 1, 2021:
incidence_plot = ggplot(joined_Data %>% filter(date=="2021-03-01" | Missing_Data=="1")) + geom_sf(aes(fill = incBinary), alpha=0.8, col="white") + scale_fill_discrete(name = "Incidence", type=c("#24de10", "#ff0000"), na.value="gray50", labels=c("below 70", "above 70")) + ggtitle("Incidence by district (Kreis), 01.03.2021")
incidence_plot
ggsave("Incidence_Plot_01Mar21.png", scale=3.5, width=1000, height=1500, units="px")

occupancy_plot = ggplot(joined_Data %>% filter(date=="2021-03-01" | Missing_Data=="1")) + geom_sf(aes(fill = OccupBinary), alpha=0.8, col="white") + scale_fill_discrete(name = "Free ICU capacity", type=c("#24de10", "#ff0000"), na.value="gray50", labels=c("above 25% free capacity", "below 25% free capacity")) + ggtitle("Free ICU Capacity by district (Kreis), 01.03.2021")
occupancy_plot
ggsave("Occupancy_Plot_01Mar21.png", scale=3.5, width=1000, height=1500, units="px")

# For Mar 19, 2021:
incidence_plot = ggplot(joined_Data %>% filter(date=="2021-03-19" | Missing_Data=="1")) + geom_sf(aes(fill = incBinary), alpha=0.8, col="white") + scale_fill_discrete(name = "Incidence", type=c("#24de10", "#ff0000"), na.value="gray50", labels=c("below 70", "above 70")) + ggtitle("Incidence by district (Kreis), 19.03.2021")
incidence_plot
ggsave("Incidence_Plot_19Mar21.png", scale=3.5, width=1000, height=1500, units="px")

occupancy_plot = ggplot(joined_Data %>% filter(date=="2021-03-19" | Missing_Data=="1")) + geom_sf(aes(fill = OccupBinary), alpha=0.8, col="white") + scale_fill_discrete(name = "Free ICU capacity", type=c("#24de10", "#ff0000"), na.value="gray50", labels=c("above 25% free capacity", "below 25% free capacity")) + ggtitle("Free ICU Capacity by district (Kreis), 19.03.2021")
occupancy_plot
ggsave("Occupancy_Plot_19Mar21.png", scale=3.5, width=1000, height=1500, units="px")

# For Apr 2, 2021:
incidence_plot = ggplot(joined_Data %>% filter(date=="2021-04-02" | Missing_Data=="1")) + geom_sf(aes(fill = incBinary), alpha=0.8, col="white") + scale_fill_discrete(name = "Incidence", type=c("#24de10", "#ff0000"), na.value="gray50", labels=c("below 70", "above 70")) + ggtitle("Incidence by district (Kreis), 02.04.2021")
incidence_plot
ggsave("Incidence_Plot_02Apr21.png", scale=3.5, width=1000, height=1500, units="px")

occupancy_plot = ggplot(joined_Data %>% filter(date=="2021-04-02" | Missing_Data=="1")) + geom_sf(aes(fill = OccupBinary), alpha=0.8, col="white") + scale_fill_discrete(name = "Free ICU capacity", type=c("#24de10", "#ff0000"), na.value="gray50", labels=c("above 25% free capacity", "below 25% free capacity")) + ggtitle("Free ICU Capacity by district (Kreis), 02.04.2021")
occupancy_plot
ggsave("Occupancy_Plot_02Apr21.png", scale=3.5, width=1000, height=1500, units="px")

# For Apr 19, 2021:
incidence_plot = ggplot(joined_Data %>% filter(date=="2021-04-19" | Missing_Data=="1")) + geom_sf(aes(fill = incBinary), alpha=0.8, col="white") + scale_fill_discrete(name = "Incidence", type=c("#24de10", "#ff0000"), na.value="gray50", labels=c("below 70", "above 70")) + ggtitle("Incidence by district (Kreis), 19.04.2021")
incidence_plot
ggsave("Incidence_Plot_19Apr21.png", scale=3.5, width=1000, height=1500, units="px")

occupancy_plot = ggplot(joined_Data %>% filter(date=="2021-04-19" | Missing_Data=="1")) + geom_sf(aes(fill = OccupBinary), alpha=0.8, col="white") + scale_fill_discrete(name = "Free ICU capacity", type=c("#24de10", "#ff0000"), na.value="gray50", labels=c("above 25% free capacity", "below 25% free capacity")) + ggtitle("Free ICU Capacity by district (Kreis), 19.04.2021")
occupancy_plot
ggsave("Occupancy_Plot_19Apr21.png", scale=3.5, width=1000, height=1500, units="px")
