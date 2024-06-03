#Assignment 7
#Name: Rutwik Borole | Id: 22224253

#Importing libraries
library(aimsir17)
library(ggplot2)
library(dplyr)
library(tidyr)

#Create a new tibble with additional column called as season
st <- filter(observations, station == "MACE HEAD" | station == "DUBLIN AIRPORT" | station == "SherkinIsland")
obs <- st %>% group_by(station, year, month, day, hour) %>%
        mutate(season = case_when(month %in% c(1,11,12) ~ "Winter",
                                  month %in% c(2:4) ~ "Spring",
                                  month %in% c(5:7) ~ "Summer",
                                  month %in% c(8:10) ~ "Autumn"))
obs
glimpse(obs)

#Extract energy data from eirgrid17 that records the average hourly demand from both regions of the island, IEDemand and NIDemand.
ener <- eirgrid17 %>% group_by(year, month, day, hour) %>%
          summarise(IE = mean(IEDemand, na.rm = T), NI = mean(NIDemand, na.rm = T), CheckObs = n()) %>% ungroup()
ener
glimpse(ener)

#Set seed value to 100 and join obs and ener datasets
set.seed(100)
ds <- left_join(ener,obs,by=c("year", "month", "day", "hour")) %>% sample_frac(0.1)
ds
glimpse(ds)

#Reducing the number of columns of ds
ds <- select(ds, station,month,temp,season,IE,NI)
ds

#Create a new dataset ds1 from ds
ds1 <- ds %>% pivot_longer(IE:NI, names_to = "Area", values_to = "Demand")
ds1

#Plot the graphs from tibble ds1
p1 <- ggplot(ds1, aes(x = temp, y = Demand, colour = Area))+
        geom_point()+
        geom_smooth(method = lm)+
        facet_grid(station~season)
p1

p2 <- ggplot(ds1, aes(x = temp, y = Demand, colour = Area))+
        geom_point()+
        geom_smooth(method = lm)+
        facet_grid(station~month)
p2

#Generate summaries from ds
cor_season <- ds %>% group_by(station, season) %>%
        summarise(Corr_IE = cor(IE,temp), Corr_NE = cor(NI,temp), Diff = Corr_IE - Corr_NE)

cor_month <- ds %>% group_by(station, month) %>%
              summarise(Corr_IE = cor(IE,temp), Corr_NE = cor(NI,temp), Diff = Corr_IE - Corr_NE)

slice(cor_season,1:nrow(cor_season))
slice(cor_month,1:nrow(cor_month))

