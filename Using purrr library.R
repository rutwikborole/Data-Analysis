#Importing Libraries and setting seed value to 200
library(aimsir17)
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(ggpubr)
library(randomcoloR)
set.seed(200)

#Creating a new tibble which stores hourly avg wind energy generated for a year
ener <- eirgrid17 %>% group_by(year, month, day, hour) %>%
        summarise(AvrWindGen = mean(IEWindGeneration))
ener
glimpse(ener)

#Join observations data set with ener tibble
ds <- left_join(ener,observations, by=c("year", "month", "day", "hour")) %>% ungroup()
ds
glimpse(ds)
sum(!complete.cases(ds))

#Retain only the complete cases in the tibble ds with sample fraction (0.01)
ds <- ds %>% na.omit %>% sample_frac(0.01)
ds
glimpse(ds)
sum(!complete.cases(ds))

#Creating a nested version of ds tibble
ds_n <- ds %>% group_by(station) %>%
          nest()
ds_n
glimpse(ds_n)

#Add a linear model to the tibble and extract the r-squared value 
ds_n <- ds_n %>% mutate(LM = map(data,~lm(wdsp~AvrWindGen,data = .)),
                        summary = map(LM,~summary(.x)),
                        R_SQ = map_dbl(summary,pluck,"r.squared")) %>%
                        arrange(desc(R_SQ))
                        
ds_n <- select(ds_n, station, data, LM, R_SQ)
ds_n

#Add plot to the ds_n tibble as a column
ds_n <- ds_n %>% mutate(Plots = map2(data,station,~{
                  ggplot(.x,aes(x=wdsp,y=AvrWindGen))+
                  geom_jitter(colour=randomColor())+
                  geom_smooth(size = 1.5, colour=randomColor(luminosity = "light"))+
                  labs(x="Speed", y="Power")+
                  ggtitle(station)+
                  theme(axis.title.y= element_text(face="bold"),plot.title = element_text(size = 6))+
                  theme_classic()}))

ggarrange(plotlist = ds_n$Plots)
