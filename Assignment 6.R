#Assignment 6
#Name: Rutwik Borole | Id: 22224253

#Importing libraries
library(aimsir17)
library(ggplot2)
library(dplyr)

#Summary of the total rainfall and avg temperature for each day of each month
s_data <- observations %>%
            group_by(station, day, month) %>%
            summarise(TotalRain = sum(rain), AvrTemp = mean(temp))
s_data            
glimpse(s_data)

#Perform difference operations on daily changes in the temperature and rain and display them
s_data_diff <- s_data %>%
                group_by(station) %>%
                arrange(month) %>%
                mutate(RainDiff = TotalRain - lag(TotalRain), AbsRainDiff = abs(RainDiff), 
                       MeanTempDiff = AvrTemp - lag(AvrTemp), AbsMeanTempDiff = abs(MeanTempDiff)) %>%
                ungroup(station)

glimpse(s_data_diff)
          
arrange(s_data_diff,desc(AbsRainDiff)) %>% slice(1:5)

arrange(s_data_diff,desc(AbsMeanTempDiff)) %>% slice(1:5)

#Create new tibble which generates monthly summaries for each station

out <- s_data_diff %>%
        group_by(station, month) %>%
        summarise(AvrDiffTemp = mean(MeanTempDiff, na.rm = T), SDDiffTemp = sd(MeanTempDiff, na.rm = T), 
                  MinDiffTemp = min(MeanTempDiff, na.rm = T), MaxDiffTemp = max(MeanTempDiff, na.rm = T),
                  AvrDiffRain = mean(RainDiff, na.rm = T), SDDiffRain = sd(RainDiff, na.rm = T),
                  MinDiffRain = min(RainDiff, na.rm = T), MaxDiffRain = max(RainDiff, na.rm = T))
out        
glimpse(out)  

#Generate a graph that shows the mean, standard deviation
g1 <- ggplot(out, aes(x = month, y = AvrDiffTemp))+
        geom_point()+
        geom_line()+
        geom_ribbon(aes(ymin=out$MinDiffTemp, ymax=out$MaxDiffTemp),alpha=0.01,colour="blue", size=1)+
        geom_ribbon(aes(ymin=out$AvrDiffTemp-out$SDDiffTemp, ymax=out$AvrDiffTemp+out$SDDiffTemp), alpha = 0.4,fill="red")+
        scale_x_continuous(n.breaks = 12)+
        facet_wrap(~station, nrow = 5)+
        coord_cartesian(ylim = c(-5,5))

g2 <- ggplot(out, aes(x = month, y = AvrDiffRain))+
        geom_point()+
        geom_line()+
        geom_ribbon(aes(ymin=out$MinDiffRain, ymax=out$MaxDiffRain),alpha=0.01,colour="red", size=1)+
        geom_ribbon(aes(ymin=out$AvrDiffRain-out$SDDiffRain, ymax=out$AvrDiffRain+out$SDDiffRain), alpha = 0.4,fill="blue")+
        scale_x_continuous(n.breaks = 12)+
        facet_wrap(~station, nrow = 5)+
        coord_cartesian(ylim = c(-20,20))

