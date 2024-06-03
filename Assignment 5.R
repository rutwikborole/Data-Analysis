#Assignment 5
#Name: Rutwik Borole | Id: 22224253

#Importing libraries
library(nycflights13)
library(ggplot2)
library(lubridate)
library(tibble)

# Creating a tibble out of the flights dataset
d <- subset(flights, select = c(month, origin,day, dep_delay, arr_delay, air_time, distance, carrier))

#Creating new columns, monthname and hour and appending them to our tibble d
m_n <- flights$time_hour

MonthName <- month(ymd_hms(m_n), label = TRUE)

Hour <- as.factor(flights$hour)

d <- add_column(d,MonthName,Hour, .before = "origin")

glimpse(d)

#1.Boxplot for the departure delays by month.
p1 <- ggplot(d, aes(x = MonthName, y = dep_delay, colour = origin))+
       geom_boxplot()+
       ylim(-30,30)+
       ggtitle("Departure Delays by Month")+
       labs(y = "Depature Delay", x = "Month")

#2.Boxplot for the departure delays by hour of the day
p2 <- ggplot(d, aes(x = Hour, y = dep_delay, colour = origin))+
        geom_boxplot()+
        ylim(-30,30)+
        ggtitle("Departure Delays by Hour of Day")+
        labs(y = "Depature Delay", x = "Hour")

#3.Faceted histogram of the air time of flights by origin
p3 <- ggplot(d, aes(x = air_time, fill= origin))+
        geom_histogram(show.legend = FALSE)+
        facet_wrap(~origin, nrow = 3)+
        scale_x_continuous(n.breaks = 15) +
        ggtitle("Histogram of Airtime from each Airport")+
        labs(y = "Number of Flights", x = "Time in Air")
  
#4.Faceted histogram of the flight distance by origin.
p4 <- ggplot(d, aes(x = distance, fill= origin))+
        geom_histogram(show.legend = FALSE)+
        facet_wrap(~origin, nrow = 3)+
        scale_x_continuous(n.breaks = 11) +
        ggtitle("Histogram of Distance from each Airport")+
        labs(y = "Number of Flights", x = "Distance")

#5. Bar chart showing the number of flights per carrier
p5 <- ggplot(d, aes(x=carrier, fill = origin))+
       geom_bar()+
       ggtitle("Count of flights from the different origins")+
       labs(y = "Number of Flights", x = "Carrier")
      
      
#6. With a seed value of 100, select a random sample of 3000 from the tibble,
#and store this in the variable in d1
    
set.seed(100)    

s_r <- sample(nrow(flights),3000)

d1 <- flights[s_r,]

head(d1)

summary(d1)

#7. Plot the departure delay v arrival delay
p6 <- ggplot(d1, aes(x = dep_delay, y = arr_delay, colour = origin))+
        geom_point()+
        geom_smooth()+
        ggtitle("Departure Delay v Arrival Delay for N=3000")+
        labs(y = "Arrival Delay", x = "Departure Delay")

#8. Plot the distance v Air Time
p7 <- ggplot(d1, aes(x = distance, y = air_time, colour = origin))+
        geom_point()+
        geom_smooth(method = lm)+
        ylim(0,700)+
        ggtitle("Distance v Air Time for N=3000")+
        labs(y = "Air Time", x = "Distance")

