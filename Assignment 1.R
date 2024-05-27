#Name: Rutwik Borole | Id: 22224253

#The first task is to set the random number generator seed to 100.
set.seed(100)

#Generate 100 random values in a normal distribution with m=7 & sd=4.
temp <- c(rnorm(100, mean = 7, sd  = 4))

#Set names for each value in the data set.
names(temp) <- paste0("D-", 1:100)

#round each value to 1 decimal place.
temp <- round(temp, digits = 1)
head(temp)
tail(temp)
#1.Calculate and display the number of days where the temperature was greater than the mean.
gt_mean <- mean(temp)
gt_temp <- temp[temp > gt_mean]
length(gt_temp)

#2.Display the day with the maximum temperature, using cat().
max_temp <- max(temp)
n = names(temp)[match(max_temp,temp)]
cat("The max temp was on day", n, "with a value of", max_temp)

#3.Display the day with the minimum temperature.
min_temp <- min(temp)
n = names(temp)[match(min_temp,temp)]
cat("The min temp was on day", n, "with a value of", min_temp)

#4.Create a parallel vector called warnings, which has two values Warning or Normal,
#where a temperature weather warning is in place if the temperature is less than or equal to 4.0.
warnings <- c(ifelse(temp <= 4.0, 'Warning','Normal'))
temp[40:44]
warnings[40:44]

# 5. Display the number of days where the weather warning was in operation.
warn_days <- c(warnings == "Warning")
disp <- temp[warn_days]
disp_days = length(disp)
cat("The number of days the warnings were in operation =",disp_days)

#6. Display the days where the weather warning was in operation.
ww = names(temp)[match(disp,temp)]
ww

#7. Display the warning in a tabular format.
tw <- table(warnings)
tw

#8. Use the function rle() to and out the maximum sequence of weather warnings in the
#data. Note rle() returns a list showing the lengths and the values.
len <- rle(warnings)
len1 <- tapply(len$lengths,len$values, max)
disp_warn <- len1[2]
disp_norm <- len1[1]
cat("The maximum run of days with warnings was", disp_warn, "\nThe maximum run of days without warnings was", disp_norm)
