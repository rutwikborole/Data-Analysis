#Assignment 9
#Name: Rutwik Borole | Id: 22224253

#importing libraries
library(purrr)
library(dplyr)
library(ggplot2)
library(tidyr)

#creating a tibble from mpg and grouping it by class
d1 <- mpg %>% group_by(class)
d1

##Creating function my_mpg_lms() and variable mods1
my_mpg_lms <- function(x){
  lms <- group_split(x) %>% map(~lm(cty~displ, data=.))
  class(lms) <- "my_mpg_lms"
  names(lms) <- unlist(group_keys(x))
  lms
}

#Storing the function call of d1 to mods1
mods1 <- my_mpg_lms(d1)

#Checking the length and Class of the mods1
length(mods1)
class(mods1)

#Exploring the names and structure of mods1
names(mods1)
str(mods1[[1]])
str(mods1[[7]])

#Creating a user defined function for summarry function
summary.my_mpg_lms <- function(x){
  cat("The following are the model groups","/n")
  cat(names(x),"\n")
  i=1
  walk2(names(x),x,~{
    cat("Model #",i,"Group",.x,"Obs =",paste(count(.y$model)))
  print(summary(.y))
  i<<-i+1
  })}

summary(mods1)

#creating a tibble from mpg and grouping it by manufacturers
d2 <- mpg %>% group_by(manufacturer)
mods2 <- my_mpg_lms(d2)
names(mods2)

summary(mods2)


