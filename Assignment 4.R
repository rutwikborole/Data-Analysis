#Assignment 4
#Name:Rutwik Borole | Id:22224253

#Create the synthetic data for 5 subjects

set.seed(100)
CX101 <- rnorm(20,45,8)
CX102 <- rnorm(20,65,8)
CX103 <- rnorm(20,85,10)
CX104 <- rnorm(20,45,10)
CX105 <- rnorm(20,60,5)


#Create a matrix from the above data:

res <- matrix(c(CX101,CX102,CX103,CX104,CX105), nrow = 20, ncol = 5,
               dimnames = list(c(paste0("Student_",1:20)),
                               c("CX101", "CX102", "CX103", "CX104", "CX105")))


#Checking the summary for our matrix
summary(res)

#Printing our result matrix
res

#Locating scores higher than 100
res[res[,"CX103"] > 100,]
outlier <- function(x) ifelse(x>100,NA,x)
outlier(res)

#Converting outliers to NA
res1 <- apply(res, 2, function(x) ifelse(x>100|x<0,NA,x))
res1
         
#Replacing NA values with the mean values of CX103
res2 <- apply(res1, 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
res2              

#Calculating Mean and range for each student and appending them to res2
m_mean <- apply(res2, 1, mean) 
m_range <- apply(res2, 1, function(x) max(x)-min(x))

res2 <- cbind(res2, m_mean)
colnames(res2)[6]<- "Mean"

res2<- cbind(res2, m_range)
colnames(res2)[7]<- "Range"

res2

#Filter Query to display student with highest average
query <- subset(res2, m_mean==max(m_mean))
query

