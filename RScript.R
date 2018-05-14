#Load class package for K Nearest Neighbors
library(class)

#Load naivebayes package for Naive Bayesian method
library(naivebayes)

#Load tidyverse package to manipulation data
library(tidyverse)

trafficSign = read_csv('knn_traffic_signs.csv')
nextSign = read_csv('nextSign.csv')
testSign = read_csv('TestSign.csv')
sign_types = trafficSign$sign_type

#Predict type of the nextSign dataset
knn(train = trafficSign[, 4:51], test = nextSign, cl = sign_types)

table(sign_types)

#Check variable average by sign_type, for example, r10:
aggregate(r10 ~ sign_type, data = trafficSign, mean)

#Predict for testSign dataset
sign_pred = knn(train = trafficSign[, 4:51], test = testSign[, 2:49], cl = sign_types)
table(sign_pred, testSign$sign_type)

location9am = read.csv('location9am.csv', row.names = 1)

#Canculate probability by Bayes formula P(A|B) = P(A and B)/P(B)

#Probability of stay at office at 9am, this is event A
p_AtOffice = nrow(subset(location9am, location == 'office'))/nrow(location9am)

#Probability of it is a weekday, this is event B
p_Weekday = nrow(subset(location9am, daytype == 'weekday'))/nrow(location9am)

#Probability of stay at office at 9am and on weekday, this is event A and B
p_OfficeAndWeekday = nrow(subset(location9am, location == 'office' & daytype == 'weekday'))/nrow(location9am)

#Probability of stay at office if it is a weekday, this is A|B
p_OfficeGivenWeekday = p_OfficeAndWeekday/p_Weekday
