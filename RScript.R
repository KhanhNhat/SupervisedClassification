library(class)
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
