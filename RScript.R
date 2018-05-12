library(class)
library(tidyverse)

trafficSign = read_csv('knn_traffic_signs.csv')
nextSign = read_csv('nextSign.csv')

sign_types = trafficSign$sign_type

knn(train = trafficSign[-1], test = nextSign, cl = sign_types)