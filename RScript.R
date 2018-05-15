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


#Now, using naivebayes package
testLocation = data.frame(daytype = as.factor(c('weekday', 'weekend')))

#Create the location prediction model
loc_pred = naive_bayes(location ~ daytype, data = location9am)

#Predict location with testLocation, return the highest probability event
predict(loc_pred, testLocation)

#Show probability of all events
predict(loc_pred, testLocation, type = 'prob')

#Try naivebayes with more complex dataset
locationHour = read.csv('locationHourType.csv', row.names = 1)

locHour_pred = naive_bayes(location ~ daytype + hourtype, data = locationHour)

testLocationHour = data.frame(daytype = as.factor(c('weekday', 'weekday', 'weekend')),
                              hourtype = as.factor(c('morning', 'night', 'morning')))

predict(locHour_pred, testLocationHour)

predict(locHour_pred, testLocationHour, type = 'prob')


#Study Logistic Regression or also called Binary Classification

#Load new dataset for this study
donors = read.csv('donors.csv')

table(donors$donated)

#Create a simpe logistic model
donation_model = glm(donated ~ bad_address + interest_religion + interest_veterans,
                     data = donors, family = 'binomial')

#Create a probability column of this dataset
donors$donation_prob = predict(donation_model, type = 'response')

#Calculate the mean of real donated
ave_prob = mean(donors$donated)

#Create a prediction column
donors$donation_pred = ifelse(donors$donation_prob > ave_prob, 1, 0)

#Calculate the accuracy of model
mean(donors$donated == donors$donation_pred)
