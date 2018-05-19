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

#Load pROC package for making ROC curve
library(pROC)

donation_ROC = roc(donors$donated, donors$donation_prob)

plot(donation_ROC)

auc(donation_ROC)

#Do the same thing, using caTools package
library(caTools)
colAUC(donors[, c(3,7,8)], donors[, 1], plotROC = TRUE)

#Study how to make decision with Tree

#Load rpart package
library(rpart)
library(rpart.plot)

#Load dataset
loans = read.csv('loans.csv')

#Modify this dataset
loans = loans %>% mutate(outcome = ifelse(default == 1, 'default', 'repaid'))
loans$outcome = as.factor(loans$outcome)
loans = loans[-c(1,2,3)]

#Create a simple tree model with rpart::rpart function, dataset kyphosis
ky_TreeModel = rpart(Kyphosis ~ Age + Number + Start,
                       data = kyphosis, method = 'class', 
                       control = rpart.control(cp = 0))

#Plot a tree with rpart.plot::rpart.plot function
rpart.plot(ky_TreeModel, type = 3, box.palette = c('green', 'tomato'), fallen.leaves = TRUE)

#Create training and test data from loans dataset
loan_sample = sample(nrow(loans), nrow(loans) * 0.75)

loan_train = loans[loan_sample,]
loan_test = loans[-loan_sample,]

#Create a complex Tree Classification model and calculate its accuracy
#It is too complex to draw with rpart.plot
loan_TreeModel = rpart(outcome ~ ., data = loan_train, method = 'class', control = rpart.control(cp = 0))
loan_test$pred = predict(loan_TreeModel, loan_test, type = 'class')
mean(loan_test$pred == loan_test$outcome)

#Now, cut down the overgrown tree with pre-prune. It can improve model accuracy
#Maximum go to 6 level only
loan_TreeModel_Pre1 = rpart(outcome ~ ., data = loan_train, method = 'class', 
                       control = rpart.control(maxdepth = 6))
loan_test$pred_pre1 = predict(loan_TreeModel_Pre1, loan_test, type = 'class')
mean(loan_test$pred_pre1 == loan_test$outcome)

#Have about 500 observations
loan_TreeModel_Pre2 = rpart(outcome ~ ., data = loan_train, method = 'class', 
                            control = rpart.control(minsplit = 500))
loan_test$pred_pre2 = predict(loan_TreeModel_Pre2, loan_test, type = 'class')
mean(loan_test$pred_pre2 == loan_test$outcome)

#Try to cut down the tree with post-prune
#Plot out the cp value to choose appropriate value for cp
plotcp(loan_TreeModel)

#Prune the tree and calculate its accuracy
loan_TreeModel_post = prune(loan_TreeModel, cp = 0.00044)
loan_test$pred_post = predict(loan_TreeModel_post, loan_test, type = 'class')
mean(loan_test$pred_post == loan_test$outcome)

#Now try Random Forest technique
library(randomForest)

loan_Model_ranForest = randomForest(outcome ~ ., data = loan_train)
loan_test$pred_ranForest = predict(loan_Model_ranForest, loan_test, type = 'class')
mean(loan_test$pred_ranForest == loan_test$outcome)


