#read dataset
library(readr)
credit_data <- read_csv("credit_data.csv", TRUE, ",")
View(credit_data)

head(credit_data)
nrows(credit_data)

#reference columns without $ operator
attach(credit_data)

#create training and test dataset 80%-20%
train_dataset <- clientid<1600
test_dataset <- !train_dataset

#run (train) logistic regression model with 3 features (income, age, loan)
model <- glm(formula = default10yr~income+age+loan, data=credit_data, subset = train_dataset, family="binomial")
summary(model)

#predict with the logistic regression model
probability_default <- predict(model, newdata=credit_data[test_dataset,], type="response")

#logistic regression assigns probability to every output class 
#print probability_default
#if probability > 50%, then it is 1 esle 0
probability_default
predictions <- ifelse(probability_default>0.5, 1, 0)

#prepare confusion matrix that describes performance of a classification model
#diagonal elements are correct classifications
#off-diagonals are incorrect predictions; want values as low as possible
table(default10yr[test_dataset], predictions)

#print accuracy of logistic regression model
mean(default10yr[test_dataset]==predictions)
#model is 94% accurate to predict if client will default






