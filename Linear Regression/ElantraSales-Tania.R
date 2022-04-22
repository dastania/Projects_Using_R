#Calling the required library
library(boot) 
library(car)
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(MASS)
library(caTools)
library(dplyr)

#Setting the woorking directory
setwd("C:/Users/TANIA DAS/Downloads")

#Reding the csv file
Elantra<-read.csv("elantra.csv")

#Exploratory Data Analysis
colnames(Elantra)
dim(Elantra)
str(Elantra)
head(Elantra)
summary(Elantra)

#Checking the missing values
colSums(is.na(Elantra))

#Making seperate data frames for train and test data
Train <- subset(Elantra, Year < 2013)  # training set containing observations for 2012 and before
Test <- subset(Elantra, Year > 2012) # training set containing observations after 2012 
print("Dimension of the Training set is")
dim(Train)
print("Dimension of the Testing set is")
dim(Test)

model <- lm(ElantraSales ~ Unemployment + Queries+ CPI_energy+ CPI_all, data = Train)
summary(model)

# New linear regression model using additional varible "Month"
model1 <- lm(ElantraSales ~Unemployment + Month + Queries+ CPI_energy+ CPI_all, data = Train)
summary(model1)

# Converting Month into factors
Months <- as.factor(Elantra$Month)
Elantra$Months <- as.factor(Elantra$Month)
str(Elantra)
ElantraTrain <- subset(Elantra, Year < 2013)  
ElantraTest <- subset(Elantra, Year > 2012)

# Linear regression model using Monmths(factor) variable
model2 <- lm(ElantraSales ~ Unemployment + Months + Queries+ CPI_energy+ CPI_all, data = ElantraTrain)
summary(model2)

cor(select(ElantraTrain,c("Unemployment","Month","Queries","CPI_energy","CPI_all")))

# Reducing the model
model3 <- lm(ElantraSales ~Unemployment + Months + CPI_energy+ CPI_all, data = ElantraTrain) # removing "Queries" from the Salesmod2 model
summary(model3)

Sales <- predict(model3, newdata = ElantraTest) # predicting on test set
SSE <- sum((ElantraTest$ElantraSales - Sales)^2) 
print("Sum of sqaured error of the model on the test set is")
SSE

# we can compute SST as the sum of the squared differences 
# between ElantraSales in the testing set and the mean of ElantraSales in the training set
SST <- sum((mean(ElantraTrain$ElantraSales) - ElantraTest$ElantraSales)^2)
# R-Squared can be comuted using (1-SSE/SST)
R2 <- 1-(SSE/SST)
print(" R-squared of the model on test set is")
R2

print("The largest error in predicted sales and actual sales is")
max(abs(ElantraTest$ElantraSales - Sales)) # the largest absolute error
print("The month with the largest absolute error is ")
ElantraTest$Month[which.max(abs(ElantraTest$ElantraSales - Sales))]  # Month with the largest absolute error
