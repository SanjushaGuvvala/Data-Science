# =======================================================
# Assignment 2: Perform Exploratory Analysis, Model Building, Prediction and Interpretation
# =======================================================

# Steps : Parts of code for Task 1
# Dataset used : assign2_MarketingData.csv

# ---------------------------------------------------------
# Libraries used
# install(DataExplorer)
# install.packages("VIM")
#install.packages("mice")
#install.packages("caret")
# install.packages("tree")
# install.packages("randomForest")
install.packages("party")
# install.packages("ROCR")
library(DataExplorer)
library(VIM)
library(mice)
library(caret)
library(tree)
library(randomForest)
library(party)
library(ROCR)
# ---------------------------------------------------------

# =======================================================
# Detailed exploratory analysis of Marketing data
# =======================================================

# ---------------------------------------------------------
# Step 1 : Import the dataset assign2_MarketingData.csv
market_data <- read.csv("assign2_MarketingData.csv", header = TRUE)
market_data[market_data=="unknown"] <- NA

# ---------------------------------------------------------
# Step 2 : Perform some basic Explorations

# 2a : dimension of the dataset
dim(market_data)     

# 2b : labels of the columns/variables
names(market_data)

# Description of variables is as follows
#The following table summarizes the variables in the dataset. Check the data description carefully.

#| Variable | Description | Type | Numeric / Categorical |
#  | ----------- | ------------------------------------------- | ------------ |
#  | Age | Age of the person | Personal Profile | Numeric |
#  | Job | Job Profile of the person | Personal Profile | Categorical |
#  | Marital | Marital Status of the person | Personal Profile | Categorical |
#  | Education	| Highest Educational Qualification of the person | Personal Profile | Categorical |
#  | Default | Does the person have Credit in Default? | Financial Profile | Categorical |
#  | Housing	| Does the person have a Housing Loan? | Financial Profile | Categorical |
#  | Loan | Does the person have a Personal Loan? | Financial Profile | Categorical |
#  | Contact | Type of Contact Number for Communication | Personal Profile | Categorical |
#  | Month | Month of Year for the last contact with the person | Campaign Data | Categorical |
#  | Day | Day of Week for the last contact with the person | Campaign Data | Categorical |
#  | Duration | The duration of contact during the Last Contact | Campaign Data | Numeric |
#  | Campaign | Number of contacts with the person during this Campaign | Campaign Data | Numeric | 
#  | PrevDays | Number of days from the last contact during the Previous Campaign | Campaign Data | Numeric |
#  | Previous | Number of contacts with the person before this Campaign | Campaign Data | Numeric | 
#  | PrevOutcome | Outcome of previous Marketing Campaign for the person | Campaign Data | Categorical | 
#  | EmpVarRate | Employment Variation Rate in the person's locality | Demographic Info | Numeric |
#| ConPriceIndex | Consumer Price Index (monthly) in the person's locality#  | Demographic Info | Numeric |
#  | ConConfIndex | Consumer Confidence Index (monthly) in the person's locality | Demographic Info | Numeric |
#| EuriborRate | Euro Interbank Offered Rate (Euribor) for 3 months | Demographic Info | Numeric |
#| NumEmp | Number of Employees (quarterly) in the person's locality | Demographic Info | Numeric |
#  | Subscribed | Whether the person has Subscribed to a Term Deposit | Response/Target | Binary("yes"/"no") |
  
# Value `unknown` in any categorical variable means the data is not available. `Duration = 0` means no communication during the last contact, or may be that there was *no* previous contact at all. `PrevDays = 999` means there was no previous contact with the person. The goal is to predict the response variable `Subscribed` for each person -- that is, whether a person would subscribe to the *Term Deposit* being sold by the Bank.

# 2c : structure of the dataset
str(market_data)

# 2d : first few rows of the data
head(market_data)     

# 2e : look at a random sample rows of the data to get the idea of the random distribution of data
sample_index = sample(1:nrow(market_data),20,replace=FALSE)
market_data[sample_index,]

# 2f : summary statistics for all variables
summary(market_data)  

# ---------------------------------------------------------
# Step 3 : Let's analyze the missing values in this dataset
aggr_plot <- aggr(market_data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(market_data), 
                  cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# 3a: Check distribution of target variable, it is observed that there isn't any data imbalance
cols <- c("darkblue","red")
counts <- table(market_data$Subscribed)
barplot(counts,col=cols,legend = rownames(counts), main = "Term Deposit")

# 3b : So for this dataset we face only the missing value problem, which can be solved by imputation method We will impute these missing values using MICE package
market_data_imp<-mice(market_data)
mktData<-complete(market_data_imp)

# 3c : check again if missing values exist
aggr_plot <- aggr(mktData, col=cols, numbers=TRUE, sortVars=TRUE, labels=names(market_data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
# No missing values observed
# Checking the dimensions and summary after cleaning
dim(mktData)      # dimension of the dataset
summary(mktData) # summary of the dataset
# 25942/30000 = 86.47% of the time outcome of previous marketing campaign is unknown.
#Duration seems to have lot more variation, it may be a good predictor
#Data is very imbalanced, only 11.32% yes in outcome.


# ---------------------------------------------------------
# Step 4 : Do some Exploratory Analysis

# 4a : plot all individual variables versus the Subscribed

# Basic visualization for continuous variables

plot(mktData$Subscribed, mktData$Age, xlab="Subscribed",
     ylab="Age", pch = 19, col = cols[as.numeric(mktData$Age)])
# Age variable have the same median for both "no" and "yes" of Subscribed variable.

plot(mktData$Subscribed, mktData$Duration, xlab="Subscribed",
     ylab="Duration", pch = 19, col = cols[as.numeric(mktData$Duration)])
# Higher median duration for instances of Subscribed "yes" compared to "no"

plot(mktData$Subscribed, mktData$Campaign, xlab="Subscribed",
     ylab="Campaign", pch = 19, col = cols[as.numeric(mktData$Campaign)])
# Campaign variable have the same median for both "no" and "yes" of Subscribed variable.

plot(mktData$Subscribed, mktData$PrevDays, xlab="Subscribed",
     ylab="PrevDays", pch = 19, col = cols[as.numeric(mktData$PrevDays)])
# PrevDays variable have the same median for both "no" and "yes" of Subscribed variable.

plot(mktData$Subscribed, mktData$Previous, xlab="Subscribed",
     ylab="Previous", pch = 19, col = cols[as.numeric(mktData$Previous)])
# Previous variable have the same median for both "no" and "yes" of Subscribed variable.

plot(mktData$EmpVarRate, mktData$Subscribed, ylab="Subscribed", xlab="EmpVarRate")
# EmpVarRate variables dont seem to provide a clear path to distuinguish the Subscribed variable at the first sight

plot(mktData$Subscribed, mktData$ConPriceIndex, xlab="Subscribed",
     ylab="ConPriceIndex", pch = 19, col = cols[as.numeric(mktData$ConPriceIndex)])
# Higher median ConPriceIndex for instances of Subscribed "no" compared to "yes"

plot(mktData$Subscribed, mktData$ConConfIndex, xlab="Subscribed",
     ylab="ConConfIndex", pch = 19, col = cols[as.numeric(mktData$ConConfIndex)])
# Slightly higher median ConConfIndex for instances of Subscribed "yes" compared to "no"

plot(mktData$Subscribed, mktData$EuriborRate, xlab="Subscribed",
     ylab="EuriborRate", pch = 19, col = cols[as.numeric(mktData$EuriborRate)])
# Relatively higher median EuriborRate for instances of Subscribed "no" compared to "yes"

plot(mktData$Subscribed, mktData$NumEmp, xlab="Subscribed",
     ylab="NumEmp", pch = 19, col = cols[as.numeric(mktData$NumEmp)])
# Slightly higher median number of employees for the case "no"(~5200) compared to "yes"(~5100) of Subscribed variable


# Basic visualization for categorical variables
plot(mktData$Job, mktData$Subscribed, ylab="Subscribed", xlab="Job")
# The Job categories are namely admin, blue-collar,entrepreneur, housemaid, management, retired, self-employed,services, student, technician,unemployed,unknown
# As observed from the plot, retired and student categories are more likely to say yes to Subscribing the Term Deposit
# The other categories from Job variable are generally have the similar effect on Subscribed variable outcome

plot(mktData$Marital, mktData$Subscribed, ylab="Subscribed", xlab="Marital")
# single people are more likely to Subscribe
# divorced or married people doesnt seem to make any difference in the Subscribed variable

plot(mktData$Education, mktData$Subscribed, ylab="Subscribed", xlab="Education")
# University students are slighly more likely to subscribe compared to other levels. This seems reasonable as they might have started earning from the part time jobs during the university period
# Illiterate people who are smaller in proportion and all of them are not likely to subscribe

plot(mktData$Default, mktData$Subscribed, ylab="Subscribed", xlab="Default")
# Here it can be observed that most of the people did not have Credit in Default before so Default doesnt seem to make any difference in Subscribed variable

plot(mktData$Housing, mktData$Subscribed, ylab="Subscribed", xlab="Housing")
# Housing doesnt seem to make any difference in Subscribed variable

plot(mktData$Loan, mktData$Subscribed, ylab="Subscribed", xlab="Loan")
# Loan doesnt seem to make any difference in Subscribed variable

plot(mktData$Contact, mktData$Subscribed, ylab="Subscribed", xlab="Contact")
# Instances with cellular contact are more likely to subscribe

plot(mktData$Month, mktData$Subscribed, ylab="Subscribed", xlab="Month")
# apr, aug, jul, june, may, nov are less likely to subscibe
# while dec, mar, oct and others are more likely to subscribe

plot(mktData$Day, mktData$Subscribed, ylab="Subscribed", xlab="Day")
# Only slight differences observed, therefore might not be much of difference to Subscribed variable 

plot(mktData$PrevOutcome, mktData$Subscribed, ylab="Subscribed", xlab="PrevOutcome")
# Clearly, people with PrevOutcome being a success are more likely to subscribe again

# Variables like PrevOutcome, Month, Education, Marital, Job seem to be of some significance. 
# It will be determined in the steps ahead.

# Checking the dimensions after cleaning
dim(mktData)      # dimension of the dataset

# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
train <- sample(nrow(mktData), 0.7*nrow(mktData), replace = FALSE)
mktTrain <- mktData[train,]
mktValid <- mktData[-train,]

# Dimension of the new split datasets
dim(mktTrain)
dim(mktValid)


#### Problem 1

#Build an optimal tree-based classification model for `Subscribed` vs the **Personal Profile** and **Financial Profile** of a person. Check the relevant accuracy parameters of your model, and use it to predict `Subscribed` in the `assign2_MarketingPred.csv`. 

#* Identify the top three important variables in this case. 
#* Why are these important? Justify in terms of the Business.
#* How would you influence `Subscribed` using these variables?


# =======================================================
# Basic Decision Trees Model building and prediction
# =======================================================

# ---------------------------------------------------------
# Step 5 : Build a Decision Tree (Subscribed vs the respective variables)

# 5a : fit a decision tree on Subscribed vs Personal Profile and Financial varibles
treeFit <- tree(Subscribed ~ (Age+Job+Marital+Education+Contact+Default+Housing+Loan), data = mktTrain)

# 5b : display the decision tree
plot(treeFit)
text(treeFit, pretty = FALSE, all = TRUE)
plot(treeFit)
text(treeFit, pretty = FALSE)

# 5c : print the complete tree structure
treeFit
# and Output Interpretation

# 5d : view the summary of the tree
summary(treeFit)
# and Output Interpretation

# 5e : what is the "complexity" of the model? How many nodes?
# Number of terminal nodes:  3

# 5f : what is the residual mean deviance of the model?
# "Deviance" measures randomness : Smaller the Better
# Residual mean deviance:  0.6682 = 14030 / 21000 

# 5g : which variable is most significant? why?
# It seems that Contact is most significant (first split)


# ---------------------------------------------------------
# Step 6 : Predict classes using the Decision Tree Model

# Predict using the tree model on training and validation data
predTrainData <- predict(treeFit, mktTrain, type = "class")
predValidData <- predict(treeFit, mktValid, type = "class")

# Classification accuracy on Train data
mean(predTrainData == mktTrain$Subscribed)

# Classification accuracy on Validation data
mean(predValidData == mktValid$Subscribed)

# Predict using the tree model
predData <- predict(treeFit, mktData, type = "class")

# Confusion matrix
table(mktData$Subscribed, predData)

# Classification accuracy
mean(predData == mktData$Subscribed)
# Can't interpret False Positive or False Negative directly
# Every diagonal element is Correct, and non-diagonal Wrong

# what is the rate of misclassification error?
# Misclassification Rate = 1 - Accurate Classification Rate
# Accurate Classification Rate = (26603) / 30000 = 0.8867667
# Misclassification Rate = 1 - 0.8867667 = 0.1132

# Cross-check with the summary of the model (5d)
# Misclassification error rate: 0.1132 = 3397 / 30000

# ---------------------------------------------------------
# Step 7: Tree Model - Performance measures for Prediction


# 7a : Confusion matrix
cm <- table(mktData$Subscribed, predData)
TP <- cm[2,2]  # True Positive (Good predicted as Good)
TN <- cm[1,1]  # True Negative (Bad predicted as Bad)
FP <- cm[1,2]  # False Positive (Bad predicted as Good) -- Type I error
FN <- cm[2,1]  # False Negative (Good predicted as Bad) -- Type II error

# 7b : Classification Accuracy
(TN + TP) / (TN + TP + FN + FP)    # Correct Classification / Total
mean(predData == mktData$Subscribed)   # Classification accuracy (alt.)

# 7c : False Positive Rate (fpr) / Type I error / 1 - Specificity
FP / (TN + FP)      # False Positive / Total Negative

# 7d : True Positive Rate (tpr) / 1 - Type II error / Sensitivity
TP / (TP + FN)      # True Positive / Total Positive

# 7e : Predict probabilities using the tree model
probData <- predict(treeFit, mktData, type = "vector")

#     - Convert probabilities to prediction with threshold 0.5
threshold <- 0.5 

predData <- rep("no", nrow(mktData))
predData[probData[,2] > threshold] = "yes"
predData <- as.factor(predData)

table(mktData$Subscribed, predData)
# No False Negatives and True Negatives observed with threshold 0.5

#     - Convert probabilities to prediction with threshold 0.9
threshold <- 0.9 

predData <- rep("no", nrow(mktData))
predData[probData[,2] > threshold] = "yes"
predData <- as.factor(predData)

table(mktData$Subscribed, predData)
# threshold 0.5 or above have the same number of False Positives and True Positives
# No False Negatives and True Negatives


#     - Convert probabilities to prediction with threshold 0.2
threshold <- 0.2

predData <- rep("no", nrow(mktData))
predData[probData[,2] > threshold] = "yes"
predData <- as.factor(predData)

table(mktData$Subscribed, predData)

#     - Convert probabilities to prediction with threshold 0.4
threshold <- 0.4

predData <- rep("no", nrow(mktData))
predData[probData[,2] > threshold] = "yes"
predData <- as.factor(predData)

table(mktData$Subscribed, predData)
# threshold 0.2 to 0.4 have the same number of False Positives and True Positives
cm <- table(mktData$Subscribed, predData)
cm[1,2] / (cm[1,1] + cm[1,2])      # False Positive Rate (fpr)
cm[2,2] / (cm[2,2] + cm[2,1])      # True Positive Rate (tpr)

#     - Convert probabilities to prediction with threshold 0.1
threshold <- 0.1 


predData <- rep("no", nrow(mktData))
predData[probData[,2] > threshold] = "yes"
predData <- as.factor(predData)

table(mktData$Subscribed, predData)

cm <- table(mktData$Subscribed, predData)
cm[1,2] / (cm[1,1] + cm[1,2])      # False Positive Rate (fpr)
cm[2,2] / (cm[2,2] + cm[2,1])      # True Positive Rate (tpr)

# Higher tpr, however, the fpr is also higher for threshold with 0.1 compared to threshold between 0.2 to 0.4

# -------------------------------------------------------
# Step 9 : Build a Bagging Model on the data set Subscribed vs Personal Profile and Financial varibles

# Each node can split using all variables
# That is, each node checks all variables
# before making the "decision" for split.

rFit <- randomForest(Subscribed ~ (Age+Job+Marital+Education+Contact+Default+Housing+Loan),                       # formula
                       data = mktTrain,                   # data set
                       ntree = 500,                      # number of trees
                       mtry = 3,                         # variables for split
                       importance = TRUE)                # importance recorded
rFit

predTrain <- predict(rFit, mktTrain, type = "class")    # prediction on train set
mean(predTrain == mktTrain$Subscribed)                    # classification accuracy
predValid <- predict(rFit, mktValid, type = "class")    # prediction on validation set
mean(predValid == mktValid$Subscribed)                    # classification accuracy

# Predict using the bagging model
predData <- predict(rFit, mktData, type = "class")

# Confusion matrix
table(mktData$Subscribed, predData)

# Classification accuracy
acc <- mean(predData == mktData$Subscribed)
print(paste('Accuracy',acc))

# mtry can be 3 according to the square root of the total number of variables, 8. 2 was providing a better accuracy on the validation set.
# Increasing the number of trees in the random forest allows to only slightly increase accuracy, so fixing trees to 500 due to only slight increase


# Bagging gives variable importance for free, depending on the splits
importance(rFit)        # importance of the variables in the model (values)
# importance of the variables in the model can be visualised on the graph above
varImpPlot(rFit)        # importance of the variables in the model (visual)
#* Identify the top three important variables in this case. 
# Job, Education & Age proved to be the top variables. Removing these variables could result in a significant decrease in accuracy.
#* Why are these important? Justify in terms of the Business. * How would you influence `Subscribed` using these variables?
# These are basic background information of the customers which clearly say a lot on how they behave
# For example, Job - as mentioned previously in the analysis, the retired and student etc are more likely to subscribe.
# Hence, Agents may target clients of job category of retired and student etc as these set of people look for safe deposit of their savings with fixed returns.

# -------------------------------------------------------
# Step 10 : Use the model to predict `Subscribed` in the `assign2_MarketingPred.csv`.  
mktTest <- read.csv("assign2_MarketingPred.csv", header = TRUE)
levels(mktTest$Age) <- levels(mktTrain$Age)
levels(mktTest$Job) <- levels(mktTrain$Job)
levels(mktTest$Marital) <- levels(mktTrain$Marital)
levels(mktTest$Education) <- levels(mktTrain$Education)
levels(mktTest$Contact) <- levels(mktTrain$Contact)
levels(mktTest$Default) <- levels(mktTrain$Default)
levels(mktTest$Housing) <- levels(mktTrain$Housing)
levels(mktTest$Loan) <- levels(mktTrain$Loan)

predTest <- predict(rFit, mktTest, type = "class")
summary(predTest)
testdata_p1 <- mktTest
testdata_p1$Subscribed<-predTest
head(testdata_p1)

#### Problem 2

#Build an optimal tree-based classification model for `Subscribed` vs **Personal Profile**, **Financial Profile**, and **Campaign Data** corresponding to a person. Check the relevant accuracy parameters of your model, and use it to predict `Subscribed` in the `assign2_MarketingPred.csv`. 

#* Identify the top three important variables in this case. 
#* Why are these important? Justify in terms of the Business.
#* How would you influence `Subscribed` using these variables?


# -------------------------------------------------------
# Step 9 : Build a Bagging Model on the data set

# Each node can split using all variables
# That is, each node checks all variables
# before making the "decision" for split.

rFit <- randomForest(Subscribed ~ (Age+Job+Marital+Education+Contact+Default+Housing+Loan
                                   +Month+Day+Duration+Campaign+PrevDays+Previous+PrevOutcome),                       # formula
                       data = mktTrain,                   # data set
                       ntree = 500,                      # number of trees
                       mtry = 4,                         # variables for split
                       importance = TRUE)                # importance recorded
rFit

predTrain <- predict(rFit, mktTrain, type = "class")    # prediction on train set
mean(predTrain == mktTrain$Subscribed)                    # classification accuracy
predValid <- predict(rFit, mktValid, type = "class")    # prediction on validation set
mean(predValid == mktValid$Subscribed)                    # classification accuracy

# Predict using the bagging model
predData <- predict(rFit, mktData, type = "class")

# Confusion matrix
table(mktData$Subscribed, predData)

# Classification accuracy
acc <- mean(predData == mktData$Subscribed)
print(paste('Accuracy',acc))

# mtry can be 4 according to the square root of the total number of variables, 15. 3 was providing a better accuracy on the validation set.
# Increasing the number of trees in the random forest allows to only slightly increase accuracy, so fixing trees to 500 due to only slight increase
# And definitely a lot more improvement in the classification accuracy, this means that some of the  campaign data are actually important and we will find out more below

# Bagging gives variable importance for free, depending on the splits
importance(rFit)        # importance of the variables in the model (values)
# importance of the variables in the model can be visualised on the graph above
varImpPlot(rFit)        # importance of the variables in the model (visual)
#* Identify the top three important variables in this case. 
# Duration, Month & Age proved to be the top variables. Removing these variables could result in a significant decrease in accuracy.
#* Why are these important? Justify in terms of the Business. * How would you influence `Subscribed` using these variables?
# Duration is something that the bank can control "Duration" has positive effect on people saying "yes". This is because the longer the conversations on the phone, the higher interest the customer will show to the term deposit. The Bank ought to focus on the potential clients who have significant call duration and moreover who have reacted emphatically amid the past campaign.
# The month in which the campaign is launched/carried out could affect Subscription rate as during festive periods, one is more likey to splurge.
# Age definitely is important, the higher the age, the more likely the customers are looking for securing a deposit for times of need.
# Hence banks can likely target the adults from age 30 onwards

# -------------------------------------------------------
# Step 10 : Use the model to predict `Subscribed` in the `assign2_MarketingPred.csv`.  
mktTest <- read.csv("assign2_MarketingPred.csv", header = TRUE)
levels(mktTest$Age) <- levels(mktTrain$Age)
levels(mktTest$Job) <- levels(mktTrain$Job)
levels(mktTest$Marital) <- levels(mktTrain$Marital)
levels(mktTest$Education) <- levels(mktTrain$Education)
levels(mktTest$Contact) <- levels(mktTrain$Contact)
levels(mktTest$Default) <- levels(mktTrain$Default)
levels(mktTest$Housing) <- levels(mktTrain$Housing)
levels(mktTest$Loan) <- levels(mktTrain$Loan)
levels(mktTest$Month) <- levels(mktTrain$Month)
levels(mktTest$Day) <- levels(mktTrain$Day)
levels(mktTest$Duration) <- levels(mktTrain$Duration)
levels(mktTest$Campaign) <- levels(mktTrain$Campaign)
levels(mktTest$PrevDays) <- levels(mktTrain$PrevDays)
levels(mktTest$Previous) <- levels(mktTrain$Previous)
levels(mktTest$PrevOutcome) <- levels(mktTrain$PrevOutcome)

predTest <- predict(rFit, mktTest, type = "class")
summary(predTest)
testdata_p2 <- mktTest
testdata_p2$Subscribed<-predTest
head(testdata_p2)

#### Problem 3

# Build an optimal tree-based classification model for `Subscribed` vs all variables, **Personal Profile**, **Financial Profile**, **Campaign Data**, **Demographic Info** corresponding to a person. Check the relevant accuracy parameters of your model, and use it to predict `Subscribed` in the `assign2_MarketingPred.csv`. 

#* Identify the top three important variables in this case. 
#* Why are these important? Justify in terms of the Business.
#* How would you influence `Subscribed` using these variables?


# -------------------------------------------------------
# Step 9 : Build a Bagging Model on the data set

# Each node can split using all variables
# That is, each node checks all variables
# before making the "decision" for split.

rFit <- randomForest(Subscribed ~ .,                       # formula
                     data = mktTrain,                   # data set
                     ntree = 500,                      # number of trees
                     mtry = 5,                         # variables for split
                     importance = TRUE)                # importance recorded
rFit

predTrain <- predict(rFit, mktTrain, type = "class")    # prediction on train set
mean(predTrain == mktTrain$Subscribed)                    # classification accuracy
predValid <- predict(rFit, mktValid, type = "class")    # prediction on validation set
mean(predValid == mktValid$Subscribed)                    # classification accuracy

# Predict using the bagging model
predData <- predict(rFit, mktData, type = "class")

# Confusion matrix
table(mktData$Subscribed, predData)

# Classification accuracy
acc <- mean(predData == mktData$Subscribed)
print(paste('Accuracy',acc))

# mtry can be 5 according to the square root of the total number of variables, 20. 4 was providing a better accuracy on the validation set.
# Increasing the number of trees in the random forest allows to only slightly increase accuracy, so fixing trees to 500 due to only slight increase
# And definitely a lot more improvement in the classification accuracy, this means that some of the  campaign data are actually important and we will find out more below

# Bagging gives variable importance for free, depending on the splits
importance(rFit)        # importance of the variables in the model (values)
# importance of the variables in the model can be visualised on the graph above
varImpPlot(rFit)        # importance of the variables in the model (visual)
#* Identify the top three important variables in this case. 
# Duration, EuriborRate & NumEmp proved to be the top variables. Removing these variables could result in a significant decrease in accuracy.
#* Why are these important? Justify in terms of the Business. * How would you influence `Subscribed` using these variables?
# Duration still remains as a top variable at this point. 
# Banks also often use the Euribor interest rates as the base rate when setting the interest rates on loans, savings and mortgages.
# When the Euribor interest rates rise or fall (substantially) there is a high likelihood that the interest rates on banking products such as mortgages, savings accounts and loans will also be adjusted. 
# And NumEmp will also have an important effect on Subscribed
# To improve their lead generation banks may hire more people or develop analytics solution, as an alternative, like we discussed here for client selection. This would improve the quality of conversation as agents would be spending more time with selective clients only. Less hiring also implies reduction in cost for the company.

# -------------------------------------------------------
# Step 10 : Use the model to predict `Subscribed` in the `assign2_MarketingPred.csv`.  
mktTest <- read.csv("assign2_MarketingPred.csv", header = TRUE)
levels(mktTest$Age) <- levels(mktTrain$Age)
levels(mktTest$Job) <- levels(mktTrain$Job)
levels(mktTest$Marital) <- levels(mktTrain$Marital)
levels(mktTest$Education) <- levels(mktTrain$Education)
levels(mktTest$Contact) <- levels(mktTrain$Contact)
levels(mktTest$Default) <- levels(mktTrain$Default)
levels(mktTest$Housing) <- levels(mktTrain$Housing)
levels(mktTest$Loan) <- levels(mktTrain$Loan)
levels(mktTest$Month) <- levels(mktTrain$Month)
levels(mktTest$Day) <- levels(mktTrain$Day)
levels(mktTest$Duration) <- levels(mktTrain$Duration)
levels(mktTest$Campaign) <- levels(mktTrain$Campaign)
levels(mktTest$PrevDays) <- levels(mktTrain$PrevDays)
levels(mktTest$Previous) <- levels(mktTrain$Previous)
levels(mktTest$PrevOutcome) <- levels(mktTrain$PrevOutcome)
levels(mktTest$EmpVarRate) <- levels(mktTrain$EmpVarRate)
levels(mktTest$ConPriceIndex) <- levels(mktTrain$ConPriceIndex)
levels(mktTest$ConConfIndex) <- levels(mktTrain$ConConfIndex)
levels(mktTest$EuriborRate) <- levels(mktTrain$EuriborRate)
levels(mktTest$NumEmp) <- levels(mktTrain$NumEmp)

predTest <- predict(rFit, mktTest, type = "class")
summary(predTest)
testdata_p3 <- mktTest
testdata_p3$Subscribed<-predTest
head(testdata_p3)


