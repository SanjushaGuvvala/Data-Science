# =======================================================
# Prediction : Building a Model for Linear Regression
# =======================================================

# Task 1 : Perform a detailed exploratory data analysis and regression model building based on assign1_EnergyData.csv
# Steps : Parts of code for Task 1
# Dataset used : assign1_EnergyData.csv

# ---------------------------------------------------------
# Library
library(DataExplorer)
# ---------------------------------------------------------

# ---------------------------------------------------------
# Step 1 : Import the dataset 
energydata <- read.csv("C:/Users/Sanjusha/Desktop/CZ4073/Assignment1/assign1_EnergyData.csv", header = TRUE)

# ---------------------------------------------------------
# Step 2 : Perform some basic Explorations

# 2a : dimension of the dataset
dim(energydata)

# 2b : labels of the columns/variables
names(energydata)

# 2c : structure of the dataset
str(energydata)

# 2d : Observing first few rows of the data
head(energydata)

# 2e : Observing last few rows of the data
tail(energydata)

# 2f : Summary statistics for all variables
summary(energydata)


# ---------------------------------------------------------
# Step 3 : View some Exploratory Plots

# 3a : histograms of all individual variables
#skewed or evenly distributed?
plot_histogram(energydata)

# 3b : boxplots of all individual variables
#quick observations outliers
plot_boxplot(energydata,by='HeatingLoad')
boxplot(energydata$Compactness, horizontal = TRUE, col = "lightgreen")
boxplot(energydata$SurfaceArea, horizontal = TRUE, col = "green")
boxplot(energydata$WallArea, horizontal = TRUE, col = "red")
boxplot(energydata$RoofArea,horizontal = TRUE, col = "lightblue")
boxplot(energydata$Height,horizontal = TRUE, col = "darkred")
boxplot(energydata$GlazingArea,horizontal = TRUE, col = "pink")
boxplot(energydata$GAreaDist,horizontal = TRUE, col = "gray")
boxplot(energydata$HeatingLoad, horizontal = TRUE,col = "blue")
boxplot(energydata$CoolingLoad,horizontal = TRUE, col = "steelblue")

# Is it meaningful to do the following?
boxplot(energydata$Orientation,horizontal = TRUE, col = "orange")


# ---------------------------------------------------------
# Step 4 : Find the correlations between variables
cor(energydata)
library(corrplot)
corrplot.mixed(cor(energydata))

# What is your intuitive inference from the correlation?
# The correlation plot shows that:
# Variables Height, WallArea and Compactness have a postive correlation with Heating Load and Cooling Load

# ---------------------------------------------------------
# Step 5 : Plot 2d scatterplots of all pairs of variables

energydata$Orientation<-as.numeric(energydata$Orientation)
pairs(energydata, pch = 19, col = "blue")
pairs(energydata[,-11], pch = 19, col="red")

#install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
chart.Correlation(energydata, histogram=TRUE, pch=19)

# What is your intuitive inference from the plot?
#. A high correlation value may be the result of chance and not significant
#. A low correlation value may be significant, a small but still meaningful relationship 


# ---------------------------------------------------------
# Step 6 : Perform Linear Regression on HeatingLoad

# 6a : fit a linear model on HeatingLoad vs all other variables 
#-- backward method consider all variables and remove variables one by one
# This is the first model --- also called the FULL MODEL
# As the prediction is for HeatingLoad and CoolingLoad separately, exclude CoolingLoad variable from the HeatingLoad model
lmFit1 <- lm(HeatingLoad~ .-CoolingLoad, data = energydata)

# 6b : display the "summary" of the linear model
summary(lmFit1)

# 6c : what is the residual standard error of the model?
# Residual standard error: 2.966 on 692 degrees of freedom

# 6d : what is the Multiple R-squared of the model?
# Multiple R-squared:  0.9137

# 6e : what is the Adjusted R-squared of the model?
# Adjusted R-squared:  0.9129 

# 6f : why is Adjusted R-Squared less than Multiple R-squared?
# Because Adjusted R-squared adjusts Multiple R-squared with
# the number of parameters used in the model, as follows:
# Adj R^2 = 1 - (1- R^2)*(n-1)/(n-p-1)

# 6g : which variable to remove from linear model? why?
#    This line "Coefficients: (1 not defined because of singularities)" indicates that
#    R can't estimate all the parameters because the predictor variables are not all linearly independent.
#    As observed from the p values of all variables, the variable RoofArea p value is NA, this means that the varianble is linearly dependent on the other 8 variables. R's lm function (and all properly constructed R regression functions as well) will automatically exclude linearly dependent variables for you.
# Therefore, remove RoofArea variables first to build the model for HeatingLoad


# ---------------------------------------------------------
# Step 7 : Build an Updated Model removing Variables

# 7a : fit a linear model with all variables but the
# Remove CoolingLoad variable from the model
lmFit2 <- update(lmFit1, ~ . - RoofArea, data = energydata)

# 7b : display the "summary" of the updated linear model
summary(lmFit2) 

# 7c : did the Multiple R-squared improve? why?
# No. It can't improve as we removed a variable.
# Multiple R-squared:  0.9137

# 7d : did the Adjusted R-squared improve? why?
# No. Because we removed a variable that will have no significance in the model.
# Adjusted R-squared:  0.9129

# Now, consider the least significant variable to remove
# 7e : which variable do you think is least significant? why?
# It seems that Orientation is least significant (p-value = 0.861144)

# ---------------------------------------------------------
# Step 8 : Build an Updated Model removing Variables

# 8a : fit a linear model with all remaining variables but
# the ones with the least significance in the previous model
lmFit3 <- update(lmFit2, ~ . - Orientation, data = energydata)

# 8b : display the "summary" of the updated linear model
summary(lmFit3)

# 8c : did the Multiple R-squared improve? why?
# No. It can't improve as we removed a variable.
# Multiple R-squared:  0.9137

# 8d : did the Adjusted R-squared improve? why?
# Yes. Because we removed a less significant variable.
# Adjusted R-squared:  0.913

# 8e : which variable do you think is least significant? why?
# It seems that GAreaDist is least significant (p-value = 0.000823)

# ---------------------------------------------------------
# Step 9 : Build an Updated Model removing Variables

# 9a : fit a linear model with all the remaining variables but
# the ones with the least significance in the previous model
lmFit4 <- update(lmFit3, ~ . - GAreaDist, data = energydata)

# 9b : display the "summary" of the updated linear model
summary(lmFit4)

# 9c : did the Multiple R-squared improve? why?
# No. It can't improve as we removed a variable.
# Multiple R-squared:  0.9123

# 9d : did the Adjusted R-squared improve? why?
# No. It slightly decreased.
# Adjusted R-squared:  0.9117
# But we did not lose by much, and by removing
# GAreaDist, we got all the remaining variables highly significant,
# and the F-statistic improved compared to the previous model.

# ---------------------------------------------------------
# Step 10 : Judge the overall status of the Model

# 10a : do you think more variables should be removed? why?
# No, it seems that all remaining variables are  highly significant.

# ---------------------------------------------------------
# Step 11 : Check for Non-linear Relations with Variables

# 11a : plot HeatingLoad vs all the remaining variables
plot(energydata$Compactness, energydata$HeatingLoad, pch = 19, col = "blue")
plot(energydata$SurfaceArea, energydata$HeatingLoad, pch = 19, col = "blue")
plot(energydata$WallArea, energydata$HeatingLoad, pch = 19, col = "blue")
plot(energydata$Height, energydata$HeatingLoad, pch = 19, col = "blue")
plot(energydata$GlazingArea, energydata$HeatingLoad, pch = 19, col = "blue")

# 11b : do you see prominent non-linearity in any plot?
# Yes. It seems that GlazingArea and HeatingLoad have a non-linear relation.

# 11c : fit a linear model on HeatingLoad vs remaining variables but
# introduce non-linear term(s) as per your observation above
lmFit5 <- update(lmFit4, ~ . + I(GlazingArea^2), data = energydata)

# 11d : did the R-squared values improve, as you expected?
summary(lmFit5)
##########################################################
# Yes, they did! In fact, it is quite a lot of improvement.
##########################################################
# 11e : is the non-linear term you introduced significant?
# Yes, the term I(GlazingArea^2) is significant (p-value = 1.48e-09).

# 11f : do you think more non-linear terms should be included?
# May be. It seems that we still have SurfaceArea, with non-linearity.

# 11g : fit a linear model on HeatingLoad vs remaining variables but
# introduce non-linear term(s) as per your observation above
lmFit6 <- update(lmFit5, ~ . + I(SurfaceArea^2), data = energydata)

# 11h : did the R-squared values improve, as you expected?
summary(lmFit6)
# Yes, they did! In fact, it is quite a lot of improvement.

# 11i : is the non-linear term you introduced significant?
# Yes, the term I(SurfaceArea^2) is significant (p-value < 2e-16).

# 11j : do you think more non-linear terms should be included?
# May be. It seems that we still have Compactness, with non-linearity.

# 11k : fit a linear model on HeatingLoad vs remaining variables but
# introduce non-linear term(s) as per your observation above
lmFit7 <- update(lmFit6, ~ . + I(Compactness^2), data = energydata)

# 11l : did the R-squared values improve, as you expected?
summary(lmFit7)
# Yes, they did improve the Multiple R-squared and Adjusted R-squared values

# 11m : are all the remaining variables still significant?
# No, it leaves some of the remaining variables as less significant.
# Hence go back to best model so far which is model lmFit6
summary(lmFit6)
# ---------------------------------------------------------
# Step 12 : Check for Non-linear mutual Interactions

# 12a : fit a linear model on HeatingLoad vs remaining variables but
# introduce a non-linear interaction term between the variables
# that showed prominent trends of non-linearity previously
lmFit8 <- update(lmFit6, ~ . + GlazingArea:SurfaceArea, data = energydata)

# 12b : did the R-squared values improve, as you expected?
summary(lmFit8)
# Yes, they did slightly improve compared to previous model lmFit6.

# 12c : are all the remaining variables still significant?
# Yes, all remaining variables remain significant, GlazingArea:SurfaceArea is significant (p-value = 3.09e-13)


# ---------------------------------------------------------
# Step 13 : Check the (so far) Best Model more carefully

# 13a : print the "summary" of your Best Model (so far)
summary(lmFit8)

# 13b : check the model for potential outliers
plot(lmFit8)

# 13c : remove outliers and high-leverage points
cd <- cooks.distance(lmFit8)

#only selecting rows with less than 4 outliers
energydata.clean <- energydata[abs(cd) < 4/nrow(energydata), ]
nrow(energydata.clean)

# 13d : fit your best model to the clean dataset
formula(lmFit8)
lmFit <- lm(formula(lmFit8), data = energydata.clean)

# 13e : did the performance of the model improve?
summary(lmFit)
plot(lmFit)

# Of course it did, in fact, quite significantly!
#multiple R squared and adjusted R squared are close then the model is good

# ------------------------------------------------------------
# REPEATING STEPS 6 TO 13 for Linear Regression on CoolingLoad
# ------------------------------------------------------------
# Step 6 : Perform Linear Regression on CoolingLoad

# 6a : fit a linear model on CoolingLoad vs all other variables 
#-- backward method consider all variables and remove variables one by one
# This is the first model --- also called the FULL MODEL
# As the prediction is for CoolingLoad and HeatingLoad separately, exclude HeatingLoad variable from the CoolingLoad model
lmFit1_c <- lm(CoolingLoad~ .-HeatingLoad, data = energydata)

# 6b : display the "summary" of the linear model
summary(lmFit1_c)

# 6c : what is the residual standard error of the model?
# Residual standard error: 3.212 on 692 degrees of freedom

# 6d : what is the Multiple R-squared of the model?
# Multiple R-squared:  0.8863

# 6e : what is the Adjusted R-squared of the model?
# Adjusted R-squared:  0.8852 

# 6f : why is Adjusted R-Squared less than Multiple R-squared?
# Because Adjusted R-squared adjusts Multiple R-squared with
# the number of parameters used in the model, as follows:
# Adj R^2 = 1 - (1- R^2)*(n-1)/(n-p-1)

# 6g : which variable to remove from linear model? why?
#    This line "Coefficients: (1 not defined because of singularities)" indicates that
#    R can't estimate all the parameters because the predictor variables are not all linearly independent.
#    As observed from the p values of all variables, the variable RoofArea p value is NA, this means that the varianble is linearly dependent on the other 8 variables. R's lm function (and all properly constructed R regression functions as well) will automatically exclude linearly dependent variables for you.
# Therefore, remove RoofArea variables first to build the model for CoolingLoad


# ---------------------------------------------------------
# Step 7 : Build an Updated Model removing Variables

# 7a : fit a linear model with all variables but the
# Remove HeatingLoad variable from the model
lmFit2_c <- update(lmFit1_c, ~ . - RoofArea, data = energydata)

# 7b : display the "summary" of the updated linear model
summary(lmFit2_c) 

# 7c : did the Multiple R-squared improve? why?
# No. It can't improve as we removed a variable.
# Multiple R-squared:  0.8863

# 7d : did the Adjusted R-squared improve? why?
# No. Because we removed a variable that will have no significance in the model.
# Adjusted R-squared:  0.8852

# Now, consider the least significant variable to remove
# 7e : which variable do you think is least significant? why?
# It seems that Orientation is least significant (p-value = 0.376)

# ---------------------------------------------------------
# Step 8 : Build an Updated Model removing Variables

# 8a : fit a linear model with all remaining variables but
# the ones with the least significance in the previous model
lmFit3_c <- update(lmFit2_c, ~ . - Orientation, data = energydata)

# 8b : display the "summary" of the updated linear model
summary(lmFit3_c)

# 8c : did the Multiple R-squared improve? why?
# No. It can't improve as we removed a variable.
# Multiple R-squared:  0.8862

# 8d : did the Adjusted R-squared improve? why?
# No change observed after the removal of the least significant variable.
# Adjusted R-squared:  0.8852

# 8e : which variable do you think is least significant? why?
# It seems that GAreaDist is least significant (p-value = 0.341)

# ---------------------------------------------------------
# Step 9 : Build an Updated Model removing Variables

# 9a : fit a linear model with all the remaining variables but
# the ones with the least significance in the previous model
lmFit4_c <- update(lmFit3_c, ~ . - GAreaDist, data = energydata)

# 9b : display the "summary" of the updated linear model
summary(lmFit4_c)

# 9c : did the Multiple R-squared improve? why?
# No. It can't improve as we removed a variable.
# Multiple R-squared:  0.886

# 9d : did the Adjusted R-squared improve? why?
# Still no change observed in Adjusted R-squared value.
# Adjusted R-squared:  0.8852
# But by removing GAreaDist, we got all the remaining variables highly significant,
# and the F-statistic improved compared to the previous model.

# ---------------------------------------------------------
# Step 10 : Judge the overall status of the Model

# 10a : do you think more variables should be removed? why?
# No, it seems that all remaining variables are  highly significant.

# ---------------------------------------------------------
# Step 11 : Check for Non-linear Relations with Variables

# 11a : plot CoolingLoad vs all the remaining variables
plot(energydata$Compactness, energydata$CoolingLoad, pch = 19, col = "blue")
plot(energydata$SurfaceArea, energydata$CoolingLoad, pch = 19, col = "blue")
plot(energydata$WallArea, energydata$CoolingLoad, pch = 19, col = "blue")
plot(energydata$Height, energydata$CoolingLoad, pch = 19, col = "blue")
plot(energydata$GlazingArea, energydata$CoolingLoad, pch = 19, col = "blue")

# 11b : do you see prominent non-linearity in any plot?
# Yes. It seems that GlazingArea and CoolingLoad have a non-linear relation.

# 11c : fit a linear model on CoolingLoad vs remaining variables but
# introduce non-linear term(s) as per your observation above
lmFit5_c <- update(lmFit4_c, ~ . + I(GlazingArea^2), data = energydata)

# 11d : did the R-squared values improve, as you expected?
summary(lmFit5_c)
##########################################################
# Yes, they only improved very slightly.
##########################################################
# 11e : is the non-linear term you introduced significant?
# No, the term I(GlazingArea^2) is not significant (p-value = 0.0124).
# So go back to the best model so far lmFit4_c
summary(lmFit4_c)
# 11f : do you think they are other non-linear terms should be included?
# May be. It seems that we still have SurfaceArea, with non-linearity.

# 11g : fit a linear model on CoolingLoad vs remaining variables but
# introduce non-linear term(s) as per your observation above
lmFit6_c <- update(lmFit4_c, ~ . + I(SurfaceArea^2), data = energydata)

# 11h : did the R-squared values improve, as you expected?
summary(lmFit6_c)
# Yes, the Multiple R-squared and Adjusted R-squared values  did improve.

# 11i : is the non-linear term you introduced significant?
# Yes, the term I(SurfaceArea^2) is significant (p-value < 2e-16).
# However, it makes the WallArea variable to be less significant (p-value = 0.0164)
# So we go back to the best model so far lmFit4_c
summary(lmFit4_c)
# 11j : do you think more non-linear terms should be included?
# It seems that we still have Compactness, with non-linearity.

# 11k : fit a linear model on CoolingLoad vs remaining variables but
# introduce non-linear term(s) as per your observation above
lmFit7_c <- update(lmFit4_c, ~ . + I(Compactness^2), data = energydata)

# 11l : did the R-squared values improve, as you expected?
summary(lmFit7_c)
# Yes, the Multiple R-squared and Adjusted R-squared values did improve slightly 

# 11m : are all the remaining variables still significant?
# No, it leaves some of the remaining variables as less significant.
# Hence go back to best model so far which is model lmFit4_c
summary(lmFit4_c)

# 11n : do you think more non-linear terms should be included?
# Maybe, it seems that we still have WallArea, with non-linearity.

# 11o : fit a linear model on CoolingLoad vs remaining variables but
# introduce non-linear term(s) as per your observation above
lmFit8_c <- update(lmFit4_c, ~ . + I(WallArea^2), data = energydata)

# 11p : did the R-squared values improve, as you expected?
summary(lmFit8_c)
# Yes, the Multiple R-squared and Adjusted R-squared values did improve more than the previous non-linear varible 

# 11q : are all the remaining variables still significant?
# Yes, it leaves all of the remaining variables as highly significant.
# Hence, the best model so far is model lmFit7_c


# ---------------------------------------------------------
# Step 12 : Check for Non-linear mutual Interactions

# 12a : fit a linear model on CoolingLoad vs remaining variables but
# introduce a non-linear interaction term between the variables
# that showed prominent trends of non-linearity previously
lmFit9_c <- update(lmFit8_c, ~ . + WallArea:SurfaceArea, data = energydata)

# 12b : did the R-squared values improve, as you expected?
summary(lmFit9_c)
# Yes, they did improve significantly

# 12c : are all the remaining variables still significant?
# Yes, all remaining variables remain significant, wallArea:SurfaceArea is significant (p-value < 2e-16)


# ---------------------------------------------------------
# Step 13 : Check the (so far) Best Model more carefully

# 13a : print the "summary" of your Best Model (so far)
summary(lmFit9_c)

# 13b : check the model for potential outliers
plot(lmFit9_c)

# 13c : remove outliers and high-leverage points
cd <- cooks.distance(lmFit9_c)
#only selecting rows with less than 4 outliers
energydata.clean <- energydata[abs(cd) < 4/nrow(energydata), ]
nrow(energydata.clean)

# 13d : fit your best model to the clean dataset
formula(lmFit9_c)
lmFit_c <- lm(formula(lmFit9_c), data = energydata.clean)

# 13e : did the performance of the model improve?
summary(lmFit_c)
plot(lmFit_c)

# Of course it did, in fact, quite significantly!
#multiple R squared and adjusted R squared are close then the model is good

# ---------------------------------------------------------
# Step 14:  Predict the two response variables, Heating Load and Cooling Load, for the samples in assign1_EnergyPred.csv
# 14a : Loading test dataset
testdata <- read.csv("C:/Users/Sanjusha/Desktop/CZ4073/Assignment1/assign1_EnergyPred.csv", header = TRUE)

# 14b : Predicting for HeatingLoad
pred_h <- predict.lm(lmFit, testdata,interval='prediction',level=0.95)
pred_h[1:10]
testdata_h <- testdata
testdata_h$HeatingLoad<-pred_h[1:10]
testdata_h

# 14c : Predicting for CoolingLoad
pred_c <- predict.lm(lmFit_c, testdata,interval='prediction',level=0.95)
pred_c[1:10]
testdata_c <- testdata
testdata_c$CoolingLoad<-pred_c[1:10]
testdata_c

