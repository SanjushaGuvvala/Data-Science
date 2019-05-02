# Library packages used


# ---------------------------------------------------------
# Step 1 : Import the dataset retailData_sample.csv

retailData <- read.csv("assign3_RetailData.csv", header = TRUE)
retailData[retailData=="unknown"] <- NA

# ---------------------------------------------------------
# Step 2 : Perform some basic Explorations

# Dimension of the dataset
dim(retailData)     

# Labels of the columns/variables
names(retailData)

# Structure of the dataset
str(retailData)

# Observing first few rows of the data
head(retailData)   

# Summary statistics for all variables
summary(retailData)  

# ---------------------------------------------------------
# Step 3 : Let's analyze the missing values in this dataset
#is.na(retailData))

# sum and the percentage of missing values in dataset
sum(is.na(retailData))
mean(is.na(retailData))

# omit records with missing values
retailData <- na.omit(retailData)


# Removing stock codes that are non-products
retailData<-retailData[!(retailData$StockCode=="AMAZONFEE" | retailData$StockCode=="B" |
                           retailData$StockCode=="BANKCHARGES" | retailData$StockCode=="C2" | 
                           retailData$StockCode=="CRUK" | retailData$StockCode=="D" | 
                           retailData$StockCode=="DOT" | retailData$StockCode=="M" |
                           retailData$StockCode=="POST" | retailData$StockCode=="S"),]

# Recheck dimension and summary of statistics
dim(retailData)      # dimension of the dataset
summary(retailData) # summary of the dataset

# ---------------------------------------------------------
# Step 4 : create matrix with values of Quantity
retailData$CustomerID <- as.factor(retailData$CustomerID)

Quantity_matrix <- xtabs(Quantity ~ CustomerID + StockCode, data = retailData, addNA = TRUE, sparse = TRUE)

# ---------------------------------------------------------
# Step 5 : identify clusters in their customer base & choosing the right distance of clustering

# Single choice for random initial centroids
K <- 20    # Experiment with different values
kMeansFit <- kmeans(Quantity_matrix, centers = K)
kMeansFit
str(kMeansFit)

# customerID dataframe
customerDF <- data.frame(unique(retailData$CustomerID))
names(customerDF) <- c("CustomerID")
customerDF <- data.frame(customerDF[order(customerDF$CustomerID),])
names(customerDF) <- c("CustomerID")
head(customerDF)

# Save the cluster number in the dataset as column 'Cluster'
customerDF$cluster <- as.factor(kMeansFit$cluster)
head(customerDF)

clustDF <- merge(retailData, customerDF, by = "CustomerID", all.x = TRUE)
head(clustDF)

str(clustDF)

cols <- c("darkblue","red")
counts <- table(customerDF$cluster)
barplot(counts,col=cols,legend = rownames(counts), main = "Cluster")

# ---------------------------------------------------------
# Step 7: Perform Expectation-Maximisation (EM) Clustering Algorithm

# install.packages("mclust")
library(mclust)
emFit <- Mclust(Quantity_matrix)
summary(emFit)
plot(emFit, what = "classification")
emFit$parameters

# customerID dataframe
customerDF <- data.frame(unique(retailData$CustomerID))
names(customerDF) <- c("CustomerID")
customerDF <- data.frame(customerDF[order(customerDF$CustomerID),])
names(customerDF) <- c("CustomerID")
head(customerDF)

# Save the cluster number in the dataset as column 'Cluster'
customerDF$cluster <- as.factor(emFit$cluster)
head(customerDF)

clustDF <- merge(retailData, customerDF, by = "CustomerID", all.x = TRUE)
head(clustDF)
tail(clustDF)

# -------------------------------------------------------
# Task 8: Perform Hierarchical Clustering on the dataset

hiercFit <- hclust(dist(Quantity_matrix, method = "euclidean"),
                   method="ward.D")   # vary the linkage
# mention and explain which method is used and why.
# method =  ward.D, single (gives outliers fast), complete, average
plot(hiercFit)
K <- 4
kMeansFit$cluster
rect.hclust(hiercFit, k = K)

pairs(Quantity_matrix, pch = 19,
      col = palette(myPal)[as.numeric(cutree(hiercFit, k = K))])
cutree(hiercFit, k = K)
