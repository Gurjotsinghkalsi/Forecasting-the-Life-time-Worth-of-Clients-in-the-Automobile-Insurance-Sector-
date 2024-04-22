# Importing packages
library(tidyverse) 
library(car) 
library(zoo)
library(lmtest) 
library(dplyr) 
library(stringr)
library(caret)
library(ggplot2) 
library(timeDate)

# Reproduce the same results always
set.seed(123) 

# Reading in the data file
InsuranceData <- read.csv("/Users/gurjotsinghkalsi/Desktop/data.csv", header = TRUE)

head(InsuranceData) # Checking top 6 observations of dataset
tail(InsuranceData) # Checking bottom 6 observations of dataset

# 2. Data Understanding

# Remove Customer ID from data set.
InsuranceData <- InsuranceData[,-c(1)] 

#Cleaning the data
colnames(InsuranceData)
colnames(InsuranceData) <- str_replace_all(colnames(InsuranceData),"[.]","")
colnames(InsuranceData)

# Data Understanding
dim(InsuranceData)
str(InsuranceData)

# Checking null values in each column and storing the value in a data frame na_counts
na_counts <- sapply(InsuranceData, function(y) sum(is.na(y)))
na_counts <- data.frame(na_counts)
na_counts

# Unique Values of each column
sapply(InsuranceData, data.table::uniqueN)

# 3. Exploratory Data Analysis (EDA)

#hist(InsuranceData$CustomerLifetimeValue, col = "#FF5733", xlab = "CLV")
hist(InsuranceData$CustomerLifetimeValue, breaks = (max(InsuranceData$CustomerLifetimeValue) - min(InsuranceData$CustomerLifetimeValue))/100, freq = FALSE, main = "CLV Histogram", xlab = "CLV", border = "#FF5733")

# 3.2. Descriptive Analysis of Monthly Premium Auto(MPA)
range(InsuranceData$MonthlyPremiumAuto)
mean(InsuranceData$MonthlyPremiumAuto)
sd(InsuranceData$MonthlyPremiumAuto)
summary(InsuranceData$MonthlyPremiumAuto)
var(InsuranceData$MonthlyPremiumAuto)
skewness(InsuranceData$MonthlyPremiumAuto)
kurtosis(InsuranceData$MonthlyPremiumAuto)
cor(InsuranceData$MonthlyPremiumAuto,InsuranceData$CustomerLifetimeValue)

#hist(InsuranceData$MonthlyPremiumAuto, col = "#00AFBB", xlab = "Monthly Premium Auto")
hist(InsuranceData$MonthlyPremiumAuto, breaks = (max(InsuranceData$MonthlyPremiumAuto) - min(InsuranceData$MonthlyPremiumAuto))/1, freq = FALSE, main = "Monthly Premium Histogram", xlab = "Monthly Premium", border = "#00AFBB")

plot(x=InsuranceData$MonthlyPremiumAuto, y=InsuranceData$CustomerLifetimeValue, col="#00AFBB", cex=1, xlab="MonthlyPremiumAuto", ylab="CustomerLifetimeValue",
     main="Scatterplot of MPA vs CLV")

range(InsuranceData$TotalClaimAmount)
mean(InsuranceData$TotalClaimAmount)
sd(InsuranceData$TotalClaimAmount)
summary(InsuranceData$TotalClaimAmount)
var(InsuranceData$TotalClaimAmount)
skewness(InsuranceData$TotalClaimAmount)
kurtosis(InsuranceData$TotalClaimAmount) 
cor(InsuranceData$TotalClaimAmount,InsuranceData$CustomerLifetimeValue)

#hist(InsuranceData$TotalClaimAmount, col = "#FC4E07", xlab = "Total Claim Amount")
hist(InsuranceData$TotalClaimAmount, breaks = (max(InsuranceData$TotalClaimAmount) - min(InsuranceData$TotalClaimAmount))/10, freq = FALSE, main = "Total Claim Amount Histogram", xlab = "Total Claim Amount", border = "#FC4E07")

plot(x=InsuranceData$TotalClaimAmount, y=InsuranceData$CustomerLifetimeValue, col="#FC4E07", cex=1, xlab="TotalClaimAmount", ylab="CustomerLifetimeValue",
     main="Scatterplot of TCA vs CLV")

# This means that variation in data is CLV > MPA > TCA

# 3.4 Descriptive Analysis of other variables:

cor(InsuranceData$Income,InsuranceData$CustomerLifetimeValue)
plot(x=InsuranceData$Income, y=InsuranceData$CustomerLifetimeValue, col="#FC4E07", cex=1, xlab="Income", ylab="CustomerLifetimeValue",main="Scatterplot of Income vs CLV")

cor(InsuranceData$MonthsSinceLastClaim,InsuranceData$CustomerLifetimeValue)
plot(x=InsuranceData$MonthsSinceLastClaim, y=InsuranceData$CustomerLifetimeValue, col="#FC4E07", cex=1, xlab="MonthsSinceLastClaim", ylab="CustomerLifetimeValue",main="Scatterplot of MonthsSinceLastClaim vs CLV")

cor(InsuranceData$MonthsSincePolicyInception,InsuranceData$CustomerLifetimeValue)
plot(x=InsuranceData$MonthsSincePolicyInception, y=InsuranceData$CustomerLifetimeValue, col="#FC4E07", cex=1, xlab="MonthsSinceLastClaim", ylab="CustomerLifetimeValue",main="Scatterplot of MonthsSincePolicyInception vs CLV")

cor(InsuranceData$NumberofOpenComplaints,InsuranceData$CustomerLifetimeValue)
plot(x=InsuranceData$NumberofOpenComplaints, y=InsuranceData$CustomerLifetimeValue, col="#FC4E07", cex=1, xlab="NumberofOpenComplaints", ylab="CustomerLifetimeValue",main="Scatterplot of NumberofOpenComplaints vs CLV")

cor(InsuranceData$NumberofPolicies,InsuranceData$CustomerLifetimeValue)
plot(x=InsuranceData$NumberofPolicies, y=InsuranceData$CustomerLifetimeValue, col="#FC4E07", cex=1, xlab="NumberofPolicies", ylab="CustomerLifetimeValue",main="Scatterplot of NumberofPolicies vs CLV")

# 4. Inferential Statistics
  
# 4.01 Effect of Insurance Coverage on Customer Life Time Value (CLV)
  
ggplot(InsuranceData, aes(x=Coverage, y= CustomerLifetimeValue, fill = Coverage)) + 
  geom_boxplot() + 
  labs(x="Coverage",y = "Customer Life Time Value", fill="Coverage") + 
  ggtitle("Visualization of CLV wrt Coverage")

aggData <- aggregate(x = InsuranceData$CustomerLifetimeValue, by=list(Coverage = InsuranceData$Coverage), FUN = sum)
aggData
ggplot(data = aggData, aes(x = Coverage, y = prop.table(stat(aggData$x)), fill = Coverage, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Coverage', y = 'CLV in Percentage', fill = 'Coverage') + 
  ggtitle("CLV Distribution by Coverage")

# 4.02 Effeect of Education on Customer Life Time Value(CLV)

ggplot(InsuranceData, aes(x=Education, y= CustomerLifetimeValue, fill = Education)) + 
  geom_boxplot() + 
  labs(x="Education",y = "Customer Life Time Value", fill="Education") + 
  ggtitle("Visualization of CLV wrt Education")

aggData <- aggregate(x = InsuranceData$CustomerLifetimeValue, by=list(Education = InsuranceData$Education), FUN = sum)

ggplot(data = aggData, aes(x = Education, y = prop.table(stat(aggData$x)), fill = Education, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Education', y = 'CLV in Percentage', fill = 'Education') + 
  ggtitle("CLV Distribution by Education")

# 4.03 Effect of employement status on customer life time value(clv) 

ggplot(InsuranceData, aes(x=EmploymentStatus, y= CustomerLifetimeValue, fill = EmploymentStatus)) + 
  geom_boxplot() + 
  labs(x="Employment Status",y = "Customer Life Time Value", fill="Employment Status") + 
  ggtitle("Visualization of CLV wrt Employment Status")

aggData <- aggregate(x = InsuranceData$CustomerLifetimeValue, by=list(EmploymentStatus = InsuranceData$EmploymentStatus), FUN = sum)

ggplot(data = aggData, aes(x = EmploymentStatus, y = prop.table(stat(aggData$x)), fill = EmploymentStatus, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'EmploymentStatus', y = 'CLV in Percentage', fill = 'EmploymentStatus') + 
  ggtitle("CLV Distribution by EmploymentStatus")

# 4.04 effect of gender on customer life time value (clv)

ggplot(InsuranceData, aes(x=Gender, y= CustomerLifetimeValue, fill = Gender)) + 
  geom_boxplot() + 
  labs(x="Gender",y = "Customer Life Time Value", fill="Gender") + 
  ggtitle("Visualization of CLV wrt Gender")


aggData <- aggregate(x = InsuranceData$CustomerLifetimeValue, by=list(Gender = InsuranceData$Gender), FUN = sum)

ggplot(data = aggData, aes(x = Gender, y = prop.table(stat(aggData$x)), fill = Gender, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Gender', y = 'CLV in Percentage', fill = 'Gender') + 
  ggtitle("CLV Distribution by Gender")

# 4.05 effect of location on customer life time value (clv)

ggplot(InsuranceData, aes(x=LocationCode, y= CustomerLifetimeValue, fill = LocationCode)) + 
  geom_boxplot() + 
  labs(x="Location",y = "Customer Life Time Value", fill="Location") + 
  ggtitle("Visualization of CLV wrt Location")

aggData <- aggregate(x = InsuranceData$CustomerLifetimeValue, by=list(LocationCode = InsuranceData$LocationCode), FUN = sum)

ggplot(data = aggData, aes(x = LocationCode, y = prop.table(stat(aggData$x)), fill = LocationCode, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'LocationCode', y = 'CLV in Percentage', fill = 'LocationCode') + 
  ggtitle("CLV Distribution by LocationCode")

# 4.06 effect of martial status on customer life time value (clv)

ggplot(InsuranceData, aes(x=MaritalStatus, y= CustomerLifetimeValue, fill = MaritalStatus)) + 
  geom_boxplot() + 
  labs(x="Marital Status",y = "Customer Life Time Value", fill="Marital Status") + 
  ggtitle("Visualization of CLV wrt Marital Status")

aggData <- aggregate(x = InsuranceData$CustomerLifetimeValue, by=list(MaritalStatus = InsuranceData$MaritalStatus), FUN = sum)

ggplot(data = aggData, aes(x = MaritalStatus, y = prop.table(stat(aggData$x)), fill = MaritalStatus, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'MaritalStatus', y = 'CLV in Percentage', fill = 'MaritalStatus') + 
  ggtitle("CLV Distribution by MaritalStatus")

# 4.07 effect of policy type on customer life time value (clv)

ggplot(InsuranceData, aes(x=PolicyType, y= CustomerLifetimeValue, fill = PolicyType)) + 
  geom_boxplot() + 
  labs(x="Policy Type",y = "Customer Life Time Value", fill="Policy Type") + 
  ggtitle("Visualization of CLV wrt Policy Type")

aggData <- aggregate(x = InsuranceData$CustomerLifetimeValue, by=list(PolicyType = InsuranceData$PolicyType), FUN = sum)

ggplot(data = aggData, aes(x = PolicyType, y = prop.table(stat(aggData$x)), fill = PolicyType, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'PolicyType', y = 'CLV in Percentage', fill = 'PolicyType') + 
  ggtitle("CLV Distribution by PolicyType")

# 4.08 effect of renew offer type on customer life time value(clv)

ggplot(InsuranceData, aes(x=RenewOfferType, y= CustomerLifetimeValue, fill = RenewOfferType)) + 
  geom_boxplot() + 
  labs(x="Renew Offer Type",y = "Customer Life Time Value", fill="Renew Offer Type") + 
  ggtitle("Visualization of CLV wrt Renew Offer Type")

aggData <- aggregate(x = InsuranceData$CustomerLifetimeValue, by=list(RenewOfferType = InsuranceData$RenewOfferType), FUN = sum)

ggplot(data = aggData, aes(x = RenewOfferType, y = prop.table(stat(aggData$x)), fill = RenewOfferType, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'RenewOfferType', y = 'CLV in Percentage', fill = 'RenewOfferType') + 
  ggtitle("CLV Distribution by RenewOfferType")

# 4.09 effect of sales channel on customer life time value (clv)

ggplot(InsuranceData, aes(x=SalesChannel, y= CustomerLifetimeValue, fill = SalesChannel)) + 
  geom_boxplot() + 
  labs(x="Sales Channel",y = "Customer Life Time Value", fill="Sales Channel") + 
  ggtitle("Visualization of CLV wrt Sales Channel")


aggData <- aggregate(x = InsuranceData$CustomerLifetimeValue, by=list(SalesChannel = InsuranceData$SalesChannel), FUN = sum)

ggplot(data = aggData, aes(x = SalesChannel, y = prop.table(stat(aggData$x)), fill = SalesChannel, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'SalesChannel', y = 'CLV in Percentage', fill = 'SalesChannel') + 
  ggtitle("CLV Distribution by SalesChannel")

# 4.10 effect of vehicle class on customer life time value (clv)

ggplot(InsuranceData, aes(x=VehicleClass, y= CustomerLifetimeValue, fill = VehicleClass)) + 
  geom_boxplot() + 
  labs(x="Vehicle Class",y = "Customer Life Time Value", fill="Vehicle Class") + 
  ggtitle("Visualization of CLV wrt Vehicle Class")

aggData <- aggregate(x = InsuranceData$CustomerLifetimeValue, by=list(VehicleClass = InsuranceData$VehicleClass), FUN = sum)

ggplot(data = aggData, aes(x = VehicleClass, y = prop.table(stat(aggData$x)), fill = VehicleClass, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'VehicleClass', y = 'CLV in Percentage', fill = 'VehicleClass') + 
  ggtitle("CLV Distribution by VehicleClass")

# 4.11 Effect of vechicle size on customer life time value (clv)

ggplot(InsuranceData, aes(x=VehicleSize, y= CustomerLifetimeValue, fill = VehicleSize)) + 
  geom_boxplot() + 
  labs(x="Vehicle Size",y = "Customer Life Time Value", fill="Vehicle Size") + 
  ggtitle("Visualization of CLV wrt Vehicle Size")

aggData <- aggregate(x = InsuranceData$CustomerLifetimeValue, by=list(VehicleSize = InsuranceData$VehicleSize), FUN = sum)

ggplot(data = aggData, aes(x = VehicleSize, y = prop.table(stat(aggData$x)), fill = VehicleSize, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'VehicleSize', y = 'CLV in Percentage', fill = 'VehicleSize') + 
  ggtitle("CLV Distribution by VehicleSize")

# 4.12 effect of states on customer life time value (clv)

ggplot(InsuranceData, aes(x=State, y= CustomerLifetimeValue, fill = State)) + 
  geom_boxplot() + 
  labs(x="State",y = "Customer Life Time Value", fill="State") + 
  ggtitle("Visualization of CLV wrt State")

aggData <- aggregate(x = InsuranceData$CustomerLifetimeValue, by=list(State = InsuranceData$State), FUN = sum)

ggplot(data = aggData, aes(x = State, y = prop.table(stat(aggData$x)), fill = State, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'State', y = 'CLV in Percentage', fill = 'State') + 
  ggtitle("CLV Distribution by State")

# 4.13 effect of policy on customer life time value (clv)

ggplot(InsuranceData, aes(x=Policy, y= CustomerLifetimeValue, fill = Policy)) + 
  geom_boxplot() + 
  labs(x="Policy",y = "Customer Life Time Value", fill="State") + 
  ggtitle("Visualization of CLV wrt Policy")

aggData <- aggregate(x = InsuranceData$CustomerLifetimeValue, by=list(Policy = InsuranceData$Policy), FUN = sum)

ggplot(data = aggData, aes(x = Policy, y = prop.table(stat(aggData$x)), fill = Policy, label = scales::percent(prop.table(stat(aggData$x))))) +
  geom_bar(stat="identity", position = "dodge") + 
  geom_text(stat = 'identity', position = position_dodge(.9),  vjust = -0.5, size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Policy', y = 'CLV in Percentage', fill = 'Policy') + 
  ggtitle("CLV Distribution by Policy")


## 5. Regression Analysis with Continous Variables 

dataContinous <- dplyr::select_if(InsuranceData, ~!is.factor(.))
str(dataContinous)

dim(dataContinous)

# split the data in training and testing set

trainIndex <- createDataPartition(dataContinous$CustomerLifetimeValue, p=0.80, list = FALSE)
#print(trainIndex)
# 80% Train dataset for regression analysis
insurncTrain <- dataContinous[trainIndex,]
# Remaining 30% Test dataset for testing
insurncTest <- dataContinous[-trainIndex,]

dim(dataContinous)

dim(insurncTrain)

dim(insurncTest)

# Linear Regression
# Creating Linear Regression Model using all the continues indepedent variables.
fit <- lm(insurncTrain$CustomerLifetimeValue ~., data = insurncTrain) 
summary(fit) 



# Random Forest Model
library(randomForest)
rfFit <- randomForest(CustomerLifetimeValue ~ ., data = insurncTrain, ntree = 500, mtry = floor(sqrt(ncol(insurncTrain))), importance = TRUE)
summary(rfFit)

# Predict on test data
rfPredictions <- predict(rfFit, insurncTest)

# Evaluate model
rfRMSE <- sqrt(mean((rfPredictions - insurncTest$CustomerLifetimeValue)^2))
rfR2 <- cor(rfPredictions, insurncTest$CustomerLifetimeValue)^2
print(paste("Random Forest RMSE:", rfRMSE))
print(paste("Random Forest R²:", rfR2))






# GBM Model
# Convert categorical variables to factors for GBM
categoricalVars <- sapply(insurncTrain, is.character)  # assuming character variables need conversion
insurncTrain[categoricalVars] <- lapply(insurncTrain[categoricalVars], as.factor)
insurncTest[categoricalVars] <- lapply(insurncTest[categoricalVars], as.factor)

library(gbm)
gbmFit <- gbm(CustomerLifetimeValue ~ ., data = insurncTrain, distribution = "gaussian", n.trees = 500, interaction.depth = 4, shrinkage = 0.01, cv.folds = 5, n.minobsinnode = 10, verbose = FALSE)

# Summary of the model
summary(gbmFit)

# Predict on test data
gbmPredictions <- predict(gbmFit, insurncTest, n.trees = 500, type = "response")  # Ensure to use type = "response" to get actual predictions

# Evaluate model
gbmRMSE <- sqrt(mean((gbmPredictions - insurncTest$CustomerLifetimeValue)^2))
gbmR2 <- cor(gbmPredictions, insurncTest$CustomerLifetimeValue)^2

# Print RMSE and R2 for GBM
print(paste("GBM RMSE:", gbmRMSE))
print(paste("GBM R²:", gbmR2))







# SVM
library(e1071)

# Fit SVM model for regression
svmFit <- svm(CustomerLifetimeValue ~ ., data = insurncTrain, type = "eps-regression", kernel = "radial")

# Print the summary of the fitted SVM model
summary(svmFit)

# Make predictions on the test set
svmPredictions <- predict(svmFit, insurncTest)

# Calculate RMSE for SVM
svmRMSE <- sqrt(mean((svmPredictions - insurncTest$CustomerLifetimeValue)^2))

# Calculate R² for SVM
totalVariation <- sum((insurncTest$CustomerLifetimeValue - mean(insurncTest$CustomerLifetimeValue))^2)
if (totalVariation == 0) {
  svmR2 <- 1  # If no variation in y, model perfectly predicts the mean value
} else {
  svmR2 <- 1 - sum((svmPredictions - insurncTest$CustomerLifetimeValue)^2) / totalVariation
}

# Print RMSE and R²
print(paste("SVM RMSE:", svmRMSE))
print(paste("SVM R²:", svmR2))





# Decision Tree
# Install the package if you haven't already
install.packages("rpart")

# Load the package
library(rpart)

# Build the decision tree model
treeModel <- rpart(CustomerLifetimeValue ~ ., data = insurncTrain, method = "anova")

# Print the tree model summary
summary(treeModel)

# Make predictions on the test set
treePredictions <- predict(treeModel, newdata = insurncTest)

# Calculate RMSE
treeRMSE <- sqrt(mean((treePredictions - insurncTest$CustomerLifetimeValue)^2))

# Calculate R²
treeR2 <- 1 - sum((treePredictions - insurncTest$CustomerLifetimeValue)^2) / sum((insurncTest$CustomerLifetimeValue - mean(insurncTest$CustomerLifetimeValue))^2)

# Print RMSE and R²
print(paste("Decision Tree RMSE:", treeRMSE))
print(paste("Decision Tree R²:", treeR2))


# 5.1.1 Model Interpretation

# 5.1.2 Rerun the model 

# Linear regression new fit
new_fit <- lm(insurncTrain$CustomerLifetimeValue ~ 
                MonthlyPremiumAuto + NumberofOpenComplaints + NumberofPolicies + TotalClaimAmount, 
              data = insurncTrain) 

summary(new_fit) 

# Predict the values for clv for all observations based on the above calculated regression model
predictedCLV <- predict(new_fit)  

#print predicted CLV.
print(predictedCLV[1:10])

#print actual CLV to compare it with above calculated predicted CLV.
print(insurncTrain$CustomerLifetimeValue[1:10])

# ** Calculate Error; Difference between actual CLV and predicted CLV**
residualsCLV <- residuals(new_fit)
print(residualsCLV[1:10])

# **Model Validation on Test dataset**
predicatedTestData=predict(new_fit,insurncTest)
print(predicatedTestData[1:10])

# **Comparision between actual and predictied results.**
InsuranceTrainData <- cbind(insurncTrain,predictedCLV,residualsCLV)
head(InsuranceTrainData)

# **Calculating error rate or MAPE**
ErrorRate <- abs((InsuranceTrainData$CustomerLifetimeValue - InsuranceTrainData$predictedCLV)/(InsuranceTrainData$CustomerLifetimeValue)*100)
print(ErrorRate[1:10])
InsuranceTrainData <- cbind(InsuranceTrainData, ErrorRate)
head(InsuranceTrainData)

# **Calculating mean of error rate**
mean(InsuranceTrainData$ErrorRate, na.rm = TRUE)




# Random Forest new fit
rf_fit <- randomForest(insurncTrain$CustomerLifetimeValue ~ MonthlyPremiumAuto + NumberofOpenComplaints + NumberofPolicies + TotalClaimAmount, data = insurncTrain, ntree = 500, importance = TRUE) 
summary(rf_fit)

# Predict CLV for all observations in the training data
RFpredictedCLV <- predict(rf_fit, newdata = insurncTrain)

# Print predicted CLV
print(RFpredictedCLV[1:10])

# Print actual CLV to compare with predicted CLV
print(insurncTrain$CustomerLifetimeValue[1:10])

# Calculate residuals as the difference between actual and predicted CLV
RFresidualsCLV <- insurncTrain$CustomerLifetimeValue - RFpredictedCLV
print(RFresidualsCLV[1:10])

# Predict on the test dataset
RFpredicatedTestData <- predict(rf_fit, newdata = insurncTest)
print(RFpredicatedTestData[1:10])

# Combine training data with predictions and residuals
RFInsuranceTrainData <- cbind(insurncTrain, RFpredictedCLV, RFresidualsCLV)
head(RFInsuranceTrainData)

# Calculate error rate or MAPE
RFErrorRate <- abs((RFInsuranceTrainData$CustomerLifetimeValue - RFInsuranceTrainData$RFpredictedCLV) / RFInsuranceTrainData$CustomerLifetimeValue * 100)
print(RFErrorRate[1:10])
RFInsuranceTrainData <- cbind(RFInsuranceTrainData, ErrorRate = RFErrorRate)
head(RFInsuranceTrainData)

# Calculate mean of error rate
meanRFErrorRate <- mean(RFInsuranceTrainData$ErrorRate, na.rm = TRUE)
print(meanRFErrorRate)





# Fit the new GBM model
gbm_fit <- gbm(CustomerLifetimeValue ~ MonthlyPremiumAuto + NumberofOpenComplaints + NumberofPolicies + TotalClaimAmount,
               data = insurncTrain,
               distribution = "gaussian", # Assuming a regression problem
               n.trees = 500, # Number of trees
               interaction.depth = 4, # Depth of tree interactions
               shrinkage = 0.01, # Learning rate
               cv.folds = 5, # Cross-validation folds
               n.minobsinnode = 10, # Minimum number of observations in the nodes
               verbose = TRUE)

# Summary of the GBM model
summary(gbm_fit)

# Predict CLV for training data
predictedCLV_GBM <- predict(gbm_fit, insurncTrain, n.trees = 500, type = "response")

# Print predicted CLV
print(predictedCLV_GBM[1:10])

# Print actual CLV to compare with predicted CLV
print(insurncTrain$CustomerLifetimeValue[1:10])

# Calculate residuals as the difference between actual and predicted CLV
residualsCLV_GBM <- insurncTrain$CustomerLifetimeValue - predictedCLV_GBM
print(residualsCLV_GBM[1:10])

# Predict on the test dataset
predictedTestData_GBM <- predict(gbm_fit, insurncTest, n.trees = 500, type = "response")
print(predictedTestData_GBM[1:10])

# Combine training data with predictions and residuals
GBMInsuranceTrainData <- cbind(insurncTrain, predictedCLV = predictedCLV_GBM, residualsCLV = residualsCLV_GBM)
head(GBMInsuranceTrainData)

# Calculate error rate or MAPE
GBMErrorRate <- abs((GBMInsuranceTrainData$CustomerLifetimeValue - GBMInsuranceTrainData$predictedCLV) / GBMInsuranceTrainData$CustomerLifetimeValue * 100)
print(GBMErrorRate[1:10])
GBMInsuranceTrainData <- cbind(GBMInsuranceTrainData, ErrorRate = GBMErrorRate)
head(GBMInsuranceTrainData)

# Calculate mean of error rate
meanGBMErrorRate <- mean(GBMInsuranceTrainData$ErrorRate, na.rm = TRUE)
print(meanGBMErrorRate)






# SVM new fit
# Fit the SVM model for regression
svm_fit <- svm(CustomerLifetimeValue ~ MonthlyPremiumAuto + NumberofOpenComplaints + NumberofPolicies + TotalClaimAmount, 
               data = insurncTrain, 
               type = "eps-regression",
               kernel = "radial")

# Predict CLV for training data
predictedCLV_SVM <- predict(svm_fit, insurncTrain)

# Print predicted CLV
print(predictedCLV_SVM[1:10])

# Print actual CLV to compare with predicted CLV
print(insurncTrain$CustomerLifetimeValue[1:10])

# Calculate residuals as the difference between actual and predicted CLV
residualsCLV_SVM <- insurncTrain$CustomerLifetimeValue - predictedCLV_SVM
print(residualsCLV_SVM[1:10])

# Predict on the test dataset
predictedTestData_SVM <- predict(svm_fit, insurncTest)
print(predictedTestData_SVM[1:10])

# Combine training data with predictions and residuals
SVMInsuranceTrainData <- cbind(insurncTrain, predictedCLV = predictedCLV_SVM, residualsCLV = residualsCLV_SVM)
head(SVMInsuranceTrainData)

# Calculate error rate or MAPE
SVMErrorRate <- abs((SVMInsuranceTrainData$CustomerLifetimeValue - SVMInsuranceTrainData$predictedCLV) / SVMInsuranceTrainData$CustomerLifetimeValue * 100)
print(SVMErrorRate[1:10])
SVMInsuranceTrainData <- cbind(SVMInsuranceTrainData, ErrorRate = SVMErrorRate)
head(SVMInsuranceTrainData)

# Calculate mean of error rate
meanSVMErrorRate <- mean(SVMInsuranceTrainData$ErrorRate, na.rm = TRUE)
print(meanSVMErrorRate)






# Fit the new decision tree 
dt_fit <- rpart(CustomerLifetimeValue ~ MonthlyPremiumAuto + NumberofOpenComplaints + NumberofPolicies + TotalClaimAmount,
                data = insurncTrain,
                method = "anova")  # Use anova for regression trees

# Predict CLV for training data
predictedCLV_DT <- predict(dt_fit, newdata = insurncTrain)

# Print predicted CLV
print(predictedCLV_DT[1:10])

# Print actual CLV to compare with predicted CLV
print(insurncTrain$CustomerLifetimeValue[1:10])

# Calculate residuals as the difference between actual and predicted CLV
residualsCLV_DT <- insurncTrain$CustomerLifetimeValue - predictedCLV_DT
print(residualsCLV_DT[1:10])

# Predict on the test dataset
predictedTestData_DT <- predict(dt_fit, newdata = insurncTest)
print(predictedTestData_DT[1:10])

# Combine training data with predictions and residuals
DTInsuranceTrainData <- cbind(insurncTrain, predictedCLV = predictedCLV_DT, residualsCLV = residualsCLV_DT)
head(DTInsuranceTrainData)

# Calculate error rate or MAPE
DTErrorRate <- abs((DTInsuranceTrainData$CustomerLifetimeValue - DTInsuranceTrainData$predictedCLV) / DTInsuranceTrainData$CustomerLifetimeValue * 100)
print(DTErrorRate[1:10])
DTInsuranceTrainData <- cbind(DTInsuranceTrainData, ErrorRate = DTErrorRate)
head(DTInsuranceTrainData)

# Calculate mean of error rate
meanDTErrorRate <- mean(DTInsuranceTrainData$ErrorRate, na.rm = TRUE)
print(meanDTErrorRate)






# 5.2 Residuals Analysis 

# Linear Regression
hist(ErrorRate, col = "blue")
boxplot(ErrorRate)

shapiro.test(residualsCLV[0:5000])

hist(residualsCLV,col = "green")

# **Residuals vs Fitted Plot**
plot(new_fit, which=1, col=c("blue"))


# RF Residuals
# Ensuring that predictions and actual values are numeric
RFpredictedCLV <- predict(rf_fit, newdata = insurncTrain, type = "response")  # Ensure this is numeric
insurncTrain$CustomerLifetimeValue <- as.numeric(insurncTrain$CustomerLifetimeValue)  # Ensure this is numeric

# Calculate residuals
RFresidualsCLV <- insurncTrain$CustomerLifetimeValue - RFpredictedCLV

# Check if the residuals are numeric
is.numeric(RFresidualsCLV)  # This should return TRUE

# Now plot the histogram and boxplot of residuals
hist(RFresidualsCLV, col = "green", main = "Histogram of RF Residuals")
boxplot(RFresidualsCLV, main = "Boxplot of RF Residuals")

# Shapiro-Wilk Test for Normality of RF residuals
if(length(RFresidualsCLV) > 5000) {
  RFresidualsCLV_sample <- sample(RFresidualsCLV, 5000)  # Sampling if data is too large
  shapiro.test(RFresidualsCLV_sample)
} else {
  shapiro.test(RFresidualsCLV)
}

# Residuals vs Fitted Plot for RF
plot(RFpredictedCLV, RFresidualsCLV, xlab = "Predicted CLV", ylab = "Residuals", main = "Residuals vs Predicted Plot for RF")
abline(h = 0, col = "red")




# Histogram and Boxplot of GBM residuals
hist(GBMInsuranceTrainData$residualsCLV, col = "blue", main = "Histogram of GBM Residuals")
boxplot(GBMInsuranceTrainData$residualsCLV, main = "Boxplot of GBM Residuals")

# Shapiro-Wilk Test for Normality of GBM residuals
shapiro.test(GBMInsuranceTrainData$residualsCLV[1:5000])

# Residuals vs Fitted Plot for GBM
plot(GBMInsuranceTrainData$CustomerLifetimeValue, GBMInsuranceTrainData$residualsCLV, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs Fitted Plot for GBM")
abline(h = 0, col = "red")




# Histogram and Boxplot of SVM residuals
hist(SVMInsuranceTrainData$residualsCLV, col = "orange", main = "Histogram of SVM Residuals")
boxplot(SVMInsuranceTrainData$residualsCLV, main = "Boxplot of SVM Residuals")

# Shapiro-Wilk Test for Normality of SVM residuals
shapiro.test(SVMInsuranceTrainData$residualsCLV[1:5000])

# Residuals vs Fitted Plot for SVM
plot(SVMInsuranceTrainData$CustomerLifetimeValue, SVMInsuranceTrainData$residualsCLV, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs Fitted Plot for SVM")
abline(h = 0, col = "red")




# Histogram and Boxplot of Decision Tree residuals
hist(DTInsuranceTrainData$residualsCLV, col = "purple", main = "Histogram of Decision Tree Residuals")
boxplot(DTInsuranceTrainData$residualsCLV, main = "Boxplot of Decision Tree Residuals")

# Shapiro-Wilk Test for Normality of Decision Tree residuals
shapiro.test(DTInsuranceTrainData$residualsCLV[1:5000])

# Residuals vs Fitted Plot for Decision Tree
plot(DTInsuranceTrainData$CustomerLifetimeValue, DTInsuranceTrainData$residualsCLV, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs Fitted Plot for Decision Tree")
abline(h = 0, col = "red")





## 6. Assumpation Testing 

# Linear Regression Analysis
# **1. Detecting multicollinearity** - checking correlation between independent variables.

cor(InsuranceTrainData) 

# Variance Inflation Factors
car::vif(new_fit)

# 2. Detecting Homoscedasticity
# Breusch-Pagan test

bptest(new_fit)

# 3.Detecting Autocorrelation - checking correlation between errors
# Durbin-Watson test

dwt(new_fit)

# 4. Detecting MAPE - mean absolute percentage error loss
ErrorRate <- mean(abs((InsuranceTrainData$CustomerLifetimeValue - InsuranceTrainData$predictedCLV)/InsuranceTrainData$CustomerLifetimeValue) *100 )
print(ErrorRate)






# Calculate residuals for all models
RFInsuranceTrainData$residualsCLV <- RFInsuranceTrainData$CustomerLifetimeValue - RFInsuranceTrainData$RFpredictedCLV
GBMInsuranceTrainData$residualsCLV <- GBMInsuranceTrainData$CustomerLifetimeValue - GBMInsuranceTrainData$predictedCLV
SVMInsuranceTrainData$residualsCLV <- SVMInsuranceTrainData$CustomerLifetimeValue - SVMInsuranceTrainData$predictedCLV
DTInsuranceTrainData$residualsCLV <- DTInsuranceTrainData$CustomerLifetimeValue - DTInsuranceTrainData$predictedCLV


# Define a function to plot residuals and calculate MAPE for any given data
plot_residuals_and_mape <- function(data, model_name) {
  # Plot residuals
  plot(data$residualsCLV, main = paste("Residuals Plot for", model_name), xlab = "Index", ylab = "Residuals")
  abline(h = 0, col = "red")
  
  # Calculate MAPE
  mape <- mean(abs(data$residualsCLV / data$CustomerLifetimeValue) * 100, na.rm = TRUE)
  print(paste("MAPE for", model_name, ":", mape))
}

# Apply the function to each model
plot_residuals_and_mape(RFInsuranceTrainData, "Random Forest")
                        plot_residuals_and_mape(GBMInsuranceTrainData, "GBM")
plot_residuals_and_mape(SVMInsuranceTrainData, "SVM")
plot_residuals_and_mape(DTInsuranceTrainData, "Decision Tree")




## 7. Prediction Curve

#. 1. Prediction Curve with MonthlyPremiumAuto

# Linear Regression
ggplot(InsuranceTrainData, aes(x = MonthlyPremiumAuto, y = CustomerLifetimeValue)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +     # regression line  
  geom_segment(aes(xend = MonthlyPremiumAuto, yend = predictedCLV), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(residualsCLV), size = abs(residualsCLV))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = predictedCLV), shape = 1) +
  theme_bw()

ggplot(InsuranceTrainData,aes(x=MonthlyPremiumAuto,y=CustomerLifetimeValue))+
  geom_point(color="red")+
  stat_smooth(method="lm")+
  scale_x_continuous(name="Monthly Premium")+
  scale_y_continuous(name="Prediction of CLV")+
  ggtitle("Prediction Curve with Monthly Premium")




# Random Forest 
# Assume 'rf_fit' is your trained Random Forest model and 'InsuranceTrainData' is your dataset

# 1. Add predictions to the dataset
InsuranceTrainData$RFpredictedCLV <- predict(rf_fit, newdata = InsuranceTrainData)

# 2. Calculate residuals for the Random Forest predictions
InsuranceTrainData$residualsCLV_RF <- InsuranceTrainData$CustomerLifetimeValue - InsuranceTrainData$RFpredictedCLV

# 3. Generate the plot
ggplot(InsuranceTrainData, aes(x = MonthlyPremiumAuto, y = CustomerLifetimeValue)) +
  geom_point(aes(color = abs(residualsCLV_RF), size = abs(residualsCLV_RF)), alpha=0.5) +  # Actual points with size and color based on residuals
  geom_line(aes(y = RFpredictedCLV), color = "blue") +  # Prediction line
  geom_segment(aes(xend = MonthlyPremiumAuto, yend = RFpredictedCLV), alpha = .2, color = "gray") +  # Line from actual to predicted
  scale_color_continuous(low = "green", high = "red") +  # Color scale for residuals
  guides(color = guide_legend(title = "Residual Size"), size = guide_legend(title = "Residual Size")) +  # Legends for color and size
  theme_bw() +  # Clean theme
  labs(title = "Random Forest Prediction Curve with Monthly Premium Auto",
       x = "Monthly Premium Auto",
       y = "Customer Lifetime Value")






# GBM 
# Assuming 'gbm_fit' is your trained GBM model and 'InsuranceTrainData' is your dataset

# Prepare a new data frame for predictions, assuming the GBM model uses MonthlyPremiumAuto,
# NumberofOpenComplaints, NumberofPolicies, and TotalClaimAmount
new_data_gbm <- data.frame(
  MonthlyPremiumAuto = seq(min(InsuranceTrainData$MonthlyPremiumAuto), max(InsuranceTrainData$MonthlyPremiumAuto), length.out = 100),
  NumberofOpenComplaints = mean(InsuranceTrainData$NumberofOpenComplaints, na.rm = TRUE),  # Use mean or another appropriate value
  NumberofPolicies = mean(InsuranceTrainData$NumberofPolicies, na.rm = TRUE),  # Use mean or another appropriate value
  TotalClaimAmount = mean(InsuranceTrainData$TotalClaimAmount, na.rm = TRUE)   # Use mean or another appropriate value
)

# Predict using the GBM model
new_data_gbm$predictedCLV_GBM <- predict(gbm_fit, newdata = new_data_gbm, n.trees = 500, type = "response")



# Plotting the actual data and the predictions
ggplot() +
  geom_point(data = InsuranceTrainData, aes(x = MonthlyPremiumAuto, y = CustomerLifetimeValue), color = "red") +
  geom_line(data = new_data_gbm, aes(x = MonthlyPremiumAuto, y = predictedCLV_GBM), color = "blue") +
  scale_x_continuous(name = "Monthly Premium Auto") +
  scale_y_continuous(name = "Predicted Customer Lifetime Value") +
  ggtitle("GBM Prediction Curve with Monthly Premium Auto") +
  theme_minimal()






# SVM
# Assuming 'svm_fit' is your trained SVM model and 'InsuranceTrainData' is your dataset

# Prepare a new data frame for predictions
new_data_svm <- data.frame(
  MonthlyPremiumAuto = seq(min(InsuranceTrainData$MonthlyPremiumAuto), max(InsuranceTrainData$MonthlyPremiumAuto), length.out = 100),
  NumberofOpenComplaints = mean(InsuranceTrainData$NumberofOpenComplaints, na.rm = TRUE),  # Use mean or another appropriate value
  NumberofPolicies = mean(InsuranceTrainData$NumberofPolicies, na.rm = TRUE),  # Use mean or another appropriate value
  TotalClaimAmount = mean(InsuranceTrainData$TotalClaimAmount, na.rm = TRUE)   # Use mean or another appropriate value
)

# Predict using the SVM model
new_data_svm$predictedCLV_SVM <- predict(svm_fit, newdata = new_data_svm)

# Plotting the actual data points and the SVM predictions
ggplot() +
  geom_point(data = InsuranceTrainData, aes(x = MonthlyPremiumAuto, y = CustomerLifetimeValue), color = "red") +
  geom_line(data = new_data_svm, aes(x = MonthlyPremiumAuto, y = predictedCLV_SVM), color = "blue") +
  scale_x_continuous(name = "Monthly Premium Auto") +
  scale_y_continuous(name = "Predicted Customer Lifetime Value") +
  ggtitle("SVM Prediction Curve with Monthly Premium Auto") +
  theme_minimal()






# Decision Tree
# Assuming 'dt_fit' is your trained Decision Tree model and 'InsuranceTrainData' is your dataset

# Prepare a new data frame for predictions, assuming the Decision Tree model uses:
# MonthlyPremiumAuto, NumberofOpenComplaints, NumberofPolicies, and TotalClaimAmount
new_data_dt <- data.frame(
  MonthlyPremiumAuto = seq(min(InsuranceTrainData$MonthlyPremiumAuto), max(InsuranceTrainData$MonthlyPremiumAuto), length.out = 100),
  NumberofOpenComplaints = mean(InsuranceTrainData$NumberofOpenComplaints, na.rm = TRUE),  # Use mean or another appropriate value
  NumberofPolicies = mean(InsuranceTrainData$NumberofPolicies, na.rm = TRUE),  # Use mean or another appropriate value
  TotalClaimAmount = mean(InsuranceTrainData$TotalClaimAmount, na.rm = TRUE)   # Use mean or another appropriate value
)

# Predict using the Decision Tree model
new_data_dt$predictedCLV_DT <- predict(dt_fit, newdata = new_data_dt)

# Plotting the actual data points and the Decision Tree predictions
ggplot() +
  geom_point(data = InsuranceTrainData, aes(x = MonthlyPremiumAuto, y = CustomerLifetimeValue), color = "red") +
  geom_line(data = new_data_dt, aes(x = MonthlyPremiumAuto, y = predictedCLV_DT), color = "blue") +
  scale_x_continuous(name = "Monthly Premium Auto") +
  scale_y_continuous(name = "Predicted Customer Lifetime Value") +
  ggtitle("Decision Tree Prediction Curve with Monthly Premium Auto") +
  theme_minimal()

    




# 2. Prediction Curve with TotalClaimAmount

  # Linear Regression
  ggplot(InsuranceTrainData,aes(x=TotalClaimAmount,y=CustomerLifetimeValue))+
  geom_point(color="red")+
  stat_smooth(method="lm")+
  scale_x_continuous(name="Total Claim Amount")+
  scale_y_continuous(name="Prediction of CLV")+
  ggtitle("Prediction Curve with Total Claim Amount")
  

  # Random Forest
  # Prepare data for prediction with all necessary predictors
  new_data_rf <- data.frame(
    TotalClaimAmount = seq(min(InsuranceTrainData$TotalClaimAmount), max(InsuranceTrainData$TotalClaimAmount), length.out = 100),
    MonthlyPremiumAuto = mean(InsuranceTrainData$MonthlyPremiumAuto, na.rm = TRUE),  # Assuming constant value
    NumberofOpenComplaints = mean(InsuranceTrainData$NumberofOpenComplaints, na.rm = TRUE),  # Assuming constant value
    NumberofPolicies = mean(InsuranceTrainData$NumberofPolicies, na.rm = TRUE)  # Assuming constant value
  )
  
  # Predict using the RF model
  new_data_rf$predictedCLV_RF <- predict(rf_fit, newdata = new_data_rf)
  
  
  # Plotting
  ggplot() +
    geom_point(data = InsuranceTrainData, aes(x = TotalClaimAmount, y = CustomerLifetimeValue), color = "red") +
    geom_line(data = new_data_rf, aes(x = TotalClaimAmount, y = predictedCLV_RF), color = "blue") +
    scale_x_continuous(name = "Total Claim Amount") +
    scale_y_continuous(name = "Predicted Customer Lifetime Value") +
    ggtitle("RF Prediction Curve with Total Claim Amount") +
    theme_minimal()
  
  
  
  
  # GBM
  # Assuming 'gbm_fit' is your trained GBM model and 'InsuranceTrainData' is your dataset
  
  # Prepare a new data frame for predictions, assuming the GBM model uses:
  # TotalClaimAmount, MonthlyPremiumAuto, NumberofOpenComplaints, and NumberofPolicies
  new_data_gbm <- data.frame(
    TotalClaimAmount = seq(min(InsuranceTrainData$TotalClaimAmount), max(InsuranceTrainData$TotalClaimAmount), length.out = 100),
    MonthlyPremiumAuto = mean(InsuranceTrainData$MonthlyPremiumAuto, na.rm = TRUE),  # Use mean or another appropriate value
    NumberofOpenComplaints = mean(InsuranceTrainData$NumberofOpenComplaints, na.rm = TRUE),  # Use mean or another appropriate value
    NumberofPolicies = mean(InsuranceTrainData$NumberofPolicies, na.rm = TRUE)   # Use mean or another appropriate value
  )
  
  # Predict using the GBM model
  new_data_gbm$predictedCLV_GBM <- predict(gbm_fit, newdata = new_data_gbm, n.trees = 500, type = "response")
  
  # Plotting the actual data points and the GBM predictions
  ggplot() +
    geom_point(data = InsuranceTrainData, aes(x = TotalClaimAmount, y = CustomerLifetimeValue), color = "red") +
    geom_line(data = new_data_gbm, aes(x = TotalClaimAmount, y = predictedCLV_GBM), color = "green") +
    scale_x_continuous(name = "Total Claim Amount") +
    scale_y_continuous(name = "Predicted Customer Lifetime Value") +
    ggtitle("GBM Prediction Curve with Total Claim Amount") +
    theme_minimal()

  
  
  
  
  # SVM
  # Assuming 'svm_fit' is your trained SVM model and 'InsuranceTrainData' is your dataset
  
  # Prepare a new data frame for predictions, assuming the SVM model uses:
  # TotalClaimAmount, MonthlyPremiumAuto, NumberofOpenComplaints, and NumberofPolicies
  new_data_svm <- data.frame(
    TotalClaimAmount = seq(min(InsuranceTrainData$TotalClaimAmount), max(InsuranceTrainData$TotalClaimAmount), length.out = 100),
    MonthlyPremiumAuto = mean(InsuranceTrainData$MonthlyPremiumAuto, na.rm = TRUE),  # Use mean or another appropriate value
    NumberofOpenComplaints = mean(InsuranceTrainData$NumberofOpenComplaints, na.rm = TRUE),  # Use mean or another appropriate value
    NumberofPolicies = mean(InsuranceTrainData$NumberofPolicies, na.rm = TRUE)   # Use mean or another appropriate value
  )
  
  # Predict using the SVM model
  new_data_svm$predictedCLV_SVM <- predict(svm_fit, newdata = new_data_svm)
  
  # Plotting the actual data points and the SVM predictions
  ggplot() +
    geom_point(data = InsuranceTrainData, aes(x = TotalClaimAmount, y = CustomerLifetimeValue), color = "red") +
    geom_line(data = new_data_svm, aes(x = TotalClaimAmount, y = predictedCLV_SVM), color = "blue") +
    scale_x_continuous(name = "Total Claim Amount") +
    scale_y_continuous(name = "Predicted Customer Lifetime Value") +
    ggtitle("SVM Prediction Curve with Total Claim Amount") +
    theme_minimal()
  
  
  
  # Decision Tree
  # Assuming 'dt_fit' is your trained Decision Tree model and 'InsuranceTrainData' is your dataset
  
  # Prepare a new data frame for predictions, assuming the Decision Tree model uses:
  # TotalClaimAmount, MonthlyPremiumAuto, NumberofOpenComplaints, and NumberofPolicies
  new_data_dt <- data.frame(
    TotalClaimAmount = seq(min(InsuranceTrainData$TotalClaimAmount), max(InsuranceTrainData$TotalClaimAmount), length.out = 100),
    MonthlyPremiumAuto = mean(InsuranceTrainData$MonthlyPremiumAuto, na.rm = TRUE),  # Use mean or another appropriate value
    NumberofOpenComplaints = mean(InsuranceTrainData$NumberofOpenComplaints, na.rm = TRUE),  # Use mean or another appropriate value
    NumberofPolicies = mean(InsuranceTrainData$NumberofPolicies, na.rm = TRUE)   # Use mean or another appropriate value
  )
  
  # Predict using the Decision Tree model
  new_data_dt$predictedCLV_DT <- predict(dt_fit, newdata = new_data_dt)
  
  # Plotting the actual data points and the Decision Tree predictions
  ggplot() +
    geom_point(data = InsuranceTrainData, aes(x = TotalClaimAmount, y = CustomerLifetimeValue), color = "red") +
    geom_line(data = new_data_dt, aes(x = TotalClaimAmount, y = predictedCLV_DT), color = "green") +
    scale_x_continuous(name = "Total Claim Amount") +
    scale_y_continuous(name = "Predicted Customer Lifetime Value") +
    ggtitle("Decision Tree Prediction Curve with Total Claim Amount") +
    theme_minimal()
  
