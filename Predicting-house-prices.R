library(ggplot2)

##Read the data
train <- read.csv("train.csv")
test <- read.csv("test.csv")

##Check for missing values
colSums(is.na(train))
colSums(is.na(test))

##Combine two sets
test$SalePrice <- 0
set <- rbind(train, test)
summary(set)

##Missing values
colSums(is.na(set))

summary(set$MSZoning)
set$MSZoning[which(is.na(set$MSZoning))] <- "RL"

summary(set$LotFrontage)
set$LotFrontage <- ifelse(is.na(set$LotFrontage),
                          ave(set$LotFrontage, FUN=function(x) mean(x, na.rm = TRUE)),
                              set$LotFrontage)

summary(set$Alley)
set$Alley <- factor(set$Alley, levels=c(levels(set$Alley), "No"))
set$Alley[which(is.na(set$Alley))] <- "No"

summary(set$Street)
set$Street[which(is.na(set$Street))] <- "Pave"

summary(set$Utilities)
set$Utilities[which(is.na(set$Utilities))] <- "AllPub"
plot(set$Utilities)

summary(set$Exterior1st)
set$Exterior1st[which(is.na(set$Exterior1st))] <- "VinylSd"

summary(set$Exterior2nd)
set$Exterior2nd[which(is.na(set$Exterior2nd))] <- "VinylSd"

summary(set$MasVnrType)
set$MasVnrType[which(is.na(set$MasVnrType))] <- "None"

summary(set$MasVnrArea)
set$MasVnrArea[which(is.na(set$MasVnrArea))] <- 0

summary(set$BsmtQual)
set$BsmtQual <- factor(set$BsmtQual, levels=c(levels(set$BsmtQual), "No"))
set$BsmtQual[which(is.na(set$BsmtQual))] <- "No"

summary(set$BsmtCond)
set$BsmtCond <- factor(set$BsmtCond, levels=c(levels(set$BsmtCond), "No"))
set$BsmtCond[which(is.na(set$BsmtCond))] <- "No"

summary(set$BsmtExposure)
set$BsmtExposure <- factor(set$BsmtExposure, levels=c(levels(set$BsmtExposure), "NB"))
set$BsmtExposure[which(is.na(set$BsmtExposure))] <- "NB"

summary(set$BsmtFinType1)
set$BsmtFinType1 <- factor(set$BsmtFinType1, levels=c(levels(set$BsmtFinType1), "No"))
set$BsmtFinType1[which(is.na(set$BsmtFinType1))] <- "No"

summary(set$BsmtFinType2)
set$BsmtFinType2 <- factor(set$BsmtFinType2, levels=c(levels(set$BsmtFinType2), "No"))
set$BsmtFinType2[which(is.na(set$BsmtFinType2))] <- "No"

summary(set$BsmtFinSF1)
set$BsmtFinSF1[which(is.na(set$BsmtFinSF1))] <- 0

summary(set$BsmtFinSF2)
set$BsmtFinSF2[which(is.na(set$BsmtFinSF2))] <- 0

summary(set$BsmtUnfSF)
set$BsmtUnfSF[which(is.na(set$BsmtUnfSF))] <- 0

summary(set$TotalBsmtSF)
set$TotalBsmtSF <- ifelse(is.na(set$TotalBsmtSF),
                          ave(set$TotalBsmtSF, FUN=function(x) mean(x, na.rm = TRUE)),
                          set$TotalBsmtSF)

summary(set$BsmtFullBath)
set$BsmtFullBath <- as.factor(set$BsmtFullBath)
set$BsmtFullBath[which(is.na(set$BsmtFullBath))] <- 0

summary(set$BsmtHalfBath)
set$BsmtHalfBath <- as.factor(set$BsmtHalfBath)
set$BsmtHalfBath[which(is.na(set$BsmtHalfBath))] <- 0

summary(set$KitchenQual)
set$KitchenQual[which(is.na(set$KitchenQual))] <- "TA"

summary(set$Electrical)
set$Electrical[which(is.na(set$Electrical))] <- "SBrkr"

summary(set$FireplaceQu)
set$FireplaceQu <- factor(set$FireplaceQu, levels=c(levels(set$FireplaceQu), "No"))
set$FireplaceQu[which(is.na(set$FireplaceQu))] <- "No"

summary(set$Functional)
set$Functional[which(is.na(set$Functional))] <- "Typ"

summary(set$GarageType)
set$GarageType <- factor(set$GarageType, levels=c(levels(set$GarageType), "No"))
set$GarageType[which(is.na(set$GarageType))] <- "No"

summary(set$GarageYrBlt)
set[which(is.na(set$GarageYrBlt)), 'GarageYrBlt'] <- set[which(is.na(set$GarageYrBlt)), 'YearBuilt']

summary(set$GarageFinish)
set$GarageFinish <- factor(set$GarageFinish, levels=c(levels(set$GarageFinish), "No"))
set$GarageFinish[which(is.na(set$GarageFinish))] <- "No"

summary(set$GarageQual)
set$GarageQual <- factor(set$GarageQual, levels=c(levels(set$GarageQual), "No"))
set$GarageQual[which(is.na(set$GarageQual))] <- "No"

summary(set$GarageCond)
set$GarageCond <- factor(set$GarageCond, levels=c(levels(set$GarageCond), "No"))
set$GarageCond[which(is.na(set$GarageCond))] <- "No"

summary(set$GarageCars)
set$GarageCars <- as.factor(set$GarageCars)
set$GarageCars[which(is.na(set$GarageCars))] <- 0

summary(set$GarageArea)
set$GarageArea[which(is.na(set$GarageArea))] <- 0

summary(set$PoolQC)
set$PoolQC <- factor(set$PoolQC, levels=c(levels(set$PoolQC), "No"))
set$PoolQC[which(is.na(set$PoolQC))] <- "No"

summary(set$Fence)
set$Fence <- factor(set$Fence, levels=c(levels(set$Fence), "No"))
set$Fence[which(is.na(set$Fence))] <- "No"

summary(set$MiscFeature)
set$MiscFeature <- factor(set$MiscFeature, levels=c(levels(set$MiscFeature), "None"))
set$MiscFeature[which(is.na(set$MiscFeature))] <- "None"

summary(set$SaleType)
set$SaleType[which(is.na(set$SaleType))] <- "WD"

colSums(is.na(set))


plot(set$MSSubClass)
plot(set$MoSold)
plot(set$Fireplaces)

##Converting into factor

summary(set)

set$Id <- as.factor(set$Id)
set$MSSubClass <- as.factor(set$MSSubClass)
plot(set$MSSubClass)
set$OverallQual <- as.factor(set$OverallQual)
set$OverallCond <- as.factor(set$OverallCond)
set$FullBath <- as.factor(set$FullBath)
set$HalfBath <- as.factor(set$HalfBath)
set$BedroomAbvGr <- as.factor(set$BedroomAbvGr)
set$KitchenAbvGr <- as.factor(set$KitchenAbvGr)
set$TotRmsAbvGrd <- as.factor(set$TotRmsAbvGrd)
set$Fireplaces <- as.factor(set$Fireplaces)
plot(set$Fireplaces)
set$MoSold <- as.factor(set$MoSold)
plot(set$MoSold)
set$YrSold <- as.factor(set$YrSold)

# Converting years into ages
set$Age <- 2010-set$YearBuilt
summary(set$Age)

set$GarAge <- 2010-set$GarageYrBlt
summary(set$GarAge)

set$RemodAge <- 2010-set$YearRemodAdd
summary(set$RemodAge)

##Remove unnecessary features

set$Id <- NULL
set$Utilities <- NULL
set$YearBuilt <- NULL
set$GarageYrBlt <- NULL
set$YearRemodAdd <- NULL

##Take SalePrice column to the end
set <- set[,c(setdiff(names(set),"SalePrice"),"SalePrice")]


##Split into train and test again

train2 <- set[1:1460,]
test2 <- set[1461:2919,]

#Some plots
plot(train2$LotFrontage,train2$SalePrice,
     xlab="LotFrontage",ylab="SalePrice")

plot(train2$GarageArea,train2$SalePrice,
     xlab="GarageArea",ylab="SalePrice")

plot(train2$TotalBsmtSF,train2$SalePrice,
     xlab="TotalBsmtSF",ylab="SalePrice")

##Take log of SalePrice in train2

train2$SalePrice <- log(train2$SalePrice)


#Plot of before and after log of SalePrice
ggplot(train)+geom_histogram(aes(SalePrice),fill="red",colour="black")+
  ggtitle("SalePrice")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(train2)+geom_histogram(aes(SalePrice),fill="red",colour="black")+
  ggtitle("Log(SalePrice)")+
  theme(plot.title = element_text(hjust = 0.5))

##Apply Random Forest algorithm

library(randomForest)
formulaT <- formula(SalePrice~.,train2)

forest <- randomForest(formula=formulaT, data=train2,ntree=1000)
plot(sqrt(forest$mse), main = "Error in Random Forest Algorithm",
     xlab="Number of trees", ylab = "RMSE")

forest_rmse<- sqrt(sum((train2$SalePrice-forest$predicted)^2)/1460)
forest_rmse

plot(forest$rsq, main = "R-squared in Random Forest Algorithm",
     xlab="Number of trees", ylab = "R-squared")
max(forest$rsq)

plot(x=exp(train2$SalePrice),y=exp(forest$predicted),
     xlab="Sale Prices", ylab = "Predicted Sale Prices")

#Test set

forest_test <- predict(forest, test2)
test2$SalePrice <- exp(forest_test)

ggplot(test2)+geom_histogram(aes(SalePrice),fill="red",colour="black")+
  ggtitle("Sale Prices of test set by Random Forest Algorithm")+
  theme(plot.title = element_text(hjust = 0.5))


forest_submission <- data.frame(Id = 1461:2919, SalePrice= test2$SalePrice)
colnames(forest_submission) <- c("Id", "SalePrice")

write.table(forest_submission, 
          file = "forest_submission.csv", 
          row.names = FALSE,sep = ",",dec = ".",qmethod = "double")

# Apply multiple linear regression model

library(stats)
mlr <- lm(formula = formulaT,data=train2)
plot(x=exp(train2$SalePrice),y=exp(mlr$fitted.values),
     xlab="Sale Prices", ylab = "Predicted Sale Prices")
mlr_rmse <- sqrt(sum((mlr$residuals)^2/1460))

## Can't get test dataset results, should do more feature engineering

# Apply gradient boosting algorithm

library(gbm)
gbr <- gbm(formula = formulaT, data = train2,n.trees = 500)
plot(x=exp(train2$SalePrice),y=exp(gbr$fit),
     xlab="Sale Prices", ylab = "Predicted Sale Prices")
gbr_rmse <- sqrt(sum((train2$SalePrice-gbr$fit)^2)/1460)


boosting_test <- predict(gbr, test2, n.trees = 500)


boosting_submission <- data.frame(Id = 1461:2919, SalePrice= exp(boosting_test))
colnames(boosting_submission) <- c("Id", "SalePrice")

write.table(boosting_submission, 
            file = "boosting_submission.csv", 
            row.names = FALSE,sep = ",",dec = ".",qmethod = "double")

## Errors of the algorithm: RF > GB > ML
