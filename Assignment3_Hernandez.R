library(sjPlot)
library(sjlabelled)
#Read in our Ames Housing Data 
ames_df <- read.csv('ames_housing_data.csv', header = TRUE, stringsAsFactors = FALSE)

ames_df$dropCondition <- ifelse(!ames_df$Zoning %in% c('RH','RL','RP','RM'), '01: Not Residential', 
  ifelse(ames_df$BldgType!='1Fam','02: Not SFR',
  ifelse(ames_df$LotArea>20000,'03: Lot Area Greater than 20,000 Square Feet',
  ifelse(ames_df$OverallCond<3,'04: Poor House Condition',  
  ifelse(ames_df$YearBuilt<1950,'05: Built Pre-1950',
  ifelse(!ames_df$ExterCond %in% c('Ex', 'GD', 'TA', 'Fa'),'06: Poor Exterior Condition',
  ifelse(ames_df$Utilities!='AllPub','07: Does not have all utilities',
  ifelse(!ames_df$Functional %in% c('Typ', 'Min1', 'Min2', 'Mod'),'08: Home Non Functional',
  ifelse(ames_df$SaleCondition!='Normal','09: Non-Normal Sale',
  ifelse(ames_df$SalePrice>400000, '10: Sale Price Greater than $400,000',
  '99: Eligible Sample'))))))))))


#Create a table summarizing Drop Conditions  
table(ames_df$dropCondition)

#Save table as 'waterfall' 
waterfall <- table(ames_df$dropCondition)

#Display the table as a column matrix
as.matrix(waterfall, 11,1)

# Eliminate all observations that are not part of the eligible sample population
eligible_population <- subset(ames_df,dropCondition=='99: Eligible Sample')

# Check that all remaining observations are eligible
table(eligible_population$dropCondition)

# Create a new DataFrame that only includes variables we like 
keep.vars <- c('SID','PID','LotFrontage','LotArea','LotConfig','Neighborhood',
               'HouseStyle','OverallQual','OverallCond','YearBuilt','YearRemodel','Exterior1',
               'BsmtFinSF1','BsmtFinSF2','CentralAir','GrLivArea','BsmtFullBath','BsmtHalfBath',
               'FullBath','HalfBath','BedroomAbvGr','TotRmsAbvGrd','Fireplaces','GarageCars',
               'GarageArea','WoodDeckSF','OpenPorchSF','EnclosedPorch','ThreeSsnPorch',
               'ScreenPorch','PoolArea','MoSold','YrSold','SaleCondition','SalePrice')

#Subset eligible_population DataFrame with keep.vars vector
skinny_ames_df <- eligible_population[, keep.vars]

#Delete observations with missing values
skinny_ames_df <- na.omit(skinny_ames_df)

#Table of correlation for EDA
cor(skinny_ames_df$LotArea, skinny_ames_df$SalePrice)
cor(skinny_ames_df$GrLivArea, skinny_ames_df$SalePrice)
cor(skinny_ames_df$GarageArea, skinny_ames_df$SalePrice)
cor(skinny_ames_df$LotFrontage, skinny_ames_df$SalePrice)
cor(skinny_ames_df$EnclosedPorch, skinny_ames_df$SalePrice)
cor(skinny_ames_df$OpenPorchSF, skinny_ames_df$SalePrice)
cor(skinny_ames_df$PoolArea, skinny_ames_df$SalePrice)
cor(skinny_ames_df$TotRmsAbvGrd, skinny_ames_df$SalePrice)

#Complete EDA on promising predictor variables in skinny_ames_df
#First, use boxplots to inspect distribution of two most promising variables 
par(mfrow=c(1,2))
boxplot(skinny_ames_df$LotArea, main = 'Boxplot of LotArea', xlab= 'Lot Size', ylab='Square Feet')
boxplot(skinny_ames_df$GrLivArea, main = 'Boxplot of GrLivArea', xlab='Above Ground Living Area', ylab = 'Square Feet')
par(mfrow=c(1,1))

#Next create scatterplot of SalePrice versus each promising predictor variable
plot(skinny_ames_df$LotArea, skinny_ames_df$SalePrice, ylab='Sale Price (USD)', main='Sale Price vs. LotArea', xlab='LotArea (Sqft)')
plot(skinny_ames_df$GrLivArea, skinny_ames_df$SalePrice, ylab='Sale Price (USD)', main='Sale Price vs. GrLivArea', xlab='GrLivArea (Sqft)')

#Fit a linear regression model using LotArea variable
LotArea_model <- lm(SalePrice ~ LotArea, data=skinny_ames_df)

#Fit a linear regression model using GrLivArea
GrLivArea_model <- lm(SalePrice ~ GrLivArea, data=skinny_ames_df)

#Display summary of LotArea_model
sjt.lm(LotArea_model)

#Display summary of GrLivArea_model
sjt.lm(GrLivArea_model)

#Display summary of both models
sjt.lm(LotArea_model, GrLivArea_model)

#Now, we need to produce some diagnostic plots to assess the goodness-of-fit of each model 
#We will check the assumptions of normality and homoscedasticity with:
# 1. QQ Plot
# 2. Scatterplot of residuals versus predictor variable

#GOF plots for LotArea_model
#Plot for normaility
qqnorm(LotArea_model$residuals)
qqline(LotArea_model$residuals)
#Plot for homoscedasticity
plot(skinny_ames_df$LotArea,LotArea_model$residuals, xlab='Lot Area (Sqft)', ylab='LotArea Model Residuals')

#GOF plots for GrLivArea_model
#Plot for normaility
qqnorm(GrLivArea_model$residuals)
qqline(GrLivArea_model$residuals)

#Plot for homoscedasticity
plot(skinny_ames_df$GrLivArea,GrLivArea_model$residuals, xlab='GrLivArea (Sqft)', ylab='GrLivArea Model Residuals')

#Now we will built a multiple linear regrssion model using the same promising predictor variables 
#Let's first look at the correlation between our predictor variables
cor(skinny_ames_df$LotArea, skinny_ames_df$GrLivArea)

#Fit the multiple linear regression model with both predictor variables
MLR_model <- lm(SalePrice ~ LotArea+GrLivArea, data=skinny_ames_df)

#GOF plots for MLR_model
#Plot for normaility
qqnorm(MLR_model$residuals)
qqline(MLR_model$residuals)
#Plot for homoscedasticity
plot(skinny_ames_df$LotArea,MLR_model$residuals, xlab='Lot Area (Sqft)', ylab='MLR Model Residuals')
plot(skinny_ames_df$GrLivArea,MLR_model$residuals, xlab='GrLivArea (Sqft)', ylab='MLR Model Residuals')
plot(MLR_model$fitted.values,MLR_model$residuals, xlab='MLR Model Fitted Values', ylab='MLR Model Residuals')

#Neighborhood Accuracy
boxplot(MLR_model$residuals~skinny_ames_df$Neighborhood, las=2, main='Residuals vs. Neighborhoods')

#Add Total Square Foot variable
skinny_ames_df$TotalSqftCal<-skinny_ames_df$BsmtFinSF1+skinny_ames_df$BsmtFinSF2+skinny_ames_df$GrLivArea
#Add Sale Price / Total Square Foot variable
skinny_ames_df$SalePricePerSquareFoot<-skinny_ames_df$SalePrice/skinny_ames_df$TotalSqftCal

#Compute the mean MAE for each neighborhood
meanMAE_MLR_model <- aggregate(abs(MLR_model$residuals), by=list(Neighborhood=skinny_ames_df$Neighborhood), FUN=mean)
meanMAE_MLR_model

#Compute the mean Sale Price / Total Square Foot for each neighborhood
meanSPperSQFT_MLR_model <- aggregate(skinny_ames_df$SalePricePerSquareFoot, by=list(Neighborhood=skinny_ames_df$Neighborhood), FUN=mean)
meanSPperSQFT_MLR_model

#Plot the mean MAE and the mean Sale Price / Total Square Foot for each neighborhood
plot(meanSPperSQFT_MLR_model[,2],meanMAE_MLR_model[,2], main = 'MAE vs. SalePrice/SQFT', xlab = 'SalePrice/SQFT', ylab='MAE')

#Group the neighborhoods by Sale Price / Total Square Foot
#This will be done by using indicator variables 
#There will be four groups - the base group will be neighborhoods that have a Sale Price / Total Square Foot less than $80
skinny_ames_df$SPSF40_80 <- ifelse(skinny_ames_df$SalePricePerSquareFoot>=40 & skinny_ames_df$SalePricePerSquareFoot<80,1,0)
skinny_ames_df$SPSF80_85 <- ifelse(skinny_ames_df$SalePricePerSquareFoot>=80 & skinny_ames_df$SalePricePerSquareFoot<85,1,0)
skinny_ames_df$SPSF85_100 <- ifelse(skinny_ames_df$SalePricePerSquareFoot>=85 & skinny_ames_df$SalePricePerSquareFoot<100,1,0)
skinny_ames_df$SPSF100 <- ifelse(skinny_ames_df$SalePricePerSquareFoot>=100,1,0)

#Refit MLR_model with the indicator variables 
MLR_model_refit <- lm(SalePrice ~ LotArea+GrLivArea+SPSF40_80+SPSF80_85+SPSF85_100+SPSF100, data=skinny_ames_df)

#Commpute MAE for MLR_model and MLR_model_refit
MAE_MLR_model <-mean(abs(MLR_model$residuals))
MAE_MLR_model_refit <-mean(abs(MLR_model_refit$residuals))

#We are going to fit two models using four continuous predictor variables and one discrete variable 
#One model will have SalePrice as the response variable and the second model will have log(SalePrice) as the response variable
Q5_model <- lm(SalePrice ~ LotArea+GrLivArea+LotFrontage+GarageArea+TotRmsAbvGrd, data=skinny_ames_df)
Q5_logmodel <- lm(log(SalePrice) ~ LotArea+GrLivArea+LotFrontage+GarageArea+TotRmsAbvGrd, data=skinny_ames_df)

#Summary of both models
summary(Q5_model)
summary(Q5_logmodel)

#Check Q5_model for normality and homoscedasticity
qqnorm(Q5_model$residuals,main = 'SalePrice Model Normal Q-Q Plot')
qqline(Q5_model$residuals)

plot(skinny_ames_df$GrLivArea,Q5_model$residuals, xlab='GrLivArea (Sqft)', ylab='SalePrice Model Residuals', main ='SalePrice Residuals vs. GrLivArea')
plot(Q5_model$fitted.values, Q5_model$residuals, xlab='SalePrice Model Fitted Values', ylab='SalePrice Model Residuals')

#Check Q5_logmodel for normality and homoscedasticity
qqnorm(Q5_logmodel$residuals,main = 'Log SalePrice Model Normal Q-Q Plot')
qqline(Q5_logmodel$residuals)

plot(skinny_ames_df$GrLivArea,Q5_logmodel$residuals, xlab='GrLivArea (Sqft)', ylab='Log SalePrice Model Residuals', main =' Log SalePrice Residuals vs. GrLivArea')
plot(Q5_logmodel$fitted.values, Q5_logmodel$residuals, xlab='Log SalePrice Model Fitted Values', ylab='Log SalePrice Model Residuals')

#Calculate MSE and MSA for each model 
MSE_Q5Model <- mean(Q5_model$residuals^2)
MAE_Q5Model <- mean(abs(Q5_model$residuals))

MSE_Q5LogModel <- mean((skinny_ames_df$SalePrice-exp(Q5_logmodel$fitted.values))^2)
MAE_Q5LogModel <- mean(abs(skinny_ames_df$SalePrice-exp(Q5_logmodel$fitted.values)))

MSE_Q5Model/MSE_Q5LogModel
MAE_Q5Model/MAE_Q5LogModel

