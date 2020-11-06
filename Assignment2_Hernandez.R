
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

#Refit models for the transformed response variable log(SalePrice)
Refit_LotArea_model <- lm(log(SalePrice) ~ LotArea, data=skinny_ames_df)
Refit_GrLivArea_model <- lm(log(SalePrice) ~ GrLivArea, data=skinny_ames_df)
Refit_MLR_model <- lm(log(SalePrice) ~ LotArea+GrLivArea, data=skinny_ames_df)



#GOF plots for refitted models using log(SalePrice)
qqnorm(Refit_LotArea_model$residuals)
qqline(Refit_LotArea_model$residuals)
qqnorm(Refit_GrLivArea_model$residuals)
qqline(Refit_GrLivArea_model$residuals)
qqnorm(Refit_MLR_model$residuals)
qqline(Refit_MLR_model$residuals)
#Plot for homoscedasticity
plot(skinny_ames_df$LotArea,Refit_LotArea_model$residuals, xlab='Lot Area (Sqft)', ylab='Refit LotArea Model Residuals')
plot(skinny_ames_df$GrLivArea,Refit_GrLivArea_model$residuals, xlab='GrLivArea (Sqft)', ylab='Refit GrLivArea Model Residuals')
plot(skinny_ames_df$LotArea,Refit_MLR_model$residuals, xlab='Lot Area (Sqft)', ylab='Refit MLR Model Residuals')
plot(skinny_ames_df$GrLivArea,Refit_MLR_model$residuals, xlab='GrLivArea (Sqft)', ylab='Refit MLR Model Residuals')
plot(Refit_MLR_model$fitted.values,Refit_MLR_model$residuals, xlab='Refit MLR Model Fitted Values', ylab='Refit MLR Model Residuals')

summary(Refit_MLR_model)
