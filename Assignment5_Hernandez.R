#Load MASS library
library(MASS)

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

# Set the seed on the random number generator so you get the same split every time that you run the code
set.seed(123)
eligible_population$u <- runif(n=dim(eligible_population)[1],min=0,max=1)

# Define these two variables for later use
eligible_population$QualityIndex <- eligible_population$OverallQual*eligible_population$OverallCond
eligible_population$TotalSqftCalc <- eligible_population$BsmtFinSF1+eligible_population$BsmtFinSF2+eligible_population$GrLivArea

# Create train/test split 
train.df <- subset(eligible_population, u<0.70) 
test.df  <- subset(eligible_population, u>=0.70)

# Create train.clean dataframe to help variable selection process 
# Create keep.list 
keep.list <- c('QualityIndex', 'TotalSqftCalc','LotArea','GarageArea','MiscVal','WoodDeckSF','OpenPorchSF','LowQualFinSF',	
              'FullBath','TotalBsmtSF','BsmtUnfSF','GarageCars','HalfBath','BedroomAbvGr','TotRmsAbvGrd', 'SalePrice')

train.clean <- train.df[,(names(eligible_population) %in% keep.list)]

# Provide summary of train.clean to see if there are any missing values
summary(train.clean)

#Need to specifiy the upper model and lower models
# Define the upper model as the FULL model 
upper.lm <- lm(SalePrice ~ .,data=train.clean)
summary(upper.lm) 

# Define the lower model as the Intercept model 
lower.lm <- lm(SalePrice ~ 1,data=train.clean) 
summary(lower.lm)

# Need a simple linear regression model  to initialize stepwise selection 
sqft.lm <- lm(SalePrice ~ TotalSqftCalc,data=train.clean) 
summary(sqft.lm) 

library(sjPlot)
library(sjlabelled)

#Create forward.lm
forward.lm <- stepAIC(object=lower.lm,scope=list(upper=formula(upper.lm),lower=~1), direction=c('forward'))
summary(forward.lm)
sjt.lm(forward.lm)

#Create backward.lm
backward.lm <- stepAIC(object=upper.lm,direction=c('backward')) 
summary(backward.lm)
sjt.lm(backward.lm)

#Create stepwise.lm
stepwise.lm <- stepAIC(object=sqft.lm,scope=list(upper=formula(upper.lm),lower=~1), direction=c('both'))
summary(stepwise.lm)
sjt.lm(stepwise.lm)

#Create junk.lm
junk.lm <- lm(SalePrice~OverallQual + OverallCond + QualityIndex + GrLivArea + TotalSqftCalc, data=train.df)
summary(junk.lm)
sjt.lm(junk.lm)

#Calculate correlation within predictor variables 
cor(train.df$OverallQual, train.df$QualityIndex)
cor(train.df$GrLivArea, train.df$TotalSqftCalc)
cor(train.df$OverallCond, train.df$QualityIndex)

#Compute the VIFs for the variable selection models
library(car)
sort(vif(forward.lm),decreasing=TRUE)
sort(vif(backward.lm),decreasing=TRUE) 
sort(vif(stepwise.lm),decreasing=TRUE)
sort(vif(junk.lm),decreasing=TRUE)

#Compute AIC, BIC, MSE and MAE for each of our models 
AIC(forward.lm)
AIC(backward.lm)
AIC(stepwise.lm)
AIC(junk.lm)

BIC(forward.lm)
BIC(backward.lm)
BIC(stepwise.lm)
BIC(junk.lm)

mse.forward <- mean(forward.lm$residuals^2)
mae.forward <- mean(abs(forward.lm$residuals))

mse.backward <- mean(backward.lm$residuals^2)
mae.backward <- mean(abs(backward.lm$residuals))

mse.stepwise <- mean(stepwise.lm$residuals^2)
mae.stepwise <- mean(abs(stepwise.lm$residuals))

mse.junk <- mean(junk.lm$residuals^2)
mae.junk <- mean(abs(junk.lm$residuals))

#Display MSE and MAE output
mse.forward
mse.backward
mse.stepwise
mse.junk

mae.forward
mae.backward
mae.stepwise
mae.junk

#Score each model on out-of-sample data set
forward.test <- predict(forward.lm, newdata = test.df)

backward.test <- predict(backward.lm, newdata=test.df)

stepwise.test <- predict(stepwise.lm, newdata=test.df)

junk.test <- predict(junk.lm, newdata=test.df)

#Calculate residuals for predicted SalePrice and test.df SalePrice
automated_residuals <- test.df$SalePrice - forward.test

junk_residuals <-  test.df$SalePrice  -junk.test    


#Calcluate MAE and MSE for each model 

automated.mse <- mean(automated_residuals^2)
automated.mae <- mean(abs(automated_residuals))

junk.mse <- mean(junk_residiuals^2)
junk.mae <- mean(abs(junk_residuals))

#Display results
automated.mse
automated.mae
junk.mse
junk.mae

#Define PredictionGuide variable 
#Training Data
# Abs Pct Error 
forward.pct <- abs(forward.lm$residuals)/train.clean$SalePrice
backward.pct <- abs(backward.lm$residuals)/train.clean$SalePrice
stepwise.pct <- abs(stepwise.lm$residuals)/train.clean$SalePrice
junk.pct <- abs(junk.lm$residuals)/train.df$SalePrice


#Assign Prediction Grades
forward.PredictionGrade <- ifelse(forward.pct<=0.10,'Grade 1: [0.0.10]', 
                                  ifelse(forward.pct<=0.15,'Grade 2: (0.10,0.15]',
                                         ifelse(forward.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                'Grade 4: (0.25+]'))) 
backward.PredictionGrade <- ifelse(backward.pct<=0.10,'Grade 1: [0.0.10]', 
                                  ifelse(backward.pct<=0.15,'Grade 2: (0.10,0.15]',
                                         ifelse(backward.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                'Grade 4: (0.25+]'))) 
stepwise.PredictionGrade <- ifelse(stepwise.pct<=0.10,'Grade 1: [0.0.10]', 
                                  ifelse(stepwise.pct<=0.15,'Grade 2: (0.10,0.15]',
                                         ifelse(stepwise.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                'Grade 4: (0.25+]'))) 
junk.PredictionGrade <- ifelse(junk.pct<=0.10,'Grade 1: [0.0.10]', 
                                  ifelse(junk.pct<=0.15,'Grade 2: (0.10,0.15]',
                                         ifelse(junk.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                'Grade 4: (0.25+]'))) 


forward.trainTable <- table(forward.PredictionGrade) 
forward.trainTable/sum(forward.trainTable)                                        

backward.trainTable <- table(backward.PredictionGrade) 
backward.trainTable/sum(backward.trainTable)                                        

stepwise.trainTable <- table(stepwise.PredictionGrade) 
stepwise.trainTable/sum(stepwise.trainTable)                                        

junk.trainTable <- table(junk.PredictionGrade) 
junk.trainTable/sum(junk.trainTable)                                        


#Test Data
# Abs Pct Error 
forward.testPCT <- abs(test.df$SalePrice-forward.test)/test.df$SalePrice 
backward.testPCT <- abs(test.df$SalePrice-backward.test)/test.df$SalePrice 
stepwise.testPCT <- abs(test.df$SalePrice-stepwise.test)/test.df$SalePrice 
junk.testPCT <- abs(test.df$SalePrice-junk.test)/test.df$SalePrice

#Assign Prediction Grade
forward.testPredictionGrade <- ifelse(forward.testPCT<=0.10,'Grade 1: [0.0.10]', 
                                      ifelse(forward.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                             ifelse(forward.testPCT<=0.25,'Grade 3: (0.15,0.25]', 
                                                    'Grade 4: (0.25+]')))
backward.testPredictionGrade <- ifelse(backward.testPCT<=0.10,'Grade 1: [0.0.10]', 
                                      ifelse(backward.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                             ifelse(backward.testPCT<=0.25,'Grade 3: (0.15,0.25]', 
                                                    'Grade 4: (0.25+]')))
stepwise.testPredictionGrade <- ifelse(stepwise.testPCT<=0.10,'Grade 1: [0.0.10]', 
                                      ifelse(stepwise.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                             ifelse(stepwise.testPCT<=0.25,'Grade 3: (0.15,0.25]', 
                                                    'Grade 4: (0.25+]')))
junk.testPredictionGrade <- ifelse(junk.testPCT<=0.10,'Grade 1: [0.0.10]', 
                                      ifelse(junk.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                             ifelse(junk.testPCT<=0.25,'Grade 3: (0.15,0.25]', 
                                                    'Grade 4: (0.25+]')))

forward.testTable <-table(forward.testPredictionGrade) 
forward.testTable/sum(forward.testTable)
                      
backward.testTable <-table(backward.testPredictionGrade) 
backward.testTable/sum(backward.testTable)

stepwise.testTable <-table(stepwise.testPredictionGrade) 
stepwise.testTable/sum(stepwise.testTable)

junk.testTable <-table(junk.testPredictionGrade) 
junk.testTable/sum(junk.testTable)
                                                                             