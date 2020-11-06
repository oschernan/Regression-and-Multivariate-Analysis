#Read in our Ames Housing Data
ames_df <- read.csv('ames_housing_data.csv', header=TRUE, stringsAsFactors = FALSE)

#Create waterfall of drop conditions 
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

#####Data Quality Check Section#####
#Create a subset of the 20 variables we want to complete a data quality check on 
qc_20 <- subset(eligible_population, select = c(SalePrice, LotArea, LotFrontage, MasVnrArea, GarageYrBlt, OverallCond, TotalBsmtSF
                                    ,GrLivArea,GarageArea, PoolArea,ScreenPorch,OpenPorchSF, WoodDeckSF, GarageCars,
                                    OverallQual, TotRmsAbvGrd,Fireplaces, EnclosedPorch, MiscVal, MoSold))

#Create a table displaying the names of our variables 
colnames(qc_20)
quality_check20_table <- colnames(qc_20)
quality_check20_table

#Print out summary of 20 variables in qc_20
summary(qc_20)

#We have a combination of continuous, discrete and ordinal variables
#For our ordinal and discrete variables, we want to create tables 
table(qc_20$OverallCond)
table(qc_20$OverallQual)
table(qc_20$GarageYrBlt)
table(qc_20$GarageCars)
table(qc_20$TotRmsAbvGrd)
table(qc_20$Fireplaces)
table(qc_20$MoSold)


###Initial Exploratory Data Analysis of continuous variables###
plot( qc_20$LotArea, qc_20$GrLivArea, ylab='LotArea', xlab='GrLivArea')
plot( qc_20$LotFrontage, qc_20$TotalBsmtSF, ylab='LotFrontage', xlab='TotalBsmtSF')
plot( qc_20$GarageArea, qc_20$MasVnrArea, ylab='GarageArea', xlab='MasVnrArea')

boxplot(qc_20$LotArea, xlab='LotArea')
boxplot(qc_20$GrLivArea, xlab='GrLivArea')

###Initial Exploratory Data Analysis of categorical variables###
par(mfrow=c(2,2))
hist(qc_20$OverallCond, main = "Histogram of OverallCond", xlab= "OverallCond", col = 'red')
hist(qc_20$GarageCars, main="Histogram of GarageCars", xlab= "GarageCars", col='blue')
hist(qc_20$Fireplaces, main = "Histogram of Fireplaces", xlab="Fireplaces", col='green')
hist(qc_20$TotRmsAbvGrd, main="Histogram of TotRmsAbvGrd", xlab= "TotRmsAbvGrd", col='purple')
par(mfrow=c(1,1))


##Initial Exploratory Data Analysis for Modeling with SalePrice###
plot(qc_20$SalePrice, qc_20$LotArea,ylab='SalesPrice', xlab='LotArea')
boxplot(qc_20$SalePrice~qc_20$OverallCond, data=qc_20, ylab='SalesPrice', xlab='OverallCond')
plot(qc_20$SalePrice, qc_20$GrLivArea,ylab='SalesPrice', xlab='GrLivArea')

##Initial Exploratory Data Analysis for Modeling with log(SalePrice)###
plot(log(qc_20$SalePrice), qc_20$LotArea,ylab='log(SalesPrice)', xlab='LotArea')
boxplot(log(qc_20$SalePrice)~qc_20$OverallCond, data=qc_20, ylab='log(SalesPrice)', xlab='OverallCond')
plot(log(qc_20$SalePrice), qc_20$GrLivArea,ylab='log(SalesPrice)', xlab='GrLivArea')


