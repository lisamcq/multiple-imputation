################################################################
#Purpose: Outline the process for using multiple imputation with 
#         example "walking" data set from mice package
#Author: Lisa McQuarrie
################################################################

#if needed, install the mice package
#install.packages("mice")
#load the mice package
library(mice)
#load the example data set "walking" from the mice package
df <- walking
# the walking data set has two items, YA and YB, that measure the walking 
# disability in samples A, B, and E

#explore the amount of missing values
summary(df)
sum(is.na(df)) # total number of missing values
md.pattern(df)
#look for variables with large proportion of values missing, the number of complete cases,
# and patterns in the missing data 
# (ex. two variables that are always missing on the same records)
# in this case, YA is observed on src=A and E, YB is observed on src=B and E

#Check the distribution of the variables
densityplot(df$age)
table(df$sex)
table(df$YA)
table(df$YB)
table(df$src)

#we need to impute YA and YB. Both are factors (categorical variables).
#specify which imputation method to use
# make.method makes a vector of method names for each variable with missing data
(imp1.method = make.method(df))
#the default imputation method for ordinal categorical variables is the proportional
# odds model. We can change this if we want, but this is a good default.

#next we need to initialize the predictor matrix, which specifies the predictor 
# variables to use in the imputation model
(imp1.pred = make.predictorMatrix(df))
# If we had more variables we may wish to modify this matrix so that only
# some of the variables are used as predictors in the imputation model.
# Setting a matrix element to 0 will mean that the variable in that column
# is not used to impute the variable in that row.

#m = number of imputations
# rule of thumb: 
# set m = total number of rows with missing values / total number of rows *100
# in this data set, that is m = 600/890 *100 =67.4
#because there is randomness involved in imputation, set.seed will ensure 
# you get the same imputed values each time
imp1 = mice(data = df, m = 68, predictorMatrix = imp1.pred, method = imp1.method, 
            set.seed = 1000)

#imp1$imp gives only the imputed observations for each iteration and each variable
imp1$imp
#complete function gives complete imputed data sets. 
# action parameter specifies which of the m data sets is returned 
# (ie action =1 returns the first imputed set)
imp1.complete = complete(imp1, action = 1)

#use the 'with' function to apply the analysis model to each imputed data set
# for example, if we were estimating a proportional odds model for YA with
# age, sex, and src as explanatory variables we would do...
library(MASS) #we need the polr function from MASS to fit the model
mod1 = with(imp1, polr(YA ~ sex+age+src, Hess=TRUE))

#pool the model estimates from each imputed data set with the pool function
mod1result = pool(mod1)
summary(mod1result)
#model estimates after multiple imputation can be interpreted in the usual way
# for the analysis model