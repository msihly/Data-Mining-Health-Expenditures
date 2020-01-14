# Open HealthExpend (Modified) file to read the data into R
HE = read.csv("HealthExpend (Modified).csv", header = T, na.strings = "?")
fix(HE)
dim(HE)
#
names(HE)
#
# Given qualitative variables such as
# Race, Maristat, Region, Educ, Phstat, Income, and  Indusclass,
# R generates dummy variables automatically.
#
# Contrasts() function returns the coding that R uses for the dummy variables
attach(HE)
contrasts(RACE)
contrasts(MARISTAT)
contrasts(REGION)
contrasts(EDUC)
contrasts(PHSTAT)
contrasts(INCOME)
contrasts(INDUSCLASS)
#
# First model with EXPENDIP as DV
# We exclude EXPENDOP from the model to perform linear regression on one DV
lm.EXPENDIP = lm(EXPENDIP~. -EXPENDOP, data = HE)
summary(lm.EXPENDIP)
#
# After running the first model, we obtain few NA values, which is a
# sign of collinearity problem in the dataset
#
library(car)
vif(lm.EXPENDIP)
# we get an error using vif () function
#
# To find out which variables are highly correlated we use alias()
alias( lm( EXPENDIP ~ ., data = HE))
#
# Create new model by excluding highly correlated variaables
lm.EXPENDIP1 = update(lm.EXPENDIP, ~. - (COLLEGE +  HIGHSCH))
summary(lm.EXPENDIP1)
#
# Run vif() again to check the data
library(car)
vif(lm.EXPENDIP1)
#
# We use Backward Selection Method to decide the optimal model with all the
# variables having p-values below 0.05 or 0.1???
#
# INDUSCLASS has largest p-value,
# meaning that this predictor is not statistically significant
# Next step is to exclude INDUSCLASS variable from the model
lm.EXPENDIP2 = update(lm.EXPENDIP1, ~. - INDUSCLASS)
summary(lm.EXPENDIP2)
#
lm.EXPENDIP3 = update(lm.EXPENDIP2, ~. - RACE)
summary(lm.EXPENDIP3)
#
lm.EXPENDIP4 = update(lm.EXPENDIP3, ~. - MARISTAT)
summary(lm.EXPENDIP4)
#
lm.EXPENDIP5 = update(lm.EXPENDIP4, ~. - USC)
summary(lm.EXPENDIP5)
#
lm.EXPENDIP6 = update(lm.EXPENDIP5, ~. - INCOME)
summary(lm.EXPENDIP6)
#
lm.EXPENDIP7 = update(lm.EXPENDIP6, ~. - ANYLIMIT)
summary(lm.EXPENDIP7)
#
lm.EXPENDIP8 = update(lm.EXPENDIP7, ~. - INSURE)
summary(lm.EXPENDIP8)
#
lm.EXPENDIP9 = update(lm.EXPENDIP8, ~. - MANAGEDCARE)
summary(lm.EXPENDIP9)
#
lm.EXPENDIP10 = update(lm.EXPENDIP9, ~. - FAMSIZE)
summary(lm.EXPENDIP10)
#
lm.EXPENDIP11 = update(lm.EXPENDIP10, ~. - UNEMPLOY)
summary(lm.EXPENDIP11)
#
lm.EXPENDIP12 = update(lm.EXPENDIP11, ~. - MNHPOOR)
summary(lm.EXPENDIP12)
#
# F-stat is growing when insignificant predictors are removed from the model
# R^2 is pretty low, starting with 22% and finishing at 21%
#
#lm.EXPENDIP13 = update(lm.EXPENDIP12, ~. - PHSTAT)
#summary(lm.EXPENDIP13)
#
#lm.EXPENDIP14 = update(lm.EXPENDIP13, ~. - EDUC)
#summary(lm.EXPENDIP14)
#
#lm.EXPENDIP15 = update(lm.EXPENDIP14, ~. - REGION)
#summary(lm.EXPENDIP15)