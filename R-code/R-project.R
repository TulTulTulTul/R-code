# Project
rm(list=ls())
#clear the termnal in R
cat("\014") 
#clear all the graphics plot in R studio
graphics.off()
### read data from csv file
x <-read.csv("C:\\data\\Folds5x2_pp.csv",header=TRUE)
str(x)
summary(x)
colnames(x)
# Features consist of hourly average ambient variables 
# - Temperature (T) in the range 1.81?C and 37.11?C,
# - Ambient Pressure (AP) in the range 992.89-1033.30 milibar,
# - Relative Humidity (RH) in the range 25.56% to 100.16%
#   - Exhaust Vacuum (V) in teh range 25.36-81.56 cm Hg
# - Net hourly electrical energy output (EP) 420.26-495.76 MW

#########
# bloxplot measures the outlier in the data
#performing boxplot before resampling the data
########

par(mfrow=c(1,5))
boxplot(x$AT)
boxplot(x$V)
boxplot(x$AP)
boxplot(x$RH)
boxplot(x$PE)
par(mfrow=c(1,1))
boxplot(x,main = "Boxplot of all the variable")
###figure shows presence of the outlier in two variabels
### PRESSURE and Relative Humidity shows there is presence of the outlier
### The circular points in the box plot is the evidence of the outlier.

#########
# Total of 9568 data is in the dataset.
# To make easy for calculation 
#2000 samples were selected randomly from the dataset
library(dplyr)
data = sample_n(x,2000)

### again taking the boxplot of the newly sampled data for the outliers
par(mfrow=c(1,5))
boxplot(data$AT)
boxplot(data$V)
boxplot(data$AP)
boxplot(data$RH)
boxplot(data$PE)
par(mfrow=c(1,1))
boxplot(data,main = "Boxplot of Sampled Data")
###figure shows presence of the outlier in two variabels
### PRESSURE and Relative Humidity shows there is presence of the outlier
### The circular points in the box plot is the evidence of the outlier.

#checking Histogram of all the variable and comparing against its log transformation
data$logPE <- log(data$PE)
data$logAT <- log(data$AT)
data$logV <- log(data$V)
data$logAP <- log(data$AP)
data$logRH <- log(data$RH)
par(mfrow=c(2,5))
hist(data$PE)
hist(data$AT)
hist(data$AP)
hist(data$RH)
hist(data$V)
hist(data$logPE)
hist(data$logAT)
hist(data$logAP)
hist(data$logRH)
hist(data$logV)
par(mfrow =c(1,1))


# scatterplot 
library(car)
pairs(~PE+AT+V+AP+RH,
      main = "Scatterplots:  Prediction of full load electrical power output of a base load operated combined cycle power plant", 
      data=data, 
      # diagonal = "histogram"
      labels=c("Power Output","X1:Temperature (AT)","X2:Exhasut Vaccum(V)",
             "X3:Ambient Pressure(AP)","X4:Relative Humidity(RH)","X4:"))
scatterplotMatrix(data,spread=FALSE, smoother.args = list(lty=2),diagonal = 'histogram')
###########################
####ANALYSIS:
## 1. Linear Negative relation between Temperature and Power Output
## 2. Linear Negative Relation between Exhasut Vaccum and Power Output
## 3. Positive Linear Relation between Ambient Pressure and Power Output
## 4. It seems a positive relation exits between Power Output and Relative Humidity
## 5. A positive linear relationship exist between Temperature and Humidity.
## 6. It seems negative linear relationship exists between Temperature and Ambient Pressure
## 7. It seems negative linear relationship exists between Temperature and Relative Humidity
## 8. No relationship between Exhasut Vaccum and Ambient Pressure
## 9. It seems no relationship exists between ambient pressure and Relative Humidity 
############################


##### Purpose modal 
model <- lm(PE~AT+V+AP+RH, data=data)
summary(model)
## Shows that every parameters are significant.
#### Summary analysis
## All the coefficients are significant
### Intercept is not significant and not interpretable as all the coefficients are significant
## All the independent variables in regression model are significant
## So the intercept has no intrinsic meaning

anova(model)
## Sequential Anova Test.
## Approach Top to down
## seems every parameters are significant

###### Residual normality 
qqnorm(model$residuals)
qqline(model$residuals)
## Most of the data follows the normal linear trend.
## It seems residuals are normal. But, further inverstigation is required

##### Histogram of the residuals
hist(model$residuals)
## It seems the histogram is normal

##### Performing the shapiro test of normality
shapiro.test(model$residuals)
## It shows that the residual distribution isnot normal

#### Data investigation for outliers
par(mfrow = c(2,2))
plot(model)
par(mfrow =c(1,1))

## boxplot of the all the data 
## check the normality of the data
boxplot(data)

##############################################
# found that the residuals are not normal 
# taking log of the PE 
##############################################

#log transformed modal
data$logPE <- log(data$PE)
model <- lm(logPE~AT+V+AP+RH,data=data)
summary(model)
anova(model)

par(mfrow=c(2,2))
plot(model)

#residual normality 
qqnorm(model$residuals)
qqline(model$residuals)

# shapiro test of normality
shapiro.test(model$residuals)
hist(model$residuals)

####################################################################################
# Taking log transformation of response didn't provide normal residual distribution
####################################################################################

#log of all variable transformed modal

model <- lm(logPE~logAT+logV+logAP+logRH,data=data)
summary(model)
anova(model)

par(mfrow=c(2,2))
plot(model)

#residual normality 
qqnorm(model$residuals)
qqline(model$residuals)

# shapiro test of normality
shapiro.test(model$residuals)
hist(model$residuals)

####################################################################################
# Taking log transformation of all data  didn't provide normal residual distribution
####################################################################################
