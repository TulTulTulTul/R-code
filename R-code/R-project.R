# Project
rm(list=ls())
#clear the termnal in R
cat("\014") 
#clear all the graphics plot in R studio
graphics.off()
### read data from csv file
x <-read.csv("C:\\data\\Folds5x2_pp.csv",header=TRUE)
x
str(x)
summary(x)
colnames(x)
# Features consist of hourly average ambient variables 
# - Temperature (T) in the range 1.81?C and 37.11?C,
# - Ambient Pressure (AP) in the range 992.89-1033.30 milibar,
# - Relative Humidity (RH) in the range 25.56% to 100.16%
#   - Exhaust Vacuum (V) in teh range 25.36-81.56 cm Hg
# - Net hourly electrical energy output (EP) 420.26-495.76 MW

# scatterplot 
library(car)
pairs(~PE+AT+V+AP+RH,
      main = "Scatterplots:  Prediction of full load electrical power output of a base load operated combined cycle power plant", 
      data=x, 
      labels=c("Power Output","X1:Temperature (AT)","X2:Exhasut Vaccum(V)",
             "X3:Ambient Pressure(AP)","X4:Relative Humidity(RH)","X4:"))
#

