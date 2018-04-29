### Final Project
### Author: Tiffany Chen
### Created: 4/22/2018
### Updated: 4/22/2018

## Test whether 
bike.data = read.csv(file = "/Users/tiffanychen/Desktop/2017 - 2018/STAT 318/Bike-Sharing-Dataset/day.csv",
                     header = TRUE)
attach(bike.data)

# investigate for multicollinearity
x.bike = cbind(mnth, temp, weekday, workingday, weathersit, holiday, hum, windspeed)
library(usdm)
vifstep(x.bike, th = 10) # no predictor variables are correlated with each other

# initial model
initial.bike.fit = lm(cnt ~ mnth + temp + weekday + workingday + 
                        weathersit + holiday + hum + windspeed)
scatter.smooth(resid(initial.bike.fit) ~ predict(initial.bike.fit)) # residual plot has linear pattern

# box cox transformation
boxcox(initial.bike.fit) # choose lambda = 0.75


