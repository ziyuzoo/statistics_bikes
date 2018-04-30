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
plot(initial.bike.fit) # possible outliers: Observations 203, 204, 442
scatter.smooth(resid(initial.bike.fit) ~ predict(initial.bike.fit)) # residual plot has linear pattern

# determine outlying observations
rstudent(initial.bike.fit)
qt(0.975, df = 731 - 8 - 1)

# remove possible outliers
bike.revised = bike.data[-c(203, 204, 442),]
model2 = lm(cnt ~ mnth + temp + weekday + workingday + 
              weathersit + holiday + hum + windspeed, data = data.frame(bike.revised))
scatter.smooth(resid(model2) ~ predict(model2))

