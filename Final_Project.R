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

# determine outlying observations
rstudent(initial.bike.fit)
qt(0.975, df = 731 - 8 - 1)

# remove possible outliers
bike.revised = bike.data[-c(203, 204, 442, 657, 645, 638, 631, 632, 457, 448, 442, 358,
                            359, 239, 205, 210, 69),]
model2 = lm(cnt ~ mnth + temp + weekday + workingday + 
              weathersit + holiday + hum + windspeed, data = data.frame(bike.revised))
scatter.smooth(resid(model2) ~ predict(model2)) # clear curvilinear pattern

# investigate possible models

# step function
step(model2, direction = "both", k = log(728))
step.fit = lm(cnt ~ mnth + temp + weathersit + hum + windspeed, 
              data = data.frame(bike.revised))
scatter.smooth(resid(step.fit) ~ predict(step.fit))

press(step.fit)

# mallow's cp
library(leaps)
result = leaps(x.bike, cnt, method = "Cp")
which.min(result$Cp)
result$which[59,]
mallow.fit = lm(cnt ~ mnth + temp + weekday + workingday + weathersit + holiday + hum + windspeed,
                data = data.frame(bike.revised))
scatter.smooth(resid(mallow.fit) ~ predict(mallow.fit))

press(mallow.fit)

# adjr2
result.adjr2 = leaps(x.bike, cnt, method = "adjr2")
which.max(result.adjr2$adjr2)
result.adjr2$which[67,] # no predictor variables removed

# choose mallow's cp model as final model
# do a box cox transformation

boxcox(mallow.fit) # choose lambda = 0.75
model3 = lm(cnt^0.75 ~ mnth + temp + weekday + workingday + 
              weathersit + holiday + hum + windspeed, data = data.frame(bike.revised))
scatter.smooth(resid(model3) ~ predict(model3))

# box cox made it worse so consider 2nd order terms --> Interaction Model



