############ Chapter 3 R Codes #################
# Simple linear regression:
rm(list = ls())

# Multiple linear regression:
# 3.2.4: ANCOVA: Qualitative and Quantitative predictors with Interaction in the Linear Model:
rm(list = ls())
library(ISLR)
attach(Advertising)
median(radio)
plot(TV, sales, pch = as.numeric(I(radio > 22.9))) #pch: 1 if radio > 22.9 & O otherwise. 
legend ("topleft", inset = 0.05, legend = c("radio < 22.09", "radio > 22.0"), pch = c(0,1)) # topleft: kep legend on the lefttop; legend: name and order of legend.
title ("Sales vs. TV vs. Radio")
cat.radio = I(radio > 22.9) # categorical variable.
esam = lm(sales ~ TV + cat.radio) #set linear model.
coef(esam) #model coefficients
summarY(esam) #model summary. 
abline(a = 4.828, b = 0.046, lwd = 3) # Draw a line. a = intercept, b = slope.
abline(a = 4.828 + 4.794, b = 0.046, lwd = 3, lyt = 2) # Draw a line; more: help(abline).

# Unequal solpe and intercept: Add in above plot.
usim = lm(sales ~ cat.radio + TV*cat.radio)
coef(usim)
abline(a = 7.1, b = 0.03, lwd = 3, col = 2)
abline(a = 7.1 + 0.636, b = 0.03 + 0.028, lwd = 3, col = 2, lyt = 2)

# Continuous interaction:
cim = lm(sales ~ TV + radio + TV*radio)
summary(lm)

# 3.2.5: Linear Models and non0linear relationships:
rm(list = ls())
data(Auto)
names(Auto) #get names of variables in dataset auto.
attach(Auto)
plot (horsepower, mpg)
hp.sq = horsepower*horsepower
qm.hp= lm(mpg ~ horsepower + hp.sq) #quadratic model.
summary(qm.hp)
co = coef(qm.hp) # coefficient of model.
curve(co[1] + co[2]*x + co[3]* x^2, add = T, lwd = 3, col = "red")

# 3.3 Residual assumptions:
plot(horsepower, mpg)
lm.hp = lm(mpg ~ horsepower)
abline(lm.hp)
par(mfrow = c(2,4))
plot(lm.hp, main = "Linear Model")
plot(qm.hp, main = "Quadratic Model")

# Correlated errors;
par(mfrow = c(1,1))
x = 1:50
y = cumsum(rnorm(50))
confint(lm(y~x))
plot(resid(lm(y~x)), type  = "b")
abline (h = 0)
summary(lm(y~x))

# Non-constant Variance:
set.seed(1)
x = 1:100/20
y = rlnorm(length(x), x)
plot(y, x)
abline(lm(y~x))
par(mfrow = c(2,2))
plot(lm(y~x))
z = log(y)
par(mfrow = c(1,1))
plot(x,z)
lm(z~x)
par(mfrow = c(2,2))
plot(lm(z~x))

# Outliars:
set.seed(1)
x = 1:10
y = x +rnorm(length(x))
y1 = y
y1[5] = 0 #replacing 5th row in y to zero
y2 = y
y2[9] = 0 #replacing 9th row in y to zero
m1 = lm(y~x)
m2 = lm(y1~x)
m3 = lm(y2~x)

par(mfrow = c(1,3))
plot(x,y, main = "m1")
abline(m1)
plot(x, y1, main = "m2")
abline(m2)
plot(x, y2, main = "m3")
abline(m3)

#Normality:
qm.hp = lm(mpg ~horsepower + hp.sq)
par(mfrow = c(1,1))
hist(rstandard(qm.hp))
qqnorm(rstandard(qm.hp))
qqline(rstandard(qm.hp))
par(mfrow = c(2,2))
plot(qm.hp)

# Multicollinearity:
# data(package = .packages(all.available = TRUE)) #view all data available.
rm(list = ls())
data(Credit)
credit = Credit
rm(Credit)
# View(credit)
pairs(~Balance + Age + Limit + Rating)
# install.packages("car")
library(car)
vif(lm(Balance ~ Age + Limit + Rating)) #Variance inflation factors.
summary(lm(Balance ~ Age + Limit + Rating))$coeff
summary(lm(Balance ~ Age + Limit))$coeff

# 3.5: Summary:
# Main R pseudo code:
# Fitting:
my.lm(y ~ x1 + x2 + , data = my.data)
my.full.lm = lm(y ~ ., data = my.data)
my.full.lm1 = lm(y ~ . -1 , data = my.data) #remove intercept
my.full.lm2 = lm(y ~ . -x1 , data = my.data) #remove x1 variable.
summary(my.lm) #sumarizing and testing.
confint(my.lm) # Interval estimation. 
predict(my.lm, newdata = data.frame(x1 = , x2 = , ....), type = )

predict(object, newdata, se.fit = FALSE, scale = NULL, df = Inf,
        interval = c("none", "confidence", "prediction"),
        level = 0.95, type = c("response", "terms"),
        terms = NULL, na.action = na.pass,
        pred.var = res.var/weights, weights = 1, ...)

# Diagnostic
plot(my.lm) 
library(car) # needed to test VIF.
vif(my.lm) #Variance inflattion factor

############### Chapter 3 Book Excercises ##################
########### Homework 3 ############################
rm(list = ls()) #Clear environment.
setwd("~/Dropbox/OSU/PhD/SemVISp2021/STAT5063ML/Homeworks/hw3") #Mac.
# install.package("MASS")
library("MASS") # Load MASS package
data(Boston, package = "MASS") #Boston dataset from MASS Package.
attach(Boston) # Attach

q1 = lm(crim ~ zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat + medv)
summary(q1)
