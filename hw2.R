rm(list = ls())
# Mac:
setwd("//Users//bmishra//Dropbox//OSU//PhD/SemVI Sp2021//STAT5063ML//Homeworks//hw2")
# Office Computer:
# setwd("C:\\Users\\bmishra\\Dropbox\\OSU\\PhD\\SemVISp2021\\STAT5063ML\\Homeworks\\Hw2")
library(readr)
library(ISLR)
library(MASS)

# Chapter 2:

# Q. 8:
# 8.a) Use read.csv() to read data into R.
college  = read.csv(file = "college.csv",  header = TRUE, sep = " ")
head(college)

# 8.b) 
fix(college)
rownames(college) = college [, 1]
college1 = college [, -1]
fix(college1)

# 8. c) 
# 8.c.i: Summary:
summary(college)

# 8.c.ii: Pair Plot
pairs(college[,1:10])

# 8.c.iii. Plot
college = na.omit(college)
plot(college$Private, college$Outstate, col = "blue", xlab = "Private School", ylab = "Out of State Tuition Fee")

# 8.c.iv.Create a new qualitative variable:
Elite = rep("No", nrow(college))
Elite[college$Top10perc >50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)
summary(Elite)
plot(college$Elite, college$Outstate, col = "blue", xlab = "Elite", ylab = "Out of State Tuition Fee")

#8.C.V: Create Histogram: Hist()
par(mfrow = c(2,2))
hist(college$Apps, xlab = "Number of Applications", ylab = "Frequency", col = 9)
hist(college$perc.alumni, col = 6, xlab = "Percentage of Alumni who Donated", ylab = "Frequency",)
hist(college$S.F.Ratio, col=3, breaks=10, xlab = "Student to Faculty Ratio", ylab = "Frequency",)
hist(college$Expend, breaks=100, col = 12, xlab = "Instructional expenditure per student", ylab = "Fequency", border = 12, axes = TRUE, freq = T)

# 8. C. VI. Continuing exploring data:
par(mfrow = c(2,2))
plot(college$Outstate, college$Room.Board, xlab = "Out-of-State Tuition", ylab = "Rooom and Board Cost", col = "red")
plot(college$Outstate, college$Apps, xlab = "Out-of-State Tuition", ylab = "Number of Application Received", col = "purple")
plot(college$Grad.Rate, college$S.F.Ratio, xlab = "Grduation Rate", ylab = "Student to Faculty Ratio", col = "blue")
plot(college$Outstate, college$Grad.Rate, xlab = "Out-of-State Tuition", ylab = "Graduation Rate", col = "green")

# 9:
# 9. a)
data("Auto")
help(Auto)
Auto = na.omit(Auto)
origin1 = as.factor(Auto$origin)
auto = data.frame(Auto, origin1)
summary(auto)
str(auto)

# 9. b)
sapply(Auto[, 1:7], range)
# 9. c)
sapply(Auto[, 1:7], mean)
sapply(Auto[, 1:7], sd)

# 9. d)
newAuto = Auto[-(10:85), ]
# Range, Mean and Standard Deviation.
sapply(newAuto[, 1:7], range)
sapply(newAuto[, 1:7], mean)
sapply(newAuto[, 1:7], sd)

# 9. e)
par(mfrow = c(2,2))
plot(y = Auto$mpg, x = Auto$weight, ylab = "Mileage Per Gallon", xlab = "Weight of Auto", col = "blue")
plot(y = Auto$mpg, x = Auto$cylinders, ylab = "Mileage Per Gallon", xlab = "Number of Cylinders", col = "green")
plot(y = Auto$mpg, x = Auto$year, ylab = "Mileage Per Gallon", xlab = "Year Manufacture", col = "red")
plot(y = Auto$mpg, x = Auto$horsepower, ylab = "Mileage Per Gallon", xlab = "Engine Horse Power", col = "black")

# 9. f)
pairs(Auto)

###############################################################################

#Outside Book:
rm(list = ls())
set.seed(1)
x = seq(from = -2, to = 2, by = .1)
y = 100 + 2*x - x^2 + rnorm(41)

# Problem 1:
xsq = x*x
xcu = xsq*x
fx = 100 + 2*x - x^2 #True Y
f1xhatt = lm(y ~ x) #Linear
f1xhatt
f2xhatt = lm(y ~ x + xsq + xcu) #Quadratic.
f2xhatt
plot(x,y)
# Predicting f1:
f1.linear = round(cbind(y,fx= 100+2*x - x^2, f1xhatt = predict(f1xhatt), error=resid(f1xhatt)),2)
f1.linear [, 2]
# Predicting f2 
f2.quad = round(cbind(y,fx= 100+2*x - x^2, f2xhatt = predict(f2xhatt), error=resid(f2xhatt)),2)
f2.quad [, 2]

# Problem 2:
plot (x, y, col = 1, xlab = "x", ylab = "Y or f-hatts", main = "Data Plot with Estimation Line")
curve (100 + 2*x - x^2, add = TRUE, col = 5, lyt = 2, lwd = 2) #True Y.
curve(98.686 + 1.9561*x, add = TRUE, col = 10, lyt = 4, lwd = 3) #Linear
curve(100.0993 + 1.6444*x -1.0097*x^2 + 0.1238*x^3, add = TRUE, col = 15, lyt = 4, lwd = 4) #Quadratic

# Probelm 3:
truef = function(x) {100 + 2*x - x^2}
truef(0)
predict (f1xhatt, data.frame( x = 0)) #Linear Function
points(0, 100, pch = 9, col = "black") #True Y
points(0, 98.68577, pch = 9, col = "blue") # Linear Function

# Probelm 4:
truef = function(x) {100 + 2*x - x^2}
truef(0)
predict (f2xhatt, data.frame( x = 0, xsq = 0, xcu = 0)) #Quadratic
points(0, 100.0993, pch = 9, col = "green") # Quadratic Function.

# Problem 5:
truef(x = 0)
y[21]
points(x[21], y[21], pch = 9, col = "orange") #

# Problem 6: #Pending Revise Code. Did Manually.
test.data1 = data.frame(x = c(-1, 0, 1), y = c(94, 100, 100))
f1xhatt_pred = predict(f1xhatt, test.data1)
f1xhatt.mse = (sum(test.data1$y-f1hatt_pred)^2)/(length(test.data1$y)) #MSE fmla.
f1hatt.mse
test.data2 = data.frame(x = c(-1, 0, 1), y = c(94, 100, 100))
f2xhatt_pred = predict(f2xhatt, test.data)
f2xhatt.mse = (sum(test.data$y-f2hatt_pred)^2)/(length(test.data$y)) #MSE fmla.
f2hatt.mse

# Problem 7:
set.seed(2)
x7 = seq(from = -2, to = 2, by = .1)
y7 = 100 + 2*x7 - x7^2 + rnorm(41)
x7sq = x7*x7
x7cu = x7sq*x7

fx7 = 100 + 2*x7 - x7^2 #True Y
f1x7hatt = lm(y7 ~ x7) #Linear
f1x7hatt
f2x7hatt = lm(y7 ~ x7 + x7sq + x7cu) #Quadratic.
f2x7hatt

truef7a = function(x7) {100 + 2*x7 - x7^2}
truef7a(0)
predict (f1x7hatt, data.frame( x7 = 0)) #Linear Function

truef7b = function(x7) {100 + 2*x7 - x7^2}
truef7b(0)
predict (f2x7hatt, data.frame( x7 = 0, x7sq = 0, x7cu = 0)) #Quadratic
