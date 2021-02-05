###############################
rm(list = ls())
setwd(
  "/Users/bmishra/Dropbox/OSU/PhD/Semester VI, Spring 2021/STAT 5063 ML/Lectures/ROutputs"
)
getwd()
#################################
# MSE
set.seed(1)
x = 1:3
y = rnorm(3, mean = x) # y = x +rnorm(3)
par(mfrow = c(1, 1))
plot(x, y) #linear model.
my.lm = lm(y ~ x)
my.lm
abline(my.lm) #

xsq = x * x
my.qm = lm(y ~ x + xsq)
my.qm
#Quadratic model.
curve(
  -3.2695 + 4.5542 * x - 0.9147 * x ^ 2,
  add = T,
  col = 2,
  lty = 2
)
abline(0, 1, lwd = 3)
# True f above: f(x) = x because we estimated y = x + e using y = rnorm(3, mean = x) # y = x +rnorm(3).

#two different fhatts are estimated.
# fhatt1(x) = -0.22 +0.90x # my.lm
# fhatt2(x) = -3.27 + 4.55x -0.91*x^2 # my.qm

# Which model has smallest Training MSE?
# Training MSE is the average distance y has from its predicted value.
# Ans: quadratic model has small training MSE.
# R-squared is not a right measure of model fit, unless two models has same numbers of paramteters.
# Which model has smallest Test MSE?
# Ans: linear model has small test MSE.

# If sample size is 10, we can always find 9th degree polnomial that gives us training MSE (training error) 0. We always can make our model to very flexible enough to make out training error  = 0. But our model might not have a good prediction of y on a new dataset. The test MSE is going to high if we fit a very flexible model. Out test MSE falls and reach to minimum as the flexibility increase As it falls, at some point the test MSE start to rise sharply. The minimum test MSE before it starts to rise should ideally identify the ideal model. Beyond the minimum test MSE, the model starts to overfit and the variance increases as we overfit the model. Test MSE is always going to be more or less always larger than training MSE. But excessively large test MSE in comparison to training MSE is probelmatic.

# Cross Validation
# In practice, we need to use some of the training data to estimate f and other test data to estimate the test MSE. This is called cross validation. For this, we split our data (eg. 90% and 10%) and leave some data from our dataset to use as test dataset to validate our model.

# Bias Vs Variance
# Expected (actual value - predicted value) = Variance of predicted f + squared bias of predicted f + variance of error of predicted value.
# Var(e) is irreducible error.
# Variance of estimators tends to be zero as we have infinitely large sample.s Even if we have slightly flexible model (non-linear model), the variance is zero for large sample size and the bias is always zero in flexible model for any n.
# Which estimator has a smaller MSE for fixed n in flexile model? Ans: Both have small variance for large n and both of model are unbiased. The discrepancies in MSEs across model be large when we have small sample size (n).

# So, if the data are generated in complex way (more flexible method such as quadratic equations) and if we use simple model (non-flexile models), we have bias. If we use flexile models, we don't have bias. If the data are generated in simple way, both models are unbiased. The variances of both estimators are small for large n but for small n

# Bias is only zero if model is flexible enough. Variane is larger for more flexible model but if we have large sample, the variance is always zero. The variance innitially is small and become zero very soo for non-flexible or simple model, but the MSE is never zero because there is always some bias contributing towards MSE. and Irreducile error Var(e) cannot be reduced.

# Large sample size ==> can use complex/flexile model to reduce bias and small sampel size ==> simple and biased due to reduction in variance. Bias never goes away for large sample.

# Classification problem:
# We want o minimize test error. Training error is not of much interest.

# Example 3:
#Iris Classification illustration
data("iris")
library(MASS)
iris2 = iris[51:150,]
attach(iris2)
plot(Petal.Length,
     Sepal.Width,
     pch = c(rep(1, 50), rep(2, 50)),
     col = c(rep(1, 50), rep(2, 50)))
abline(-1.5,0.9, lwd = 3)

# Bayes Classifier
library(MASS)
lda.model = lda(Species ~ Petal.Length + Sepal.Width, data = iris)
predict(lda.model)$posterior[1:5,]

# K-nearest Neighbors:
# Most flexible and non-parameric.
# First pick total observations based on distance to nearest neighbor. Then count which category(ies) each observations falls and calcualte conditional probability based on Bayes classifier.
