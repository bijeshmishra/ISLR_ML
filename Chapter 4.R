# Chapter 4:
rm(list = ls()) #Clear environment. #Run manually.

### 4.1 Classification: Motivation and Overview
library(ISLR)
help(Default)
plot(Default, pch = ".")
dim(Default)
Default[1:3,]
summary(Default)

logit = function(b0 = 0, b1 = 1, xlim = NULL) {
  curve(exp(b0 + b1*x)/(1 + exp(b0 + b1*x)),
          ylab = "p(x)",
          add = TRUE, xlim=xlim, lwd=3)}

logit(b0=0,b1=1, xlim = c(-10,10))
logit(b0=5, b1=1)
logit(b0 = -5, b1=1)
logit(b0=0, b1=-1)
logit(b0=0, b1= -.5)
logit(b0=0, b1 = -2)
logit(b0=0, b1 = -.2)