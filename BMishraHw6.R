# Q3: LASSO.
# Q3.A:

moma.q3 = model.matrix(snapchat ~ . -1, hw6.data) # Design Matrix.
y.q3 = hw6.data$snapchat # y for design matrix.
row1.momaq3 = moma.q3[1,] # First Row of Design Matrix.
row1.momaq3
cat( "Q3A Answer: Using the first row of design matrix, 
     the person was not enrolled in STAT 5063 and is not a pinterest user." )

# Q3.B:
train.moma.q3 = moma.q3[2:47,] #Training set w/o Ist row od design matrix.
train.y.q3 = y.q3[2:47] # y without first row of Design matrix.
test.moma.q3 = moma.q3[1,] # First Row of Design Matrix.

# Step 1: Get lambda estimate, Cross Validation (CV) w/ Test Set.
set.seed(1)
q3.cv.glmnet = cv.glmnet(x = train.moma.q3, #Dependent Variables.
                         y = train.y.q3, # Snapchat: 1/0.
                         family = "binomial",  # Logistic Regression.
                         type.measure = "class", # Compute Classification Error.
                         alpha = 1, #1 is LASSO ; 0 is Ridge Regression.
                         nfolds = length(train.y.q3)) # K-fold = # Obs. for LOOCV? 
plot(q3.cv.glmnet)

# Step 2: Get Minimum Lambda (Tuning Parameter) to tune the model.
names(q3.cv.glmnet) # names of elements of q3.cv.glmnet fit.
lambda.hat.q3 = q3.cv.glmnet$lambda.min # Lambda.hat or Optimum Lambda.
round(lambda.hat.q3, 3) # The LOOCV estimate for optimum lambda that minimize LOOCV test error
cat( "Answer: The LOOCV estimate for lambda is", 
     round(lambda.hat.q3,3))


# Q3.C: Final Prediction Equation for Pr(Snapchat): STRICTLY DOUBLE CHECK.
# Step 3: Use "lambda.min" to get LASSO (Or Ridge Regression) estimates:
set.seed(1)
q3.glmnet = glmnet(x = moma.q3, # Independent Varia bles
                   y = y.q3, # Dependent Variables 
                   family = "binomial", #Logistic Regression.
                   alpha = 1, #LASSO
                   nfolds = length(train.y.q3), # K-fold = # Obs for LOOCV ?
                   lambda = lambda.hat.q3) #Optimum Lambda.
coef(q3.glmnet) #Coefficients. 
# Predict on Hold Out Data (ie. newx  = row1.momaq3): Row1 entered manually.
predict.q3c = predict(q3.glmnet, newx = 
                        data.matrix(
                          cbind(
                            genderF = 0,
                            genderM = 1,
                            classSTAT5063 = 0,
                            hasclass = 1,
                            txtsent = 1,
                            txtrec = 1,
                            fbtime  = 30,
                            pinterestY= 0, 
                            introvert = 8,
                            year = 2016)))
pred.prob.q3c = exp(predict.q3c)/(1 + exp(predict.q3c)) # Pr(Snapchat)
round(pred.prob.q3c, 3) #Predicted probability of Snapchat Pr(Snapchat)


# Q3.D: Estimated Probability of Student Having Snapchat Account. DOUBLE CHECK
newstu.q3d = data.matrix(cbind(
  genderF = 0,
  genderM = 1,
  pinterestY= 1,
  classSTAT5063 = 1,
  hasclass = 200,
  txtsent = 100,
  txtrec = 100,
  fbtime  = 60,
  introvert = 5,
  year = 2021))
predict.q3d = predict(q3.glmnet, 
                      newx = newstu.q3d)
pred.prob.q3d = exp(predict.q3d)/(1 + exp(predict.q3d))

cat("Q3.D Answer: Estimated probability of a student having a Snapchat account given that 
they are: male, having pinterest account, are enrolled in STAT 5063, HSClass is 200, 
txtsent is 100, txtrec is 100, Fbtime is 60, introvert is 5 and year in 2021 =", round(pred.prob.q3d, 3), ".")



# Q4.A: Ridge Regression:
# moma.q3a is Model Design Matrix.
moma.q4 = model.matrix(snapchat ~ . -1, hw6.data)
y.q4 = hw6.data$snapchat
q4.fit = glmnet(x = moma.q4, 
                y = y.q4, 
                family = "binomial",
                alpha = 0,# Ridge Regression. 1 = LASSO.
                lambda = c(0, 10, 1000))
names(q4.fit)
q4.fit$lambda
coef(q4.fit, s = c(0, 10, 1000))


# Q4.A: Ridge Regression:
# moma.q3a is Model Design Matrix X and y.q4 is dependent variable.
moma.q4 = model.matrix(snapchat ~ . -1, hw6.data)
y.q4 = hw6.data$snapchat

# Step 1: Get lambda estimate, Cross Validation (CV) w/ Test Set.
set.seed(1)
q4.cv.glmnet = cv.glmnet(x = moma.q4,
                         y = y.q4, # Snapchat: 1/0.
                         family = "binomial",  # Logistic Regression.
                         alpha = 0, # Ridge Regression. 1 = LASSO.
                         nfolds = 10) # K = 10 Fold CV.
plot(q4.cv.glmnet)
# Step 2: Get Minimum Lambda (Tuning Parameter) to tune the model.
lambda.hat.q4 = q4.cv.glmnet$lambda.min #Lambda.hat
cat("Q4.A Answer: The LOOCV Estimate for Lambda = ",
    round(lambda.hat.q4, 3))
# Step 3: Use "lambda.min.q4" (lambda.hat) to get LASSO estimates:
set.seed(1)
q4.glmnet = glmnet(x = moma.q4,
                   y = y.q4,
                   family = "binomial",
                   alpha = 0,
                   lambda = lambda.hat.q4)
coef(q4.glmnet)