# Data Loading, Processing and Package Loading:
rm(list = ls())
library(MASS)
library(boot)
library(ISLR)
library(class)
library(readxl)
# studentdata2019 = read_excel("Dropbox//OSU/PhD//SemVISp2021//STAT5063ML//Data//StudentData2019.xlsx") #office
studentdata2019 = read_excel("//Users//bmishra//Dropbox//OSU//PhD//SemVISp2021//STAT5063ML//Data//StudentData2019.xlsx") #mac
hw5.data = setNames(studentdata2019,
                    tolower(names(studentdata2019))) #lower case names.
# hw5.data = as.data.frame(cbind(hw5.data[,3:6],
#                               hw5.data[,9]))
attach(as.data.frame(hw5.data, pos = 2L, 
                     warn.conflicts = F))
hw5.names = names(hw5.data) #names of dataset.
as.data.frame(hw5.data[c(4,36),])

# Q1:
# help(lda)
# help(qda)
# train.q1 = sample(length(hw5.data$pinterest), round((length(hw5.data$pinterest)/2), 0))
# test.q1 = hw5.data$pinterest[-train.q1]

# Q1.A:
# Full LDA Model: Trianing Error
set.seed(1)
lda.fm = lda(pinterest ~ txtsent + txtrec + hsclass + fbtime + introvert,
             data = hw5.data) # LDA Full Model
ldafm.trr = mean(lda.fm$class != pinterest) # LDA FM Training error rate
ldafm.trr
# ldafm.predict = predict(lda.fm, 
#                         newdata = hw5.data) #LDA FM Prediction for new data
# ldafm.confmat = table(hw5.data$pinterest, 
#                       ldafm.predict$class) #LDA FM Confusion matrix
# ldafm.TrErr = c(ldafm.confmat[1,2] + 
#                   ldafm.confmat[2,1]) # LDA Full Model Trainig Error

# Full LDA Model: Test Error
set.seed(1)
lda.fmCV = lda(pinterest ~ txtsent + txtrec + hsclass + fbtime + introvert,
             data = hw5.data, 
             CV = TRUE) # LDA Full Model + LOOCV
ldafm.CV = mean(lda.fmCV$class != pinterest) # LDA FM Test error rate.
ldafm.CV
# ldafmCV.predict = predict(lda.fmCV, 
#                           newdata = hw5.data) #LDA FM Prediction for new data.
# ldafmCV.confmat = table(hw5.data$pinterest, 
#                         ldafmCV.predict$class) #Confusion matrix.
# ldafm.TstErr = c(ldafmCV.confmat[1,2] + 
#                    ldafmCV.confmat[2,1]) # LDA Full Model Test Error

# Partial LDA Model: Training Error
set.seed(1)
lda.pm = lda(pinterest ~ txtsent, 
             data = hw5.data) #LDA Partial Model
ldapm.trr = mean(lda.pm$class != pinterest) # LDA PM Training error rate.
ldapm.trr
# ldapm.predict = predict(lda.pm, 
#                         newdata = hw5.data) #LDA PM Prediction for new data
# ldapm.confmat = table(hw5.data$pinterest, 
#                       ldapm.predict$class) #LDA PM Confusion matrix
# ldapm.TrErr = c(ldapm.confmat[1,2] + 
#                   ldapm.confmat[2,1]) # LDA PM Trainig Error

# Partial LDA Model: Test Error
set.seed(1)
lda.pmCV = lda(pinterest ~ txtsent, 
               data = hw5.data, 
               CV = TRUE) #LDA Partial Model + LOOCV
ldapm.CV = mean(lda.pmCV$class != pinterest) # LDA PM Test error rate.
ldapm.CV
# ldapmCV.predict = predict(lda.pmCV, 
#                           newdata = hw5.data) #LDA FM Prediction for new data.
# ldapmCV.confmat = table(hw5.data$pinterest, 
#                         ldapmCV.predict$class) #LDA PM Confusion matrix.
# ldapm.TstErr = c(ldapmCV.confmat[1,2] + 
#                    ldapmCV.confmat[2,1]) # LDA PM Test Error


# Full QDA Model: Trianing Error
set.seed(1)
qda.fm = qda(pinterest ~ txtsent + txtrec + hsclass + fbtime + introvert,
             data = hw5.data) # QDA Full Model
qdafm.trr = mean(qda.fm$class != pinterest) # QDA FM Training error rate
qdafm.trr
# qdafm.predict = predict(qda.fm, 
#                         newdata = hw5.data) #QDA FM Prediction for new data
# qdafm.confmat = table(hw5.data$pinterest, 
#                       qdafm.predict$class) #QDA FM Confusion matrix
# qdafm.TrErr = c(qdafm.confmat[1,2] + 
#                   qdafm.confmat[2,1]) # QDA Full Model Trainig Error

# Full QDA Model: Test Error
set.seed(1)
qda.fmCV = qda(pinterest ~ txtsent + txtrec + hsclass + fbtime + introvert,
               data = hw5.data, 
               CV = TRUE) # QDA Full Model + LOOCV
qdafm.CV = mean(qda.fmCV$class != pinterest) # QDA FM Test error rate.
qdafm.CV
# qdafmCV.predict = predict(qda.fmCV, 
#                           newdata = hw5.data) #QDA FM Prediction for new data.
# qdafmCV.confmat = table(hw5.data$pinterest, 
#                         qdafmCV.predict$class) #Confusion matrix.
# qdafm.TstErr = c(qdafmCV.confmat[1,2] + 
#                    qdafmCV.confmat[2,1]) # LDA Full Model Test Error

# Partial QDA Model: Training Error
set.seed(1)
qda.pm = qda(pinterest ~ txtsent, 
             data = hw5.data) # QDA Partial Model
qdapm.trr = mean(qda.pm$class != pinterest) # QDA PM Training error rate.
qdapm.trr
# qdapm.predict = predict(qda.pm, 
#                         newdata = hw5.data) # QDA PM Prediction for new data
# qdapm.confmat = table(hw5.data$pinterest, 
#                       qdapm.predict$class) # QDA PM Confusion matrix
# qdapm.TrErr = c(qdapm.confmat[1,2] + 
#                   qdapm.confmat[2,1]) # LDA PM Trainig Error

# Partial QDA Model: Test Error
set.seed(1)
qda.pmCV = qda(pinterest ~ txtsent, 
               data = hw5.data, 
               CV = TRUE) # QDA Partial Model + LOOCV
qdapm.CV = mean(qda.pmCV$class != pinterest) # QDA PM Test error rate.
qdapm.CV
# qdapmCV.predict = predict(qda.pmCV, 
#                           newdata = hw5.data) # QDA FM Prediction for new data.
# qdapmCV.confmat = table(hw5.data$pinterest, 
#                         qdapmCV.predict$class) # QDA PM Confusion matrix.
# qdapm.TstErr = c(qdapmCV.confmat[1,2] + 
#                    qdapmCV.confmat[2,1]) # QDA PM Test Error

Predictors = c("Full Model", "Text Sent Only")
LDA.Train = c(ldafm.trr, 
              ldapm.trr)
LDA.Test = round(c(ldafm.CV, 
                   ldapm.CV),3)
QDA.Train = c(qdafm.trr, 
              qdapm.trr)
QDA.Test = round(c(qdafm.CV, 
                   qdafm.CV),3)
as.data.frame(cbind(Predictors, 
                    LDA.Train, 
                    LDA.Test,
                    QDA.Train, 
                    QDA.Test))

# Q1.B: KNN and KNN.CV
# Training Error Using KNN:
sc.hsclass = (scale(hsclass))
sc.txtsent = (scale(txtsent))
sc.txtrec = (scale(txtrec))
sc.fbtime = (scale(fbtime))
sc.introvert = (scale(introvert))
knn.data = as.data.frame(cbind(sc.txtsent, 
                               sc.hsclass,
                               sc.txtrec, 
                               sc.fbtime, 
                               sc.introvert))
# View(knn.data)
# KNN Full Model:
set.seed(1)
knnfm = knn(knn.data, 
            knn.data, 
            pinterest, 
            k = 1)
knnfm.err = mean(knnfm != pinterest)
knnfm.err

# KNN Partial Model:
set.seed(1)
knnpm = knn(sc.txtsent, 
            sc.txtsent, 
            pinterest, 
            k = 1)
knnpm.err = mean(knnpm != pinterest)
knnpm.err

# Test Error Using KNN:
# KNN CV Full Model
set.seed(1)
knncvfm = knn.cv(knn.data, 
                cl = pinterest, 
                k = 1)
knncvfm.err = mean(knncvfm != pinterest)
knncvfm.err

# KNN CV Parital Model
set.seed(1)
knncvpm = knn.cv(sc.txtsent, 
                 cl = pinterest, 
                 k = 1)
knncvpm.err = mean(knncvpm != pinterest)
knncvpm.err

KNN.Train = round(c(knnfm.err, 
                    knnpm.err),
                  3)
KNN.Test = round(c(knncvfm.err, 
                   knncvpm.err),
                 3)
as.data.frame(cbind(Predictors, 
                    KNN.Train, 
                    KNN.Test))

# Q1.C:
#Define two datasets, for full model and partial model respectively:
knn.q1c = (scale(hw5.data[,c(3,4,5,6,9)]))
textsnt = (scale(hw5.data[,4]))

# Define knn.train function as f(trainSet, TestSet, i) and write knn function in terms of trainSet, TestSet and i. Return training error (train.err) and round to two digits after decimal, and return and print the result.
set.seed(1)
knn.train = function(trainSet, testSet, i){
  knn.q1c = knn(train = trainSet, test = testSet, 
                cl = hw5.data$pinterest, k = i)
  train.err = round(mean(knn.q1c != hw5.data$pinterest), 2)
  return(print(c("Training error =" , train.err)))
}

#Use knn.train function to input values of trainSet, testSet and k in above function and generate output.
knn.train(trainSet = knn.q1c, testSet = knn.q1c, i = 2) # Full Model
knn.train(trainSet = textsnt, testSet = textsnt, i = 2) # Partial Model

# Q1.D:
set.seed(1)
knn.test = function(trainSet, k){
  knncv.q1c = knn.cv(train = trainSet, 
                     cl = hw5.data$pinterest, 
                     k = k)
  test.err = round(mean(knncv.q1c != hw5.data$pinterest), 3)
  return(print(c("Test error =" , test.err)))
}

Full.Model = knn.test(trainSet = knn.q1c, 
         k = 2) # Full Model
Partial.Model = knn.test(trainSet = textsnt, 
         k = 2) # Partial Model

# Q1.E:
# Training Error (KNN):
Q1E.TrainError = rep(0, 25) # Stores Training Error from Q1E KNN
Q1E.TestError = rep(0, 25) # Stores Test Error from Q1E KNN.CV
set.seed(1)
for (i in 1:25) {
  q1e.knn = knn(train = knn.q1c, 
                test = knn.q1c, 
                cl = pinterest, 
                k = i)
  Q1E.TrainError[i] = round(mean(q1e.knn != hw5.data$pinterest), 3)
  
  # Test Error (KNN.CV):  
  q1e.knncv = knn.cv(train = knn.q1c,
                     cl = pinterest, 
                     k = i)
  Q1E.TestError[i] = round(mean(q1e.knncv != hw5.data$pinterest), 3)
}

k = seq(1:25) #K value from 1 to 25.

# This plot was modified for homework:
plot(x = k, y = Q1E.TrainError,
     lty = 2,
     col = ifelse(q1e.knn == "Y", 1, 2),
     pch = ifelse(q1e.knn == "Y", 1, 2),
     ylim = c(0, 0.45),
     xlim = c(0, 25),
     main = "K VS Training and Test Errors",
     ylab = "Training and Test Error Rates",
     xlab = "Number of Folds/Groups (K)")
points(x = k, y = Q1E.TestError,
       lty = 1,
       col = ifelse(q1e.knncv == "Y", 3, 4),
       pch = ifelse(q1e.knncv == "Y", 3, 4))
legend("bottomright", 
       col = c(1, 2, 3, 4), 
       pch = c(1, 2, 3, 4 ),
       title = "Legend:",
       legend = c("Training error = Yes", 
                  "Training error = No", 
                  "CV Test error = Yes", 
                  "CV Test error = No"))

#Final Plot Code: This changes color, pch and related labels.
plot(x = k, y = Q1E.TrainError,
     lty = 2,
     col = 2,
     pch = "o",
     ylim = c(0, 0.45),
     xlim = c(0, 25),
     main = "K VS Training and Test Errors",
     ylab = "Training and Test Error Rates",
     xlab = "Number of Folds/Groups (K)")
points(x = k, y = Q1E.TestError,
       lty = 1,
       col = 3,
       pch = "x")
legend("bottomright", 
       col = c(2, 3), 
       pch = c("o", "x"),
       title = "Legend:",
       legend = c("Training error",
                  "CV Test error"))


# Q1.F:
as.data.frame(cbind(Predictors, 
                    LDA.Train, 
                    LDA.Test,
                    QDA.Train, 
                    QDA.Test,
                    KNN.Train, 
                    KNN.Test))

# Q2: Bootstrapping:
# Q2.A:
q2.data = (hw5.data[,c(3,4,5,6,9)]) #Data.
index = sample(seq(1:47))
q2afunction = function(data, index){
  q2a.pca = princomp(data[index,], cor = TRUE)
  IPC.stdev = round(q2a.pca$sdev[1],3)
  return(IPC.stdev)
}
Stdev = q2afunction(q2.data, index)

cat("Standard Deviation of Ist PC = ", Stdev)


# Q2.B:
boots = boot(q2.data, q2afunction, 1000)
boots
CI = boot.ci(boot.out = boots, type = "perc")
CI
cat("95% Confidence interval of 
    Standard Deviation of Ist PC = [",
    round(CI$percent[,4],3),",", 
    round(CI$percent[,5],3),"]")

# Q2.C:
q2.data = (hw5.data[,c(3,4,5,6,9)]) #Data.
index = sample(seq(1:47))
q2afunctionc = function(data, index){
  q2a.pcac = princomp(data[index,], cor = TRUE)
  IPC.loadingc = round(q2a.pcac$loadings[1],3)
  return(IPC.loadingc)
}
pcloadingc = q2afunctionc(q2.data, index)
cat("Standard Deviation of Ist PC = ", pcloadingc)

bootsc = boot(q2.data, q2afunctionc, 1000)
bootsc
CIc = boot.ci(boot.out = bootsc, type = "perc")
CIc
cat("95% Confidence interval of Ist PC loading = [", round(CIc$percent[,4],3),",", 
    round(CIc$percent[,5],3),"]")

# Function Demonstration:
i = seq(1:180)
x = sin(i)
y = cos(45)
z = tan(75)
bijesh = function(a, b, c, d, e){
  eta = x*a*b
  theta = d/(y*e)
  kappa = ((a+e)^2)/(c*z)
  gamma = a + b + c + d + e
  final = (((eta + theta)*kappa)/gamma)*(sin(50)/cos(65))
}
# Return output
bijesh(1, 2, 3, 4, 5)
bijesh(1, 1, 1, 1, 1)
