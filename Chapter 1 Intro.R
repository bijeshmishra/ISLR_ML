# Refresher and Getting familiar with R.
# Clear Console => Control + L (Mac)
data () # Datasets available in R CRAN
my.data = c (1, 3, 39, 4, 5, 7, 2, 39, 0, 1, 8) # genrate variable
exp (log({(2+3*(2*3))/4}*(4+6))) # Doing math.
help ("Introduction")
# setwd () #set working directory
# choose.dir(setwd()) # Pop up window to set up directory; works in windows only.
getwd() #get working directlry
ls () #list
View() #View sth.
rm (objectlist) #remove
# source() #Source
example("source") #Examples.
rm (list = ls()) #clear your workspace.

###### Introduction to Statistical Learning with Application in R #####
# Webiste: www.StatLearning.org

##### Chapter 1: Introduction #####
# install.packages("ISLR") # Data for the book.
library("ISLR") #load library
# islr.khan = (ISLR::Auto) #Save Khan data from ISLR
# View(islr.khan) #View islr.khan dataset.

##### Chapter 2 Lab: Introduction to R #####
# This command clears your "workspace"
rm (list = ls())

# Basic Commands
x <- c(1,3,2,5) # join numbers inside parenthesis together into a vector x. Can replace <-" by "="; both are same.
x
y = c(1,4,3)
length(x) # total number of X. Equal to sample size of X.
length(y) # total number of Y. Equal to sample size of Y.
x+y #does not adds up as lenght of x and y are not equal.
x*y # does not multiply
x-y #does not substract
x/y #does divide.
y/x #does divide.
y = c(1, 4, 5, 3) #length of y changed to 4.
length(y) #check length of y.
x+y # now adds up as lengths of x and y are equal.
x*y # Multiplies.
x-y # substracts
x/y # Divides
y/x # Divides

ls() #list vectors/characters.
rm(x,y) #remove x and y
ls() # No Characters left.
rm(list=ls()) # Clears workspace if there are several characters.

# Create Matrix:
?matrix 
x = matrix (data = c(1,2,3,4), nrow = 2, ncol = 2) # generate 2*2 matrix from given data. Matrices are not stored below.
x
matrix(c(1,2,3,4), 2, 2) #same as above.
matrix(c(1,2,3,4),2,2,byrow=TRUE)  # "byrow = TRUE" populate matrix by row.
matrix(c(1,2,3,4),2,2,byrow=FALSE)
matrix(c(1,2,3,4),2,2,bycol = TRUE) # doesn't work.
sqrt(x) #square root of matrix x.
x^2 #square of matrix x.

# !!!Always read the help file for a new function!!!
# take note of inputs (usage) and outputs (values)
# an error message usually means incorrect inputs used
help(lm) # Open help file.
x = rnorm(50) # generate vector of random normal variable with 50 numbers x ~ N(mean = 0, stdev = 1).
y = x + rnorm(50,mean=50,sd=.1) # alter default mean and sd.
lm(y~x) #fit linear model y = f(x).
cor(x, y) #correlation between x & y

set.seed(1303) #reproduce exact same set of random numbers.
z = rnorm(100, mean = 75, sd = 0.25)
mean(z) #mean
var(z) #variance
sqrt(var(z)) #standard deviation
sd(z) #standard deviation

# Graphics
x=rnorm(100)
y=rnorm(100)
plot(x,y)
plot(x,y,xlab="this is the x-axis",ylab="this is the y-axis",main="Plot of X vs Y")
plot(x,y, pch = 1, col="red") # Change "pch" & "col" to chagne shape and color respectively.
plot(1:10, 1:10, pch = 1:10, col=1:10, main = "Some Plotting Characters and Colors")
plot(x, y, xlab = "x-axis", ylab = "y-axis", pch = 1:3, col = 1:10)

# contour() and image()plots
x = seq(1:50)
y = x
f = outer(x, y, function (x, y) cos(y)/(1 + x^2))
fa = (f-t(f))/2

contour (x, y, f)
contour(x, y, f, nlevels = 45, add = TRUE)
contour(x, y, fa, nlevels = 15)

# image() works similar as contour() except it produces color coded plots whose color depends upon z-value. This is also called as heatmap
image (x, y, f)
image(x, y, f, nlevels = 45, add = TRUE)
image(x, y, fa, nlevels = 15)

# persp() can be used to produce three-dimensional plot. The arguments theta and phi control angles as which plots are viewed.
image(x, y, fa)
persp(x, y, fa)
persp(x, y, fa, theta = 0, phi = 100)
persp(x, y, fa, theta = 10, phi = 90)
persp(x, y, fa, theta = 20, phi = 80)
persp(x, y, fa, theta = 30, phi = 70)
persp(x, y, fa, theta = 40, phi = 60)
persp(x, y, fa, theta = 50, phi = 50)
persp(x, y, fa, theta = 60, phi = 40)
persp(x, y, fa, theta = 70, phi = 30)
persp(x, y, fa, theta = 80, phi = 20)
persp(x, y, fa, theta = 90, phi = 10)
persp(x, y, fa, theta = 100, phi = 0)
persp(x, y, fa, theta = 0, phi = 0)
persp(x, y, fa, theta = 10, phi = 10)
persp(x, y, fa, theta = 20, phi = 20)
persp(x, y, fa, theta = 30, phi = 30)
persp(x, y, fa, theta = 40, phi = 40)
persp(x, y, fa, theta = 50, phi = 50)
persp(x, y, fa, theta = 60, phi = 60)
persp(x, y, fa, theta = 70, phi = 70)
persp(x, y, fa, theta = 80, phi = 80)
persp(x, y, fa, theta = 90, phi = 90)
persp(x, y, fa, theta = 100, phi = 100)

pdf("figure.pdf") #save figure as pdf.
jpeg("figure.jpeg") #save as jpeg.
dev.off() #we are done creting plots.
seq() #create sequence of numbers.
a = seq(0, 1, length = 10) # sequence of 10 numbers equally spaced between 0 and 1.
a
a = 1:10 #integers only.
a
a = seq(1:10) #integers only
a
ab = seq(-pi, pi, length  = 50) #50 numbers between -pi to +pi ab

# Indexing Data
A = matrix(50:65, 4, 4)
A
A1 = matrix(50:65, nrow = 4, ncol = 4)
identical(A, A1)
A1
A[2,3] # [2nd row, 3rd col]
A[c(1,4),] # [1st and 4th row, all column]
A[c(1,3),c(2,4)] #[1st and 3r row, 2nd and 4th column]
A[1:3, 2:4] # [1st to 3rd row, 2nd to 4th col]
A[1:2,]
A[,1:2]
A[1,]
A[-c(1,3),] # -ve sign means remove
A[-c(1,3),-c(1,3,4)]
A[c(T,T,F,T),]
A[c(F, T, T, F), c(T, T, F, T)]
dim(A) # row, col.
str(A) #structure.

# Loading Data
# Set your working directory and save data in same folder
# See the file menu in R
getwd()

# Notice some missing values "?" and the first row is actually variable names

Auto = ISLR::Auto #import data from ISLR package online.
write.table(ISLR::Auto, file = "auto.csv") #save my_data as auto.csv in working directory.
Auto1 = read.csv("auto.csv") # all variables in one column (not good).
auto = read.csv("auto.csv", check.names = TRUE, sep = " ", header = TRUE, na.strings = "?") # sep = " " identifies space as seperator. na.string = ? means set "?" as missing. Replace ? by anything that denotes missing value.

Auto = na.omit(auto) #remove missing observations.
View(auto) #view dataset auto.
ls(auto) #list variables in the dataset.
head(auto) #first few data of auto for all variables.

# Additional Graphical and Numerical Summaries
plot(cylinders, mpg) #gives error.
plot(x = cylinders, y = mpg) # same as above.
plot(x = Auto$cylinders, y = Auto$mpg) # generates plots but not efficient.
plot(data = Auto, cylinders, mpg, add = TRUE) 
attach(auto) #make names of variables permanently attached.
plot (cylinders, mpg)

cylinders=as.factor(cylinders) # Quantative to qualitatiave variable.
plot(cylinders, mpg) #box plot
plot(cylinders, mpg, col="red") #fill red colors
plot(cylinders, mpg, col="red", varwidth=T) #set width of variable.
plot(cylinders, mpg, col="red", varwidth=T,horizontal=T) #horizantal plot.
plot(cylinders, mpg, col="red", varwidth=T, xlab="cylinders", ylab="MPG") #label axises.
hist(mpg) #histogram.
hist(mpg,col=2) #fill with color #2 which is red. replacing red by 2 above also gives same result.
hist(mpg,col=2,breaks=15) # Draw 15 bars.

pairs(Auto) #generates scatterplot matrix for all variables.
pairs(~ mpg + displacement, data = Auto) #scatterplot matrix for selected variables.
pairs(~ mpg + displacement + horsepower, data = Auto)
pairs(~ mpg + displacement + horsepower + acceleration, data = Auto)
identify(Auto) #identify the value for particular variable on a plot.

str(Auto) #structure of dataset.
summary(Auto) # descriptive and numerical summary of variables.
summary(mpg)
summary(displacement)

# 5 records had missing values
Auto=na.omit(Auto)
str(Auto)
fix(Auto) #did not work.
Auto=read.csv("Auto.csv", na.strings = "?")
Auto=read.csv("Auto.csv", sep = " ", na.strings = "?")

# 5 records had missing values and can be removed
# Missing data should be save as "NA" in R
Auto=na.omit(Auto)
str(Auto)
names(Auto)
plot(horsepower,mpg)

# Click on a point in the plot to identify it
identify(horsepower, mpg, name)
summary(Auto)
summary(mpg)
savehistory() #save command history.
loadhistory() #load history when entering R again.
# q() #Quit R.

#Homework 1
x = matrix(c(1, 1, 0, 0, 0, 0, 1, 1), nrow = 4, ncol = 2, byrow = F) # matrix x
dim(x) #dimension (n, p) of x
x
y = matrix(c(1, 2, 3, 4), nrow = 4, ncol = 1, byrow = F) #matrix y
dim(y) #dimension (n, p) of y
y
xt = t(x) #trasverse of matrix x.
dim(xt) #dimension of transverse of matrix x.
xt
yt = t(y) #transverse of y
dim(yt) #dimension of tranverse of matrix y.
yt
xxtdet = det(t(x)%*%x) # determinant of x and x transverse.
xxtdet
xxtinv = solve(t(x)%*%x) #Multiply x & t(x) and inverse resulting matrix.
xxtinv
xty = t(x)%*%y # multiply t(x) and y.
xty
betas = xxtinv%*%xty #beta coefficients gives values of x1 and x2.
betas
betas1 = solve(t(x)%*%x)%*%(t(x))%*%y #compute everything done above in one line.
betas1
identical(betas, betas1) #check if both ways give same results or not. The answer must be "TRUE".
betas #check values of coefficients. These are slopes of each variables.

x1 = x[,1] #Extract variable x1 from matrix x. This is p = 1.
x2 = x[,2] #Extract variable x2 from matrix x. This is p = 2.

model = lm (y ~ x1 + x2)
summary(model)
model2 = lm (y ~ x1)
summary(model2)
# identical (model2, model1)
model1 = lm (y ~ x1 + x2 - 1)
summary(model1)

rm(list = ls())
