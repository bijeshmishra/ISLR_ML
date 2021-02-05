
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
