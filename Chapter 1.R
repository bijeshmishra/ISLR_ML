###############################
rm(list = ls())
setwd("/Users/bmishra/Dropbox/OSU/PhD/Semester VI, Spring 2021/STAT 5063 ML/Lectures/ROutputs")
getwd()
#################################
# Iris data set
data(iris) # may not need to run this line, but it won't hurt.
help(iris)
iris
plot(iris, pch = as.vector(iris$Species))

USArrests
help("USArrests")
biplot(princomp(USArrests, corr = TRUE))