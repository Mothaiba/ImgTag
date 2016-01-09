
# Import libraries

library(jpeg)
library(randomForest)
library(rpart)
library(ggplot2)
library(lattice)
library(caret)

# Define constants

dataLink = '../../images'
nbins = 16
pyramidLevel = 3
lvlWeight = c(1, 1, 1, 1, 1)
outLink = '../OutputFiles/'
reportFile = '../OutputFiles/trainErrorLinearSVM.txt'
