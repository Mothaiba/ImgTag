
# Constants
# ============================

library(jpeg)

#================ Constants for Featuring ====================

# link to all 3 datasets
allDataLink = '../../images3'

# to select visual word, each image will be divided into ('npart' x 'npart') non-overlapping patches
npart = 8
# number of patches for each image
npatches = npart * npart

# size of each patch, this value is different for each image, initially set = 0
xpatch = 0
ypatch = 0

# the percentage of patches we take from each image to actually select word
percentage = 0.3

# the number of keypoints selected in 1 image, accord with 'percentage' and 'npatches'
nkeypoints = ceiling(npatches * percentage)

# the number of visual words
nWords = 500

#output Folder
outBOWFolder = '../BOW - interesting points/'

# different threshold for detecting interesting points
differThreshold = 0.05

# 2 adjacent-points vectors
p = c(-1, -1, -1, 0, 0, 0, 1, 1, 1)
q = c(-1, 0, 1, -1, 0, 1, -1, 0, 1)

# ================== Constants for classfication =============

library(caret)

reportFile = '../error - InterestingPoints/TrainingErrorNB.txt'
