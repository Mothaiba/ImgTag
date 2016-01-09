#Featuring functions
# ======================================================================================

#-------------------------------------------------------
# given a dataset, which contains all the links of the categories of the images
# return an array, contains the category of each image, based on its folder

getTarget <- function(dataset){
    
    target = vector()
    
    categories = list.dirs(dataset, recursive = F)
    # print(categories)
    
    for (i in 1 : length(categories)){
        
        category = list.files(categories[i])
        target = c(target, array(i, dim = length(category)))
        
    }
    
    target = as.factor(target)
    # print(target)
    
    return (target)
    
}
# -------------------------------------------------------



# ------------------------------------------------------------
# main function of featuring

getFeatures <- function(img){
    
    if(length(dim(img)) == 2){
        tmpImg = array(0, c(nrow(img), ncol(img), 3))
        for(i in 1 : nrow(img)){
            for(j in 1 : ncol(img)){
                tmpImg[i, j, 1] = tmpImg[i, j, 2] = tmpImg[i, j, 3] = img[i, j]
            }
        }
        img = tmpImg
    }
    xresolution = nrow(img[,,1])
    yresolution = ncol(img[,,1])
    
    # img = floor(img * 255 / ( 256 / nbins))
    
    # return(toSpatialPyramid(img, 0, 1, xresolution, 1, yresolution))
    return(toColorPyramid(img, 0, 1, xresolution, 1, yresolution))
    
}

#-------------------------------------------------------------


#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

toColorAverage <- function(img, lvl, xmin, xmax, ymin, ymax){
    
    ave = c(0, 0, 0)
    
    ave[1] = sum(img[xmin : xmax, ymin : ymax, 1]) /
        ((xmax - xmin + 1) * (ymax - ymin + 1)) * lvlWeight[lvl + 1]

    ave[2] = sum(img[xmin : xmax, ymin : ymax, 2]) /
        ((xmax - xmin + 1) * (ymax - ymin + 1)) * lvlWeight[lvl + 1]
    
    ave[3] = sum(img[xmin : xmax, ymin : ymax, 3]) /
        ((xmax - xmin + 1) * (ymax - ymin + 1)) * lvlWeight[lvl + 1]
        
    return(ave)
    
}

toColorPyramid <- function(img, lvl, xmin, xmax, ymin, ymax){
    
    ave = toColorAverage(img, lvl, xmin, xmax, ymin, ymax)
    
    if(lvl < pyramidLevel){
        
        xmid = floor((xmin + xmax) / 2)
        ymid = floor((ymin + ymax) / 2)
        
        ave = c(ave, toColorPyramid(img, lvl + 1, xmin, xmid, ymin, ymid))
        ave = c(ave, toColorPyramid(img, lvl + 1, xmid + 1, xmax, ymin, ymid))
        ave = c(ave, toColorPyramid(img, lvl + 1, xmin, xmid, ymid + 1, ymax))
        ave = c(ave, toColorPyramid(img, lvl + 1, xmid + 1, xmax, ymid + 1, ymax))
        
    }
    
    return(ave)
    
}



#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^




# ------------------------------------------------------------------------------------
# given an image and pixel-ranges (xmin -> xmax, ymin -> ymax)
# return the histogram of that image
#   represented by a matrix of floats [nbins * nbins * nbins]

# toHistogram <- function(img, lvl, xmin, xmax, ymin, ymax){
#     histo = array(0 * (nbins * nbins * nbins), dim = c(nbins, nbins, nbins))
#     # print(dim(histo))
#     for (i in xmin : xmax){
#         
#         for (j in ymin : ymax){
#             
#             histo[img[i, j, 1], img[i, j, 2], img[i, j, 3]] =
#                 histo[img[i, j, 1], img[i, j, 2], img[i, j, 3]] + 1
#             
#         }
#         
#     }
#     
#     histo = histo / ((xmax - xmin + 1) * (ymax - ymin + 1)) # normalization
#     histo = array(histo, c(1, nbins * nbins * nbins)) * lvlWeight[lvl + 1] # weight for level i is lvlWeight[i + 1]
#     # print(head(histo))
#     return (histo)
#     
# }

# ------------------------------------------------------------------------------------




# -----------------------------------------------------------------
# recursively compute the spatial pyramid of an image

# toSpatialPyramid <- function(img, lvl, xmin, xmax, ymin, ymax){
#     
#     histo = toHistogram(img, lvl, xmin, xmax, ymin, ymax)
#     
#     if(lvl < pyramidLevel){
#         
#         xmid = floor((xmin + xmax) / 2)
#         ymid = floor((ymin + ymax) / 2)
#         histo = cbind(histo, toSpatialPyramid(img, lvl + 1, xmin, xmid, ymin, ymid))
#         histo = cbind(histo, toSpatialPyramid(img, lvl + 1, xmid + 1, xmax, ymin, ymid))
#         histo = cbind(histo, toSpatialPyramid(img, lvl + 1, xmin, xmid, ymid + 1, ymax))
#         histo = cbind(histo, toSpatialPyramid(img, lvl + 1, xmid + 1, xmax, ymid + 1, ymax))
#         
#     }
#     # print(length(histo))
#     return (histo)
#     
# }
