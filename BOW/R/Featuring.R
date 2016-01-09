# This is getting Accepted!
# Phung Minh Tung
#==============================================


datasets = list.dirs(allDataLink, recursive = F)

for(dataset in datasets){
    
    # 'target' is the list of categories corresponding to list of all images
    target = getTarget(dataset)
    
    # the number of images
    nImages = length(target)
    
    # create an array for keypoints
    # this should be changed when we change features
    kPoints1 = kPoints2 = kPoint3 = NULL
    
    # 
    nFrameIn = array(dim = 0)
    
    categories = list.dirs(dataset, recursive = F)
    for(category in categories){
        
        images = list.files(category, full.names =  T)
        for(image in images){
            
            img = readJPEG(image)
            
            # if image is greyscale, change it to RGB color
            if(length(dim(img)) == 2)
                img = toRGB(img)
            
            xresolution = nrow(img[,,1])
            yresolution = ncol(img[,,1])
            
            #size of each patch
            xpatch = floor(xresolution / npart)
            ypatch = floor(yresolution / npart)

#=======================================================
# The following is code to generate BOW from Color Averageing only
# 
#             #randomly choosing from patches
#             randP = sample(1 : npatches, nkeypoints)
#             for(number in randP){
#                 
#                 # get top-left co-ordinate of the number-th patch of the image
#                 coordinate = getCoordinate(xpatch, ypatch, number)
#                 
#                 # extract color-average feature, save it to array kPoints
#                 CA = getColorAverage(img[coordinate[1] : (coordinate[1] + xpatch - 1), 
#                                      coordinate[2] : (coordinate[2] + ypatch - 1),])
#                 kPoints1 = c(kPoints1, CA[1])
#                 kPoints2 = c(kPoints1, CA[2])
#                 kPoints3 = c(kPoints1, CA[3])
#                 
#             }
#             nFrameIn = c(nFrameIn, nkeypoints)
#=======================================================

            
#=======================================================
# The following is for using Interesting point - technique from SVM
            
            xpatch2 = floor(xpatch / 2)
            ypatch2 = floor(ypatch / 2)
            
            iPosition = getInterestingPoints(img)
            print('after get points')
            len = dim(iPosition)[1]
            print(len)
            nFrameIn = c(nFrameIn, len)
            for(i in 1 : len){
                
                coordinate = c(iPosition[i, 1] - xpatch2, iPosition[i, 2] - ypatch2)
                CA = getColorAverage(img[coordinate[1] : (coordinate[1] + xpatch - 1), 
                                                coordinate[2] : (coordinate[2] + ypatch - 1),])
                kPoints1 = c(kPoints1, CA[1])
                kPoints2 = c(kPoints1, CA[2])
                kPoints3 = c(kPoints1, CA[3])  
            }
            
            
            
#=======================================================


        }
        
    }
    
    kPoints = cbind(kPoints1, kPoints2, kPoint3)
    
    set.seed(123)
    kmeansModel = kmeans(kPoints, centers = nWords, iter.max = 20)
    clus = kmeansModel$cluster
    
    BOW = array(data = 0, dim = c(nImages, nWords))
    
    cnt = 0
    for(i in 1 : length(target)){
        
        for(j in 1 : nFrameIn[i]){
            
            cnt = cnt + 1
            BOW[i, clus[cnt]] = BOW[i, clus[cnt]] + 1
            
        }
        
        
    }
    
    BOW = cbind(BOW, target)
    
#     write.csv(BOW, file = paste(outBOWFolder, substr(dataset, regexpr("/[^/]*$", dataset) + 1,
#                                 nchar(dataset)),'_BOW.csv'), row.names = F)
    
    
}