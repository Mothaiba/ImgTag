# Define functions

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

toRGB <- function(img){
    
    xresolution = nrow(img)
    yresolution = ncol(img)
    
    newImg = array(dim = c(xresolution, yresolution, 3))
    for(i in 1 : xresolution){
        for(j in 1 : yresolution){
            newImg[i, j, 1] = newImg[i, j, 2] = newImg[i, j, 3] = img[i, j]
        }
    }
    
    return(newImg)
    
}

getCoordinate <- function(xpatch, ypatch, number){
    
    x = floor((number - 1) / npart) * xpatch + 1
    y = (number %% npart - 1) * ypatch + 1
    if(y <= 0)
        y = (npart - 1) * ypatch + 1
    
    return(c(x, y))
    
}

getColorAverage <- function(patch){
    return(c(mean(patch[,,1]), mean(patch[,,2]), mean(patch[,,3])))
}

toGrayScale <- function(colorImg){

    img = 0.3 * colorImg[,,1] + 0.59 * colorImg[,,2] + 0.11 * colorImg[,,3]
    return (img)
    
}

getBlurred <- function(img, xres, yres){
    
    res = array(dim = c(xres, yres))
    
    for(i in 1 : xres)
        for(j in 1 : yres){
            cnt = 0
            sum = 0
            for(k in 1 : 9){
                u = i + p[k]
                v = j + q[k]
                if(u < 1 || u > xres || v < 1 || v > yres)
                    next
                cnt = cnt + 1
                sum = sum + img[u, v]
            }
            res[i, j] = sum / cnt
            
        }
    return(res)
}

getInterestingPoints <- function(colorImg){
    
    xresolution = nrow(colorImg[,,1])
    yresolution = ncol(colorImg[,,1])
    img = toGrayScale(colorImg)
    
    blurred = array(dim = c(5, xresolution, yresolution))
    blurred[1,,] = img
    for(i in 2 : 5){
        blurred[i,,] = getBlurred(blurred[i - 1,,], xresolution, yresolution)
        # writeJPEG(blurred[i,,], paste(toString(i), 'blurred.jpg'))
    }
    
    differ = array(dim = c(4, xresolution, yresolution))
    for(i in 1 : 4){
        differ[i,,] = blurred[i + 1,,] - blurred[i,,]
        writeJPEG(differ[i,,], paste(toString(i), 'differ.jpg'))
    }
    
    res = array(0, dim = c(xresolution, yresolution))
    xpatch2 = floor(xpatch / 2)
    ypatch2 = floor(ypatch / 2)
    
    xpos = NULL
    ypos = NULL
    
    for(i in (xpatch2 + 1) : (xresolution - xpatch2 - 1))
        for(j in (ypatch2 + 1) : (yresolution - ypatch2 - 1)){
            isInteresting = F
            for(k in 2 : 3){
                if(differ[k, i, j] < differThreshold)
                    next
                toAdd = T
                for(t in 1 : 9){
                    u = i + p[t]
                    v = j + q[t]
                    if (
                        # differ[k, i, j] < differ[k - 1, u, v] ||
                       differ[k, i, j] < differ[k, u, v]
                       || differ[k, i, j] < differ[k + 1, u, v]
                      ){
                        toAdd = F
                        break
                    }
                }
                if(toAdd == T){
                    isInteresting = T
                    break
                }
                
                toAdd = T
                for(t in 1 : 9){
                    u = i + p[t]
                    v = j + q[t]
                    if ( 
                         differ[k, i, j] > differ[k - 1, u, v]
                         || differ[k, i, j] > differ[k, u, v]
                         # || differ[k, i, j] > differ[k + 1, u, v]
                         ){
                        toAdd = F
                        break
                    }
                }
                if(toAdd == T){
                    isInteresting = T
                    break
                }                
            
            }
            if(isInteresting){
                xpos = c(xpos, i)
                ypos = c(ypos, j)
            }
        }

    pos = cbind(xpos, ypos)
    
    return (pos)
    
    
}



# a function to test if kmeans works as intended
###
# testCluster <- function(img, cluster, cnt){
#     
#     testFolder = '../testFolder/'
#         
#     for(j in 1 : length(cluster)){
#         i = cluster[j]
#         cnt = cnt + 1
#         pName = paste(testFolder, toString(i), '--', toString(cnt), '.jpg')
#         writeJPEG(img[[j]], target = pName)
#         
#     }
#     
# }
###

