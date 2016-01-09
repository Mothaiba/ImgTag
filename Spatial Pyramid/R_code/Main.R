# This is getting Accepted!
# Phung Minh Tung
#==============================================

realCnt = 0 ###################

datasets = list.dirs(dataLink, recursive = F)

for(pyramidLevel in 0 : 3){
for (dataset in datasets){
    
    # print(dataset)
    
    target = getTarget(dataset)
    
    nFeatures = 0
    for(i in 0 : pyramidLevel){
        nFeatures = nFeatures + 4 ^ i
    }
    # df = data.frame(matrix(0, nrow = length(target), ncol = nFeatures * nbins * nbins * nbins))
    df = data.frame(matrix(0, nrow = length(target), ncol = nFeatures * 3))
    
    categories = list.dirs(dataset, recursive = F)
    cnt = 0
    
    for (category in categories){
        # print(category)
        images = list.files(category, full.names = T)
        for (image in images){
            # print(cnt)
            cnt = cnt + 1
            realCnt = realCnt + 1 ###################
            # print(realCnt)#######################
            img = readJPEG(image)
            df[cnt, ] = getFeatures(img)
        }
        print(realCnt)
    }
    
    df = cbind(df, target)
    
    
    csvFile = paste(outLink, substr(dataset, regexpr("/[^/]*$", dataset) + 1, nchar(dataset)),
                    '_level', toString(pyramidLevel), '.csv', sep = '')
    write.csv(df, file = csvFile, row.names = F)
    
}
}




print(Sys.time())

write('Training Error Calculated', file = reportFile)

dfLinks = list.files('../OutputFiles/', recursive = F, full.names = T)
    
for(dfLink in dfLinks){
    if(substr(dfLink, nchar(dfLink) - 3, nchar(dfLink)) == '.csv'){
     
        print(dfLink)
        
        df = read.csv(dfLink)
        df$target = as.factor(df$target)
        
        trainControl = trainControl(method = 'cv', number = 5)

        M = train(target ~ ., data = df, method = 'svmLinear', trControl = trainControl,
                  tuneGrid = data.frame(C = 1))
        
        print(M)
             
        prediction = predict(M, df[, 1 : (dim(df)[2] - 1)])
        trError = (sum(prediction == df$target) / length(df$target))
         
        print(trError)
        confusion = as.table(confusionMatrix(prediction, df$target))
             
        write(paste(dfLink, trError, sep = ': '), file = reportFile, append = T)               
        
        write.table(confusion, file = reportFile, append = T)
           
    }
    
}
    

print(Sys.time())
