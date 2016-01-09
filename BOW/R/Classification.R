
write('Training Error Calculated', file = reportFile)

dataFrames = list.files(outBOWFolder, full.names = T)

for(each in dataFrames){
    
    print(each)
    print(Sys.time())
            
    df = read.csv(each)
    df$target = as.factor(df$target)
    
    trainControl = trainControl(method = 'none', number = 5)
    
    M = train(target ~ ., data = df, method = 'nb', trControl = trainControl,
              tuneGrid = data.frame(fL = 0, usekernel = T))
    
    print(M)
    
    prediction = predict(M, df[, 1 : (dim(df)[2] - 1)])
    trError = (sum(prediction == df$target) / length(df$target))
    
    print(trError)
    confusion = as.table(confusionMatrix(prediction, df$target))
    
    write(paste(each, trError, sep = ': '), file = reportFile, append = T)               
    
    write.table(confusion, file = reportFile, append = T)
    
    print(Sys.time())
    
    
}