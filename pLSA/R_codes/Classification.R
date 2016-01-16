library(class)
library(caret)
simpleCV <- function(df, ntimes = 5, partition = 0.8){
    
    n = dim(df)[1]
    testResult = matrix(nrow = ntimes, ncol = 4)
    colnames(testResult) = c('Index', 'nTrain', 'nTest', 'Accuracy')
    
    for (i in 1 : ntimes){
        
        boo = runif(n, 0, 1) < partition
        trainSet = df[boo, ]
        testSet = df[!boo, ]
        cl = trainSet[, ncol(trainSet)]
        print(nrow(trainSet))
        print(nrow(testSet))
        
        M = knn(trainSet[, 1 : (ncol(trainSet) - 1)], testSet[, 1 : (ncol(testSet) - 1)], cl, 3, prob = F)
        
        prediction = array(M)
        confusion = confusionMatrix(prediction, testSet[, ncol(testSet)])
        
        accuracy = sum(prediction == testSet[, ncol(testSet)]) / nrow(testSet)
        
        print(i)
        print(accuracy)
        # print(confusion)
        print('====================')
        
        testResult[i, ] = c(i, nrow(trainSet), nrow(testSet), accuracy)
        
    }
    
    return(testResult)
    
}

datasets = c('Corel1000', 'MIT8', 'Caltech101')
outFile = '../testResult_2.csv'
write('Test Result', outFile)

for(dataset in datasets){
    
    print(dataset)
    link = paste('../pLSA_2/', dataset, '.csv', sep = '')
    df = read.csv(link)
    testResult = simpleCV(df)
    
    write(dataset, outFile, append = T)
    write.table(testResult, outFile, append = T, row.names = F, sep = ',')
}
