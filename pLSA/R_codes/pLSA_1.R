pLSA_1 <- function(BOW, wordTopicTable){
    
    target = BOW$target
    BOW = t(BOW[, 1 : (ncol(BOW) - 1)])
    for(col in ncol(BOW)){
        BOW[, col] = BOW[, col] / sum(BOW[, col])
    }
    
    nImage = dim(BOW)[2]
    nWord = dim(BOW)[1]
    nTopic = dim(wordTopicTable)[2]
    
    if(nWord < nTopic){
        print('The Number of Words must not be smaller than the number of topic')
        return(NULL)
    }
    
    topicImageTable = matrix(NA, nTopic, nImage)

    a = wordTopicTable[1 : nTopic, 1 : nTopic]
    for(col in 1 : nImage){
        b = BOW[1 : nTopic, col]
        topicImageTable[,col] = solve(a, b)
    }
    
    topicImageTable = data.frame(t(topicImageTable))
    topicImageTable = cbind(topicImageTable, target)
    
    return(topicImageTable)
}

randomTable <- function(row, col){
    mat = matrix(runif(row * col), nrow = row)
    for(i in row){
        mat[i,] = mat[i,] / sum(mat[i,])
    }
    return(mat)
}

ntopic = 100
datasets = c('Corel1000', 'MIT8', 'Caltech101')


for(dataset in datasets){
    print(dataset)
    link = paste('../BOW/', dataset, '.csv', sep = '')
    
    BOW = read.csv(link)
    wordTopicTable = randomTable(ncol(BOW) - 1, ntopic)
    pLSA = pLSA_1(BOW, wordTopicTable)
    
    # print(pLSA)
    
    write.csv(pLSA, file = paste('../pLSA_1/', dataset, '.csv', sep = ''), row.names = F )
}
