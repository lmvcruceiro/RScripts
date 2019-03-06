getKLDDivergence = function(df, threshold = 0.2){
  
  df <- data.frame(df)
  numVars <- unlist(lapply(df, is.numeric))
  df <- df[, numVars]
  divergenceValues <- c()
  divergenceNames <- c()
  
  for( i in 1:length(df) ){
    
    for( j in 1:length(df) ){
      
      if( (colnames(df)[i] == colnames(df)[j]) | (j < i) ){
        
        next
        
      }
      
      #KLDresult <- KLD(density(df[!is.na(df[,i]), i])$y, density(df[!is.na(df[,j]), j])$y)$intrinsic.discrepancy
      klresult <- kl.dist(density(df[!is.na(df[,i]), i])$y, density(df[!is.na(df[,j]), j])$y)$D
      #kldMeanSum <- min(KLDresult, klresult)
      kldMeanSum <- klresult
      divergenceValues <- c(divergenceValues, kldMeanSum)
      divergenceNames <- c(divergenceNames, paste0(colnames(df)[i], "vs", colnames(df)[j]))
      
    }
    
  }
  
  valuesDF <- data.frame(list("Comparison" = divergenceNames, "Value" = divergenceValues))
  valuesDF <- valuesDF[valuesDF$Value < threshold, ]
  
  return(valuesDF)
  
}