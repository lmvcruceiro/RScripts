creditData <- fread("C:/master/MÓDULO III - Data Science/Spark ML/20181026 PySpark-ML/entrega alumnos/practica/train.csv")
#n <- 1000000
n <- length(creditData)
norm1 <- rnorm(n)
norm2 <- rnorm(n)
norm3 <- rnorm(n, mean = 4)

pois1 <- rpois(n, lambda = 3)
pois2 <- rpois(n, lambda = 3)
pois3 <- rpois(n, lambda = 5)

binom1 <- rbinom(n, 20, 0.2)
binom2 <- rbinom(n, 20, 0.2)
binom3 <- rbinom(n, 15, 0.1)

exp1 <- rexp(n)
exp2 <- rexp(n)

varChar <- rep("char", n)

#transSum <- ac_client_district$trans_sum
#loanAmount <- ac_client_district$loan_amount
#avgSal <- ac_client_district$avg_sal
#unempRate <- ac_client_district$unemp_rate




# plots
#plot(density(norm1))
#plot(density(norm2))
#plot(density(pois1))
#plot(density(pois2))
#plot(density(binom1))
#plot(density(binom2))
#plot(density(exp1))
#plot(density(exp2))

# creating data frame

df <- data.frame(
  list(
    "Norm1" = norm1, 
    "Norm2" = norm2,
    "Norm3" = norm3,
    "Pois1" = pois1,
    "Pois2" = pois2,
    "Pois3" = pois3,
    "Binom1" = binom1,
    "Binom2" = binom2,
    "Binom3" = binom3,
    "Exp1" = exp1,
    "Exp2" = exp2,
    "VarChar" = varChar,
    "TransSum" = transSum,
    "LoanAmount" = loanAmount,
    "AvgSal" = avgSal,
    "UnempRate" = unempRate
    )
  )

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

result <- getKLDDivergence(df)
