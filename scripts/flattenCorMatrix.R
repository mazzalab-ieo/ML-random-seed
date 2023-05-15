flattenCorrMatrix <- function(cormat)#, pmat)
{
  # cormat : matrix of the correlation coefficients
  # pmat : matrix of the correlation p-values
  
  ut <- upper.tri(cormat)
  df<-data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut]#,
    # p = pmat[ut]
  )
  return(df)
}