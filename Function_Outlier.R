##---THIS FUNCTION IS DEFINED TO GENERATE A TABLE WITH
     #UPPER AND LOWER LIMITS FOR OUTLIERS BY COMMODITY---##

outlier = function(df, cmdCodeColname, UV_Colname, constant) {
  a = which(colnames(df) == cmdCodeColname)
  b = which(colnames(df) == UV_Colname)
  freq = as.data.frame(table(df[,a]))
  HS8 = freq[freq$Freq>3, ] #Outlier detection doesn't work if there are 3 or less records
  df_new = df[df[,a] %in% HS8$Var1, ]

  outlier = data.frame(unique(df_new[,a]))
  outlier$upper = 0
  outlier$lower = 0
  names(outlier)[1] = cmdCodeColname

  for (i in 1:nrow(outlier))
  {
    d = df_new[df_new[,a] == outlier[i,1], ]
    d = as.data.frame(d) #the object is otherwise a table for which quantile function throws error
    q = quantile(as.vector(d[,b]))
    outlier$upper[i] = q[4]+constant*(q[4]-q[2])
    outlier$lower[i] = q[2]-constant*(q[4]-q[2])
  }
 return(outlier)
}
