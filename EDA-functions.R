
# 1. Numeric to Factor.

# * data = dataframe.
# * categories = Integer. If the variable has <= unique values than categories will be converted to factor.
# * dataframe = 
#   - FALSE: It will return the unique values of the variables and their column position.
#   - TRUE: It will return the modified dataframe.


factorVariables <- function(data, categories, dataframe = F) {
  
  if (dataframe==T) {
    
    levels <- apply(data, 2, function(x) length(unique(x)))
    data[,which(levels < categories)] <- lapply(data[,which(levels < categories)], factor )
    data
    
  } else {
    
    levels <- apply(data, 2, function(x) length(unique(x)))
    data[,which(levels < categories)] <- lapply(data[,which(levels < categories)], factor )
    review <- sapply(data, function(x) l = levels(x))
    review[sapply(review, function(x) length(x)!=0)]
    list(review=review[sapply(review, function(x) length(x)!=0)], colnumber =  which(levels < categories))
    
  }
  
}



# 2. Basic Graphics.

# * df = A dataframe.
# * box =
#   - If FALSE, it will return a density plot of the **NUMERIC** variables overlayed by the density plot of the Normal distribution, so that, it can be visualised the kurtosis and the skew of the data.
#   - If TRUE, it will plot histograms (mean included).



allPlots <- function(df, box = F){
  
  df.n <- Filter(is.numeric, df)
  df.f <- Filter(is.factor, df)
  
  name.n = names(df.n)
  name.f = names(df.f)
  
  for (i in 1:length(name.f)) {
    df.ff = data.frame(df.f[,i])
    colnames(df.ff) = "stat"
    print(
      ggplot(df.ff, aes(stat)) +
        geom_bar(fill="dodgerblue", alpha=.5) +
        ggtitle(name.f[i])
    )
  }
  
  if (box == T) {
    
    for (i in 1:length(name.n)) {
      df.nn = data.frame(df.n[,i])
      colnames(df.nn) = "stat"
      print(
        ggplot(df.nn, aes(x=1,stat)) +
          geom_boxplot(fill="salmon", alpha=.5) +
          ggtitle(name.n[i]) +
          stat_summary(fun.y=mean, geom="point", shape=4, size=2, color="red", fill="red")
      )
    }
    
  } else {
    
    for (i in 1:length(name.n)) {
      df.nn = data.frame(df.n[,i])
      colnames(df.nn) = "stat"
      print(
        ggplot(df.nn, aes(stat)) +
          geom_density(fill="salmon", alpha=.5) +
          ggtitle(name.n[i]) +
          stat_function(
            fun = dnorm,
            args = list(mean = mean(df.nn$stat, na.rm = T), sd = sd(df.nn$stat, na.rm = T)),
            color = "red"
          )
      )
    }
    
  }
  
}



# 2.1 Faster graphics.

# It avoids the usage of loops.



fasterAllPlots <- function(df) {
  apply(Filter(is.factor, df), 2 , function(x) {
    ggplot(data.frame(x), aes(x)) + geom_bar(fill="dodgerblue", alpha=.5) 
  }
  )
  
  apply(Filter(is.numeric, df),2 , function(x) {
    ggplot(data.frame(x), aes(x)) + 
      geom_density(fill="salmon", alpha=.5) +
      stat_function(
        fun = dnorm,
        args = list(mean = mean(x, na.rm = T), sd = sd(x, na.rm = T)),
        color = "red"
      )
  })
  
}

