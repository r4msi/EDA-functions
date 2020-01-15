


# 1. Proportion contigency table.


# * df: dataframe
# * categories: Integer. If the variable has <= unique values than categories will plot its contigency table.


propContingency <- function(df, categories) {
  
  d = apply(df, 2, function(table){
    if (length(unique(table)) < categories ) {
      prop.table(table(table))
    }
  }
  )
  
  mask <- sapply(d, function(d) length(d)!=0)
  d[mask]
  
}


# 2. Numeric to Factor.

# If you equal a data.frame <- factorVariables(df, 10, dataframe=T), it will convert all the variables with less than 10 unique values to a factor.
# If df = F,  this function returns a list. 1st argument: Number of unique values. 2nd: The column position of the variables with less unique values than "categories", 
# ... so that you can choose which one convert to factor. Ex. position <- factorVariables(df, 10, F); dataframe[,position[1:4]] <- sapply(dataframe[,position[1:4]], factor)

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

# 3. Factor to Numeric

numericVariables <- function(data, categories) {
  
  levels <- apply(data, 2, function(x) length(unique(x)))
  data[,which(levels >= categories)] <- sapply(data[,which(levels >= categories)], as.numeric )
  data
  
}


# 4. Basic Graphics.

# * df = A dataframe.
# * box =
#   - If FALSE, it will return a density plot of the **NUMERIC** variables overlayed by the density plot of the Normal distribution, so that, it can be visualised the kurtosis and the skew of the data.
#   - If TRUE, it will plot histograms (mean included).
# Note: It needs at least 1 factor variable and 1 numeric one.



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



# 4.1 Faster graphics.

# It avoids the usage of loops.



fasterAllPlots <- function(df) {
  apply(Filter(is.factor, df), 2 , function(x) {
    ggplot(data.frame(x), aes(x)) + geom_bar(fill="dodgerblue", alpha=.5) 
  }
  )
  
  apply(Filter(is.numeric, df),2 , function(x) {
    ggplot(data.frame(x), aes(x)) + 
      geom_density(fill="salmon", alpha=.5) 
  })
  
}

# 5. Mosaic ggplot2

mosaic_BinaryTarget <- function(var, target, xname = "Ordinal", targetname = "Nominal/Binary"){
  df <- data.frame(variable = var, target = target)
  ggplot(df) +
    geom_mosaic(aes(product(variable), fill=target)) +
    labs(x= xname, y=targetname, title="Mosaic Plot")
}

# 6. V cramer ggplot2

# Needed for the ggplot2 function.
Vcramer<-function(v,target){
  if (is.numeric(v)){
    v<-cut(v,5)
  }
  if (is.numeric(target)){
    target<-cut(target,5)
  }
  cramer.v(table(v,target))
}

# Just needs the df and the y variable. 

VcramerGraphic <- function(matriz, target) {
  
  salidaVcramer<-sapply(matriz,function(x) Vcramer(x,target))
  nam = names(salidaVcramer)
  df = list()
  
  for (i in 1:length(names(salidaVcramer))) {
    
    df[[nam[i]]] <- data.frame(value = salidaVcramer[i], variable = nam[i])
    
  }
  
  d = 2
  s= data.frame(df[[1]])
  
  while(d <= length(names(salidaVcramer)) ) {
    s = rbind(s, data.frame(df[[d]]))
    d = d+1
  }
  
  ggplot(s, aes(reorder(variable, -value), value, fill=value)) +
    geom_col() +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_fill_gradient(low="red", high = "blue") +
    ylim(c(0,1)) +
    labs(title = "V-Crammer", x= "Predictors", y="V+Crammer Value")
  
}