brief <- function (dataframe){
  # Calculate number of data
  num.of.data <- nrow(dataframe)
  
  # Calculate number of attributes
  num.of.attr <- ncol(dataframe)
  
  # Find the column index which has the type of factor (symbolic attribute)  
  symb.index <- unname(which((sapply(dataframe, class) == "factor") == T))
  
  # Generate the column index and remove the symbolic index from the list
  # leaving out the real attributes
  col.index <- 1:num.of.attr
  real.index <- setdiff(col.index, symb.index)
  
  # taking the columns with real values
  real.data <- dataframe[real.index]
  
  # taking each columns name
  name.data <- names(real.data)
  
  # Calculate each data statistics for each real columns
  miss.data   <- unname(sapply(real.data, function(x) sum(is.na(x))))
  mean.data   <- unname(sapply(real.data, function(x) round(mean(x, na.rm = T),2)))
  median.data <- unname(sapply(real.data, function(x) round(median(x, na.rm = T),2)))
  stdev.data  <- unname(sapply(real.data, function(x) round(sd(x, na.rm = T),2)))
  min.data    <- unname(sapply(real.data, function(x) round(min(x, na.rm = T),2)))
  max.data    <- unname(sapply(real.data, function(x) round(max(x, na.rm = T),2)))
  
  # Create a new data frame to store all of the columns
  real.frame <- data.frame(Attribute_ID = real.index, 
                           Attribute_name = name.data, 
                           Missing = miss.data,
                           Mean = mean.data, 
                           Median = median.data, 
                           SDev = stdev.data,
                           Min = min.data, 
                           Max = max.data)
  
  # taking the columns with symbolic values
  symb.data <- dataframe[symb.index]
  
  # taking each columns name
  name.data <- names(symb.data)
  
  # Calculate each data statistics for each symbolic columns
  miss.data   <- unname(sapply(symb.data, function(x) sum(is.na(x))))
  arity.data  <- unname(sapply(symb.data, function(x) length(levels(x))))
  mcv.data    <- unname(sapply(symb.data, function(x) {
    sorted.x <- sort(summary(x),decreasing = T)
    result <- ""
    if(is.na(sorted.x[1]) == F & names(sorted.x)[1] != "") result <- 
      paste(result, names(sorted.x)[1], "(", sorted.x[1], ")", sep = "")
    if(is.na(sorted.x[2]) == F & names(sorted.x)[2] != "") result <- 
      paste(result, " ", names(sorted.x)[2], "(", sorted.x[2], ")", sep = "")
    if(is.na(sorted.x[3]) == F & names(sorted.x)[3] != "") result <- 
      paste(result, " ", names(sorted.x)[3], "(", sorted.x[3], ")", sep = "")
    return(result)}))
  
  symb.frame = data.frame(Attribute_ID = symb.index,
                          Attrbute_name = name.data,
                          Missing = miss.data,
                          arity = arity.data, 
                          MCVs_count = mcv.data)
  
  ## Print the data
  
  cat("This dataset has ", num.of.data, " Rows ", num.of.attr, "Attributes", "\n\n",
      "real valued attributes", "\n",
      "----------------------", "\n")
  
  print(real.frame)
  
  cat("\n", "symbolic attributes", "\n",
      "-------------------", "\n")
  
  print(symb.frame)
  
}
