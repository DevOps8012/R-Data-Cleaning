# Quickly categorize a data frame with a column of messy character strings.

categorize <- function(df, strings, categories) {
  # create empty data frame to hold categories
  catDF <- data.frame(matrix(ncol=ncol(df), nrow=0))
  colnames(catDF) <- paste0(names(df))
  
  # add sequence so original order can be restored
  df$sequence <- seq(nrow(df))
  
  # iterate through the strings
  for (i in seq_along(strings)) {
    rownames(df) <- NULL
    index <- grep(strings[i], df[,1], ignore.case=TRUE)
    tempDF <- df[index,]
    tempDF$Category <- categories[i]
    catDF <- rbind(catDF, tempDF)
    df <- df[-index,]
  }
  
  # add OTHER to unmatched rows
  if (nrow(df) > 0) {
    df$Category <- "OTHER"
    catDF <- rbind(catDF, df)
  }
  
  # return to the original order & remove the sequence data
  catDF <- catDF[order(catDF$sequence),]
  catDF$sequence <- NULL
  
  # clear the row names
  rownames(catDF) <- NULL
  
  # set new categories to factors
  catDF$Category <- as.factor(catDF$Category)
  catDF
}
