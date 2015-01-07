# Quickly categorize a data frame with a column of messy character strings.

categorize <- function(df, strings, categories) {
  catDF <- data.frame(matrix(ncol=ncol(df), nrow=0))
  colnames(catDF) <- paste0(names(df))
  df$sequence <- seq(nrow(df))
  for (i in seq_along(strings)) {
    rownames(df) <- NULL
    index <- grep(strings[i], df[,1], ignore.case=TRUE)
    tempDF <- df[index,]
    tempDF$Category <- categories[i]
    catDF <- rbind(catDF, tempDF)
    df <- df[-index,]
  }
  if (nrow(df) > 0) {
    df$Category <- "OTHER"
    catDF <- rbind(catDF, df)
  }
  catDF <- catDF[order(catDF$sequence),]
  catDF$sequence <- NULL
  rownames(catDF) <- NULL
  catDF$Category <- as.factor(catDF$Category)
  catDF
}
