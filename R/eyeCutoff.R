#' Find values that exceed a given cutoff
#'
#' Provide a data frame and a cutoff point (in units of standard deviation) and receive
#' a list of columns with extreme values, along with the number of values. If a column
#' is provided in the call, then this function will instead return a listing of rows in that
#' column that have missing values.
#'
#' @param eData A data frame
#' @param stdevs The cutoff to define extreme scores in units of standard deviation.
#' @param col Optional column. If provided, eyeCutoff will return the rows with missing values, for that column.
#'
#' @return Returns either a listing of columns with missing values (and totals) or one column with the missing rows.
#'
#' @export

eyeCutoff <- function(eData, stdevs=NULL, col=NULL){
  if (is.null(stdevs)) return("Please indicate standard deviation cutoff.")
  # If col is null, search for number of outlying points per column
  if (is.null(col)){
    # strip non-numeric columns
    eData <- keepNumeric(eData)
    # store data here
    outliers <- data.frame(matrix(nrow = 0, ncol = 2))
    colnames(outliers) <- c("Variable", "Count")
    for (i in 1:ncol(eData)){
      dataMean <- mean(eData[[i]])
      dataSD <- sd(eData[[i]])
      # check to see if  there are any outliers. If so, store column name and
      # total number of outliers
      if (length(which(eData[[i]]>(dataMean + stdevs*dataSD) |
                       eData[[i]]<(dataMean - stdevs*dataSD))) > 0)
      {
        outliers[length(outliers[,1]) + 1, 1] <- colnames(eData)[[i]]
        outliers[length(outliers[,1]), 2] <- length(which(eData[[i]] >
                                                            (dataMean + stdevs*dataSD) |
                                                            eData[[i]] < (dataMean - stdevs*dataSD)))
      }
    }
  } else {
    dataMean <- mean(eData[[col]])
    dataSD <- sd(eData[[col]])
    outliers <- which(eData[[col]] > (dataMean + stdevs*dataSD) |
                        eData[[col]] < (dataMean - stdevs*dataSD))
  }
  outliers
}

