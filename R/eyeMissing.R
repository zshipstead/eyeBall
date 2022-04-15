#' Find missing data
#'
#' If data frame is entered this function will return the columns with missing values
#' and the total number of missing values. If a column name is included in the call,
#' this function will instead return a list of rows that have missing values.
#'
#' @param eData A data frame
#' @param col Optional column to be searched for missing values. Enter as either 'col_name' or type the column number.
#'
#' @return Returns either a listing of columns with missing values or missing values in a specified column.
#'
#' @export

eyeMissing <- function(eData, col=NULL){
  # If no column specified, return number of missing values in each column
  if (is.null(col)) {
    # store data
    missingData <- data.frame(matrix(nrow = 0, ncol = 2))
    colnames(missingData) <- c("Variable", "Count")
    for (i in 1:ncol(eData)){
      if (length(which(is.na(eData[[i]]))) != 0){
        missingData[length(missingData[,1]) + 1, 1] <- colnames(eData)[[i]]
        missingData[length(missingData[,1]), 2] <- length(which(is.na(eData[[i]])))
      }
    }
    # If column specified, return the row numbers for the missing data
  } else missingData <- which(is.na(eData[[col]]))
  missingData
}
