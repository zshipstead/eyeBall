#' Eyeball the distributions
#'
#' Accepts numeric and factored categorical variables and produces histograms or
#' bar plots that can be cycled through using '>' or '<'. Returns a listing of any
#' columns that are of interest. Columns are added to this list by pressing 's'.
#'
#' @param eData A data frame that includes at least one numeric or factored column.
#' @param brks Optional number of histogram breaks. Will default if empty.
#'
#' @return Returns a listing of columns for which 's' was pressed during viewing.
#'
#' @export


# Present distributions
eyeDist <- function(eData, brks = "Sturges"){
  i <- 1
  report <- NULL
  while (i != -1){
    # Histogram for numeric data
    if (is.numeric(eData[[i]])) {
      # calculate skew for title
      sk <- invisible(eyeskew(eData[[i]][!is.na(eData[[i]])]))
      print(hist(eData[[i]],
                 xlab = colnames(eData[i]),
                 main = paste0("Skewness (Type 3): ", signif(sk, digits = 3)),
                 breaks = brks))
      moveOptions()
      nextMove <- invisible(readline(prompt="Press [enter] to continue "))
    }
    # Barplot for categorical
    else if (is.factor(eData[[i]])) {
      print(barplot(table(eData[[i]]),
                    xlab = colnames(eData[i]),
                    axis.lty = "solid",
                    main = "Categorical Data"))
      moveOptions()
      nextMove <- invisible(readline(prompt="Press [enter] to continue "))
    }
    # Anything that isn't numeric or categorical
    else {
      print(hist(0,
                 main = paste0("Variable '", colnames(eData[i]),
                               "' is not a valid data type")))
      moveOptions()
      nextMove <- invisible(readline(prompt="Press [enter] to continue "))
    }
    # Use input to decide what to do with next round
    if (nextMove == "<" | nextMove == ">") i <- determineMove(nextMove, i, length(eData))
    else if (nextMove == "s") report <- append(report, colnames(eData[i]))
    else if (nextMove == "q") i <- -1
  }
  report
}
