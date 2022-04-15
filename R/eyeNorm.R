#' Cycle through data to check distribution normality of numeric data
#'
#' Provides qqplot, line and Shapiro-Wilks test. Cycle through data using '<'
#' or '>'. Add a column to a returned list by pressing 's'.
#'
#' @param eData A data frame containing at least on numeric column.
#'
#' @return Returns a list of columns of interest, which is created by pressing 's'.
#'
#' @export

eyeNorm <- function(eData){
  i <- 1
  report <- NULL
  while (i != -1) {
    if (is.numeric(eData[[i]])){
      shap <- format(round(shapiro.test(eData[[i]])$p.value, 4), nsmall = 4)
      qqnorm(eData[,i],
             xlab = colnames(eData)[[i]],
             main = paste0("Shapiro-Wilk test: ", shap))
      qqline(eData[,i])
      moveOptions()
      nextMove <- invisible(readline(prompt="Press [enter] to continue "))
    } else {
      qqnorm(0, main = paste0(colnames(eData)[[i]], "is non-numeric"))
      moveOptions()
      nextMove <- invisible(readline(prompt="Press [enter] to continue "))
    }
    if (nextMove == "<" | nextMove == ">") i <- determineMove(nextMove, i, length(eData))
    else if (nextMove == "s") report <- append(report, colnames(eData[i]))
    else if (nextMove == "q") i <- -1
  }
  report
}
