#' Cycle through scatter plots
#'
#' Accepts a data frame and one column to appear on the y-axis. Allows user to cycle
#' through columns, which will appear on the x-axis. This is done by pressing '<' or
#' '>'. User can save a list of columns of interest by pressing 's'. NOTE: It will make
#' categorical variables numeric whenever possible.
#'
#' @param eData A data frame containing numeric and/or categorical variables
#' @param y The column to appear on the y-axis entered as either 'col_name' or simply by typing the column number.
#' @return Returns a listing of columns for which 's' was pressed.
#' @export

eyeScatter <- function(eData, y) {
  # If y is character, transform into numeric
  if (is.character(y)) y <- transformY(eData, y)
  # verify y was correctly entered
  res <- try(y,silent = TRUE)
  if (class(res) == "try-error") return(message(noquote("Invalid input for y")))
  # Make factor columns numeric, if applicable
  eData <- convertFactors(eData, y)
  # set initial x variable
  i <- startX(eData, y)
  # if no valid i could be found, throw error
  if (i == -999) return(message(noquote("Data set lacks appropriate data")))
  # Report is where columns of interest are saved
  report <- NULL
  # Main Loop
  if (is.numeric(eData[[y]])) {
    while (i != -1){
      if (is.numeric(eData[[i]])){
        print(plot(eData[[y]] ~ eData[[i]],
                   xlab = colnames(eData[i]),
                   ylab = colnames(eData[y])))
        moveOptions()
        nextMove <- invisible(readline(prompt="Press [enter] to continue "))
      }
      #Implement command
      if (nextMove == "<" | nextMove == ">") i <- determineMove(nextMove, i, length(eData))
      else if (nextMove == "s") report <- append(report, colnames(eData[i]))
      else if (nextMove == "q") i <- -1
      # Don't let x = y
      if (i == y && length(eData) > 1) i <- determineMove(nextMove, i, length(eData))
    }
  }
  else return(message(noquote("Y is not a valid data type")))
  report
}
