# Calculate Skew (type 3)
eyeskew <- function(colSkew){
  eDeviation <- colSkew - mean(colSkew)
  n <- length(colSkew)
  type3 <- sqrt(n) * sum(eDeviation^3)/(sum(eDeviation^2)^(3/2))
  type3 * ((1 - 1/n)^(3/2))
}

# Present options for histogram
moveOptions <- function(){
  message(noquote("Type '>' to go forward"))
  message(noquote("Type '<' to go backward"))
  message(noquote("Type 's' save column name to list"))
  message(noquote("Type 'q' to quit"))
}



# Go forward or backward in screens
determineMove <- function(nextMove, i, j){
  if (nextMove == ">") ifelse((i+1) > j, 1, i+1)
  else
    if (nextMove == "<") ifelse((i-1) == 0, j, i-1)
}


# set initial x for scatter plot
startX <- function(eData, y){
  startVal <- NULL
  breakout <- 999
  # Verify input type
  if (is.character(y)) startVal <- which(colnames(eData)==y)
  else if (is.numeric(y)) startVal <- y
  # If y is first column, move starting x to next appropriate column
  if (startVal == 1) {
    while (breakout == 999){
      ifelse((startVal+1) > length(eData), return(-999),
             startVal <- startVal + 1)
      if (is.numeric(eData[[startVal]]))
        breakout <- 998
    }
    # if not start at column 1...but make sure it's a good column
  } else {
    if (is.numeric(eData[[1]]) | is.factor(eData[[1]])) startVal <- 1
    else {
      while (breakout == 999){
        ifelse((startVal+1) > length(eData), return(-999),
               startVal <- startVal + 1)
        if (is.numeric(eData[[startVal]]))
          breakout <- 998
      }
    }
  }
  startVal
}

# Convert any "numeric" factor columns to numeric
convertFactors <- function(eData, y){
  for (i in 1:length(eData)){
    # If it can me numeric, make it numeric
    if (is.factor(eData[[i]])){
      res <- try(as.numeric(eData[[i]]), silent = TRUE)
      if (class(res) != "try-error")
        eData[[i]] <- as.numeric(eData[[i]])
    }
  }
  eData
}

# drop non-numeric columns
keepNumeric <- function(eData){
  for (i in length(eData):1){
    if (!is.numeric(eData[[i]])){
      eData <- eData[,-i]
    }
  }
  eData
}

# Shapiro Check verify number of observations between 3 and 5000, return result
shapiroCheck <- function(eData){
  result <- NULL
  if (length(which(!is.na(eData))) >= 3 && length(which(!is.na(eData))) <= 5000) {
    result <- format(round(shapiro.test(eData)$p.value, 4), nsmall = 4)
  } else result <- "S-W test requires between 3 and 5000 observations"
  result
}


transformY <- function(eData, y){
  y <- which(colnames(eData)==y)
}
