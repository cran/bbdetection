#' @importFrom Rcpp sourceCpp
#' @importFrom stats median
#' @import zoo
#' @import xtable
#' @import ggplot2
NULL

findStartEndPoints <- function(signal) {
  # find the beginning and end of each Bull-Bear phase
  # input <- signal - vector that contains TRUE for Bull states and FALSE for Bear states

  bull.start <- vector()
  bull.end <- vector()
  bear.start <- vector()
  bear.end <- vector()

  # find the beginning and end of each bull - bear periods
  n <- length(signal)
  for(i in 1:n) {
    if((i==1) & (signal[i]==TRUE) ) {
      # the firsts observation is Bull
      bull.start <- c(bull.start, i)
      next
    }
    if( (i==1) & (signal[i]==FALSE) ) {
      # the firsts observation is Bear
      bear.start <- c(bear.start, i)
      next
    }
    if((signal[i-1]==TRUE) & (signal[i]==FALSE)) {
      # this is the end of Ones and beginning of Zeros
      # the top is in element i-1
      bull.end <- c(bull.end, i-1)
      bear.start <- c(bear.start, i)
    }
    if((signal[i-1]==FALSE) & (signal[i]==TRUE)) {
      # this is the end of Zeros and beginning of Ones
      # the bottom in the element i-1
      bear.end <- c(bear.end, i-1)
      bull.start <- c(bull.start, i)
    }
    if((i==n) & (signal[i]==TRUE)) {
      # the last observation is Bull
      bull.end <- c(bull.end, i)
    }
    if((i==n) & (signal[i]==FALSE)) {
      # the last observation is Bear
      bear.end <- c(bear.end, i)
    }
  }

  # final check for consistency
  nBulls <- length(bull.start)
  nBears <- length(bear.start)
  if(length(bull.end)!=nBulls)
    stop("Length of Bull start is not equal to length of Bull end!")
  if(length(bear.end)!=nBears)
    stop("Length of Bear start is not equal to length of Bear end!")

  return(list(bull.start=bull.start, bull.end=bull.end,
              bear.start=bear.start, bear.end=bear.end) )
}

phaseInfo <- function(price, dates, start, end) {
  n <- length(start)
  Period <- rep(" ", n)
  Length <- rep(0,n)
  Amp <- rep(0,n)
  for(i in 1:n) {
    Period[i] <- paste(dates[start[i]],dates[end[i]],sep=" to ")
    Length[i] <- end[i] - start[i] + 1
    Amp[i] <- round((price[end[i]]-price[start[i]])/price[start[i]]*100)
  }
  return(data.frame(Dates=Period, Duration=Length, Amplitude=Amp))
}

#' Prints out the summary statistics of bull-bear states
#'
#' This function prints out (in console window) the summary statistics of bull-bear states.
#' The outcome of this function is a table in LaTeX format.
#'
#' @param price a numeric vector of price values
#' @param bull a logical vector that contains the states of the market. This vector
#' is returned by function \code{\link{run_dating_alg}} or \code{\link{run_filtering_alg}}.
#' @return A data frame that contains the descriptive statistics.
#' @usage bb.summary.stat(price, bull)
#' @examples{
#' library(zoo)
#' library(xtable)
#' library(ggplot2)
#' sp500 <- sp500m # choose the monthly data
#' price <- as.vector(coredata(sp500)) # retrieve prices
#' setpar_dating_alg(4, 6, 4, 16, 20) # parameters for monthly data
#' bull <- run_dating_alg(price) # detect the states
#' bb.summary.stat(price, bull)
#' }
#' @export bb.summary.stat

bb.summary.stat <- function(price, bull) {

  # initial check for robustness
  if(!is.logical(bull))
    stop("Argument 'bull' must be of logical type!")
  if(!is.numeric(price))
    stop("Argument 'price' must be of numeric type!")
  nobs <- length(bull)
  if(length(price)!=nobs)
    stop("Mismatch in the number of observations between 'price' and 'bull'!")

  #===============================================
  # find the beginning and end of each phase
  res <- findStartEndPoints(bull)
  one.start <- res$bull.start
  one.end <- res$bull.end
  zero.start <- res$bear.start
  zero.end <- res$bear.end

  # count the lengths of bulls and bears
  nOnes <- length(one.start)
  if(nOnes <= 2)
    stop("Too little bull phases!")
  nZeros <- length(zero.start)
  if(nZeros <= 2)
    stop("Too little bear phases!")

  one.length <- rep(0,nOnes-2)
  zero.length <- rep(0,nZeros-2)

  for(i in 2:(nOnes-1)) one.length[i-1] <- one.end[i] - one.start[i] + 1
  for(i in 2:(nZeros-1)) zero.length[i-1] <- zero.end[i] - zero.start[i] + 1

  min.one <- min(one.length)
  max.one <- max(one.length)
  mean.one <- mean(one.length)
  median.one <- median(one.length)

  min.zero <- min(zero.length)
  max.zero <- max(zero.length)
  mean.zero <- mean(zero.length)
  median.zero <- median(zero.length)

  # compute the amplitudes (percentage change)
  one.amplitude <- rep(0,nOnes-2)
  zero.amplitude <- rep(0,nZeros-2)

  for(i in 2:(nOnes-1))
    one.amplitude[i-1] <- (price[one.end[i]] - price[one.start[i]-1])/price[one.start[i]-1]
  for(i in 2:(nZeros-1))
    zero.amplitude[i-1] <- (price[zero.end[i]] - price[zero.start[i]-1])/price[zero.start[i]-1]

  mean.one.amp <- mean(one.amplitude)*100
  median.one.amp <- median(one.amplitude)*100
  min.one.amp <- min(one.amplitude)*100
  max.one.amp <- max(one.amplitude)*100

  mean.zero.amp <- mean(zero.amplitude)*100
  median.zero.amp <- median(zero.amplitude)*100
  min.zero.amp <- -min(abs(zero.amplitude))*100
  max.zero.amp <- -max(abs(zero.amplitude))*100

  Bull <- c(nOnes, min.one, mean.one, median.one, max.one,
                   min.one.amp, mean.one.amp, median.one.amp, max.one.amp)
  Bear <- c(nZeros, min.zero, mean.zero, median.zero, max.zero,
                    min.zero.amp, mean.zero.amp, median.zero.amp, max.zero.amp)

  df <- data.frame(Bull=Bull, Bear=Bear)
  row.names(df) <- c("Number of phases",
                     "Minimum duration",
                     "Average duration",
                     "Median duration",
                     "Maximum duration",
                     "Minimum amplitude",
                     "Average amplitude",
                     "Median amplitude",
                     "Maximum amplitude")

  xtab <- xtable(df, digits=0, align=c("l","r","r"))
  print(xtab)
  return(df)
}

#' Prints out the dating of bull-bear states
#'
#' This function prints out (in console window) the dating of bull-bear states.
#' The outcome of this function is a table in LaTeX format.
#'
#' @param price a numeric vector of price values
#' @param bull a logical vector that contains the states of the market. This vector
#' is returned by function \code{\link{run_dating_alg}} or \code{\link{run_filtering_alg}}.
#' @param dates a vector of dates
#' @return A data frame object that contains the dating of bull-bear states.
#' @usage bb.dating.states(price, bull, dates)
#' @examples{
#' library(zoo)
#' library(xtable)
#' library(ggplot2)
#' sp500 <- sp500m # choose the monthly data
#' dates <- index(sp500) # retrieve dates
#' dates <- as.yearmon(dates) # convert dates to "yearmon" format if monthly data
#' price <- as.vector(coredata(sp500)) # retrieve prices
#' setpar_dating_alg(4, 6, 4, 16, 20) # parameters for monthly data
#' bull <- run_dating_alg(price) # detect the states
#' bb.dating.states(price, bull, dates)
#' }
#' @export bb.dating.states

bb.dating.states <- function(price, bull, dates) {

  # initial check for robustness
  if(!is.logical(bull))
    stop("Argument 'bull' must be of logical type!")
  if(!is.numeric(price))
    stop("Argument 'price' must be of numeric type!")
  nobs <- length(bull)
  if(length(price)!=nobs)
    stop("Mismatch in the number of observations between 'price' and 'bull'!")
  if(length(dates)!=nobs)
    stop("Mismatch in the number of observations between 'dates' and 'bull'!")

  #===============================================
  # find the beginning and end of each phase
  res <- findStartEndPoints(bull)
  bull.start <- res$bull.start
  bull.end <- res$bull.end
  bear.start <- res$bear.start
  bear.end <- res$bear.end

  df.bull <- phaseInfo(price, dates, bull.start, bull.end)
  df.bear <- phaseInfo(price, dates, bear.start, bear.end)

  empty.row <- data.frame(Dates=" ", Duration=NA, Amplitude=NA)
  if(bull.start[1]>bear.start[1]) {
    df.bull <- rbind(empty.row, df.bull)
  }
  len.bull <- nrow(df.bull)
  len.bear <- nrow(df.bear)
  if(len.bear < len.bull) df.bear <- rbind(df.bear,empty.row)

  empty.col <- data.frame(Col=rep(" ",len.bull))

  df.phases <- cbind(df.bull, empty.col, df.bear)

  addtorow <- list()
  addtorow$pos <- list(-1)
  addtorow$command <- paste0("\\multicolumn{3}{l}{\\bf Bull markets} & & \\multicolumn{3}{l}{\\bf Bear markets} \\\\ \\cline{1-3} \\cline{5-7} \\\\[-1.8ex]")
  hlineafter <- c(0,nrow(df.phases))

  names(df.phases) <- c("Dates", "Duration", "Amplitude", "", "Dates", "Duration", "Amplitude")
  xtab <- xtable(df.phases, digits=0)
  print(xtab, add.to.row=addtorow, hline.after=hlineafter, include.rownames = FALSE)
  return(df.phases)
}

#' Plots the log of prices and highlight bear states
#'
#' This function plots the log of prices and highlights bear states
#'
#' @param price a numeric vector of price values
#' @param bull a logical vector that contains the states of the market. This vector
#' is returned by function \code{\link{run_dating_alg}} or \code{\link{run_filtering_alg}}.
#' @param dates a vector of dates in Date format
#' @param price.name the name of the time-series of prices that will appear on the y-axis of the plot
#' @param log.scale a logical variable that specifies whether to use log scale along the y-axis
#' @return None
#' @usage bb.plot(price, bull, dates, price.name=NULL, log.scale=TRUE)
#' @examples{
#' library(zoo)
#' library(xtable)
#' library(ggplot2)
#' price <- as.vector(coredata(sp500m)) # retrieve monthly prices
#' dates <- index(sp500m) # retrieve dates from zoo-object
#' setpar_dating_alg(4, 6, 5, 15, 20) # parameters for monthly data
#' bull <- run_dating_alg(price) # detect bull-bear states
#' bb.plot(price, bull, dates, "S&P 500") # plot the result
#' }
#' @export bb.plot

bb.plot <- function(price, bull, dates, price.name=NULL, log.scale=TRUE) {

  # initial check for robustness
  if(!is.logical(bull))
    stop("Argument 'bull' must be of logical type!")
  if(!is.logical(log.scale))
    stop("Argument 'log.scale' must be of logical type!")
  if(!is.numeric(price))
    stop("Argument 'price' must be of numeric type!")
  nobs <- length(bull)
  if(length(price)!=nobs)
    stop("Mismatch in the number of observations between 'price' and 'bull'!")
  if(length(dates)!=nobs)
    stop("Mismatch in the number of observations between 'dates' and 'bull'!")

  # find the starting and ending dates of the Bear stock market phases
  xstart <- vector()
  xend <- vector()
  n <- length(bull)
  isBear <- FALSE
  for(i in 1:n) {
    if((bull[i] == FALSE) & (isBear == FALSE)) {
      xstart <- c(xstart, dates[i])
      isBear <- TRUE
    }
    if((bull[i] == TRUE) & (isBear == TRUE)) {
      xend <- c(xend, dates[i])
      isBear <- FALSE
    }
    if((i == n) & (isBear == TRUE)) xend <- c(xend, dates[i])
  }

  # create data frame with rectangle areas
  rects <- data.frame(xstart=as.Date(xstart), xend=as.Date(xend) )

  if(is.null(price.name)) {
    str <- ""
  } else {
    str <- price.name
  }

  Date=as.Date(dates)
  if(log.scale==TRUE) {
    Value=log(price)
  } else {
    Value=price
  }

  # plot the index with shaded areas for Bear states
  df <- data.frame(Date, Value)
  ggplot() +
    geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), fill="gray") +
    geom_line(data = df, aes(Date,Value)) +
    theme_bw() + xlab("") + ylab(str)

}
