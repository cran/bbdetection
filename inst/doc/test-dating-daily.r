rm(list=ls(all=TRUE))
library(bbdetection)
library(zoo)
library(xtable)
library(ggplot2)

prices <- as.vector(coredata(sp500d)) # retrieve prices
dates <- index(sp500d) # retrieve dates from zoo-object

setpar_dating_alg(8*21, 6*21, 4*21, 16*21, 20) # parameters for daily data
bull <- run_dating_alg(prices) # detect bull-bear states

# plot the result
bb.plot(prices, bull, dates, "S&P 500")

# prints out the dating of bull-bear states
Sys.setlocale("LC_TIME", "English") # Use English names for months
df <- bb.dating.states(prices, bull, dates)

# print out the summary statistis of bull-bear states
df <- bb.summary.stat(prices, bull)

