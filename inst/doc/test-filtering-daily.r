rm(list=ls(all=TRUE))
library(bbdetection)
library(zoo)
library(xtable)
library(ggplot2)

prices <- as.vector(coredata(sp500d)) # retrieve prices
dates <- index(sp500d) # retrieve dates from zoo-object

setpar_filtering_alg(15, 15) # same parameters for daily and monthly data
bull <- run_filtering_alg(prices) # detect bull-bear states

# plot the result
bb.plot(prices, bull, dates, "S&P 500")

# prints out the dating of bull-bear states
Sys.setlocale("LC_TIME", "English") # Use English names for months
df <- bb.dating.states(prices, bull, dates)

# print out the summary statistis of bull-bear states
df <- bb.summary.stat(prices, bull)

