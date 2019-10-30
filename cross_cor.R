##------------------------------------------------DYNAMIC CROSSCORRELATION ON WTI DATA--------------------------------------------------------##



## the first function checks whether readxl package is installed on your computer or not
## this is a side function for import_dataset_WTI

pack_check <- function()
{
  my_packages <- library()$results
  n = length(my_packages)
  readpackage = 0
  for(i in 1:n)
  {
    if (my_packages[i] == "readxl")
    {
      readpackage <- i
    }
  }
  return(readpackage)
}



## the next function imports the (WTI) dataset that we gave in and then shows the database

import_dataset_WTI <- function()
{
  path_wti <- readline(prompt = "Please type your excel's path (C:/...): ")
  
  if (pack_check() == 0)
  {
    # readxl package is not installed, so now we have to
    install.packages("readxl")
  }
  library("readxl")
  WTI_fut <<- read_excel(path_wti)
  View(WTI_fut)
  return(WTI_fut)
}


##the next function converts the raw dataset into a new one by making the returns from day n to day n + 1
##the first column remains the same as it is our date vector, and the first row is empty as we don't have data before the first day

return_maker <- function(WTI_fut)
{
  n <- nrow(WTI_fut)
  m <- ncol(WTI_fut)
  ret_WTI_fut <<- matrix(nrow = n, ncol = m)
  for (i in 2:n)
  {
    for (j in 2:m)
    {
      ret_WTI_fut[i,j] <<- (WTI_fut[[i,j]] / WTI_fut[[i-1,j]]) - 1
    }
  }
  
  ret_WTI_fut <<- cbind(WTI_fut[,1], ret_WTI_fut)
  ret_WTI_fut = ret_WTI_fut[-1,-2]
  colnames(ret_WTI_fut) = colnames(WTI_fut)
  View(ret_WTI_fut)
  return(ret_WTI_fut)
}



#this function's inputs will be saved for other functions's parameters
#please give the dates in this format : "yyyy-mm-dd"

parameter_lists <- function(asset1, asset2, startDate, endDate, windowLength, lag)
{
  parameters<-list(asset1,asset2,startDate,endDate,windowLength,lag)
  return(parameters)
  
}



#this function generates the correlation-timeseries table
#the column name's first part shows the unlagged vector,the second part shows the lagged one
#please type the dates in this format : "yyyy-mm-dd"
#lag can be negative or positive, depends on the direction you want. e.g. -5 lagg means that the other vector will be lagged back in time
#asset1 and asset2 are integers

time_series_generating <- function(parameters)
{
  asset1<-parameters[[1]]
  asset2<-parameters[[2]]
  startDate<-parameters[[3]]
  endDate<-parameters[[4]]
  windowLength<-parameters[[5]]
  lag<-parameters[[6]]
  
  
  absStart <- min(ret_WTI_fut[,1]) #first date in the vector
  absEnd <- max(ret_WTI_fut[,1]) #largest date in the vector
  
  if (as.Date(startDate, origin = "1970-01-01") > as.Date(endDate, origin = "1970-01-01"))
  {
    return("StartDate starts after EndDate!")
  }
  if (as.Date(startDate, origin = "1970-01-01") + lag < as.Date(absStart, origin = "1970-01-01"))
  { 
    return("The starting date and the lag gets out of our dataset!")
  }
  if (as.Date(endDate, origin = "1970-01-01") + lag > as.Date(absEnd, origin = "1970-01-01")) 
  {
    return("The ending date and the lagg gets out of the dataset!")
  }
  
  for (i in 1:length(asset1))
  {
    if (as.Date(ret_WTI_fut[[i,1]]) - as.Date(startDate, origin = "1970-01-01") == 0)
    {
      startDateIndex = i 
    }
  }
  
  #first row if return matrix is NA for each item!
  n <- (as.numeric(as.Date(endDate), origin = "1970-01-01")) - (as.numeric(as.Date(startDate), origin = "1970-01-01")) - windowLength 
  #n is the length of the time series
  
  
  correlations <- vector(length = n)
  timeVector <- vector(length = n)
  #calculate the values of the vector for each item
  for (i in 1:n)
  {
    correlations[i] = cor(asset1[(startDateIndex + i - 1):(startDateIndex + i + windowLength - 1)],
                        asset2[(startDateIndex - 1 + i + lag):(startDateIndex - 1 + i + lag + windowLength)])
    timeVector[i] = as.Date(as.numeric(as.Date(startDate), origin="1970-01-01") + i - 1, origin = "1970-01-01")
  }
  
  class(timeVector) <- "Date"
  
  timeSeries <- data.frame(timeVector, correlations)
  return(timeSeries)
}

#the last function plots certain time series we made with the previous functions
plot_timeseries <- function()
{
  
  ts1 <- time_series_generating(parameter_lists(ret_WTI_fut$`2`, ret_WTI_fut$`3`, "2011-05-06", "2014-09-12", 20, 150))
  ts2 <- time_series_generating(parameter_lists(ret_WTI_fut$`4`, ret_WTI_fut$`16`, "2011-05-06", "2014-09-12", 20, 50))
  ts3 <- time_series_generating(parameter_lists(ret_WTI_fut$`5`, ret_WTI_fut$`9`, "2011-05-06", "2014-09-12", 20, 100))
  ts4 <- time_series_generating(parameter_lists(ret_WTI_fut$`7`, ret_WTI_fut$`19`, "2011-05-06", "2014-09-12", 20, 200))
  ts5 <- time_series_generating(parameter_lists(ret_WTI_fut$`22`, ret_WTI_fut$`24`, "2011-05-06", "2014-09-12", 20, 10))
  
  
  plot(ts1[,1], ts1[,2], "l", col = "red", xlab = "Time", ylab = "Correlation", main = "Dynamic cross-correlations")
  
  lines(ts1[,1], ts2[,2], "l", col = "blue")
  
  lines(ts1[,1], ts3[,2], "l", col="green")
  
  lines(ts1[,1], ts4[,2], "l", col = "darkgoldenrod1")
  
  lines(ts1[,1], ts5[,2], "l", col = "darkorchid1")
  
  legend("bottomleft", legend = c("ts1","ts2","ts3","ts4","ts5"), fill = c("red","blue","green","darkgoldenrod1","darkorchid1"), cex = 0.6)
}



##trying out the functions

pack_check()

import_dataset_WTI()

return_maker(WTI_fut)

plot_timeseries()
