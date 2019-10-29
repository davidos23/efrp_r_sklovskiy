
pack_check <- function(){
  my_packages <- library()$results
  n=length(my_packages)
  readpackage=0
  for(i in 1:n){
    if (my_packages[i] == "readxl"){
      readpackage<-i
    }
  }
  return(readpackage)
  ## this function checks whether readxl package is installed on your computer or not . 
  ##side function for import_dataset_WTI
}

import_dataset_WTI <-function(){
   path_wti <- readline(prompt="Please type your excel's path (C:/...): ")
   
   if (pack_check() == 0){
     #readxl package is not installed so now we have to
     install.packages("readxl")
   }
   library("readxl")
   WTI_fut <<- read_excel(path_wti)
  # View(WTI_fut)
   return(WTI_fut)
   ## this function imports the (WTI) dataset that we gave and then shows it
 }
 
return_maker<-function(WTI_fut){
   n<-nrow(WTI_fut)
   m<-ncol(WTI_fut)
   ret_WTI_fut<<-matrix(nrow=n,ncol=m)
   for (i in 2:n){
     for (j in 2:m){
       ret_WTI_fut[i,j]<<-(WTI_fut[[i,j]]/WTI_fut[[i-1,j]])-1
     }
   }
   ret_WTI_fut<<-cbind(WTI_fut[,1],ret_WTI_fut)
   ret_WTI_fut = ret_WTI_fut[-1,-2]
   colnames(ret_WTI_fut)=colnames(WTI_fut)
   View(ret_WTI_fut)
   return(ret_WTI_fut)
   #this function converts the raw dataset into a new one by making the returns from day n to day n+1
   #the first column remains the same as it is our date vector, and the first row is empty as we don't have older data
 }
  
parameter_lists <-function(AssetList,StartDate,EndDate,WindowLength,Lagg){
  #here build in the return_maker function in order to have 1 return matrix instead of two!
  AssetList <<- AssetList
  StartDate <<- as.Date.character(StartDate)
  EndDate <<- as.Date.character(EndDate)
  WindowLength <<- WindowLength
  Lagg <<- Lagg
  #this function's inputs will be saved for other functions's parameters
  #please give the date in this format : "yyyy-mm-dd"
}




time_series_generating<-function(Asset1,Asset2,StartDate,EndDate,WindowLength,Lagg){
  
  AbsStart <- min(ret_WTI_fut[,1]) # first date in the vector
  AbsEnd <- max(ret_WTI_fut[,1]) # largest date in the vector
  
  if (as.Date(StartDate, origin = "1970-01-01")>as.Date(EndDate, origin = "1970-01-01")){return("StartDate starts after EndDate!")}
  if (as.Date(StartDate, origin = "1970-01-01")+Lagg<as.Date(AbsStart, origin = "1970-01-01")){ return("The starting date and the lagg gets out of our dataset!")}
  if (as.Date(EndDate, origin = "1970-01-01")+Lagg>as.Date(AbsEnd, origin = "1970-01-01")) {return("The ending date and the lagg gets out of the dataset!")}
  
  for (i in 1:length(Asset1)){# Dávid kódja kevesebb futtatást igényel,de a formátumban nem vagyok biztos ezért most ezt hagyom
    if (as.Date(ret_WTI_fut[[i,1]])-as.Date(StartDate, origin = "1970-01-01")==0){
      StartDateIndex=i 
    }
  }
  
  
  #first row if return matrix is NA for each item!
  n <- (as.numeric(as.Date(EndDate), origin = "1970-01-01"))-(as.numeric(as.Date(StartDate), origin = "1970-01-01"))-WindowLength #n is the length of the time series ITT BESZARIK MERT NEM JÓ A DÁTUM, RAKJ BELE as.Date()
  Correlations <- vector(length = n)
  TimeVector <-vector(length=n)
  for (i in 1:n){ # calculate the values of the vector for each item
    Correlations[i]=cor(Asset1[(StartDateIndex+i-1):(StartDateIndex+i+WindowLength-1)],
                      Asset2[(StartDateIndex-1+i+Lagg):(StartDateIndex-1+i+Lagg+WindowLength)])
    TimeVector[i]=as.Date(as.numeric(as.Date(StartDate), origin="1970-01-01")+i-1, origin="1970-01-01")
  }
  class(TimeVector)<-"Date"
  #please type the dates in this format : "yyyy-mm-dd"
  #Lagg can be negative or positive, depends on the direction you want. ex. -5 lagg means that the other vector will be lagged back in time
  #Asset1 and Asset2 are integers
  TimeSeries <- data.frame(TimeVector,Correlations)
  return(TimeSeries)
}
 #this function generates the correlation-timeseries table
 #first you have to run parameter_lists function to get the parameters (as it was asked in the e-mail)
 #the column name's first part shows the unlagged vector,the second part shoes the lagged one

plot_timeseries<-function(){
  
  ts1<-time_series_generating(ret_WTI_fut$`2`, ret_WTI_fut$`3`, "2011-05-06", "2014-09-12", 20, 5)
  ts2<-time_series_generating(ret_WTI_fut$`2`, ret_WTI_fut$`6`, "2011-05-06", "2014-09-12", 20, 5)
  ts3<-time_series_generating(ret_WTI_fut$`2`, ret_WTI_fut$`9`, "2011-05-06", "2014-09-12", 20, 5)
  ts4<-time_series_generating(ret_WTI_fut$`2`, ret_WTI_fut$`12`, "2011-05-06", "2014-09-12", 20, 5)
  ts5<-time_series_generating(ret_WTI_fut$`2`, ret_WTI_fut$`15`, "2011-05-06", "2014-09-12", 20, 5)
  
  plot(ts1[,1],ts1[,2],"l",col="red", xlab="Time", ylab="Cross-Correlation")
  par(new=TRUE)
  plot(ts1[,1],ts2[,2],"l",col="blue", xlab="Time", ylab="Cross-Correlation")
  par(new=TRUE)
  plot(ts1[,1],ts3[,2],"l",col="green", xlab="Time", ylab="Cross-Correlation")
  par(new=TRUE)
  plot(ts1[,1],ts4[,2],"l",col="darkgoldenrod1", xlab="Time", ylab="Cross-Correlation")
  par(new=TRUE)
  plot(ts1[,1],ts5[,2],"l",col="darkorchid1", xlab="Time", ylab="Cross-Correlation")
}





pack_check()

import_dataset_WTI()

return_maker(WTI_fut)

TimeSeries <- time_series_generating(ret_WTI_fut$`2`, ret_WTI_fut$`3`, "2011-05-06", "2014-09-12", 20, 5)

plot_timeseries()
