# This function returns the Hospital name in the specified state which hs the least death rate for the specified disease.

best<-function(state,outcome){

      file_read<-read.csv("outcome-of-care-measures.csv", na.string="Not Available",stringsAsFactors=FALSE)

      if(!(state %in% file_read[,7])){stop("ivalid state")} #for invalid argument

      data<-file_read[,c(2,7,11,17,23)]  #taking only the required data 2-Hospital name,7-State,(11,17,23)=death rate due to the disease
      if(outcome=="heart attack"){outcome<-3}

      if(outcome=="heart failure"){outcome<-4}# in data the columns for death rate will be 3,4,5

      if(outcome=="pneumonia"){outcome<-5}

      if(!(outcome==3||outcome==4||outcome==5)){stop("invalid outcome")}# for invalid disease

      data2<-data[,c(1,2,outcome)] #2nd data set consisting of state , hospital,and the specific disease mentioned in the argument
      ordered_data2<-data2[order(data2[,1],data2[,3]),] #ordering of dataset in increasing order of death rate

      val_data<-na.omit(ordered_data2)#Removing the NA values
      val_data1<-split(val_data[,3],val_data[,2])# Gives a list of the death rate of the disesase of various hospital in every state

      val_data2<-split(val_data[,1],val_data[,2])# Gives a list of hospitals in every indic=vidual state
      needed_data<-as.numeric(unlist(val_data1[state]))  # converting a list into a numeric vector

      hosp_data<-as.character(unlist(val_data2[state])) # converting the list into character vector

      minimun_index<-which.min(needed_data) # finding the index of the minimum value of deth rate
      return(hosp_data[minimun_index])# returns the hospital name in the specified state with the least death rate of th especified disease
}
