# This functions gives data frame of the specified ranked hospital of every state in the fight against the specified outcome
# example rankall("Heart Attack",5) will give the 5th ranked hospital in every state dealing with heart attack
rankall<-function(outcome,num="best"){
  file_read<-read.csv("outcome-of-care-measures.csv", na.string="Not Available",stringsAsFactors=FALSE)
  
  if(num=="best"){num<-1} # rank=1
  
  data<-file_read[,c(2,7,11,17,23)]  # taking all the required data
  if(outcome=="Heart Attack"){outcome<-3} 
  if(outcome=="Heart Failure"){outcome<-4}  # disesases in the dataset 
  if(outcome=="Pneumonia"){outcome<-5}
  if(!(outcome==3||outcome==4||outcome==5)){stop("invalid outcome")}
  
  data2<-data[,c(1,2,outcome)]   # 2nd data for the specified disease, state and hospital 
  ordered_data2<-data2[order(data2[,3],data2[,1]),] # ordering death rate of the disease in increasing order
  
  val_data<-na.omit(ordered_data2) # removing the NA values
  processed<-val_data[,c(1,2)] # taking the state and hospital name only
  
  val_dataspl<-split(processed[,1],processed[,2]) #list in which state-wise hospitals are mentioned
  state<-unique(processed[,2])
  char<-character()
  stach<-character()
  state<-sort(state)
  if(num!="worst"){ 
      for(i in 1:length(state)){
          unlink<-as.character(unlist(val_dataspl[i]))
          wrt<-unlink[num]
          char<-append(char,wrt)

          stach<-append(stach,state[i])
      }
  }
  if(num=="worst"){  #worst is the last ranked hospital for each state
      for(i in 1:length(state)){
          unlink<-as.character(unlist(val_dataspl[i]))
          wrt<-unlink[length(unlink)]
          char<-append(char,wrt)

          stach<-append(stach,state[i])
      }

  }
  ans<-data.frame(Hospital=char,State=stach,stringsAsFactors=FALSE) #data frame consisting of the mentioned rank hospital of every state
  row.names(ans)<-stach
  return(ans)

}
