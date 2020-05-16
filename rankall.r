rankall<-function(outcome,num="best"){
  file_read<-read.csv("outcome-of-care-measures.csv", na.string="Not Available",stringsAsFactors=FALSE)
  if(num=="best"){num<-1}
  data<-file_read[,c(2,7,11,17,23)]
  if(outcome=="Heart Attack"){outcome<-3}
  if(outcome=="Heart Failure"){outcome<-4}
  if(outcome=="Pneumonia"){outcome<-5}
  if(!(outcome==3||outcome==4||outcome==5)){stop("invalid outcome")}
  data2<-data[,c(1,2,outcome)]
  ordered_data2<-data2[order(data2[,3],data2[,1]),]
  val_data<-na.omit(ordered_data2)
  processed<-val_data[,c(1,2)]
  val_dataspl<-split(processed[,1],processed[,2])
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
  if(num=="worst"){
      for(i in 1:length(state)){
          unlink<-as.character(unlist(val_dataspl[i]))
          wrt<-unlink[length(unlink)]
          char<-append(char,wrt)

          stach<-append(stach,state[i])
      }

  }
  ans<-data.frame(Hospital=char,State=stach,stringsAsFactors=FALSE)
  row.names(ans)<-stach
  return(ans)

}
