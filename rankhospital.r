rankhospital<-function(state,outcome,num="best"){
         file_read<-read.csv("outcome-of-care-measures.csv", na.string="Not Available",stringsAsFactors=FALSE)
         if(num=="best"){num<-1}
         if(!(state %in% file_read[,7])){stop("ivalid state")}
         data<-file_read[,c(2,7,11,17,23)]
         if(outcome=="Heart Attack"){outcome<-3}
         if(outcome=="Heart Failure"){outcome<-4}
         if(outcome=="Pneumonia"){outcome<-5}
         if(!(outcome==3||outcome==4||outcome==5)){stop("invalid outcome")}
         data2<-data[,c(1,2,outcome)]
         ordered_data2<-data2[order(data2[,3],data2[,1]),]
         val_data<-na.omit(ordered_data2)
         val_data1<-split(val_data[,3],val_data[,2])
         val_data2<-split(val_data[,1],val_data[,2])
         needed_data<-as.numeric(unlist(val_data1[state]))
         hosp_data<-as.character(unlist(val_data2[state]))
         if(num=="worst"){num<-length(needed_data)}
         ranked<-data.frame(Hospital=hosp_data,Rank=needed_data,stringsAsFactors=FALSE)
         return(ranked[num,1])
}
