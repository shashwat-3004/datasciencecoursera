complete<- function(directory,id=1:332){
        nob<-list()
        idnum<-list()
        wd<-getwd()
        for (val in id){

            if (val<10){
                 id_value<-paste("0","0",val,sep="")
            }
            if (val>=10 & val<100){
                 id_value<-paste("0",val,sep="")
            }
            if (val>=100){
                 id_value<-val
            }
            file_name<-paste(wd,"/",directory,"/",id_value,".csv",sep="")
            file_read<-read.csv(file_name,header=TRUE)

            ans<-length(which(is.na(file_read["nitrate"])==FALSE &is.na(file_read["sulfate"])==FALSE))

            nob<-append(nob,ans)
            idnum<-append(idnum,val)
        }
        final<-matrix(c(idnum,nob),ncol=2)
        colnames(final)<-c("id","nobs")
        rownames(final)<-c(1:length(idnum))
        return(final)
}
cor_value<-function(directory, threshold){
  wd<-getwd()
  id=1:332
  count<-0
  fb<-numeric()
  for (val in id){

      if (val<10){
           id_value<-paste("0","0",val,sep="")
      }
      if (val>=10 & val<100){
           id_value<-paste("0",val,sep="")
      }
      if (val>=100){
           id_value<-val
      }
      file_name<-paste(wd,"/",directory,"/",id_value,".csv",sep="")
      file_read<-read.csv(file_name,header=TRUE)
      compobs<-sum(complete.cases(file_read))
      if (compobs>= threshold){
          cvalue<-cor(file_read["nitrate"],file_read["sulfate"],use="na.or.complete")
          count<-count+1
          fb<-append(fb,cvalue)
      }

  }
  if (count==0){return(c(0))}
  return(fb)
}
