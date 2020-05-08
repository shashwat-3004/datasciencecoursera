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
        print(final)
}
