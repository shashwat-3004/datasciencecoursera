pollutantmean<- function(directory,pollutant,id=1:332){
        sum_value<-list()
        nvalue<-list()

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
            xrt<-colSums(file_read[pollutant],na.rm=TRUE)
            sum_value<-append(sum_value,xrt)
            nco<-nrow(na.omit(file_read[pollutant]))
            nvalue<-append(nvalue,nco)
        }
        mat_sum<-matrix(as.numeric(sum_value),ncol=1)
        n_mean<-matrix(as.numeric(nvalue),ncol=1)
        ans<-colSums(mat_sum)/colSums(n_mean)
        print(ans)
}
