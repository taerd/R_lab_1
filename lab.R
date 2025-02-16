read.csv2("DATA\\data.csv",stringsAsFactors=TRUE)->data

data<-na.omit(data) #������� ����������� ��������

#� dat�.frame ����������� �� �������� ������ ',' �� '.' � ����������� � �������� 2( as.numeric)
data <- as.data.frame(apply(apply(data, 2, gsub, patt=",", replace="."), 2, as.numeric))

classes<-as.vector(data[,1])#������ ������� ��������
values<-as.vector(data[,10])# ��������� �������� ��������

data_0<-values[classes == 0]#���������� � data_0 ��� ����������� 0
data_1<-values[classes == 1]#���������� � data_1 ��� ����������� 1

#hist(data_0,breaks = round(log(length(data_0),base=exp(1)) )+1 )

#������� ��� ������ ����������, x- vector(numeric) , label- username
Statistics<-function(x,label){
sink(paste("RESULTS\\statisticOf",label,".txt",sep=""))
cat(paste("statistic of ",label,"\n"))
cat(paste("mean : ", mean(x)),sep="\n")
cat(paste("median : ",median(x)),append=TRUE,sep="\n")
cat(paste("mode : ��� �������� ������������� "),append=TRUE,sep="\n")
cat(paste("variation : ",var(x)),append=TRUE,sep="\n")
cat(paste("difference : ",max(x)-min(x)),append=TRUE,sep="\n")
cat("quantilies : ","\n")
cat(names(quantile(x)),append=TRUE,sep="\t","\n")
cat(quantile(x),append=TRUE,sep="\t")
sink()
}

Statistics(data_0,"data_0")
Statistics(data_1,"data_1")

pdf("RESULTS\\plots.pdf")
#hist(data_0,breaks = round(log(length(data_0),base=exp(1)) )+1,main="Histogram of data_0")
h1<-hist(data_0,breaks = round(log(length(data_0),base=exp(1))+1),main="Histogram of data_0 and polygon")
#lines(density(data_0),col="green",lwd=2)
lines(h1$counts~h1$mids,col="red")
rug(data_0)

#hist(data_1,breaks = round(log(length(data_1),base=exp(1)) )+1,main="Histogram of data_1")
h2<-hist(data_1,breaks = round(log(length(data_1),base=exp(1)) )+1,main="Histogram of data_1 and polygon")
lines(h2$counts~h2$mids,col="red")
rug(data_1)

f0<-sort(data_0)
v0<-seq(from=1/length(f0), to=1, by=1/length(f0))
plot(f0,v0,type="s",main="F(x) ��� data_0",xlab="�������� data_0",ylab="�����������")
#lines(f0,v0,col="red")

f1<-sort(data_1)
v1<-seq(from=1/length(f1), to=1, by=1/length(f1))
plot(f1,v1,type="s",main="F(x) ��� data_1",xlab="�������� data_1",ylab="�����������")
#lines(f1,v1,col="red")

boxplot(data_0,data_1,xlab="0 � 1",ylab="��������")

dev.off()
