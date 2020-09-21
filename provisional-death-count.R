Data<-read.csv("Weekly_counts_of_death_by_age_Full_Data_data_under25.csv")
Weekly<-matrix(NA,52,6,
	dimnames=list(1:52,2015:2020))
for(i in 1:6){
	ii<-which(Data$Year==(2014+i))
	Weekly[ii-52*(i-1),as.character(2014+i)]<-Data[ii,"Number.of.Deaths"]
}
png("Excess-deaths-u25.png",width=10.8,height=9.5,units="in",res=600)
par(mfrow=c(2,1),mar=c(5.1,5.1,2.1,2.1))
plot(NA,xlim=c(1,52),ylim=c(0,max(Weekly,na.rm=TRUE)),bty="n",xlab="Week (1:52)",
	ylab="",las=1,cex.axis=0.8,cex.lab=0.9)
title(ylab="CDC provisional death count (<25)",line=3.5,cex.lab=0.9)
barplot(Weekly[,"2020"],width=0.8,space=0.25,add=TRUE,axes=FALSE,names.arg="")
for(i in 1:5){
	lines(1:52,Weekly[,i])
}
lines(1:52,rowMeans(Weekly[,1:5]),lwd=4,col=palette()[4])
legend("bottomright",c("average weekly deaths","weekly deaths 2015-2019",
	"weekly deaths 2020"),bty="n",pch=c(NA,NA,22),pt.bg=c(NA,NA,"grey"),
	cex=0.7,pt.cex=c(NA,NA,1.2),lwd=c(2,1,NA),col=c(palette()[4],"black",
	"black"))
Expected<-rowMeans(Weekly[,1:5])
deaths.2020<-Weekly[,"2020"]
deaths.2020<-deaths.2020[!is.na(deaths.2020)]
excess.2020<-deaths.2020-Expected[1:length(deaths.2020)]
cs<-cumsum(excess.2020)
plot(NA,xlim=c(1,52),ylim=c(-1000,200),
	bty="n",xlab="Week (1:52)",
	ylab="",las=1,cex.axis=0.8,cex.lab=0.9)
title(ylab="Cumulative CDC excess deaths (<25)",line=3.5,cex.lab=0.9)
lines(1:(length(deaths.2020)-6),cs[1:(length(cs)-6)],lwd=4,col=palette()[4])
lines(1:7+(length(deaths.2020)-7),cs[1:7+(length(deaths.2020)-7)],lwd=4,
	lty="dotted",,col=palette()[4])
legend("bottomleft","source: https://www.cdc.gov/nchs/nvss/vsrr/covid19/excess_deaths.htm",
	cex=0.7,bty="n")
legend("topright","Data in recent weeks\nare incomplete.",lty="dashed",
	lwd=2,col=palette()[4],bty="n",cex=0.7,seg.len=4)
dev.off()


Data<-read.csv("Weekly_counts_of_death_by_age_Full_Data_data_over65.csv")
ages<-levels(as.factor(Data[,"ï..Age.Group"]))
Weekly<-matrix(NA,52,6,
	dimnames=list(1:52,2015:2020))
for(i in 1:length(ages)){
	ii<-which(Data[,"ï..Age.Group"]==ages[i])
	Tmp<-Data[ii,]
	for(j in 1:6){
		ii<-which(Tmp$Year==(2014+j))
		if(i==1) Weekly[ii-52*(j-1),as.character(2014+j)]<-0
		Weekly[ii-52*(j-1),as.character(2014+j)]<-
			Weekly[ii-52*(j-1),as.character(2014+j)]+
			Tmp[ii,"Number.of.Deaths"]
	}
}
png("Excess-deaths-o65.png",width=10.8,height=9.5,units="in",res=600)
par(mfrow=c(2,1),mar=c(5.1,5.1,2.1,2.1))
plot(NA,xlim=c(1,52),ylim=c(0,max(Weekly,na.rm=TRUE)),bty="n",xlab="Week (1:52)",
	ylab="",las=1,cex.axis=0.8,cex.lab=0.9)
title(ylab="CDC provisional death count (>65)",line=3.5,cex.lab=0.9)
barplot(Weekly[,"2020"],width=0.8,space=0.25,add=TRUE,axes=FALSE,names.arg="")
for(i in 1:5){
	lines(1:52,Weekly[,i])
}
lines(1:52,rowMeans(Weekly[,1:5]),lwd=4,col=palette()[4])
Expected<-rowMeans(Weekly[,1:5])
deaths.2020<-Weekly[,"2020"]
deaths.2020<-deaths.2020[!is.na(deaths.2020)]
excess.2020<-deaths.2020-Expected[1:length(deaths.2020)]
cs<-cumsum(excess.2020)
plot(NA,xlim=c(1,52),ylim=c(-100,220000),
	bty="n",xlab="Week (1:52)",
	ylab="",las=1,cex.axis=0.8,cex.lab=0.9)
title(ylab="Cumulative CDC excess deaths (>65)",line=3.5,cex.lab=0.9)
lines(1:(length(deaths.2020)-6),cs[1:(length(cs)-6)],lwd=4,col=palette()[4])
lines(1:7+(length(deaths.2020)-7),cs[1:7+(length(deaths.2020)-7)],lwd=4,
	lty="dotted",,col=palette()[4])
legend("bottomright","source: https://www.cdc.gov/nchs/nvss/vsrr/covid19/excess_deaths.htm",
	cex=0.7,bty="n")
legend("topright","Data in recent weeks\nare incomplete.",lty="dashed",
	lwd=2,col=palette()[4],bty="n",cex=0.7,seg.len=4)
dev.off()