png(file="deceptive-death-count.png",width=10,height=5.4,units="in",res=600)
average<-rowMeans(corrected[,1:4])
par(mar=c(5.1,4.1,2.1,2.1))
plot(NA,xlim=c(1,52),ylim=c(50000,80000),bty="n",
	xlab="Week (1:52)",ylab="CDC provisional death count (all ages)",
	axes=FALSE)
axis(1,cex.axis=0.8)
axis(2,labels=FALSE,at=c(60000,70000,80000))
mtext(expression(paste(6,"x",10^4)),2,line=1,las=1,cex=0.5,at=60000)
mtext(expression(paste(7,"x",10^4)),2,line=1,las=1,cex=0.5,at=70000)
mtext(expression(paste(8,"x",10^4)),2,line=1,las=1,cex=0.5,at=80000)
for(i in 1:34){ 
	polygon(x=c(0.05,0.5,0.5,0.05)+(i-1),
		c(50000,50000,corrected[i,"2020"],
		corrected[i,"2020"]),col=palette()[2],
		border=FALSE)
}
for(i in 1:52){
	polygon(x=c(0.5,0.95,0.95,0.5)+(i-1),
		c(50000,50000,average[i],average[i]),
		col=palette()[4],border=FALSE)

}
legend("topright",c("average weekly deaths\n2016-2019",
	"weekly deaths 2020"),bty="n",pch=c(15,15),col=palette()[c(4,2)],
	cex=0.7,pt.cex=1.5)
dev.off()