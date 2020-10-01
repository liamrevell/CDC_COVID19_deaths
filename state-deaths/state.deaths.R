state.deaths<-function(
	state="Massachusetts",
	plot=c("raw","per capita","excess","excess per capita"),
	corrected=FALSE,data=list(),...){
	plot<-plot[1]
	ss<-state
	if(state=="New York (excluding NYC)") state<-"New York"
	if(!is.null(data$Counts)) Counts<-data$Counts
	else Counts<-read.csv("https://liamrevell.github.io/data/Weekly_Counts_of_Deaths_by_State_and_Select_Causes__2014-2018.csv")
	if(!is.null(data$Provis)) Provis<-data$Provis
	else Provis<-read.csv("https://liamrevell.github.io/data/Weekly_Counts_of_Deaths_by_State_and_Select_Causes__2019-2020.csv")
	if(!is.null(data$States)) States<-data$States
	else States<-read.csv("https://liamrevell.github.io/data/US-population-by-state.csv")
	US.popn<-c(320878310,323015995,325084756,327096265,329064917,331002651)
	y2019.est<-setNames(as.numeric(gsub(",","",States$Y2019.Estimated.)),
	  sub(" ","",States$State.Territory))
	y2019.est["New York"]<-y2019.est["New York"]-8400000
	y2019.est<-c(y2019.est,setNames(8400000,"New York City"))
	State.percent<-y2019.est/sum(y2019.est)
	Deaths<-matrix(NA,52,6,dimnames=list(1:52,2015:2020))
	ii<-which(Counts[,1]==state)
	for(i in 1:4){
		jj<-which(Counts$MMWR.Year==2014+i)
		jj<-intersect(ii,jj)
		Deaths[1:52,i]<-Counts[jj[1:52],"All..Cause"]
	}
	ii<-which(Provis[,1]==state)
	for(i in 5:6){
		jj<-which(Provis$MMWR.Year==2014+i)
		jj<-intersect(ii,jj)
		Deaths[1:length(jj),i]<-Provis[jj,"All.Cause"]
	}
	PerCapita<-(Deaths/(State.percent[state]*matrix(US.popn[1:6],52,6,
		byrow=TRUE)))*1000000
	PerCapitaExcess<-PerCapita-matrix(rowMeans(PerCapita[,1:5]),52,6)
	if(corrected){
		Deaths<-Deaths*matrix(US.popn[6]/US.popn[1:6],52,6,
			byrow=TRUE)
	}
	Normal<-rowMeans(Deaths[,1:5])
	Excess<-Deaths-matrix(Normal,52,6)
	if(plot=="raw"){
		par(mfrow=c(2,1),mar=c(5.1,5.1,3.1,2.1))
		plot(NA,xlim=c(1,52),ylim=c(0,1.05*max(Deaths,na.rm=TRUE)),
			bty="n",xlab="",ylab="Death count",...)
		for(i in 1:5) lines(1:52,Deaths[,i],col="grey",...)
		d2020<-Deaths[,"2020"]
		d2020<-d2020[!is.na(d2020)]
		lines(1:(length(d2020)-5),d2020[1:(length(d2020)-5)],lwd=3,
			col=palette()[4],...)
		lines((length(d2020)-5):length(d2020),
			d2020[(length(d2020)-5):length(d2020)],lwd=3,
			col=palette()[4],lty="dashed",...)
		legend(x=mean(par()$usr[1:2]),y=par()$usr[3]-0.13*diff(par()$usr[3:4]),
			c("weekly deaths 2015-2019",
			"weekly deaths 2020"),bty="n",cex=1,
			lwd=c(1,2),col=c("grey",palette()[4]),seg.len=4,xpd=TRUE,
			xjust=0.5,yjust=1)
		legend("topleft","Data in recent weeks\nare incomplete.",lty="dashed",
			lwd=3,col=palette()[4],bty="n",cex=0.8,seg.len=4)
		grid()
		mtext(paste("a) total death count (or provisional count), all causes,",
			ss),adj=0,line=1,cex=1)
		plot(NA,xlim=c(1,52),ylim=c(0,1.05*max(colSums(Deaths,na.rm=TRUE))),
			bty="n",xlab="",ylab="Death count",...)
		for(i in 1:5) lines(1:52,cumsum(Deaths[,i]),col="grey",...)
		d2020<-Deaths[,"2020"]
		d2020<-d2020[!is.na(d2020)]
		d2020<-cumsum(d2020)
		lines(1:(length(d2020)-5),d2020[1:(length(d2020)-5)],lwd=3,
			col=palette()[4],...)
		lines((length(d2020)-5):length(d2020),
			d2020[(length(d2020)-5):length(d2020)],lwd=3,
			col=palette()[4],lty="dashed",...)
		legend(x=mean(par()$usr[1:2]),y=par()$usr[3]-0.13*diff(par()$usr[3:4]),
			c("cumulative deaths 2015-2019",
			"cumulative deaths 2020"),bty="n",cex=1,
			lwd=c(1,2),col=c("grey",palette()[4]),seg.len=4,xpd=TRUE,
			xjust=0.5,yjust=1)
		grid()
		mtext(paste("b) cumulative death count (or provisional count), all causes,",
			ss),adj=0,line=1,cex=1)
	} else if(plot=="excess"){
		par(mfrow=c(2,1),mar=c(5.1,5.1,3.1,2.1))
		plot(NA,xlim=c(1,52),ylim=c(min(Excess[,1:5]),
			1.05*max(Excess,na.rm=TRUE)),bty="n",
			xlab="",ylab="Excess death count",...)
		lines(c(1,52),c(0,0))
		for(i in 1:5) lines(1:52,Excess[,i],col="grey",...)
		e2020<-Excess[,"2020"]
		e2020<-e2020[!is.na(e2020)]
		lines(1:(length(e2020)-5),e2020[1:(length(e2020)-5)],lwd=3,
			col=palette()[4],...)
		lines((length(e2020)-5):length(e2020),
			e2020[(length(e2020)-5):length(e2020)],lwd=3,
			col=palette()[4],lty="dashed",...)
		legend(x=mean(par()$usr[1:2]),y=par()$usr[3]-0.13*diff(par()$usr[3:4]),
			c("excess weekly deaths 2015-2019",
			"excess weekly deaths 2020"),bty="n",cex=1,
			lwd=c(1,2),col=c("grey",palette()[4]),seg.len=4,xpd=TRUE,
			xjust=0.5,yjust=1)
		legend("topleft","Data in recent weeks\nare incomplete.",lty="dashed",
			lwd=3,col=palette()[4],bty="n",cex=0.8,seg.len=4)
		grid()
		mtext(paste("a) excess death count (or provisional count), all causes,",
			ss),adj=0,line=1,cex=1)
		plot(NA,xlim=c(1,52),ylim=c(min(apply(Excess,2,cumsum),na.rm=TRUE),
	    1.05*max(apply(Excess,2,cumsum),na.rm=TRUE)),
			bty="n",xlab="",ylab="Excess death count",...)
		lines(c(1,52),c(0,0))
		for(i in 1:5) lines(1:52,cumsum(Excess[,i]),col="grey",...)
		e2020<-cumsum(e2020)
		lines(1:(length(e2020)-5),e2020[1:(length(e2020)-5)],lwd=3,
			col=palette()[4],...)
		lines((length(e2020)-5):length(e2020),
			e2020[(length(e2020)-5):length(e2020)],lwd=3,
			col=palette()[4],lty="dashed",...)
		legend(x=mean(par()$usr[1:2]),y=par()$usr[3]-0.13*diff(par()$usr[3:4]),
			c("excess deaths 2015-2019",
			"excess deaths 2020"),bty="n",cex=1,
			lwd=c(1,2),col=c("grey",palette()[4]),seg.len=4,xpd=TRUE,
			xjust=0.5,yjust=1)
		grid()
		mtext(paste(
			"b) cumulative excess death count (or provisional count), all causes,",
			ss),adj=0,line=1,cex=1)
	} else if(plot=="per capita"){
		par(mfrow=c(2,1),mar=c(5.1,5.1,3.1,2.1))
		plot(NA,xlim=c(1,52),ylim=c(0,
			1.05*max(PerCapita,na.rm=TRUE)),bty="n",
			xlab="",ylab="Deaths/1M population",...)
		lines(c(1,52),c(0,0))
		for(i in 1:5) lines(1:52,PerCapita[,i],col="grey",...)
		pc2020<-PerCapita[,"2020"]
		pc2020<-pc2020[!is.na(pc2020)]
		lines(1:(length(pc2020)-5),pc2020[1:(length(pc2020)-5)],lwd=3,
			col=palette()[4],...)
		lines((length(pc2020)-5):length(pc2020),
			pc2020[(length(pc2020)-5):length(pc2020)],lwd=3,
			col=palette()[4],lty="dashed",...)
		legend("topleft","Data in recent weeks\nare incomplete.",lty="dashed",
			lwd=3,col=palette()[4],bty="n",cex=0.8,seg.len=4)
		legend(x=mean(par()$usr[1:2]),y=par()$usr[3]-0.13*diff(par()$usr[3:4]),
			c("weekly deaths/1M population 2015-2019",
			"weekly deaths/1M population 2020"),bty="n",cex=1,
			lwd=c(1,2),col=c("grey",palette()[4]),seg.len=4,xpd=TRUE,
			xjust=0.5,yjust=1)
		grid()
		mtext(paste("a) deaths/1M population (or provisional), all causes,",
			ss),adj=0,line=1,cex=1)
		plot(NA,xlim=c(1,52),ylim=c(0,1.05*max(apply(PerCapita,2,cumsum),na.rm=TRUE)),
			bty="n",xlab="",ylab="Deaths/1M population",...)
		for(i in 1:5) lines(1:52,cumsum(PerCapita[,i]),col="grey",...)
		pc2020<-cumsum(pc2020)
		lines(1:(length(pc2020)-5),pc2020[1:(length(pc2020)-5)],lwd=3,
			col=palette()[4],...)
		lines((length(pc2020)-5):length(pc2020),
			pc2020[(length(pc2020)-5):length(pc2020)],lwd=3,
			col=palette()[4],lty="dashed",...)
		legend(x=mean(par()$usr[1:2]),y=par()$usr[3]-0.13*diff(par()$usr[3:4]),
			c("cumulative deaths/1M population 2015-2019",
			"cumulative deaths/1M population 2020"),bty="n",cex=1,
			lwd=c(1,2),col=c("grey",palette()[4]),seg.len=4,xpd=TRUE,
			xjust=0.5,yjust=1)
		grid()
		mtext(paste(
			"b) cumulative deaths/1M population (or provisional), all causes,",
			ss),adj=0,line=1,cex=1)
	} else if(plot=="excess per capita"){
		par(mfrow=c(2,1),mar=c(5.1,5.1,3.1,2.1))
		plot(NA,xlim=c(1,52),ylim=c(min(PerCapitaExcess[,1:5]),
			1.05*max(PerCapitaExcess,na.rm=TRUE)),bty="n",
			xlab="",ylab="Excess deaths/1M population",...)
		lines(c(1,52),c(0,0))
		for(i in 1:5) lines(1:52,PerCapitaExcess[,i],col="grey",...)
		pce2020<-PerCapitaExcess[,"2020"]
		pce2020<-pce2020[!is.na(pce2020)]
		lines(1:(length(pce2020)-5),pce2020[1:(length(pce2020)-5)],lwd=3,
			col=palette()[4],...)
		lines((length(pce2020)-5):length(pce2020),
			pce2020[(length(pce2020)-5):length(pce2020)],lwd=3,
			col=palette()[4],lty="dashed",...)
		legend(x=mean(par()$usr[1:2]),y=par()$usr[3]-0.13*diff(par()$usr[3:4]),
			c("excess weekly deaths/1M population 2015-2019",
			"excess weekly deaths/1M population 2020"),bty="n",cex=1,
			lwd=c(1,2),col=c("grey",palette()[4]),seg.len=4,xpd=TRUE,
			xjust=0.5,yjust=1)
		legend("topleft","Data in recent weeks\nare incomplete.",lty="dashed",
			lwd=3,col=palette()[4],bty="n",cex=0.8,seg.len=4)
		grid()
		mtext(paste("a) excess deaths/1M population (or provisional), all causes,",
			ss),adj=0,line=1,cex=1)
		plot(NA,xlim=c(1,52),ylim=c(min(apply(PerCapitaExcess,2,cumsum),na.rm=TRUE),
		  1.05*max(apply(PerCapitaExcess,2,cumsum),na.rm=TRUE)),
			bty="n",xlab="",ylab="Excess deaths/1M population",...)
		lines(c(1,52),c(0,0))
		for(i in 1:5) lines(1:52,cumsum(PerCapitaExcess[,i]),col="grey",...)
		pce2020<-cumsum(pce2020)
		lines(1:(length(pce2020)-5),pce2020[1:(length(pce2020)-5)],lwd=3,
			col=palette()[4],...)
		lines((length(pce2020)-5):length(pce2020),
			pce2020[(length(pce2020)-5):length(pce2020)],lwd=3,
			col=palette()[4],lty="dashed",...)
		legend(x=mean(par()$usr[1:2]),y=par()$usr[3]-0.13*diff(par()$usr[3:4]),
			c("cumulative excess deaths/1M population 2015-2019",
			"cumulative excess deaths/1M population 2020"),bty="n",cex=1,
			lwd=c(1,2),col=c("grey",palette()[4]),seg.len=4,xpd=TRUE,
			xjust=0.5,yjust=1)
		grid()
		mtext(paste(
			"b) cumulative excess deaths/1M population (or provisional), all causes,",
			ss),adj=0,line=1,cex=1)
	}
}
