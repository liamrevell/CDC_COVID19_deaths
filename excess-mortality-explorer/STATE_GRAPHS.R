source("age.deaths.R")

age.Counts<-read.csv("https://liamrevell.github.io/data/Weekly_counts_of_deaths_by_jurisdiction_and_age_group.csv")
States<-read.csv("https://liamrevell.github.io/data/nst-est2019-01.csv",row.names=1)
partisanship<-read.csv(file="partisanship.csv")

states<-partisanship$State
pops<-setNames(States$X2019,rownames(States))[states]
percapita<-setNames(rep(NA,length(states)),states)
total<-setNames(rep(NA,length(states)),states)

for(i in 1:length(states)){
	E<-age.deaths(state=states[i],return="Excess",
		data=list(Counts=age.Counts,States=States))
	if(states[i]=="New York"){
		E2<-age.deaths(state="New York City",return="Excess",
			data=list(Counts=age.Counts,States=States))
		E<-E+E2
	} else if(states[i]=="North Carolina"){
		E[34:52,"2020"]<-NA
	}
	percapita[i]<-sum(E[,"2020"],na.rm=TRUE)/(pops[i]/1000000)
	total[i]<-sum(E[,"2020"],na.rm=TRUE)
}

bluered<-setNames(rep(0,length(states)),states)
pvi<-setNames(partisanship$PVI,states)
for(i in 1:length(states)){
	if(pvi[i]!="EVEN"){
		tmp<-strsplit(pvi[i],"+",fixed=TRUE)[[1]]
		if(tmp[1]=="R") bluered[i]<--as.numeric(tmp[2])
		else bluered[i]<-as.numeric(tmp[2])
	}
}
colfunc<-colorRamp(c("red","blue"))
COLS<-colfunc((bluered+max(abs(bluered)))/(2*max(abs(bluered))))
cols<-setNames(rep(NA,length(states)),states)
for(i in 1:nrow(COLS)) cols[i]<-rgb(COLS[i,1],COLS[i,2],COLS[i,3],maxColorValue=256)
percapita<-sort(percapita,decreasing=FALSE)
cols<-cols[names(percapita)]
pvi<-pvi[names(percapita)]

png("Excess-deaths-partisanship.png",width=8,height=10,units="in",res=600)
par(mar=c(5.1,4.1,1.1,1.1),bg="black",fg="white",col.axis="white")
h<-barplot(percapita,horiz=TRUE,col=cols,names.arg="",axes=FALSE,xlim=c(0,2700))
axis(1,at=c(0,500,1000,1500,2000),cex.axis=0.8)
title(xlab="excess deaths (2020) / 1M population",col.lab="white")
for(i in 1:length(states)) text(max(0,percapita[i]),h[i],
	paste(names(percapita)[i]," (",pvi[i],")",sep=""),pos=4,
	cex=0.7)
dev.off()

png("Total-excess-deaths-partisanship.png",width=8,height=10,units="in",res=600)
total<-sort(total,decreasing=FALSE)
cols<-cols[names(percapita)]
pvi<-pvi[names(percapita)]
par(mar=c(5.1,4.1,1.1,1.1),bg="black",fg="white",col.axis="white")
h<-barplot(total,horiz=TRUE,col=cols,names.arg="",axes=FALSE,xlim=c(0,55000))
axis(1,at=c(0,20000,40000),cex.axis=0.8)
title(xlab="excess deaths (2020)",col.lab="white")
for(i in 1:length(states)) text(max(0,total[i]),h[i],
	paste(names(percapita)[i]," (",pvi[i],")",sep=""),pos=4,
	cex=0.7)
dev.off()