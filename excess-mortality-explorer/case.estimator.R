## https://data.cdc.gov/Case-Surveillance/United-States-COVID-19-Cases-and-Deaths-by-State-o/9mfq-cb36

moving.average<-function(x,window=7){
	if(window%%2==0){
		cat("window should be an even value - adding +1.\n")
		window<-window+1
	}
	xx<-c(rep(0,window/2),x,rep(x[length(x)],window/2))
	ma<-rep(NA,length(x))
	for(i in 1:length(x)) ma[i]<-mean(xx[0:(window-1)+i])
	ma
}

case.estimator<-function(state="Massachusetts",
	cumulative=FALSE,
	data=list(),
	delay=20,
	ifr=0.005,
	...){ 
	if(length(ifr)==1) ifr<-rep(ifr,366)
	else if(length(ifr)>1){
		tmp<-vector()
		dd<-round(seq(0,366,length.out=length(ifr)))
		for(i in 1:(length(ifr)-1)){
			tmp<-c(tmp,seq(ifr[i],ifr[i+1],length.out=dd[i+1]-dd[i]))
		}
		ifr<-tmp
	}
	ms<-cumsum(c(0,31,29,31,30,31,30,31,31,30,31,30,31))
	mm<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
		"Sep","Oct","Nov","Dec","Jan (2021)")
	par(mfrow=c(2,1),mar=c(5.1,5.1,3.1,2.1),bg="transparent")
	plot(NA,xlim=c(1,366),ylim=100*c(0,0.02),bty="n",
		ylab="",xlab="",axes=FALSE)
	title(ylab="assumed IFR (%)",line=4)
	Args<-list(...)
	Args$side<-2
	h<-do.call(axis,Args)
	abline(h=h,col=grey(0.75),lwd=1,lty="dotted")
	Args$side<-1
	Args$at<-ms
	Args$labels<-mm
	v<-do.call(axis,Args)
	lines(1:length(ifr),100*ifr,lwd=2,col=palette()[4])
	mtext("a) assumed infection fatality ratio (%)",adj=0,line=1,cex=1.2)
	state.codes<-setNames(
		c("US","AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI",
		"ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS",
		"MO","MT","NE","NV","NH","NJ","NM","NY","NYC","NC","ND","OH","OK",
		"OR","PA","PR","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV",
		"WI","WY"),
		c("United States","Alabama","Alaska","Arizona",
		"Arkansas","California","Colorado","Connecticut",
		"Delaware","District of Columbia","Florida",
		"Georgia","Hawaii","Idaho","Illinois",
		"Indiana","Iowa","Kansas","Kentucky","Louisiana",
		"Maine","Maryland","Massachusetts","Michigan","Minnesota",
		"Mississippi","Missouri","Montana","Nebraska","Nevada",
		"New Hampshire","New Jersey","New Mexico","New York (excluding NYC)",
		"New York City","North Carolina","North Dakota",
		"Ohio","Oklahoma","Oregon","Pennsylvania","Puerto Rico",
		"Rhode Island","South Carolina","South Dakota","Tennessee",
		"Texas","Utah","Vermont","Virginia",
		"Washington","West Virginia","Wisconsin","Wyoming"))
	if(!is.null(data$Cases)) Cases<-data$Cases
	else Cases<-read.csv(file="United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv")
	Cases<-Cases[Cases$state==state.codes[state],]
	if(cumulative==FALSE){
		estCases<-moving.average(c(rep(0,21),Cases$new_death))
		estCases<-estCases[(delay+1):length(estCases)]
		estCases<-estCases/ifr[1:length(estCases)]
		plot(NA,xlim=c(1,366),ylim=c(0,max(estCases)),bty="n",
			xlab="",
			ylab="",axes=FALSE)
		title(ylab="cases (observed or estimated)",line=4)
		Args<-list(...)
		Args$side<-2
		h<-do.call(axis,Args)
		abline(h=h,col=grey(0.75),lwd=1,lty="dotted")
		Args$side<-1
		Args$at<-ms
		Args$labels<-mm
		v<-do.call(axis,Args)
		polygon(c(1:length(estCases),length(estCases),1),
			c(estCases,0,0),
			border=FALSE,col="darkgreen")
		obsCases<-moving.average(c(rep(0,21),Cases$new_case))
		polygon(c(1:length(obsCases),length(obsCases),1),
			c(obsCases,0,0),
			border=FALSE,col=palette()[4])
		mtext(paste("b)",state,"daily observed or estimated cases"),
			adj=0,line=1,cex=1.2)
		legend(x="topright",c("observed cases",
			"estimated cases"),pch=c(22,22),
			pt.bg=c(palette()[4],"darkgreen"),pt.cex=1.5,
			bty="n",cex=1,xpd=TRUE,
			xjust=0.5,yjust=1)
	} else {
		estCases<-c(rep(0,21),Cases$tot_death)
		estCases<-estCases[(delay+1):length(estCases)]
		estCases<-estCases/ifr[1:length(estCases)]
		plot(NA,xlim=c(1,366),ylim=c(0,max(estCases)),bty="n",
			ylab="",
			xlab="",axes=FALSE)
		title(ylab="cases (observed or estimated)",line=4)
		Args<-list(...)
		Args$side<-2
		h<-do.call(axis,Args)
		abline(h=h,col=grey(0.75),lwd=1,lty="dotted")
		Args$side<-1
		Args$at<-ms
		Args$labels<-mm
		v<-do.call(axis,Args)
		polygon(c(1:length(estCases),length(estCases),1),
			c(estCases,0,0),
			border=FALSE,col="darkgreen")
		obsCases<-c(rep(0,21),Cases$tot_case)
		polygon(c(1:length(obsCases),length(obsCases),1),
			c(obsCases,0,0),
			border=FALSE,col=palette()[4])
		mtext(paste("b)",state,"cumulative observed or estimated cases"),
			adj=0,line=1,cex=1.2)
		legend(x="topright",c("observed cases",
			"estimated cases"),pch=c(22,22),
			pt.bg=c(palette()[4],"darkgreen"),pt.cex=1.5,
			bty="n",cex=1,xpd=TRUE,
			xjust=0.5,yjust=1)
	}
}

