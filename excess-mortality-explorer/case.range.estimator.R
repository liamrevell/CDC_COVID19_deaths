infection.range.estimator<-function(state="Massachusetts",
	cumulative=FALSE,
	data=list(),
	delay=20,
	ifr.low=0.004,
	ifr.high=0.006,
	window=7,
	smooth=TRUE,
	span=c(0.2,0.3),
	percent=FALSE,
	plot=TRUE,
	bg="transparent",
	xlim=c(45,366-15),
	...){
	if(length(ifr.low)>1&&length(ifr.low)==length(ifr.high)){
		IFR<-rbind(ifr.low,ifr.high)
		ifr.low<-apply(IFR,2,min)
		ifr.high<-apply(IFR,2,max)
	}
	if(smooth) if(length(span)==1) span<-c(span,0.3)
	cols<-c("darkgreen",palette()[c(4,2)])
	if(length(ifr.low)==1) ifr.low<-rep(ifr.low,366)
	else if(length(ifr.low)>1&&length(ifr.low)<366){
		tmp<-vector()
		dd<-round(seq(0,366,length.out=length(ifr.low)))
		for(i in 1:(length(ifr.low)-1)){
			tmp<-c(tmp,seq(ifr.low[i],ifr.low[i+1],length.out=dd[i+1]-dd[i]))
		}
		if(smooth){
			tt<-1:366
			tmp<-predict(loess(tmp~tt,span=span[2]))
		}
		ifr.low<-tmp
	} else ifr.low<-ifr.low[1:366]
	if(length(ifr.high)==1) ifr.high<-rep(ifr.high,366)
	else if(length(ifr.high)>1&&length(ifr.high)<366){
		tmp<-vector()
		dd<-round(seq(0,366,length.out=length(ifr.high)))
		for(i in 1:(length(ifr.high)-1)){
			tmp<-c(tmp,seq(ifr.high[i],ifr.high[i+1],length.out=dd[i+1]-dd[i]))
		}
		if(smooth){
			tt<-1:366
			tmp<-predict(loess(tmp~tt,span=span[2]))
		}
		ifr.high<-tmp
	} else ifr.high<-ifr.high[1:366]
	ms<-cumsum(c(0,31,29,31,30,31,30,31,31,30,31,30,31))
	mm<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
		"Sep","Oct","Nov","Dec","Jan (2021)")
	if(plot){
		par(mfrow=c(2,1),mar=c(5.1,5.1,3.1,3.1),bg=bg)
		plot(NA,xlim=xlim,ylim=100*c(0,0.02),bty="n",
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
		polygon(c(1:length(ifr.low),length(ifr.high):1),
			100*c(ifr.low,ifr.high[length(ifr.high):1]),
			col=make.transparent(cols[2],0.25),
			border=FALSE)
		ifr.mid<-colMeans(rbind(ifr.low,ifr.high))
		lines(1:length(ifr.low),100*ifr.mid,
			lwd=2,col=cols[2])
		mtext("a) assumed infection fatality ratio (%) and daily deaths",
			adj=0,line=1,cex=1.2)
	}
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
	else Cases<-read.csv("https://liamrevell.github.io/data/United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv")
	Cases<-fixCases(Cases)
	if(!(state%in%c("New York","United States"))) Cases<-Cases[Cases$state==state.codes[state],]
	else if(state=="United States"){
		Temp<-list(
			new_death=lapply(state.codes[2:length(state.codes)],
				function(x,Data) Data[Data$state==x,"new_death"],
				Data=Cases))
		ll<-sapply(Temp$new_death,length)
		max.ll<-max(ll)
		if(any(ll!=max.ll)){
			ww<-which(ll!=max.ll)
			for(i in 1:length(ww))
				Temp$new_death[[ww[i]]]<-c(Temp$new_death[[ww[i]]],
					rep(0,max.ll-length(Temp$new_death[[ww[i]]])))
		}
		Temp<-data.frame(
			new_death=rowSums(as.data.frame(Temp$new_death)))
		Cases<-Temp
	} else if(state=="New York"){
		ii<-grep(state,names(state.codes))
		Temp<-data.frame(
			new_death=rowSums(sapply(state.codes[ii],
				function(x,Data) Data[Data$state==x,"new_death"],
				Data=Cases)))
		Cases<-Temp
	}
	newDeaths<-c(rep(0,21),Cases$new_death)
	ylim<-c(-0.04,1.04)*max(newDeaths)
	par(usr=c(par()$usr[1:2],ylim))
	for(i in 1:length(newDeaths)){
		col<-col2rgb(cols[3])/256
		col<-rgb(col[1],col[2],col[2],alpha=0.2)
		polygon(i+c(-0.5,0.5,0.5,-0.5),
			c(0,0,newDeaths[i],newDeaths[i]),
			border=FALSE,col=col)
	}
	Args<-list(...)
	Args$side<-4
	Args$labels<-FALSE
	h<-do.call(axis,Args)
	Args$at<-h
	Args$labels<-if(max(newDeaths)>1000) paste(h/1000,"k",sep="") else h
	do.call(axis,Args)
	legend("topright",c("assumed IFR (%)",
		"daily COVID-19 deaths"),pch=c(NA,15),
		col=c(cols[2],col),
		cex=0.9,pt.cex=c(NA,1.5),lwd=c(2,NA),
		box.col="transparent")
	if(state!="New York"){
		e.high<-infection.estimator(state,cumulative=cumulative,data=data,delay=delay,
			ifr=ifr.low,window=window,smooth=smooth,span=span,plot=FALSE,
			percent=percent)
		e.mid<-infection.estimator(state,cumulative=cumulative,data=data,delay=delay,
			ifr=ifr.mid,window=window,smooth=smooth,span=span,plot=FALSE,
			percent=percent)
		e.low<-infection.estimator(state,cumulative=cumulative,data=data,delay=delay,
			ifr=ifr.high,window=window,smooth=smooth,span=span,plot=FALSE,
			percent=percent)
	} else {
		states<-c("New York (excluding NYC)","New York City")
		if(percent){ 
			States<-state.deaths(plot="States")
			pp<-sum(States[c("New York","New York City"),"2020"])/100
		} else pp<-1
		e.high<-(infection.estimator(states[1],cumulative=cumulative,data=data,delay=delay,
			ifr=ifr.low,window=window,smooth=smooth,span=span,plot=FALSE,
			percent=FALSE)+
			infection.estimator(states[2],cumulative=cumulative,data=data,delay=delay,
			ifr=ifr.low,window=window,smooth=smooth,span=span,plot=FALSE,
			percent=FALSE))/pp
		e.mid<-(infection.estimator(states[1],cumulative=cumulative,data=data,delay=delay,
			ifr=ifr.mid,window=window,smooth=smooth,span=span,plot=FALSE,
			percent=FALSE)+
			infection.estimator(states[2],cumulative=cumulative,data=data,delay=delay,
			ifr=ifr.mid,window=window,smooth=smooth,span=span,plot=FALSE,
			percent=FALSE))/pp
		e.low<-(infection.estimator(states[1],cumulative=cumulative,data=data,delay=delay,
			ifr=ifr.high,window=window,smooth=smooth,span=span,plot=FALSE,
			percent=FALSE)+
			infection.estimator(states[2],cumulative=cumulative,data=data,delay=delay,
			ifr=ifr.high,window=window,smooth=smooth,span=span,plot=FALSE,
			percent=FALSE))/pp
	}
	plot(NA,xlim=xlim,ylim=c(0,1.2*max(e.high)),
		bty="n",xlab="",
		ylab="",axes=FALSE)
	title(ylab="estimated infections",line=4)
	Args<-list(...)
	Args$side<-2
	Args$labels<-FALSE
	h<-do.call(axis,Args)
	Args$at<-h
	if(percent)
		Args$labels<-paste(h,"%",sep="")
	else
		Args$labels<-if(max(e.high)>1000000) paste(h/1000000,"M",sep="") else
			if(max(e.high)>1000) paste(h/1000,"k",sep="") else h
	do.call(axis,Args)
	abline(h=h,col=grey(0.75),lwd=1,lty="dotted")
	Args$side<-1
	Args$at<-ms
	Args$labels<-mm
	v<-do.call(axis,Args)
	T<-length(e.low)
	polygon(c(1:T,T:1),c(e.low,e.high[T:1]),border=FALSE,
		col=make.transparent(cols[1],0.25))
	lines(1:T,e.mid,lty="dotted",lwd=2,col=cols[1])
	if(cumulative) mtext(paste("b)",state,"estimated cumulative infections"),
		adj=0,line=1,cex=1.2) else mtext(paste("b)",state,
		"estimated daily infections"),adj=0,line=1,cex=1.2)
	legend(x="topright",
		c("mid-IFR infections",
		"range"),
		pch=c(NA,15),cex=0.9,col=c(cols[1],
		make.transparent(cols[1],0.25)),
		lty=c("dotted",NA),lwd=c(2,NA),
		pt.cex=c(NA,1.5),bty="n",xpd=TRUE,
		xjust=0.5,yjust=1)
}

