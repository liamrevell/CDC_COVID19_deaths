## https://data.cdc.gov/Case-Surveillance/United-States-COVID-19-Cases-and-Deaths-by-State-o/9mfq-cb36

moving.average<-function(x,window=7){
	xx<-c(rep(0,floor(window/2)),x,rep(x[length(x)],
		ceiling(window/2)))
	ma<-rep(NA,length(x))
	for(i in 1:length(x)) ma[i]<-mean(xx[0:(window-1)+i])
	ma
}

case.estimator<-function(state="Massachusetts",
	cumulative=FALSE,
	data=list(),
	delay=20,
	ifr=0.005,
	window=7,
	smooth=TRUE,
	span=c(0.2,0.3),
	percent=FALSE,
	plot=TRUE,
	bg="transparent",
	xlim=c(45,366-45),
	...){
	if(smooth) if(length(span)==1) span<-c(span,0.3)
	cols<-make.transparent(c("darkgreen",palette()[c(4,2)]),
		0.8)
	if(length(ifr)==1) ifr<-rep(ifr,366)
	else if(length(ifr)>1&&length(ifr)<366){
		tmp<-vector()
		dd<-round(seq(0,366,length.out=length(ifr)))
		for(i in 1:(length(ifr)-1)){
			tmp<-c(tmp,seq(ifr[i],ifr[i+1],length.out=dd[i+1]-dd[i]))
		}
		if(smooth){
			tt<-1:366
			tmp<-predict(loess(tmp~tt,span=span[2]))
		}
		ifr<-tmp
	} else ifr<-ifr[1:366]
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
		lines(1:length(ifr),100*ifr,lwd=2,col=cols[2])
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
	if(state!="United States") Cases<-Cases[Cases$state==state.codes[state],]
	else {
		Temp<-data.frame(
			new_death=rowSums(sapply(state.codes[2:length(state.codes)],
				function(x,Data) Data[Data$state==x,"new_death"],
				Data=Cases)),
			new_case=rowSums(sapply(state.codes[2:length(state.codes)],
				function(x,Data) Data[Data$state==x,"new_case"],
				Data=Cases)),
			tot_death=rowSums(sapply(state.codes[2:length(state.codes)],
				function(x,Data) Data[Data$state==x,"tot_death"],
				Data=Cases)),
			tot_cases=rowSums(sapply(state.codes[2:length(state.codes)],
				function(x,Data) Data[Data$state==x,"tot_cases"],
				Data=Cases)))
		Cases<-Temp
	}
	if(percent){ 
		SS<-state.deaths(plot="States",data=data)
		rownames(SS)[which(rownames(SS)=="New York")]<-
			"New York (excluding NYC)"
		population<-SS[state,"2020"]
	}
	newDeaths<-c(rep(0,21),Cases$new_death)
	if(plot){
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
		h<-do.call(axis,Args)
		legend("topright",c("assumed IFR (%)",
			"daily COVID-19 deaths"),pch=c(NA,15),
			col=c(cols[2],col),
			cex=0.9,pt.cex=c(NA,1.5),lwd=c(2,NA),
			box.col="transparent")
	}
	if(cumulative==FALSE){
		newDeaths<-moving.average(newDeaths,window)
		obsCases<-moving.average(c(rep(0,21),Cases$new_case),window)
		if(smooth){
			estCases<-moving.average(c(rep(0,21),Cases$new_death),
				window)
			estCases<-estCases[(delay+1):length(estCases)]
			estCases<-estCases/ifr[1:length(estCases)]
			T<-length(estCases)
			tt<-1:T
			cr<-obsCases[1:length(estCases)]/estCases
			cr[is.nan(cr)]<-0
			cr[cr==Inf]<-0
			cr[cr==-Inf]<-0
			if(window<7) cr<-moving.average(cr,7)
			cr[cr>1]<-1
			cr[cr<0]<-0
			fm<-try(nls(cr~a/(1+exp(-b*(tt-c))),
				start=list(a=0.3,b=0.05,c=100),
				control=list(maxiter=1000)))
			ntries<-0
			while(attr(fm,"class")=="try-error"&&ntries<10){
				ii<-sort(sample(1:T,100))
				fm<-try(nls(cr[ii]~a/(1+exp(-b*(tt[ii]-c))),
					start=list(a=0.3,b=0.05,c=100),
					control=list(maxiter=1000)))
				ntries<-ntries+1
			}
			if(attr(fm,"class")=="try-error"){
				tt<-(T-99):T
				cr<-rep(mean(cr[(T-99):T]),100)
				fm<-lm(cr~tt)
			}
			tt<-1:(length(obsCases)-length(estCases))+length(estCases)
			CR<-predict(fm,newdata=data.frame(tt=tt))
			if(length(obsCases[tt])!=length(CR)){
				tt<-(T-99):T
				cr<-rep(mean(cr[(T-99):T]),100)
				fm<-lm(cr~tt)
				tt<-1:(length(obsCases)-length(estCases))+length(estCases)
				CR<-predict(fm,newdata=data.frame(tt=tt))
			}
			estCases<-c(estCases,obsCases[tt]/CR)
			tt<-1:length(estCases)
			fit<-loess(estCases~tt,span=span[1])
			estCases<-predict(fit)
			estCases[estCases<0]<-0
			if(delay<=21) estCases[1:(21-delay)]<-0	
		} else {
			estCases<-moving.average(c(rep(0,21),Cases$new_death),window)
			estCases<-estCases[(delay+1):length(estCases)]
			estCases<-estCases/ifr[1:length(estCases)]
			T<-length(estCases)
		}
		if(percent){
			estCases<-estCases/population*100
			newDeaths<-newDeaths/population*100
			obsCases<-obsCases/population*100
		}
		if(plot){
			plot(NA,xlim=xlim,ylim=c(0,1.05*max(estCases)),bty="n",
				xlab="",
				ylab="",axes=FALSE)
			title(ylab=if(percent) "cases (observed or estimated) %" else
				"cases (observed or estimated)",line=4)
			Args<-list(...)
			Args$side<-2
			h<-do.call(axis,Args)
			abline(h=h,col=grey(0.75),lwd=1,lty="dotted")
			Args$side<-1
			Args$at<-ms
			Args$labels<-mm
			v<-do.call(axis,Args)
			polygon(c(1:T,T,1),
				c(estCases[1:T],0,0),
				border=FALSE,col=cols[1])
			lines(T:length(estCases),
				estCases[T:length(estCases)],
				lty="dotted",lwd=2,col=cols[1])
			polygon(c(1:length(obsCases),length(obsCases),1),
				c(obsCases,0,0),
				border=FALSE,col=cols[2])
			polygon(c(1:length(newDeaths),length(newDeaths),1),
				c(newDeaths,0,0),
				border=FALSE,col=cols[3])
			mtext(paste("b)",state,"daily observed or estimated cases"),
				adj=0,line=1,cex=1.2)
			legend(x="topright",c("observed cases",
				"estimated cases","deaths"),pch=15,cex=0.9,
				col=c(cols[2],cols[1],cols[3]),
				pt.cex=1.5,bty="n",xpd=TRUE,
				xjust=0.5,yjust=1)
		}
	} else {
		newDeaths<-c(rep(0,21),Cases$tot_death)
		obsCases<-c(rep(0,21),Cases$tot_cases)
		if(smooth){
			ObsCases<-moving.average(c(rep(0,21),Cases$new_case),
				window)
			estCases<-moving.average(c(rep(0,21),Cases$new_death),
				window)
			estCases<-estCases[(delay+1):length(estCases)]
			estCases<-estCases/ifr[1:length(estCases)]
			T<-length(estCases)
			cr<-ObsCases[1:length(estCases)]/estCases
			tt<-1:length(cr)
			cr[is.nan(cr)]<-0
			cr[cr==Inf]<-0
			cr[cr==-Inf]<-0
			if(window<7) cr<-moving.average(cr,7)
			cr[cr>10*mean(cr[100:length(cr)])]<-mean(cr)
			cr[cr<0]<-0
			object<-data.frame(tt,cr)
			fm<-try(nls(cr~a/(1+exp(-b*(tt-c))),
				start=list(a=0.3,b=0.05,c=100),
				control=list(maxiter=100),data=object))
			ntries<-0
			while(attr(fm,"class")=="try-error"&&ntries<10){
				ii<-sort(sample(1:T,100))
				fm<-try(nls(cr[ii]~a/(1+exp(-b*(tt[ii]-c))),
					start=list(a=0.3,b=0.05,c=100),
					control=list(maxiter=1000)))
				ntries<-ntries+1
			}
			if(attr(fm,"class")=="try-error"){
				tt<-(T-99):T
				cr<-rep(mean(cr[(T-99):T]),100)
				fm<-lm(cr~tt)
			}
			tt<-1:(length(obsCases)-length(estCases))+length(estCases)
			CR<-predict(fm,newdata=data.frame(tt=tt))
			if(length(obsCases[tt])!=length(CR)){
				tt<-(T-99):T
				cr<-rep(mean(cr[(T-99):T]),100)
				fm<-lm(cr~tt)
				tt<-1:(length(obsCases)-length(estCases))+length(estCases)
				CR<-predict(fm,newdata=data.frame(tt=tt))
			}
			estCases<-c(rep(0,21),Cases$tot_death)
			estCases<-estCases[(delay+1):length(estCases)]
			estCases<-estCases/ifr[1:length(estCases)]
			estCases<-c(estCases,estCases[length(estCases)]+cumsum(ObsCases[tt]/CR))
			tt<-1:length(estCases)
			estCases<-c(rep(0,21),fitted(loess(estCases~tt,span=span[1])))
			estCases[estCases<0]<-0
			estCases<-estCases[(delay+1):length(estCases)]
			if(delay<=21) estCases[1:(21-delay)]<-0
		} else {
			estCases<-c(rep(0,21),Cases$tot_death)
			estCases<-estCases[(delay+1):length(estCases)]
			estCases<-estCases/ifr[1:length(estCases)]
			T<-length(estCases)
		}
		if(percent){
			newDeaths<-newDeaths/population*100
			estCases<-estCases/population*100
			obsCases<-obsCases/population*100
		}
		if(plot){
			plot(NA,xlim=xlim,ylim=c(0,1.05*max(estCases)),bty="n",
				ylab="",
				xlab="",axes=FALSE)
			title(ylab=if(percent) "cases (observed or estimated) %" else
				"cases (observed or estimated)",line=4)
			Args<-list(...)
			Args$side<-2
			h<-do.call(axis,Args)
			abline(h=h,col=grey(0.75),lwd=1,lty="dotted")
			Args$side<-1
			Args$at<-ms
			Args$labels<-mm
			v<-do.call(axis,Args)
			polygon(c(1:T,T,1),
				c(estCases[1:T],0,0),
				border=FALSE,col=cols[1])
			lines(T:length(estCases),
				estCases[T:length(estCases)],
				lty="dotted",lwd=2,col=cols[1])
			polygon(c(1:length(obsCases),length(obsCases),1),
				c(obsCases,0,0),
				border=FALSE,col=cols[2])
			polygon(c(1:length(newDeaths),length(newDeaths),1),
				c(newDeaths,0,0),
				border=FALSE,col=cols[3])
			mtext(paste("b)",state,"cumulative observed or estimated cases"),
				adj=0,line=1,cex=1.2)
			legend(x="topright",c("observed cases",
				"estimated cases","deaths"),pch=15,cex=0.9,
				col=c(cols[2],cols[1],cols[3]),
				pt.cex=1.5,bty="n",xpd=TRUE,
				xjust=0.5,yjust=1)
		}
	}
	invisible(estCases)
}

cases.by.state<-function(states=NULL,
	cumulative=FALSE,
	stacked=TRUE,
	data=list(),
	delay=20,
	ifr=0.005,
	window=7,
	smooth=TRUE,
	span=c(0.2,0.3),
	show.ifr=TRUE,
	bg="transparent",
	xlim=c(45,366-45),
	...){
	if(length(ifr)==1) ifr<-rep(ifr,366)
	else if(length(ifr)>1){
		tmp<-vector()
		dd<-round(seq(0,366,length.out=length(ifr)))
		for(i in 1:(length(ifr)-1)){
			tmp<-c(tmp,seq(ifr[i],ifr[i+1],length.out=dd[i+1]-dd[i]))
		}
		if(smooth){
			tt<-1:366
			tmp<-predict(loess(tmp~tt,span=span[2]))
		}
		ifr<-tmp
	}
	ms<-cumsum(c(0,31,29,31,30,31,30,31,31,30,31,30,31))
	mm<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
		"Sep","Oct","Nov","Dec","Jan (2021)")
	if(is.null(states)) 
		states<-c("Alabama","Alaska","Arizona",
			"Arkansas","California","Colorado","Connecticut",
			"Delaware","District of Columbia","Florida",
			"Georgia","Hawaii","Idaho","Illinois",
			"Indiana","Iowa","Kansas","Kentucky","Louisiana",
			"Maine","Maryland","Massachusetts","Michigan","Minnesota",
			"Mississippi","Missouri","Montana","Nebraska","Nevada",
			"New Hampshire","New Jersey","New Mexico","New York (excluding NYC)",
			"New York City","North Carolina","North Dakota",
			"Ohio","Oklahoma","Oregon","Pennsylvania",
			"Rhode Island","South Carolina","South Dakota","Tennessee",
			"Texas","Utah","Vermont","Virginia",
			"Washington","West Virginia","Wisconsin","Wyoming")
	if(!is.null(data$Centers)) Centers<-data$Centers
	else Centers<-read.csv("Centers.csv")
	args<-list(data=data,
		cumulative=cumulative,
		delay=delay,
		ifr=ifr,
		window=window,
		smooth=TRUE,
		span=span,
		plot=FALSE)
	foo<-function(state,args){
		args$state<-state
		do.call(case.estimator,args)
	}
	tmp<-lapply(states,foo,args)
	names(tmp)<-states
	nd<-max(sapply(tmp,length))
	Cases<-matrix(NA,nd,length(states)-1,dimnames=list(NULL,
		states[-34]))
	ii<-grep("New York",colnames(Cases))
	colnames(Cases)[ii]<-"New York"
	for(i in 1:ncol(Cases)){
		ss<-colnames(Cases)[i]
		if(ss=="New York"){
			Cases[1:length(tmp[["New York (excluding NYC)"]]),i]<-
				tmp[["New York (excluding NYC)"]]+tmp[["New York City"]]
		} else Cases[1:length(tmp[[ss]]),i]<-tmp[[ss]]
	}
	Cases[is.na(Cases)]<-0
	tots<-colSums(Cases)
	ii<-order(tots)
	Centers<-Centers[-which(Centers$name=="Alaska"),]
	Centers<-Centers[-which(Centers$name=="Hawaii"),]
	Centers<-Centers[-which(Centers$name=="Puerto Rico"),]
	colors<-setNames(rep(NA,length(Centers$name)),Centers$name)
	for(i in 1:length(colors)){
		fl<-which(Centers$name=="Florida")
		g<-(max(Centers$longitude)-Centers$longitude[i])/
			diff(range(Centers$longitude))
		r<-(max(Centers$latitude)-Centers$latitude[i])/
			diff(range(Centers$latitude))
		dist2fl<-function(ss,fl){
			sqrt((Centers$longitude[fl]-Centers$longitude[ss])^2+
				(Centers$latitude[fl]-Centers$latitude[ss])^2)
		}
		b<-dist2fl(i,fl)/max(sapply(1:length(Centers$name),dist2fl,
			fl=fl))
		colors[i]<-rgb(r=r,g=g,b=b)
	}
	colors<-c(colors,setNames(rep("black",2),c("Alaska","Hawaii")))
	if(stacked){
		cumCases<-t(apply(Cases[,ii],1,cumsum))
		par(mar=c(5.1,5.1,2.1,3.1),bg=bg)
		yex<-if(cumulative) 1.05 else 1.2
		plot(NA,xlim=xlim,ylim=c(0,yex*max(cumCases,na.rm=TRUE)),
			bty="n",xlab="",
			ylab="",axes=FALSE)
		tt<-1:nrow(cumCases)
		for(i in 1:ncol(cumCases)){
			if(i>1){
				polygon(c(tt,tt[length(tt):1]),
					c(cumCases[,i-1],cumCases[nrow(cumCases):1,i]),
					border=FALSE,col=colors[colnames(cumCases)[i]])
			} else {
				polygon(c(tt,tt[length(tt)],1),
					c(cumCases[,i],0,0),
					border=FALSE,col=colors[colnames(cumCases)[i]])
			}
		}
		Args<-list(...)
		Args$side<-2
		Args$labels<-FALSE
		h<-do.call(axis,Args)
		Args$at<-h
		Args$labels<-if(cumulative) paste(h/1000000,"M",sep="") else
			paste(h/1000,"k",sep="")
		do.call(axis,Args)
		abline(h=h,col=grey(0.75),lwd=1,lty="dotted")
		title(ylab=if(cumulative) "estimated cumulative infections" else
			"estimated new infections / day",line=4,
			cex.lab=if(is.null(Args$cex.lab)) 1 else Args$cex.lab)
		Args$side<-1
		Args$at<-ms
		Args$labels<-mm
		v<-do.call(axis,Args)
		if(show.ifr){
			par(usr=c(par()$usr[1:2],-0.08,2.08))
			Args<-list(...)
			Args$side<-4
			h<-do.call(axis,Args)
			lines(1:length(ifr),ifr*100,col=palette()[4],lwd=2)
			legend("topleft","assumed IFR (%)",
				col=palette()[4],
				cex=0.9,lwd=2,
				box.col="transparent")
		}
		aspect<-par()$din[2]/par()$din[1]
		if(cumulative) par(usr=c(-135,42,55-110*aspect,55)) else
			par(usr=c(-240,-63,55-110*aspect,55))
		for(i in 1:(length(colors)-2))
			map("state",region=names(colors)[i],fill=TRUE,add=TRUE,
				col=colors[i],border="white")
	} else {
		plot(NA,xlim=xlim,ylim=c(0,1.05*max(Casecols,na.rm=TRUE)),
			bty="n",xlab="",
			ylab="",axes=FALSE)
		nulo<-apply(Cases,2,lines)
	}
	invisible(Cases)
}
