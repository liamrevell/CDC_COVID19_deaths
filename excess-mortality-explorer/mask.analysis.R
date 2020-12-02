source("state.deaths.R")
source("age.deaths.R")
source("case.estimator.R")
Counts<-read.csv("https://liamrevell.github.io/data/Weekly_Counts_of_Deaths_by_State_and_Select_Causes__2014-2018.csv")
Provis<-read.csv("https://liamrevell.github.io/data/Weekly_Counts_of_Deaths_by_State_and_Select_Causes__2019-2020.csv")
States<-read.csv("https://liamrevell.github.io/data/nst-est2019-01.csv",row.names=1)
age.Counts<-read.csv("https://liamrevell.github.io/data/Weekly_counts_of_deaths_by_jurisdiction_and_age_group.csv")
Cases<-read.csv("https://liamrevell.github.io/data/United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv")

DATA<-list(Counts=Counts,Provis=Provis,States=States,age.Counts=age.Counts,Cases=Cases)

state.codes<-setNames(
		c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI",
		"ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS",
		"MO","MT","NE","NV","NH","NJ","NM","NY","NYC","NC","ND","OH","OK",
		"OR","PA","PR","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV",
		"WI","WY"),
		c("Alabama","Alaska","Arizona",
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

Masks<-read.csv("masks.csv")

States<-age.deaths(return="States",data=DATA)

pcExcess<-setNames(rep(NA,nrow(Masks)),Masks$State)

for(i in 1:nrow(Masks)){
	ss<-names(state.codes)[which(state.codes==Masks$State[i])]
	if(ss!="New York (excluding NYC)"){
		PP<-age.deaths(ss,return="Excess",
			plot="raw & excess",
			data=DATA)[,"2020"]
		PP<-PP[!is.na(PP)]
		pcExcess[i]<-sum(PP)/(States[ss,"2020"]/1e6)
	} else {
		PP<-age.deaths(ss,return="Excess",
			plot="raw & excess",
			data=DATA)[,"2020"]
		PP<-PP[!is.na(PP)]
		PP2<-age.deaths("New York City",return="Excess",
			plot="raw & excess",
			data=DATA)[,"2020"]
		PP2<-PP2[!is.na(PP2)]
		pcExcess[i]<-(sum(PP)+sum(PP2))/
			((States["New York City","2020"]+
			States["New York City","2020"])/1e6)
	}
	print(pcExcess[i])
	flush.console()
	Sys.sleep(1)
}

plot(NA,xlim=range(Masks$Mask),ylim=range(pcExcess),bty="n",
	xlab="Mask use (%)",ylab="Excess deaths / 1M population")
grid()
for(i in 1:nrow(Masks)){
	points(Masks$Mask[i],pcExcess[i],pch=21,cex=4,bg=palette()[4])
	text(Masks$Mask[i],pcExcess[i],Masks$State[i],cex=0.8,font=3,
		col="white")
}

state.codes<-setNames(
		c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI",
		"ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS",
		"MO","MT","NE","NV","NH","NJ","NM","NY","NYC","NC","ND","OH","OK",
		"OR","PA","PR","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV",
		"WI","WY"),
		c("Alabama","Alaska","Arizona",
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

Masks<-read.csv("masks.csv")

States<-age.deaths(return="States",data=DATA)

for(i in 1:nrow(Masks)){
	ss<-names(state.codes)[which(state.codes==Masks$State[i])]
	if(ss!="New York (excluding NYC)"){
		PP<-age.deaths(ss,return="Excess",
			plot="raw & excess",
			data=DATA)[,"2020"]
		PP<-PP[!is.na(PP)]
		pcExcess[i]<-max(PP)/(States[ss,"2020"]/1e6)
	} else {
		PP<-age.deaths(ss,return="Excess",
			plot="raw & excess",
			data=DATA)[,"2020"]
		PP<-PP[!is.na(PP)]
		PP2<-age.deaths("New York City",return="Excess",
			plot="raw & excess",
			data=DATA)[,"2020"]
		PP2<-PP2[!is.na(PP2)]
		pcExcess[i]<-(max(PP)+max(PP2))/
			((States["New York City","2020"]+
			States["New York City","2020"])/1e6)
	}
	print(pcExcess[i])
	flush.console()
	Sys.sleep(1)
}

plot(NA,xlim=range(Masks$Mask),ylim=range(pcExcess),bty="n",log="xy",
	xlab="Mask use (%)",ylab="Excess deaths / 1M population")
grid()
for(i in 1:nrow(Masks)){
	points(Masks$Mask[i],pcExcess[i],pch=21,cex=4,bg=palette()[4])
	text(Masks$Mask[i],pcExcess[i],Masks$State[i],cex=0.8,font=3,
		col="white")
}


for(i in 1:nrow(Masks)){
	ss<-names(state.codes)[which(state.codes==Masks$State[i])]
	if(ss!="New York (excluding NYC)"){
		PP<-age.deaths(ss,return="Excess",
			plot="raw & excess",
			data=DATA)[,"2020"]
		PP<-PP[!is.na(PP)]
		pcExcess[i]<-sum(PP)
	} else {
		PP<-age.deaths(ss,return="Excess",
			plot="raw & excess",
			data=DATA)[,"2020"]
		PP<-PP[!is.na(PP)]
		PP2<-age.deaths("New York City",return="Excess",
			plot="raw & excess",
			data=DATA)[,"2020"]
		PP2<-PP2[!is.na(PP2)]
		pcExcess[i]<-sum(PP+PP2)
	}
	print(pcExcess[i])
	flush.console()
	#Sys.sleep(1)
}

plot(NA,xlim=range(Masks$Mask),ylim=range(pcExcess),bty="n",log="y",
	xlab="Mask use (%)",ylab="Excess deaths")
grid()
for(i in 1:nrow(Masks)){
	points(Masks$Mask[i],pcExcess[i],pch=21,cex=4,bg=palette()[4])
	text(Masks$Mask[i],pcExcess[i],Masks$State[i],cex=0.8,font=3,
		col="white")
}

fit<-lm(log(pcExcess)~Masks$Mask)
anova(fit)
lines(Masks$Mask,exp(predict(fit)))