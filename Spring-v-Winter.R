data(Data)
library(RColorBrewer)
object<-covid.deaths(data=Data)

CD<-object$CD


foo<-function(x)
	sum(rowSums(x[as.character(14:23),]))


Spring<-sapply(CD,foo)

foo<-function(x)
	sum(rowSums(x[as.character(43:53),]))

Winter<-sapply(CD,foo)

Spring<-Spring/sum(Spring)*100
Winter<-Winter/sum(Winter)*100

Deaths<-rbind(Spring,Winter)
colnames(Deaths)<-c("<1","1-4","5-14","15-24","25-34","35-44","45-54","55-64","65-74","75-84",">85")

png(file="covid19-deaths-by-age.png",width=8,height=6,units="in",res=600)
barplot(Deaths,beside=TRUE,ylab="% of COVID-19 deaths in each wave",
	xlab="age group (years)",
	legend.text=c("Apr - Jun, 2020","Oct - Dec, 2020"),
	args.legend=list(x="topleft",bty="n"),
	main="% of COVID-19 deaths by age group",font.main=3)
dev.off()



