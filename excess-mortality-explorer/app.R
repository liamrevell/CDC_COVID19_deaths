library(shiny)
library(shinyWidgets)
library(phytools)
library(maps)
source("state.deaths.R")
source("age.deaths.R")
source("case.estimator.R")
Counts<-read.csv("https://liamrevell.github.io/data/Weekly_Counts_of_Deaths_by_State_and_Select_Causes__2014-2018.csv")
Provis<-read.csv("https://liamrevell.github.io/data/Weekly_Counts_of_Deaths_by_State_and_Select_Causes__2019-2020.csv")
States<-read.csv("https://liamrevell.github.io/data/nst-est2019-01.csv",row.names=1)
age.Counts<-read.csv("https://liamrevell.github.io/data/Weekly_counts_of_deaths_by_jurisdiction_and_age_group.csv")
Cases<-read.csv("https://liamrevell.github.io/data/United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv")
## Centers<-read.csv("https://liamrevell.github.io/data/Centers.csv")
Centers<-read.csv("Centers.csv")

Data<-list(Counts=Counts,Provis=Provis,States=States,age.Counts=age.Counts,Cases=Cases,Centers=Centers)

ui<-fluidPage(
  setBackgroundColor(
    color = c("white", "#E8E8E8"),
    gradient = "linear",
    direction = "bottom"
	),
	tabsetPanel(
	  tabPanel("COVID-19 cases by state", fluid = TRUE,
	           verticalLayout(
	             sidebarLayout(
	               sidebarPanel(
	                 sliderInput(inputId="ifr1.bs",label="IFR Jan. 1, 2020 (%):",value=0.65,
	                             min=0.05,max=1.5,step=0.05,round=2,ticks=FALSE),
	                 sliderInput(inputId="ifr2.bs",label="IFR Apr. 1, 2020 (%):",value=0.55,
	                             min=0.05,max=1.5,step=0.05,round=2,ticks=FALSE),
	                 sliderInput(inputId="ifr3.bs",label="IFR Jul. 1, 2020 (%):",value=0.45,
	                             min=0.05,max=1.5,step=0.05,round=2,ticks=FALSE),
	                 sliderInput(inputId="ifr4.bs",label="IFR Oct. 1, 2020 (%):",value=0.4,
	                             min=0.05,max=1.5,step=0.05,round=2,ticks=FALSE),
	                 sliderInput(inputId="ifr5.bs",label="IFR Jan. 1, 2021 (%):",value=0.4,
	                             min=0.05,max=1.5,step=0.05,round=2,ticks=FALSE),
	                 sliderInput(inputId="delay.bs",label="Lag-time from infection to death (for estimated cases):",
	                             value=21,min=0,max=30,ticks=FALSE),
	                 sliderInput(inputId="window.bs",label="Window for moving averages:",
	                             value=14,min=1,max=21,ticks=FALSE),
	                 checkboxInput(inputId="smooth.bs",
	                               label="use smoothing for estimation",
	                               value=TRUE),
	                 checkboxInput(inputId="show.ifr",
	                               label="show assumed IFR (%)",
	                               value=FALSE),
	                 width=3
	               ),
	               mainPanel(
	                 plotOutput("plot.allstates",width="100%",height="800px"),
	                 width=9
	               )
	             ),
	             sidebarPanel(
	               p(strong("Details:"),"The data for these plots come from the U.S. CDC ",
	                 a("COVID-19 Cases and Deaths by State over Time",
	                   href="https://data.cdc.gov/Case-Surveillance/United-States-COVID-19-Cases-and-Deaths-by-State-o/9mfq-cb36",.noWS="outside"),
	                 ".",
	                 em("Estimated"),"cases are based on moving average or LOESS smoothed CDC mortality data and an infection fatality ratio (IFR) specified by the user.",
	                 "Number of cases in the last period of the data (during the lag-time to death) are based on observed cases and a fitted model for the relationship between observed and estimated cases through time.",
	                 "All data files & code are available",a("here",
	                  href="https://github.com/liamrevell/CDC_COVID19_deaths/",target="_blank",.noWS="after"),".",
	                 "Please",a("contact me",href="mailto:liamrevell@umb.edu")," with any questions."),
	               width=12)
	           )
	  ),
	  tabPanel("COVID-19 case estimator", fluid = TRUE,
	           verticalLayout(
	             sidebarLayout(
	               sidebarPanel(
	                 selectInput(inputId="state.cases",label="State or jurisdiction",
	                             choices=c("United States","Alabama","Alaska","Arizona",
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
	                                       "Washington","West Virginia","Wisconsin","Wyoming"),
	                             selected="Massachusetts"),
	                 sliderInput(inputId="ifr1",label="IFR Jan. 1, 2020 (%):",value=0.65,
	                             min=0.05,max=1.5,step=0.05,round=2,ticks=FALSE),
	                 sliderInput(inputId="ifr2",label="IFR Apr. 1, 2020 (%):",value=0.55,
	                             min=0.05,max=1.5,step=0.05,round=2,ticks=FALSE),
	                 sliderInput(inputId="ifr3",label="IFR Jul. 1, 2020 (%):",value=0.45,
	                             min=0.05,max=1.5,step=0.05,round=2,ticks=FALSE),
	                 sliderInput(inputId="ifr4",label="IFR Oct. 1, 2020 (%):",value=0.4,
	                             min=0.05,max=1.5,step=0.05,round=2,ticks=FALSE),
	                 sliderInput(inputId="ifr5",label="IFR Jan. 1, 2021 (%):",value=0.4,
	                             min=0.05,max=1.5,step=0.05,round=2,ticks=FALSE),
	                 sliderInput(inputId="delay",label="Lag-time from infection to death (for estimated cases):",
	                             value=21,min=0,max=30,ticks=FALSE),
	                 sliderInput(inputId="window",label="Window for moving averages:",
	                             value=14,min=1,max=21,ticks=FALSE),
	                 checkboxInput(inputId="smooth",
	                               label="use smoothing for estimation",
	                               value=TRUE),
	                 checkboxInput(inputId="percent",
	                               label="show as percent of total population",
	                               value=FALSE),
	                 checkboxInput(inputId="cumulative.cases",
	                               label="show cumulative cases",
	                               value=FALSE),
	                 width=3
	               ),
	               mainPanel(
	                 plotOutput("plot.cases",width="100%",height="800px"),
	                 width=9
	               )
	             ),
	             sidebarPanel(
	               p(strong("Details:"),"The data for these plots come from the U.S. CDC ",
	                 a("COVID-19 Cases and Deaths by State over Time",
	                 href="https://data.cdc.gov/Case-Surveillance/United-States-COVID-19-Cases-and-Deaths-by-State-o/9mfq-cb36",.noWS="outside"),
	                 ".",
	                 em("Estimated"),"cases are based on moving average or LOESS smoothed CDC mortality data and an infection fatality ratio (IFR) specified by the user.",
	                 em("Observed"),"cases are the sum of confirmed and presumed cases according to CDC data.",
	                 "Number of cases in the last period of the data (during the lag-time to death) are based on observed cases and a fitted model for the relationship between observed and estimated cases through time.",
	                 "All data files & code are available",a("here",
	                 href="https://github.com/liamrevell/CDC_COVID19_deaths/",target="_blank",.noWS="after"),".",
	                 "Please",a("contact me",href="mailto:liamrevell@umb.edu")," with any questions."),
	               width=12)
	           )
	  ),
		tabPanel("Excess mortality by age", fluid = TRUE,
		  verticalLayout(
			sidebarLayout(
				sidebarPanel(
					selectInput(inputId="state.age",label="State or jurisdiction",
						choices=c("United States","Alabama","Alaska","Arizona",
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
							"Washington","West Virginia","Wisconsin","Wyoming"),
							selected="Massachusetts"),
					checkboxGroupInput(inputId="age.group",label="Select age group(s):",
						choices=c("Under 25 years","25-44 years","45-64 years",
						"65-74 years","75-84 years","85 years and older"),
						selected=c("Under 25 years","25-44 years","45-64 years",
						"65-74 years","75-84 years","85 years and older")),
					selectInput(inputId="plot.age",label="Show plot of:",
						choices=c("raw & excess","raw & percent above normal")),
					checkboxInput(inputId="corrected.age",
						label="correct to 2020 population",
						value=TRUE),
					checkboxInput(inputId="cumulative",
						label="show cumulative excess mortality",
						value=TRUE),
					dateRangeInput(inputId="start.end.age",label="Starting & ending dates:",
						start=as.Date("01/01/2020",format="%m/%d/%Y"),
						end=as.Date("12/31/2020",format="%m/%d/%Y"),
						min=as.Date("01/01/2020",format="%m/%d/%Y"),
						max=as.Date("12/31/2020",format="%m/%d/%Y"),startview="month"),
				  width=3
				),
				mainPanel(
				  plotOutput("plot.age",width="100%",height="800px"),
				  width=9
				)
			),
			sidebarPanel(
		  	p(strong("Details:"),"The data for these plots come from the U.S. CDC provisional death counts.", 
			    "Data for recent weeks are estimated based on reporting in past years.",
			    "Mortality data are from the ",a("CDC",href="https://data.cdc.gov/NCHS/Weekly-counts-of-deaths-by-jurisdiction-and-age-gr/y5bj-9g5w",
			    target="_blank",.noWS="outside"),". All data files & code are available",a("here",
			    href="https://github.com/liamrevell/CDC_COVID19_deaths/",target="_blank",.noWS="after"),".",
			    "Counts below 10 are not reported, so a Poisson model was used to estimate them.",
			    "Please",a("contact me",href="mailto:liamrevell@umb.edu")," with any questions."),
			  width=12)
		  )
		),
		tabPanel("Excess mortality by state", fluid = TRUE,
		         verticalLayout(
		         sidebarLayout(
		           sidebarPanel(
		             selectInput(inputId="state",label="State or jurisdiction",
		                         choices=c("United States","Alabama","Alaska","Arizona",
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
		                                   "Washington","West Virginia","Wisconsin","Wyoming"),
		                         selected="Massachusetts"),
		             selectInput(inputId="plot",label="Show plot of:",
		                         choices=c("raw","per capita","excess",
		                                   "excess per capita","percent above normal")),
		             checkboxInput(inputId="corrected",
		                           label="correct to 2020 population (per capita rates are automatically corrected)",
		                           value=TRUE),
		             dateRangeInput(inputId="start.end",label="Starting & ending dates:",
		                            start=as.Date("01/01/2020",format="%m/%d/%Y"),
		                            end=as.Date("12/31/2020",format="%m/%d/%Y"),
		                            min=as.Date("01/01/2020",format="%m/%d/%Y"),
		                            max=as.Date("12/31/2020",format="%m/%d/%Y"),startview="month"),
		            selectInput(inputId="type",label="Line type",choices=c("smooth","step")),
                width=3  
		           ),
		           mainPanel(
		             plotOutput("plot.state",width="100%",height="800px"),
		             width=9
		           )
		         ),
		         sidebarPanel(
		           p(strong("Details:"),
		              "The data for these plots come from the U.S. CDC provisional death counts.",
		              "Data for recent weeks are incomplete. For more information please refer to the",
		              a("CDC technical notes",href="https://www.cdc.gov/nchs/nvss/vsrr/covid19/tech_notes.htm",target="_blank",.noWS="after"),".",
		              "Mortality data are from the CDC (",a("1",href="https://data.cdc.gov/NCHS/Weekly-Counts-of-Deaths-by-State-and-Select-Causes/3yf8-kanr",
		              target="_blank",.noWS="outside"),", ",a("2",
		              href="https://data.cdc.gov/NCHS/Weekly-Counts-of-Deaths-by-State-and-Select-Causes/muzy-jte6",
		              target="_blank",.noWS="outside"),"). State population size data are from the",a("U.S. census bureau",
		              href="https://www.census.gov/data/datasets/time-series/demo/popest/2010s-state-total.html",target="_blank",.noWS="after"),
                  ". All data files & code are available",a("here",
		              href="https://github.com/liamrevell/CDC_COVID19_deaths/",target="_blank",.noWS="after"),".",
		              "Detailed methodology is",a("here",
		              href="https://liamrevell.github.io/excess-mortality-methodology.html",target="_blank",.noWS="after"),".",
		              "Please",a("contact me",href="mailto:liamrevell@umb.edu")," with any questions."),
		           width=12)
		         )
		)
	)
)

server <- function(input, output, data=Data) {
	output$plot.state<-renderPlot({
		options(scipen=10)
		par(lend=1)
		state.deaths(state=input$state,plot=input$plot,
			las=1,cex.axis=0.8,cex.lab=0.9,
			type=if(input$type=="step") "s" else "l",
			data=data,corrected=input$corrected,
			date.range=list(start.date=input$start.end[1],
			end.date=input$start.end[2]))
	})
	output$plot.age<-renderPlot({
		options(scipen=10)
		par(lend=1)
		age.deaths(state=input$state.age,
			plot=input$plot.age,
			las=1,cex.axis=0.8,cex.lab=0.9,
			age.group=input$age.group,
			data=data,
			corrected=input$corrected.age,
			cumulative=input$cumulative,
			date.range=list(
			start.date=input$start.end.age[1],
			end.date=input$start.end.age[2]))
	})
	output$plot.cases<-renderPlot({
		options(scipen=10)
		par(lend=1)
		case.estimator(state=input$state.cases,
			las=1,cex.axis=0.8,cex.lab=0.9,
			data=data,
			cumulative=input$cumulative.cases,
			ifr=c(input$ifr1,input$ifr2,input$ifr3,input$ifr4,input$ifr5)/100,
			delay=input$delay,window=input$window,
			smooth=input$smooth,
			percent=input$percent)
		})
	output$plot.allstates<-renderPlot({
	  options(scipen=10)
	  par(lend=1)
	  cases.by.state(las=1,cex.axis=0.8,cex.lab=0.9,
	    data=data,
	    ifr=c(input$ifr1.bs,input$ifr2.bs,input$ifr3.bs,input$ifr4.bs,input$ifr5.bs)/100,
	    delay=input$delay.bs,window=input$window.bs,
	    smooth=input$smooth.bs,show.ifr=input$show.ifr)
	})
}

shinyApp(ui = ui, server = server)
