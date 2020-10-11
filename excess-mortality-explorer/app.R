library(shiny)
source("state.deaths.R")
source("age.deaths.R")
Counts<-read.csv("https://liamrevell.github.io/data/Weekly_Counts_of_Deaths_by_State_and_Select_Causes__2014-2018.csv")
Provis<-read.csv("https://liamrevell.github.io/data/Weekly_Counts_of_Deaths_by_State_and_Select_Causes__2019-2020.csv")
States<-read.csv("https://liamrevell.github.io/data/nst-est2019-01.csv",row.names=1)

age.Counts<-read.csv("Weekly_counts_of_deaths_by_jurisdiction_and_age_group.csv")

ui<-fluidPage(
	tabsetPanel(
		tabPanel("Excess mortality by age", fluid = TRUE,
			sidebarLayout(
				mainPanel(
					plotOutput("plot.age",width="100%",height="800px")
				),
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
					selectInput(inputId="plot.age",label="show plot of:",
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
					h4("Details:\n"),
					p("The data for these plots come from the U.S. CDC provisional death counts.", 
						"Data for recent weeks are and estimated based on reporting in past years."),
					p("Mortality data are from the ",a("CDC",href="https://data.cdc.gov/NCHS/Weekly-counts-of-deaths-by-jurisdiction-and-age-gr/y5bj-9g5w",
						target="_blank",.noWS="outside"),". All data files & code are available",a("here",
						href="https://github.com/liamrevell/CDC_COVID19_deaths/",target="_blank",.noWS="after"),"."),
					p("Counts below 10 are not reported, so a Poisson model was used to estimate them."),
					p("Please",a("contact me",href="mailto:liamrevell@umb.edu")," with any questions.")
				)
			)
		),
		tabPanel("Excess mortality by state", fluid = TRUE,
		         sidebarLayout(
		           mainPanel(
		             plotOutput("plot.state",width="100%",height="800px")
		           ),
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
		             selectInput(inputId="type",label="line type",choices=c("smooth","step")),
		             h4("Details:\n"),
		             p("The data for these plots come from the U.S. CDC provisional death counts. Data for recent weeks are incomplete. For more information please refer to the",
		               a("CDC technical notes",href="https://www.cdc.gov/nchs/nvss/vsrr/covid19/tech_notes.htm",target="_blank",.noWS="after"),"."),
		             p("Mortality data are from the CDC (",a("1",href="https://data.cdc.gov/NCHS/Weekly-Counts-of-Deaths-by-State-and-Select-Causes/3yf8-kanr",
		                                                     target="_blank",.noWS="outside"),", ",a("2",
		                                                                                             href="https://data.cdc.gov/NCHS/Weekly-Counts-of-Deaths-by-State-and-Select-Causes/muzy-jte6",
		                                                                                             target="_blank",.noWS="outside"),"). State population size data are from the",a("U.S. census bureau",
		                                                                                                                                                                             href="https://www.census.gov/data/datasets/time-series/demo/popest/2010s-state-total.html",target="_blank",.noWS="after"),
		               ". All data files & code are available",a("here",
		                                                         href="https://github.com/liamrevell/CDC_COVID19_deaths/",target="_blank",.noWS="after"),"."),
		             p("Detailed methodology is available",a("here",
		                                                     href="https://liamrevell.github.io/excess-mortality-methodology.html",target="_blank",.noWS="after"),"."),
		             p("Please",a("contact me",href="mailto:liamrevell@umb.edu")," with any questions.")
		           )
		         )
		)
	)
)

server <- function(input, output) {
	output$plot.state<-renderPlot({
		options(scipen=10)
		par(lend=1)
		state.deaths(state=input$state,plot=input$plot,
			las=1,cex.axis=0.8,cex.lab=0.9,
			type=if(input$type=="step") "s" else "l",
			data=list(Counts=Counts,Provis=Provis,
			States=States),corrected=input$corrected,
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
			data=list(Counts=age.Counts,States=States),
			corrected=input$corrected.age,
			cumulative=input$cumulative,
			date.range=list(
			start.date=input$start.end.age[1],
			end.date=input$start.end.age[2]))
		})
}

shinyApp(ui = ui, server = server)
