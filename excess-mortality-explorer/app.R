## shiny app for https://covid19-explorer.org developed by Liam J. Revell 2020/21

library(shiny)
library(shinyWidgets)
library(covid19.Explorer)

load("Data.rda")
if(Data$date<(Sys.Date()-2)){
	Data<-updateData(Data)
	save(Data,file="Data.rda")
}

tweet_url<-"https://twitter.com/intent/tweet?text=Check%20out%20this%20cool%20COVID-19%20app:&url=http://covid19-explorer.org"
facebook_url<-"https://www.facebook.com/sharer/sharer.php?u=http://covid19-explorer.org"

ui<-fluidPage(
	setBackgroundColor(
		color = c("white", "#E8E8E8"),
		gradient = "linear",
		direction = "bottom"
	),
	tabsetPanel(
		type="pills",
		selected="Just cases & deaths",
		tabPanel("Methodology", fluid = TRUE,
			mainPanel(
				includeHTML("https://liamrevell.github.io/methodology/index.html")
			)
		),
		tabPanel("U.S. COVID-19 infections", fluid = TRUE,
			verticalLayout(
				sidebarLayout(
					sidebarPanel(
						p(strong("Estimated SARS-CoV-2 infections by U.S. state."),br(),"See",
							a("Methodology",href="https://covid19-explorer.org/methodology/",.noWS="after"),
							" for more details.",style="text-align:center;"),
						sliderInput(inputId="ifr1.bs",label="IFR Feb. 1, 2020 (%):",value=0.85,
							min=0.05,max=1.5,step=0.05,round=2,ticks=FALSE),
						sliderInput(inputId="ifr2.bs",label="IFR May. 2, 2020 (%):",value=0.65,
							min=0.05,max=1.5,step=0.05,round=2,ticks=FALSE),
						sliderInput(inputId="ifr3.bs",label="IFR Aug. 2, 2020 (%):",value=0.55,
							min=0.05,max=1.5,step=0.05,round=2,ticks=FALSE),
						sliderInput(inputId="ifr4.bs",label="IFR Nov. 1, 2020 (%):",value=0.5,
							min=0.05,max=1.5,step=0.05,round=2,ticks=FALSE),
						sliderInput(inputId="ifr5.bs",label="IFR Jan. 31, 2021 (%):",value=0.5,
							min=0.05,max=1.5,step=0.05,round=2,ticks=FALSE),
						sliderInput(inputId="delay.bs",label="Average lag-time from infection to death:",
							value=21,min=0,max=30,ticks=FALSE),
						sliderInput(inputId="span.bs",label="Span (for LOESS smoothing):",
							value=0.12,min=0.05,max=0.4,ticks=FALSE),
						checkboxInput(inputId="cumulative.bs",
							label="show cumulative infections",
							value=FALSE),
						checkboxInput(inputId="show.as.percent",
							label="show as % of all infections",
							value=FALSE),
						checkboxInput(inputId="show.ifr",
							label="show assumed IFR (%)",
							value=TRUE),
						actionButton("twitter_share",
							label = "",icon = icon("twitter"),
							onclick = sprintf("window.open('%s')", tweet_url)),
						actionButton("facebook_share",
							label = "",icon = icon("facebook"),
							onclick = sprintf("window.open('%s')", facebook_url)),
						downloadButton(outputId="down.allstates",label="Save Plot"),
							width=3
					),
					mainPanel(
						plotOutput("plot.allstates",width="auto",height="800px"),
						width=9
					)
				),
				sidebarPanel(
					p(strong("Details:"),
						em("Estimated"),
						"infections are based on moving average or LOESS smoothed CDC mortality data and an infection fatality ratio (IFR) specified by the user.",
						"Number of infections in the last period of the data (during the lag-time to death) are based on observed cases and a fitted model for the relationship between observed and estimated infections through time.",
						"Complete code and more details of methodology are available here:",
						a("1",href="https://github.com/liamrevell/covid19.Explorer/",.noWS="after"),", ",
						a("2",href="http://covid19-explorer.org/methodology/",.noWS="after"),".",
						"Horizontal axis no longer being extended in this application tab.",
						"Please",a("contact me",href="mailto:liamrevell@umb.edu")," with any questions."),
						width=12
				)
			)
		),
		tabPanel("Iceberg plot", fluid = TRUE,
			verticalLayout(
				sidebarLayout(
					sidebarPanel(
						p(strong("Estimated observed/unobserved infections."),br(),"See",
							a("Methodology",href="https://covid19-explorer.org/methodology/",.noWS="after"),
							" for more details.",style="text-align:center;"),
						selectInput(inputId="state.ib",label="State or jurisdiction",
							choices=c("United States","Alabama","Alaska","Arizona",
							"Arkansas","California","Colorado","Connecticut",
							"Delaware","District of Columbia","Florida",
							"Georgia","Hawaii","Idaho","Illinois",
							"Indiana","Iowa","Kansas","Kentucky","Louisiana",
							"Maine","Maryland","Massachusetts","Michigan","Minnesota",
							"Mississippi","Missouri","Montana","Nebraska","Nevada",
							"New Hampshire","New Jersey","New Mexico","New York",
							"New York City","North Carolina","North Dakota",
							"Ohio","Oklahoma","Oregon","Pennsylvania","Puerto Rico",
							"Rhode Island","South Carolina","South Dakota","Tennessee",
							"Texas","Utah","Vermont","Virginia",
							"Washington","West Virginia","Wisconsin","Wyoming"),
							selected="United States"),
						sliderInput(inputId="ifr1.ib",label="IFR Feb. 1, 2020 (%):",value=0.85,
							min=0.05,max=1.5,step=0.05,round=2,ticks=FALSE),
						sliderInput(inputId="ifr2.ib",label="IFR May. 2, 2020 (%):",value=0.65,
							min=0.05,max=1.5,step=0.05,round=2,ticks=FALSE),
						sliderInput(inputId="ifr3.ib",label="IFR Aug. 2, 2020 (%):",value=0.55,
							min=0.05,max=1.5,step=0.05,round=2,ticks=FALSE),
						sliderInput(inputId="ifr4.ib",label="IFR Nov. 1, 2020 (%):",value=0.5,
							min=0.05,max=1.5,step=0.05,round=2,ticks=FALSE),
						sliderInput(inputId="ifr5.ib",label="IFR Jan. 31, 2021 (%):",value=0.5,
							min=0.05,max=1.5,step=0.05,round=2,ticks=FALSE),
						sliderInput(inputId="delay.ib",label="Average lag-time from infection to death:",
							value=21,min=0,max=30,ticks=FALSE),
						sliderInput(inputId="span.ib",label="Span (for LOESS smoothing):",
							value=0.12,min=0.05,max=0.4,ticks=FALSE),
						actionButton("twitter_share",
							label = "",icon = icon("twitter"),
							onclick = sprintf("window.open('%s')", tweet_url)),
						actionButton("facebook_share",
							label = "",icon = icon("facebook"),
							onclick = sprintf("window.open('%s')", facebook_url)),
						downloadButton(outputId="down.iceberg",label="Save Plot"),
						width=3
					),
					mainPanel(
						plotOutput("plot.iceberg",width="auto",height="800px"),
						width=9
					)
				),
				sidebarPanel(
					p(strong("Details:"),
						"The data for these plots come from the U.S. CDC ",
						a("COVID-19 Cases and Deaths by State over Time",
						href="https://data.cdc.gov/Case-Surveillance/United-States-COVID-19-Cases-and-Deaths-by-State-o/9mfq-cb36",.noWS="outside"),
						".",
						em("Estimated"),"cases are based on moving average or LOESS smoothed CDC mortality data and an infection fatality ratio (IFR) specified by the user.",
						em("Observed"),"cases are the sum of confirmed and presumed cases according to CDC data.",
						"Number of cases in the last period of the data (during the lag-time to death) are based on observed cases and a fitted model for the relationship between observed and estimated cases through time.",
						"Complete code and more details of methodology are available here:",
						a("1",href="https://github.com/liamrevell/covid19.Explorer/",.noWS="after"),
						", ",a("2",href="http://covid19-explorer.org/methodology/",.noWS="after"),".",
						"Horizontal axis no longer being extended in this application tab.",
						"Please",a("contact me",href="mailto:liamrevell@umb.edu")," with any questions."),
					width=12
				)
			)
		),
		tabPanel("Deaths by age", fluid = TRUE,
			verticalLayout(
				sidebarLayout(
					sidebarPanel(
						p(strong("COVID-19 deaths by age."),br(),"See",
							a("Methodology",href="https://covid19-explorer.org/methodology/",.noWS="after"),
							" for more details.",style="text-align:center;"),
						selectInput(inputId="ages",
							label="Select age group(s):",
							choices=c("Under 1 year",
							"1-4 years",
							"5-14 years",
							"15-24 years",
							"25-34 years",
							"35-44 years",
							"45-54 years",
							"55-64 years",
							"65-74 years",
							"75-84 years",
							"85 years and over"),
							selected=c("Under 1 year",
							"1-4 years",
							"5-14 years",
							"15-24 years",
							"25-34 years",
							"35-44 years",
							"45-54 years",
							"55-64 years",
							"65-74 years",
							"75-84 years",
							"85 years and over"),
							multiple=TRUE),
						selectInput(inputId="sex",
							label="Select sex(es):",
							choices=c("Male","Female"),
							selected=c("Male","Female"),multiple=TRUE),
						selectInput(inputId="show.cd",
							label="Show plot of:",
							choices=c("raw deaths","deaths / 1M population",
							"as % of total","as % of COVID-19 deaths"),
							selected="raw deaths"),
						selectInput(inputId="plot.cd",
							label="Plotting options:",
							choices=c("bar plot","polygons","smoothed"),
							selected="polygons"),
						checkboxInput(inputId="split.cd",
							label="split groups",
							value=TRUE),
						checkboxInput(inputId="cumulative.cd",
							label="show cumulative mortality",
							value=FALSE),
						actionButton("twitter_share",
							label = "",icon = icon("twitter"),
							onclick = sprintf("window.open('%s')", tweet_url)),
						actionButton("facebook_share",label = "",
							icon = icon("facebook"),
							onclick = sprintf("window.open('%s')", facebook_url)),
						downloadButton(outputId="down.cd",label="Save Plot"),
						width=3
					),
					mainPanel(
						plotOutput("plot.cd",width="auto",height="800px"),
						width=9
					)
				),
				sidebarPanel(
					p(strong("Details:"),
						"Mortality data are from the ",
						a("CDC",href="https://data.cdc.gov/NCHS/Provisional-COVID-19-Death-Counts-by-Sex-Age-and-W/vsak-wrfu",
						target="_blank",.noWS="outside"),
						". Data for recent weeks are incomplete and likely underestimate true deaths.",
						"Population numbers by age group are based on U.S. Census Bureau projections and are available ",
						a("here",href="https://wonder.cdc.gov/population-projections-2014-2060.html",target="_blank",.noWS="after"),
						". ","Complete code and more details of methodology are available here:",
						a("1",href="https://github.com/liamrevell/covid19.Explorer/",.noWS="after"),", ",
						a("2",href="http://covid19-explorer.org/methodology/",.noWS="after"),".",
						"Please",a("contact me",href="mailto:liamrevell@umb.edu")," with any questions."),
					width=12
				)
			)
		),
		tabPanel("Just cases & deaths", fluid = TRUE,
			verticalLayout(
				sidebarLayout(
					sidebarPanel(
						p(strong("Confirmed SARS-CoV-2 infections & COVID-19 deaths by U.S. state."),br(),"See",
							a("Methodology",href="https://covid19-explorer.org/methodology/",.noWS="after"),
							" for more details.",style="text-align:center;"),
						selectInput(inputId="states",label="State or jurisdiction to compare",
							choices=c("United States","Alabama","Alaska","Arizona",
							"Arkansas","California","Colorado","Connecticut",
							"Delaware","District of Columbia","Florida",
							"Georgia","Hawaii","Idaho","Illinois",
							"Indiana","Iowa","Kansas","Kentucky","Louisiana",
							"Maine","Maryland","Massachusetts","Michigan","Minnesota",
							"Mississippi","Missouri","Montana","Nebraska","Nevada",
							"New Hampshire","New Jersey","New Mexico","New York",
							"New York City","North Carolina","North Dakota",
							"Ohio","Oklahoma","Oregon","Pennsylvania","Puerto Rico",
							"Rhode Island","South Carolina","South Dakota","Tennessee",
							"Texas","Utah","Vermont","Virginia",
							"Washington","West Virginia","Wisconsin","Wyoming"),
							multiple=TRUE,
							selected="Massachusetts"),
						sliderInput(inputId="window.c",label="Window for moving averages:",
							value=7,min=1,max=14,ticks=FALSE),
						checkboxInput(inputId="per.capita",
							label="show as rate / 1M population",
							value=TRUE),
						checkboxInput(inputId="cumulative.c",
							label="show cumulative",
							value=FALSE),
						actionButton("twitter_share",
							label = "",icon = icon("twitter"),
							onclick = sprintf("window.open('%s')", tweet_url)),
						actionButton("facebook_share",
							label = "",icon = icon("facebook"),
							onclick = sprintf("window.open('%s')", facebook_url)),
						downloadButton(outputId="down.comparison",label="Save Plot"),
						width=3
					),
					mainPanel(
						plotOutput("plot.comparison",width="auto",height="800px"),
						width=9
					)
				),
				sidebarPanel(
					p(strong("Details:"),
						"The data for these plots come from the U.S. CDC ",
						a("COVID-19 Cases and Deaths by State over Time",
						href="https://data.cdc.gov/Case-Surveillance/United-States-COVID-19-Cases-and-Deaths-by-State-o/9mfq-cb36",.noWS="outside"),
						".","Please",a("contact me",href="mailto:liamrevell@umb.edu")," with any questions."),
					width=12
				)
			)
		),
		tabPanel("Plausible range", fluid = TRUE,
			verticalLayout(
				sidebarLayout(
					sidebarPanel(
						p(strong("Estimated SARS-CoV-2 daily or cumulative infections."),br(),"See",
							a("Methodology",href="https://covid19-explorer.org/methodology/",.noWS="after"),
							" for more details.",style="text-align:center;"),
						selectInput(inputId="state.range",label="State or jurisdiction",
							choices=c("United States","Alabama","Alaska","Arizona",
							"Arkansas","California","Colorado","Connecticut",
							"Delaware","District of Columbia","Florida",
							"Georgia","Hawaii","Idaho","Illinois",
							"Indiana","Iowa","Kansas","Kentucky","Louisiana",
							"Maine","Maryland","Massachusetts","Michigan","Minnesota",
							"Mississippi","Missouri","Montana","Nebraska","Nevada",
							"New Hampshire","New Jersey","New Mexico","New York",
							"New York City","North Carolina","North Dakota",
							"Ohio","Oklahoma","Oregon","Pennsylvania","Puerto Rico",
							"Rhode Island","South Carolina","South Dakota","Tennessee",
							"Texas","Utah","Vermont","Virginia",
							"Washington","West Virginia","Wisconsin","Wyoming"),
							selected="Massachusetts"),
						sliderInput(inputId="ifr1.range",label="IFR range Feb. 1, 2020 (%):",
							value=c(0.5,1.2),min=0.05,max=1.5,step=0.025,round=2,ticks=FALSE),
						sliderInput(inputId="ifr2.range",label="IFR range May. 2, 2020 (%):",
							value=c(0.4,0.9),min=0.05,max=1.5,step=0.025,round=2,ticks=FALSE),
						sliderInput(inputId="ifr3.range",label="IFR range Aug. 2, 2020 (%):",
							value=c(0.35,0.75),min=0.05,max=1.5,step=0.05,round=2,ticks=FALSE),
						sliderInput(inputId="ifr4.range",label="IFR range Nov. 1, 2020 (%):",
							value=c(0.325,0.675),min=0.05,max=1.5,step=0.025,round=2,ticks=FALSE),
						sliderInput(inputId="ifr5.range",label="IFR range Jan. 31, 2021 (%):",
							value=c(0.35,0.65),min=0.05,max=1.5,step=0.025,round=2,ticks=FALSE),
						sliderInput(inputId="delay.range",label="Average lag-time from infection to death:",
							value=21,min=0,max=30,ticks=FALSE),
						sliderInput(inputId="span.range",label="Span (for LOESS smoothing):",
							value=0.12,min=0.05,max=0.4,ticks=FALSE),
						checkboxInput(inputId="percent.range",
							label="show as percent of total population",
							value=FALSE),
						checkboxInput(inputId="cumulative.range",
							label="show cumulative infections",
							value=FALSE),
						actionButton("twitter_share",
							label = "",icon = icon("twitter"),
							onclick = sprintf("window.open('%s')", tweet_url)),
						actionButton("facebook_share",
							label = "",icon = icon("facebook"),
							onclick = sprintf("window.open('%s')", facebook_url)),
						downloadButton(outputId="down.range",label="Save Plot"),
						width=3
					),
					mainPanel(
						plotOutput("plot.range",width="auto",height="800px"),
						width=9
					)
				),
				sidebarPanel(
					p(strong("Details:"),
						"The data for these plots come from the U.S. CDC ",
						a("COVID-19 Cases and Deaths by State over Time",
						href="https://data.cdc.gov/Case-Surveillance/United-States-COVID-19-Cases-and-Deaths-by-State-o/9mfq-cb36",.noWS="outside"),
						".",em("Estimated"),
						"infections are based on moving average or LOESS smoothed CDC mortality data and an infection fatality ratio (IFR) specified by the user.",
						"Number of infections in the last period of the data (during the lag-time to death) are based on observed cases and a fitted model for the relationship between observed and estimated infections through time.",
						"Complete code and more details of methodology are available here:",
						a("1",href="https://github.com/liamrevell/covid19.Explorer/",.noWS="after"),", ",
						a("2",href="http://covid19-explorer.org/methodology/",.noWS="after"),".",
						"Horizontal axis no longer being extended in this application tab.",
						"Please",a("contact me",href="mailto:liamrevell@umb.edu")," with any questions."),
					width=12
				)
			)
		),
		tabPanel("Infection estimator", fluid = TRUE,
			verticalLayout(
				sidebarLayout(
					sidebarPanel(
						p(strong("Estimated SARS-CoV-2 daily or cumulative infections."),br(),"See",
							a("Methodology",href="https://covid19-explorer.org/methodology/",.noWS="after"),
							" for more details.",style="text-align:center;"),
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
						sliderInput(inputId="ifr1",label="IFR Feb. 1, 2020 (%):",value=0.85,
							min=0.05,max=1.5,step=0.05,round=2,ticks=FALSE),
						sliderInput(inputId="ifr2",label="IFR May. 2, 2020 (%):",value=0.65,
							min=0.05,max=1.5,step=0.05,round=2,ticks=FALSE),
						sliderInput(inputId="ifr3",label="IFR Aug. 2, 2020 (%):",value=0.55,
							min=0.05,max=1.5,step=0.05,round=2,ticks=FALSE),
						sliderInput(inputId="ifr4",label="IFR Nov. 1, 2020 (%):",value=0.5,
							min=0.05,max=1.5,step=0.05,round=2,ticks=FALSE),
						sliderInput(inputId="ifr5",label="IFR Jan. 31, 2021 (%):",value=0.5,
							min=0.05,max=1.5,step=0.05,round=2,ticks=FALSE),
						sliderInput(inputId="delay",label="Average lag-time from infection to death:",
							value=21,min=0,max=30,ticks=FALSE),
						sliderInput(inputId="window",label="Window for moving averages:",
							value=1,min=1,max=21,ticks=FALSE),
						sliderInput(inputId="span",label="Span (for LOESS smoothing):",
							value=0.12,min=0.05,max=0.4,ticks=FALSE),
						checkboxInput(inputId="smooth",
							label="use smoothing for estimation",
							value=TRUE),
						checkboxInput(inputId="percent",
							label="show as percent of total population",
							value=FALSE),
						checkboxInput(inputId="cumulative.cases",
							label="show cumulative infections",
							value=FALSE),
						checkboxInput(inputId="show.points",label="show points (used in smoothing)",
							value=FALSE),
						actionButton("twitter_share",
							label = "",icon = icon("twitter"),
							onclick = sprintf("window.open('%s')", tweet_url)),
						actionButton("facebook_share",
							label = "",icon = icon("facebook"),
							onclick = sprintf("window.open('%s')", facebook_url)),
						downloadButton(outputId="down.cases",label="Save Plot"),
						width=3
					),
					mainPanel(
						plotOutput("plot.cases",width="auto",height="800px"),
						width=9
					)
				),
				sidebarPanel(
					p(strong("Details:"),
						"The data for these plots come from the U.S. CDC ",
						a("COVID-19 Cases and Deaths by State over Time",
						href="https://data.cdc.gov/Case-Surveillance/United-States-COVID-19-Cases-and-Deaths-by-State-o/9mfq-cb36",.noWS="outside"),
						".",em("Estimated"),
						"cases are based on moving average or LOESS smoothed CDC mortality data and an infection fatality ratio (IFR) specified by the user.",
						em("Observed"),"cases are the sum of confirmed and presumed cases according to CDC data.",
						"Number of cases in the last period of the data (during the lag-time to death) are based on observed cases and a fitted model for the relationship between observed and estimated cases through time.",
						"Complete code and more details of methodology are available here:",
						a("1",href="https://github.com/liamrevell/covid19.Explorer/",.noWS="after"),", ",
						a("2",href="http://covid19-explorer.org/methodology/",.noWS="after"),".",
						"Horizontal axis no longer being extended in this application tab.",
						"Please",a("contact me",href="mailto:liamrevell@umb.edu")," with any questions."),
					width=12
				)
			)
		),
		tabPanel("Excess mortality by age", fluid = TRUE,
			verticalLayout(
				sidebarLayout(
					sidebarPanel(
						p(strong("Weekly excess mortality by age & jurisdiction."),br(),"See",
							a("Methodology",href="https://covid19-explorer.org/methodology/",.noWS="after"),
							" for more details.",style="text-align:center;"),
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
						selectInput(inputId="year",label="Year",choices=c("2020","2021"),
							selected="2020"),
						checkboxGroupInput(inputId="age.group",label="Select age group(s):",
							choices=c("Under 25 years","25-44 years","45-64 years",
							"65-74 years","75-84 years","85 years and older"),
							selected=c("Under 25 years","25-44 years","45-64 years",
							"65-74 years","75-84 years","85 years and older")),
						selectInput(inputId="plot.age",label="Show plot of:",
							choices=c("raw & excess","raw & percent above normal")),
						checkboxInput(inputId="regression",
							label="use regression to obtain expected mortality (recommended)",
							value=TRUE),
						checkboxInput(inputId="corrected.age",
							label="correct to 2020 population",
							value=FALSE),
						checkboxInput(inputId="cumulative",
							label="show cumulative excess mortality",
							value=TRUE),
						actionButton("twitter_share",
							label = "",icon = icon("twitter"),
							onclick = sprintf("window.open('%s')", tweet_url)),
						actionButton("facebook_share",
							label = "",icon = icon("facebook"),
							onclick = sprintf("window.open('%s')", facebook_url)),
						downloadButton(outputId="down.age",label="Save Plot"),
						width=3
					),
					mainPanel(
						plotOutput("plot.age",width="auto",height="800px"),
						width=9
					)
				),
				sidebarPanel(
					p(strong("Details:"),
						"The data for these plots come from the U.S. CDC provisional death counts.", 
						"Data for recent weeks are estimated based on reporting in past years.",
						"Mortality data are from the ",a("CDC",href="https://data.cdc.gov/NCHS/Weekly-counts-of-deaths-by-jurisdiction-and-age-gr/y5bj-9g5w",
						target="_blank",.noWS="outside"),
						". Counts below 10 are not reported, so a Poisson model was used to estimate them.",
						"Complete code and more details of methodology are available here:",
						a("1",href="https://github.com/liamrevell/covid19.Explorer/",.noWS="after"),", ",
						a("2",href="http://covid19-explorer.org/methodology/",.noWS="after"),".",
						"Please",a("contact me",href="mailto:liamrevell@umb.edu")," with any questions."),
					width=12
				)
			)
		),
		tabPanel("Excess vs. COVID deaths",fluid = TRUE,
			verticalLayout(
				sidebarLayout(
					sidebarPanel(
						p(strong("Excess vs. confirmed COVID-19 deaths."),br(),
							"See",a("Methodology",href="https://covid19-explorer.org/methodology/",
							.noWS="after")," for more details.",style="text-align:center;"),
						selectInput(inputId="ages.evc",
							label="Select age group(s):",
							choices=c("Under 25 years",
							"25-44 years",
							"45-64 years",
							"65-74 years",
							"75-84 years",
							"85 years and older"),
						selected=c("Under 25 years",
							"25-44 years",
							"45-64 years",
							"65-74 years",
							"75-84 years",
							"85 years and older"),
							multiple=TRUE),
						selectInput(inputId="show.evc",
							label="Show plot of:",
							choices=c("weekly deaths","cumulative deaths","both"),
							selected="both"),
						actionButton("twitter_share",
							label = "",icon = icon("twitter"),
							onclick = sprintf("window.open('%s')", tweet_url)),
						actionButton("facebook_share",
							label = "",icon = icon("facebook"),
							onclick = sprintf("window.open('%s')", facebook_url)),
						downloadButton(outputId="down.evc",label="Save Plot"),
						width=3
					),
					mainPanel(
						plotOutput("plot.evc",width="auto",height="800px"),
						width=9
					)
				),
				sidebarPanel(
					p(strong("Details:"),
						"Mortality data are from the CDC.",
						"Please",a("contact me",href="mailto:liamrevell@umb.edu")," with any questions."),
					width=12
				)
			)
		)
	)
)

server <- function(input, output, data=Data, session) {
	makeIFR<-function(ifr,smooth=TRUE){
		ifr<-c(ifr[1],ifr,ifr[length(ifr)])
		tt<-c(1,32,32+91,32+91+92,32+91+92+91,366+32,366+32+28+31+30)
		tmp<-vector()    
		for(i in 1:(length(ifr)-1)){
			tmp<-c(tmp,seq(ifr[i],ifr[i+1],length.out=tt[i+1]-tt[i]))
		}
		if(smooth){
			tt<-1:(max(tt)-min(tt))
			tmp<-predict(loess(tmp~tt,span=0.3))
		}
		tmp
	}
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
	output$down.state<-downloadHandler(
		filename =  function() {
			paste("covid19.Explorer.plot-", Sys.Date(), ".png", sep="")
		},
		content = function(file) {
			png(file,width=12,height=10,units="in",res=300) # open the png device
			par(lend=1)
			state.deaths(state=input$state,plot=input$plot,
				las=1,cex.axis=0.8,cex.lab=0.9,
				type=if(input$type=="step") "s" else "l",
				data=data,corrected=input$corrected,
				bg="white")
			dev.off()
		}
	)
	output$plot.age<-renderPlot({
		options(scipen=10)
		par(lend=1)
		age.deaths(state=input$state.age,
			year=as.numeric(input$year),
			plot=input$plot.age,
			las=1,cex.axis=0.8,cex.lab=0.9,
			age.group=input$age.group,
			data=data,
			corrected=input$corrected.age,
			cumulative=input$cumulative,
			regression=input$regression)
	})
	output$down.age<-downloadHandler(
		filename =  function() {
			paste("covid19.Explorer.plot-", Sys.Date(), ".png", sep="")
		},
		content = function(file) {
			png(file,width=12,height=10,units="in",res=300) # open the png device
			par(lend=1)
			age.deaths(state=input$state.age,
				year=as.numeric(input$year),
				plot=input$plot.age,
				las=1,cex.axis=0.8,cex.lab=0.9,
				age.group=input$age.group,
				data=data,
				corrected=input$corrected.age,
				cumulative=input$cumulative,
				regression=input$regression,
				date.range=list(
				start.date=input$start.end.age[1],
				end.date=input$start.end.age[2]),
				bg="white")
			dev.off()
		}
	)
	output$plot.iceberg<-renderPlot({
		options(scipen=10)
		par(lend=1)
		iceberg.plot(state=input$state.ib,
			las=1,cex.axis=0.8,cex.lab=0.9,
			data=data,
			ifr=makeIFR(c(input$ifr1.ib,input$ifr2.ib,input$ifr3.ib,input$ifr4.ib,input$ifr5.ib)/100,
			smooth=TRUE),
			delay=input$delay.ib,window=1,
			span=input$span.ib)
	})
	output$down.iceberg<-downloadHandler(
		filename = function() {
			paste("covid19.Explorer.plot-", Sys.Date(), ".png", sep="")
		},
		content = function(file) {
			png(file,width=12,height=10,units="in",res=300) # open the png device
			options(scipen=10)
			par(lend=1)
			iceberg.plot(state=input$state.ib,
				las=1,cex.axis=0.8,cex.lab=0.9,
				data=data,
				ifr=makeIFR(c(input$ifr1.ib,input$ifr2.ib,input$ifr3.ib,input$ifr4.ib,input$ifr5.ib)/100,
				smooth=TRUE),
				delay=input$delay.ib,window=1,
				span=input$span.ib,
				bg="white")
			dev.off()
		}
	)	
	output$plot.cases<-renderPlot({
		options(scipen=10)
		par(lend=1)
		infection.estimator(state=input$state.cases,
			las=1,cex.axis=0.8,cex.lab=0.9,
			data=data,
			cumulative=input$cumulative.cases,
			ifr=makeIFR(c(input$ifr1,input$ifr2,input$ifr3,input$ifr4,input$ifr5)/100,
			smooth=input$smooth),
			delay=input$delay,window=input$window,
			smooth=input$smooth,
			percent=input$percent,
			show.points=input$show.points,
			span=input$span)
	})
	output$down.cases<-downloadHandler(
		filename =  function() {
			paste("covid19.Explorer.plot-", Sys.Date(), ".png", sep="")
		},
		content = function(file) {
			png(file,width=12,height=10,units="in",res=300) # open the png device
			par(lend=1)
			infection.estimator(state=input$state.cases,
				las=1,cex.axis=0.8,cex.lab=0.9,
				data=data,
				cumulative=input$cumulative.cases,
				ifr=makeIFR(c(input$ifr1,input$ifr2,input$ifr3,input$ifr4,input$ifr5)/100,
				smooth=input$smooth),
				delay=input$delay,window=input$window,
				smooth=input$smooth,
				percent=input$percent,
				show.points=input$show.points,
				span=input$span,
				bg="white")
			dev.off()
		}
	)
	output$plot.allstates<-renderPlot({
		options(scipen=10)
		par(lend=1)
		infections.by.state(las=1,cex.axis=0.8,cex.lab=0.9,
			data=data,
			ifr=makeIFR(c(input$ifr1.bs,input$ifr2.bs,input$ifr3.bs,input$ifr4.bs,input$ifr5.bs)/100,
			smooth=TRUE),
			delay=input$delay.bs,window=1,
			smooth=TRUE,show.ifr=input$show.ifr,
			cumulative=input$cumulative.bs,show.as.percent=input$show.as.percent,
			span=input$span.bs,
			cdr="average")
	})
	output$down.allstates<-downloadHandler(
		filename =  function() {
			paste("covid19.Explorer.plot-", Sys.Date(), ".png", sep="")
		},
		content = function(file) {
			png(file,width=12,height=8,units="in",res=300) # open the png device
			par(lend=1)
			infections.by.state(las=1,cex.axis=0.8,cex.lab=0.9,
				data=data,
				ifr=makeIFR(c(input$ifr1.bs,input$ifr2.bs,input$ifr3.bs,input$ifr4.bs,input$ifr5.bs)/100,
				smooth=TRUE),
				delay=input$delay.bs,window=1,
				smooth=TRUE,show.ifr=input$show.ifr,
				cumulative=input$cumulative.bs,show.as.percent=input$show.as.percent,
				span=input$span.bs,
				bg="white",
				cdr="average")
			dev.off()
		}
	)
	output$plot.range<-renderPlot({
		options(scipen=10)
		par(lend=1)
		infection.range.estimator(state=input$state.range,
			las=1,cex.axis=0.8,cex.lab=0.9,
			data=data,
			ifr.low=makeIFR(c(input$ifr1.range[1],input$ifr2.range[1],input$ifr3.range[1],
			input$ifr4.range[1],input$ifr5.range[1])/100,smooth=TRUE),
			ifr.high=makeIFR(c(input$ifr1.range[2],input$ifr2.range[2],input$ifr3.range[2],
			input$ifr4.range[2],input$ifr5.range[2])/100,smooth=TRUE),
			delay=input$delay.range,window=1,
			cumulative=input$cumulative.range,percent=input$percent.range,
			span=input$span.range)
	})
	output$down.range<-downloadHandler(
		filename =  function() {
			paste("covid19.Explorer.plot-", Sys.Date(), ".png", sep="")
		},
		content = function(file) {
			png(file,width=12,height=10,units="in",res=300) # open the png device
			par(lend=1)
			infection.range.estimator(state=input$state.range,
				las=1,cex.axis=0.8,cex.lab=0.9,
				data=data,
				ifr.low=makeIFR(c(input$ifr1.range[1],input$ifr2.range[1],input$ifr3.range[1],
				input$ifr4.range[1],input$ifr5.range[1])/100,smooth=TRUE),
				ifr.high=makeIFR(c(input$ifr1.range[2],input$ifr2.range[2],input$ifr3.range[2],
				input$ifr4.range[2],input$ifr5.range[2])/100,smooth=TRUE),
				delay=input$delay.range,window=1,
				cumulative=input$cumulative.range,percent=input$percent.range,
				span=input$span.range,
				bg="white")
			dev.off()
		}
	)
	output$plot.comparison<-renderPlot({
		options(scipen=10)
		par(lend=1,lwd=2)
		just.cases(state=input$states,las=1,cex.axis=0.8,cex.lab=0.9,
			data=data,window=input$window.c,cumulative=input$cumulative.c,
			per.capita=input$per.capita)
	})
	output$down.comparison<-downloadHandler(
		filename =  function() {
			paste("covid19.Explorer.plot-", Sys.Date(), ".png", sep="")
		},
		content = function(file) {
			png(file,width=12,height=10,units="in",res=300) # open the png device
			par(lend=1,lwd=2)
			just.cases(state=input$states,las=1,cex.axis=0.8,cex.lab=0.9,
				data=data,window=input$window.c,cumulative=input$cumulative.c,
				per.capita=input$per.capita,bg="white")
			dev.off()
		}
	)
	observe({
		x<-input$split.cd
		y<-input$show.cd
		if(x&&y%in%c("deaths / 1M population","as % of total"))
			updateSelectInput(session, "plot.cd",
				label="Plotting options:",
				choices=c("smoothed"),
				selected="smoothed")
	})
	observe({
		x<-input$split.cd
		if(x==FALSE)
			updateSelectInput(session, "plot.cd",
				label="Plotting options:",
				choices=c("bar plot","polygons","smoothed"),
				selected=input$plot.cd)
	})
	observe({
		x<-input$show.cd
		if(x=="raw deaths")
		updateSelectInput(session, "plot.cd",
			label="Plotting options:",
			choices=c("bar plot","polygons","smoothed"),
			selected=input$plot.cd)
	})
	output$plot.cd<-renderPlot({
		options(scipen=10)
		show<-if(input$show.cd=="raw deaths") "raw" else 
			if(input$show.cd=="deaths / 1M population") "per.capita" else
			if(input$show.cd=="as % of total") "percent" else
			if(input$show.cd=="as % of COVID-19 deaths") "percent.of.covid.deaths"
		plot<-if(input$plot.cd=="bar plot") "bar" else 
			if(input$plot.cd=="polygons") "standard" else
			if(input$plot.cd=="smoothed") "smooth"
		covid.deaths(age.group=input$ages,
			sex=input$sex,
			las=2,cex.axis=0.8,cex.lab=0.9,
			data=data,
			cumulative=input$cumulative.cd,
			show=show,plot=plot,
			split.groups=input$split.cd)
	})
	output$down.cd<-downloadHandler(
		filename =  function() {
			paste("covid19.Explorer.plot-", Sys.Date(), ".png", sep="")
		},
		content = function(file) {
			png(file,width=12,height=10,units="in",res=300) # open the png device
			par(lend=1)
			show<-if(input$show.cd=="raw deaths") "raw" else 
				if(input$show.cd=="deaths / 1M population") "per.capita" else
				if(input$show.cd=="as % of total") "percent" else
				if(input$show.cd=="as % of COVID-19 deaths") "percent.of.covid.deaths"
			plot<-if(input$plot.cd=="bar plot") "bar" else 
				if(input$plot.cd=="polygons") "standard" else
				if(input$plot.cd=="smoothed") "smooth"
			covid.deaths(age.group=input$ages,
				sex=input$sex,
				las=2,cex.axis=0.8,cex.lab=0.9,
				data=data,
				cumulative=input$cumulative.cd,
				show=show,plot=plot,
				split.groups=input$split.cd,
				bg="white")
			dev.off()
		}
	)
	output$plot.evc<-renderPlot({
		options(scipen=10)
		if(input$show.evc=="both"){ 
			par(mfrow=c(2,1))
			excess.vs.covid(age.group=input$ages.evc,las=1,cex.axis=0.8,cex.lab=0.9,
				data=data,cumulative=FALSE,lwd=4)
		}
		cumulative<-if(input$show.evc%in%c("cumulative deaths","both")) TRUE else FALSE
		excess.vs.covid(age.group=input$ages.evc,las=1,cex.axis=0.8,cex.lab=0.9,
			data=data,cumulative=cumulative,lwd=4)
	})
	output$down.evc<-downloadHandler(
		filename =  function() {
			paste("covid19.Explorer.plot-", Sys.Date(), ".png", sep="")
		},
		content = function(file) {
			png(file,width=12,height=10,units="in",res=300) # open the png device
			par(lend=1)
			if(input$show.evc=="both"){ 
				par(mfrow=c(2,1))
				excess.vs.covid(age.group=input$ages.evc,las=1,cex.axis=0.8,cex.lab=0.9,
					data=data,cumulative=FALSE,lwd=4,bg="white")
			}
			cumulative<-if(input$show.evc%in%c("cumulative deaths","both")) TRUE else FALSE
			excess.vs.covid(age.group=input$ages.evc,las=1,cex.axis=0.8,cex.lab=0.9,
				data=data,cumulative=cumulative,lwd=4,bg="white")
			dev.off()
		}
	)
}

shinyApp(ui = ui, server = server)
