library(shiny)
library(shinyWidgets)
library(plotly)
library(covid19.Explorer)
#Counts<-read.csv("https://liamrevell.github.io/data/Weekly_Counts_of_Deaths_by_State_and_Select_Causes__2014-2018.csv")
#Provis<-read.csv("https://liamrevell.github.io/data/Weekly_Counts_of_Deaths_by_State_and_Select_Causes__2019-2020.csv")
#States<-read.csv("https://liamrevell.github.io/data/nst-est2019-01.csv",row.names=1)
#age.Counts<-read.csv("https://liamrevell.github.io/data/Weekly_counts_of_deaths_by_jurisdiction_and_age_group.csv")
#Cases<-read.csv("https://liamrevell.github.io/data/United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv")
#Centers<-read.csv("https://liamrevell.github.io/data/Centers.csv")

#Data<-list(Counts=Counts,Provis=Provis,States=States,age.Counts=age.Counts,Cases=Cases,Centers=Centers)

data(CDC.data)
Data<-CDC.data

ui<-fluidPage(
  setBackgroundColor(
    color = c("white", "#E8E8E8"),
    gradient = "linear",
    direction = "bottom"
  ),
  tabsetPanel(
    type="pills",
    tabPanel("Infection estimator", fluid = TRUE,
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
            sliderInput(inputId="delay",label="Average lag-time from infection to death:",
              value=21,min=0,max=30,ticks=FALSE),
            sliderInput(inputId="window",label="Window for moving averages:",
              value=7,min=1,max=21,ticks=FALSE),
            sliderInput(inputId="span",label="Span (for LOESS smoothing):",
              value=0.15,min=0.05,max=0.4,ticks=FALSE),
            checkboxInput(inputId="smooth",
              label="use smoothing for estimation",
              value=TRUE),
            checkboxInput(inputId="percent",
              label="show as percent of total population",
              value=FALSE),
            checkboxInput(inputId="cumulative.cases",
              label="show cumulative infections",
              value=FALSE),
            width=3
          ),
          mainPanel(
            plotlyOutput("plot",width="auto",height="800px"),
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
          href="https://github.com/liamrevell/covid19.Explorer/",target="_blank",.noWS="after"),".",
          "Please",a("contact me",href="mailto:liamrevell@umb.edu")," with any questions."),
          width=12)
        )
      )
    )
)

server <- function(input, output, data=Data) {
  output$plot<-renderPlotly({
    infections<-infection.estimator(state=input$state.cases,
      las=1,cex.axis=0.8,cex.lab=0.9,
      data=data,
      cumulative=input$cumulative.cases,
      ifr=c(input$ifr1,input$ifr2,input$ifr3,input$ifr4,input$ifr5)/100,
      delay=input$delay,window=input$window,
      smooth=input$smooth,
      percent=input$percent,
      show.points=input$show.points,
      span=input$span,plot=FALSE)
    infections<-infections[45:length(infections)]
    dd<-seq(from=as.Date("2020/3/15"),by=1,length.out=length(infections))
    plot_ly(x=dd,y=infections,type="scatter",
      mode="line")
  })
}

shinyApp(ui = ui, server = server)