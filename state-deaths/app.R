source("state.deaths.R")
Counts<-read.csv("https://liamrevell.github.io/data/Weekly_Counts_of_Deaths_by_State_and_Select_Causes__2014-2018.csv")
Provis<-read.csv("https://liamrevell.github.io/data/Weekly_Counts_of_Deaths_by_State_and_Select_Causes__2019-2020.csv")
States<-read.csv("https://liamrevell.github.io/data/nst-est2019-01.csv",row.names=1)

ui<-fluidPage(
  titlePanel("Provisional death counts by state (all causes)"),
  sidebarLayout(
    mainPanel(
      plotOutput("plot",width="100%",height="800px")
    ),
    sidebarPanel(
      selectInput(inputId="state",label="State or Jurisdiction",
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
      selectInput(inputId="plot",
                  label="show plot of:",choices=c("raw","per capita","excess",
                                                  "excess per capita",
                                                  "percent above normal")),
      checkboxInput(inputId="corrected",
                    label="correct to 2020 population (per capita rates are automatically corrected)",
                    value=TRUE),
      dateRangeInput(inputId="start.end",label="starting & ending dates:",
                     start=as.Date("01/01/2020",format="%m/%d/%Y"),
                     end=as.Date("12/31/2020",format="%m/%d/%Y"),
                     min=as.Date("01/01/2020",format="%m/%d/%Y"),
                     max=as.Date("12/31/2020",format="%m/%d/%Y"),startview="month"),
      selectInput(inputId="type",label="line type",choices=c("smooth","step")),
      h4("Details:\n"),
      p("The data for these plots come from the U.S. CDC provisional death counts through September, 2020. Data for recent weeks are incomplete. For more information please refer to the",
        a("CDC technical notes",href="https://www.cdc.gov/nchs/nvss/vsrr/covid19/tech_notes.htm",target="_blank",.noWS="after"),"."),
      p("Mortality data are from the CDC (",a("1",href="https://data.cdc.gov/NCHS/Weekly-Counts-of-Deaths-by-State-and-Select-Causes/3yf8-kanr",
        target="_blank",.noWS="outside"),", ",a("2",
        href="https://data.cdc.gov/NCHS/Weekly-Counts-of-Deaths-by-State-and-Select-Causes/muzy-jte6",
        target="_blank",.noWS="outside"),"). State population size data are from the",a("U.S. census bureau",
        href="https://www.census.gov/data/datasets/time-series/demo/popest/2010s-state-total.html",target="_blank",.noWS="after"),
        ". All data files & code are available",a("here",
        href="https://github.com/liamrevell/CDC_COVID19_deaths/tree/master/state-deaths",target="_blank",.noWS="after"),"."),
      p("Detailed methodology is available",a("here",
        href="https://liamrevell.github.io/excess-mortality-methodology.html",target="_blank",.noWS="after"),"."),
      p("Please",a("contact me",href="mailto:liamrevell@umb.edu")," with any questions.")
    )
  )
)

server <- function(input, output) {
  output$plot<-renderPlot({
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
}

shinyApp(ui = ui, server = server)
