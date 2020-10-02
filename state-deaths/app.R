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
                                                  "excess per capita")),
      checkboxInput(inputId="corrected",
                    label="correct to 2020 U.S. population (per capita rates are automatically corrected)",
                    value=TRUE),
      selectInput(inputId="type",label="line type",choices=c("smooth","step")),
      h4("Details:\n"),
      p("The data for these plots come from the U.S. CDC provisional death counts through September, 2020."),
      p("Data for recent weeks are incomplete. I nonetheless report all the data here - but this is why we tend to see a drop in excess deaths towards the far right of each plot."),
      p("To correct for data incompleteness, the CDC uses a weighting algorithm based on reporting patterns in previous years to correct estimated weekly totals."),
      p("From the CDC: Provisional death counts are weighted to account for incomplete data. However, data for the most recent week(s) are still likely to be incomplete. Weights are based on completeness of provisional data in prior years, but the timeliness of data may have changed in 2020 relative to prior years, so the resulting weighted estimates may be too high in some jurisdictions and too low in others."),
      p("Data is from the",a("CDC",href="https://data.cdc.gov/NCHS/Weekly-Counts-of-Deaths-by-State-and-Select-Causes/3yf8-kanr",
        .noWS="after"),". State population size data is from the",a("U.S. census bureau",
        href="https://www.census.gov/data/datasets/time-series/demo/popest/2010s-state-total.html",.noWS="after"),
        ". All data files & code are available",a("here",
        href="https://github.com/liamrevell/CDC_COVID19_deaths/tree/master/state-deaths",.noWS="after"),"."),
      p("Please",a("contact me",href="mailto:liamrevell@umb.edu")," with any questions.")
    )
  )
)

server <- function(input, output) {
  output$plot<-renderPlot({
    options(scipen=10)
    par(bg="#f8fdff",lend=1)
    state.deaths(state=input$state,plot=input$plot,
              las=1,cex.axis=0.7,cex.lab=0.8,
              type=if(input$type=="step") "s" else "l",
              data=list(Counts=Counts,Provis=Provis,
              States=States),corrected=input$corrected)
  })
}

shinyApp(ui = ui, server = server)
