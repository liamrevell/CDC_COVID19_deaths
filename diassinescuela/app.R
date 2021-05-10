library(shiny)

ui<-fixedPage(
		fixedRow(
		p(br(),br()),
		dateInput(inputId="last.day",
			label="Last day of in-person school:",
			value=as.Date("3/13/2020",format="%m/%d/%Y"),
		  min=as.Date("01/01/2020",format="%m/%d/%Y"),
		 	max=Sys.Date(),startview="month"),
		dateInput(inputId="first.day",
			label="First day back:",
			value=Sys.Date(),
		  min=as.Date("01/01/2020",format="%m/%d/%Y"),
		 	max=as.Date("12/31/2021",format="%m/%d/%Y"),
			startview="month"),
		selectInput(inputId="language",
			label="Language:",
			choices=c("English","Spanish"),
			selected="English"),
		),
		fixedRow(
		plotOutput("plot.lastday",width="auto",height="400px")
		)
)

server <- function(input, output, data=Data, session) {
	output$plot.lastday<-renderPlot({
		days<-input$first.day-input$last.day
		plot(NA,xlim=c(0,1),ylim=c(0,1),axes=FALSE,xlab="",ylab="")
		text(0.5,0.75,days,cex=8)
		if(input$language=="English") text(0.5,0.4,"days since\nin-person school",cex=2)
		else if(input$language=="Spanish") text(0.5,0.4,
			"d\u00edas seguidos\nsin colegio presencial",cex=2)
	})
}

shinyApp(ui = ui, server = server)
