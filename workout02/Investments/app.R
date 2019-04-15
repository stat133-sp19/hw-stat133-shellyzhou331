#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Investment Accumulative Values"),
   
   # Sidebar with a slider input for number of bins 
fluidRow(
  column(4, 
         sliderInput("initial",
                     label = "Initial Amount",
                     min = 0,
                     max = 100000,
                     value = 1000, step = 500),
           sliderInput("contribution",
                       label = "Annual Contribution",
                       min = 0,
                       max = 50000,
                       value = 2000, step = 500)
         ),
  column(4, 
         sliderInput("rate",
                     label = "Return Rate (in %)",
                     min = 0,
                     max = 20,
                     value = 5, step = 0.1),
         sliderInput("growth",
                     label = "Growth Rate (in %)",
                     min = 0,
                     max = 20,
                     value = 2, step = 0.1)
         ),
  column(4, 
         sliderInput("year",
                     label = "Years",
                     min = 0,
                     max = 50,
                     value = 20, step = 1),
         selectInput("facet",
                     label = "Facet?",
                     c("No", "Yes")
                     )
  )
      ),
      hr(), 
      # Show a plot of the generated distribution
        h4("Timeline"),
         plotOutput("moneyPlot"), 
        h4("Balances"),
         verbatimTextOutput("table")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #create data 
   mydata <- reactive({
     no_contrib <- rep(0, input$year + 1)
     no_contrib[1] <- input$initial
     for (i in 2:(input$year + 1)){
       no_contrib[i] <- no_contrib[i-1]*(1 + input$rate/100)
     }
     #Fixed Contribution with 3 savings 
     fixed_contrib <- rep(0, input$year + 1)
     fixed_contrib[1] <- input$initial
     for (i in 2:(input$year + 1)){
       fixed_contrib[i] <- fixed_contrib[i-1]*(1+input$rate/100) + input$contribution
     }
     #Growing Contribution with 3 savings 
     grow_contrib <- rep(0, input$year + 1)
     grow_contrib[1] <- input$initial
     for (i in 2:(input$year + 1)){
       grow_contrib[i] <- grow_contrib[i-1]*(1+input$rate/100) + input$contribution*((1 + input$growth/100)^(i-2))
     }
     #Create Data Frame
     years <- c(rep(0:input$year, 3))
     modality <- c(rep("no_contrib", input$year + 1), rep("fixed_contrib", input$year + 1), rep("grow_contrib", input$year + 1))
     con <- c(no_contrib, fixed_contrib, grow_contrib)
     table <- cbind(con, years, modality)
     table1 <- as.data.frame(table, stringsAsFactors = FALSE)
     table1[, 2] <- as.numeric(table1[, 2])
     table1[, 1] <- as.numeric(table1[, 1])
     table1
   }
   )
   mytable <- reactive({
     no_contrib <- rep(0, input$year + 1)
     no_contrib[1] <- input$initial
     for (i in 2:(input$year + 1)){
       no_contrib[i] <- no_contrib[i-1]*(1 + input$rate/100)
     }
     #Fixed Contribution with 3 savings 
     fixed_contrib <- rep(0, input$year + 1)
     fixed_contrib[1] <- input$initial
     for (i in 2:(input$year + 1)){
       fixed_contrib[i] <- fixed_contrib[i-1]*(1+input$rate/100) + input$contribution
     }
     #Growing Contribution with 3 savings 
     grow_contrib <- rep(0, input$year + 1)
     grow_contrib[1] <- input$initial
     for (i in 2:(input$year + 1)){
       grow_contrib[i] <- grow_contrib[i-1]*(1+input$rate/100) + input$contribution*((1 + input$growth/100)^(i-2))
     }
     #Create Data Frame
     years <- c(0:input$year)
     tables <- cbind(years, no_contrib, fixed_contrib, grow_contrib)
     table2 <- as.data.frame(tables, stringsAsFactors = FALSE, col.names = c("year", "no_contrib", "fixed_contrib", "growing_contrib"))
     table2
   })
   output$moneyPlot <- renderPlot({
     table1 <- mydata()
     if (input$facet == "No"){
     ggplot(table1) + geom_line(aes(x = years, y = con, color = modality, group = modality), size = 1) + geom_point(aes(x = years, y = con, color = modality, group = modality)) + labs(x = "Year", y = "Future Value")
     }
     else {
     ggplot(table1) + geom_line(aes(x = years, y = con, color = modality, group = modality), size = 1) + geom_point(aes(x = years, y = con, color = modality, group = modality)) + facet_wrap(~modality) + labs(x = "Year", y = "Future Value") + geom_area(aes(x = years, y = con, fill = modality), alpha = 0.2)
     }}
     )

   output$table <- renderPrint({
     table2 <- mytable()
     table2
   }
   )
}

# Run the application 
shinyApp(ui = ui, server = server)

