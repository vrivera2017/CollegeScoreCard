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
library(tidyverse)
library(shinyBS)

mydata = read.csv("scorecard-edited.csv") #read CSV file
mydata$ADM_RATE_ALL <- as.numeric(as.character(mydata$ADM_RATE_ALL))
mydata$TUITIONFEE_IN <- as.numeric(as.character(mydata$TUITIONFEE_IN))
mydata$TUITIONFEE_OUT <- as.numeric(as.character(mydata$TUITIONFEE_OUT))
mydata$FTFTPCTFLOAN <- as.numeric(as.character(mydata$FTFTPCTFLOAN))
mydata$FTFTPCTPELL <- as.numeric(as.character(mydata$FTFTPCTPELL))

newdata <- na.omit(mydata)
state = newdata %>% pull(STABBR)
print(state)
edited_state = state[!duplicated(state)] #duplicates removed 
cali = filter (newdata, STABBR == "CA")
print(cali)

#values = newdata %>% mutate(admissions_rate = ADM_RATE_ALL, in_state_tuition = TUITIONFEE_IN, percent_students_with_loans = FTFTPCTFLOAN)
#print(values)                  
columnlist <- colnames(newdata)
print(columnlist)
columnlistshort <- list(admissions_rate = columnlist[9], 
                        in_state_tuition = columnlist[37], 
                        out_of_state_tuition = columnlist[38],
                        percent_students_with_federal_loans = columnlist[319],
                        percent_students_with_pell_grant = columnlist[318])
print(columnlistshort)

#basic scatterplot
#filterpractice <- filter (newdata, STABBR=='AL')
#ggplot(data = filterpractice, aes(x=ADM_RATE_ALL, y=TUITIONFEE_IN)) + geom_point() 

ui <- fluidPage(
    headerPanel("College Score Card Data Explorer"),
    hr(),
    title = "College Score Card Explorer",
    plotOutput("scatterplot"),
    hr(),
    fluidRow(
      column(4, 
        h4("Instructions"),
        p("Here are the instructions for using this app. It is so much fun to use. I encourage you to try and I hope that I can pass this comp exam this quarter and be done with this.")),
      column(3, 
        offset = 1,
        selectInput(inputId = "x", 
                    label = "Select a value for the x-axis", 
                    choices = c("Select a value"="",columnlistshort)), 
        selectInput(inputId = "y", 
                    label = "Select a value for the y-axis", 
                    choices = c("Select a value"="",columnlistshort))
      ),
      column(4,
        checkboxGroupInput(inputId = "state", 
                           label = "Select state(s)",
                           inline = TRUE,
                           choices = edited_state),
        actionButton("selectall", label="Select/Deselect all states"),
        br(),
        br(),
        actionButton("goButton", "Plot!"))
    )
)

server <- function(input, output, session){
  observe({
    if(input$selectall > 0) {
      if(input$selectall %% 2 == 0) {
        updateCheckboxGroupInput(session = session, 
                                 inputId = "state", 
                                 inline = TRUE,
                                 choices = edited_state,
                                 selected = NULL)
      }
      else {
        updateCheckboxGroupInput(session = session, 
                                 inputId = "state", 
                                 inline = TRUE,
                                 choices = edited_state, 
                                 selected = edited_state)
      }
    }
  })
  
  output$scatterplot <- renderPlot({
    title <- "test plot"
    filterdata <- filter(newdata, STABBR %in% input$state)
    print(input$state)
    if (input$goButton == 0)
      return()
    isolate({ggplot(data = filterdata, aes_string(x=input$x, y=input$y, color="STABBR")) + geom_point(size = 3)}) 
  })
}

shinyApp(ui = ui, server = server)