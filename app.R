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

mydata = read.csv("scorecard-edited.csv") #read CSV file
mydata$ADM_RATE_ALL <- as.numeric(as.character(mydata$ADM_RATE_ALL))
mydata$TUITIONFEE_IN <- as.numeric(as.character(mydata$TUITIONFEE_IN))
mydata$FTFTPCTFLOAN <- as.numeric(as.character(mydata$FTFTPCTFLOAN))

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
columnlistshort <- list(admissions_rate = columnlist[9], in_state_tuition = columnlist[37])
print(columnlistshort)

#basic scatterplot
filterpractice <- filter (newdata, STABBR=='AL')
ggplot(data = filterpractice, aes(x=ADM_RATE_ALL, y=TUITIONFEE_IN)) + geom_point() 

ui <- fluidPage(
  headerPanel("College Score Card Data"),
  selectInput(inputId = "x", 
              label = "Select a value for the x-axis", 
              choices = c("Select a value"="",columnlistshort)), 
  selectInput(inputId = "y", 
              label = "Select a value for the y-axis", 
              choices = c("Select a value"="",columnlistshort)), 
  checkboxGroupInput(inputId = "state", 
                label = "Select state(s)",
                inline = TRUE,
                choices = edited_state),
  actionButton("goButton", "Go!"),
  plotOutput("scatterplot")
)

server <- function(input, output){
  output$scatterplot <- renderPlot({
    title <- "test plot"
    #filterdata <- filter(newdata, STABBR==input$state)
    ggplot(data = newdata, aes_string(x=input$x, y=input$y)) + geom_point() #+ aes(color=input$state)
  })
}

shinyApp(ui = ui, server = server)











# # Define UI for application 
# ui <- fluidPage(
#   tags$h1(tags$em("Practice App")),
#   sliderInput(inputId = "num",
#               label = "Choose a number", 
#               value = 25, min = 1, max = 100),
#   plotOutput("hist") #this just creates space for the output object. you have to build the output object in the server function 
#   
# ) 
# 
# server <- function(input, output){
#   output$hist <- renderPlot({
#     title <- "n random normal values"
#     random_numbers <- rnorm(input$num)
#     #df <- data.frame(random_numbers)
#     hist(random_numbers, main = title)
#     #ggplot(df) + geom_histogram(aes(x=random_numbers))
#     
#   })
# }


