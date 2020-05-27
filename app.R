library(shiny)
p <- read.csv("C:/Users/yihxun/Desktop/ippt data/pushup.csv", header = TRUE)
s <- read.csv("C:/Users/yihxun/Desktop/ippt data/situp.csv", header = TRUE)
r <- read.csv("C:/Users/yihxun/Desktop/ippt data/run.csv", header = TRUE)

ui <- fluidPage(
  titlePanel("IPPT Calculator"),
  
  column(3,
         selectInput("cat", h3("Category"), 
                     choices = list("Service/Combat" = 1, "Diver/Guards/Commando" = 2
                                    ), selected = 1),
         
         
         numericInput("age", h3("Age"), min = 16, max = 60, value = 16)),
  
  column(4, 
           sliderInput("push", h3("Push Ups"),
                       min = 0, max = 60, value = 30),
           sliderInput("sit", h3("Sit Ups"),
                       min = 0, max = 60, value = 30),
         sliderInput("run", h3("2.4km Run"),
                     min = as.POSIXct("2020-01-01 00:08:30"), 
                     max = as.POSIXct("2020-01-01 00:18:20"),
                     value = as.POSIXct("2020-01-01 00:11:00"),
                     step = 10, timeFormat = "%M:%S")
    ),
  
  sidebarPanel(
    textOutput("pushup"), position = "right"
  ),
  
  sidebarPanel(
    textOutput("situp"), position = "right"
  ),
  
  sidebarPanel(
    textOutput("running"), position = "right"
  ),
  
  sidebarPanel(
    textOutput("points"), position = "right"
  ),
  
  sidebarPanel(
    textOutput("result"), position = "right"
  )
  
)

# Define server logic ----
server <- function(input, output) {
  
  output$pushup <- renderText({ 
    paste("Pushup Points:", p[61 - input$push, input$age - 14])
  })
  
  output$situp <- renderText({ 
    paste("Situp Points:", s[61 - input$sit, input$age - 14])
  })
  
  output$running <- renderText({ 
    paste("2.4km Run Points:", r[r[,2] == as.character(input$run), input$age - 13])
  })
  
  output$points <- renderText({ 
    paste("Total Points:",
          p[61 - input$push, input$age - 14] +
            s[61 - input$sit, input$age - 14] +
            r[r[,2] == as.character(input$run), input$age - 13])
  })
  
  output$result <- renderText({
    paste("Result:",
          if(p[61 - input$push, input$age - 14] +
                  s[61 - input$sit, input$age - 14] +
                  r[r[,2] == as.character(input$run), input$age - 13] >=90 && input$cat == "2"){
            print("GOLD")}
          
          else if(p[61 - input$push, input$age - 14] +
                  s[61 - input$sit, input$age - 14] +
                  r[r[,2] == as.character(input$run), input$age - 13] >=85 && input$cat == "1"){
            print("GOLD")}
          
          else if(p[61 - input$push, input$age - 14] +
                  s[61 - input$sit, input$age - 14] +
                  r[r[,2] == as.character(input$run), input$age - 13] >=75){
            print("SILVER")}
          
          else if(p[61 - input$push, input$age - 14] +
                  s[61 - input$sit, input$age - 14] +
                  r[r[,2] == as.character(input$run), input$age - 13] >=61){
            print("PASS with Incentive")}
          
          else if(p[61 - input$push, input$age - 14] +
                  s[61 - input$sit, input$age - 14] +
                  r[r[,2] == as.character(input$run), input$age - 13] >=51){
            print("PASS")}
          
          else if(p[61 - input$push, input$age - 14] +
                  s[61 - input$sit, input$age - 14] +
                  r[r[,2] == as.character(input$run), input$age - 13] <51){
            print("FAIL")}
    
    
    )
    
    }
  )
  
}

# Run the app ----
shinyApp(ui = ui, server = server)