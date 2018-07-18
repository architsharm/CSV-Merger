library(shiny)
library(dplyr)
# Define UI for data upload app ----
ui <- fluidPage(
  titlePanel("Uploading Files"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      tags$hr(),
      
      checkboxInput("header", "Header", TRUE),
      
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      tags$hr(),
      
      downloadButton("downloadData", "Download")
      
    ),
    
    mainPanel(
      tableOutput("contents")
      
    )
    
  )
)

server <- function(input, output) {
  data2<<-data.frame()
  output$downloadData <- downloadHandler(
    
    filename = function() {
      paste("combined",".csv", sep = "")
    },
    
    content = function(file) {
      write.csv(combine(input$file1$datapath), file, row.names = FALSE)
    }
  )
  
  
}


combine<-function(list){
  list1<-list%>%as.data.frame()
  for(i in 1:nrow(list1)){
    path<-list1[i,1]
    d1<-read.csv(path%>%as.character(),stringsAsFactors = F)
    data2<<-plyr::rbind.fill(data2,d1)
  }
  
  return(data2)
}


shinyApp(ui, server)