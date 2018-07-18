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
      
      checkboxInput("header", "Files contain Header", TRUE),
      
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

server <- function(input, output,session) {
  data2<<-data.frame()
  path<<-data.frame()
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("combined",".csv", sep = "")
    },
    content = function(file) {
      write.csv(combine(path$Datapath,input$header,input$sep,input$quote), file, row.names = FALSE)
    }
  )
  
  output$contents<-renderTable(
    
    if(!is.null(input$file1)){
    d2<-readfiles(input$file1)
    d2<-select(d2,-Datapath)
    return(d2)
    }
    
  )
  
  
  
}


readfiles<-function(test){
  test1<-test%>%as.data.frame()
  names(test1)<-c("Name","Size","Type","Datapath")
  test1$Name<-as.character(test1$Name)
  test1$Size<-as.character(test1$Size)
  test1$Type<-as.character(test1$Type)
  test1$Datapath<-as.character(test1$Datapath)
  path<<-rbind(path,test1)
  
}


combine<-function(list,head,sep,quote){
  list1<-list%>%as.data.frame()
  for(i in 1:nrow(list1)){
    path1<-list1[i,1]
    d1<-read.csv(path1%>%as.character(),header = head,sep = sep,quote = quote,stringsAsFactors = F)
    data2<<-plyr::rbind.fill(data2,d1)
  }
  
  return(data2)
}


shinyApp(ui, server)
