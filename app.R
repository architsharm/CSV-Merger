library(shiny)
library(dplyr)
# Define UI for data upload app ----
ui <- fluidPage(
  titlePanel("Combine CSV"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      tags$hr(),
      
      checkboxInput("header", "Files contain Header", TRUE),
      #checkboxInput("recursive","Include files in subdirectories",FALSE),
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
      textInput("name", "Name of combined file", value = "Combined", width = NULL, placeholder = "Name of the combined File"),
      downloadButton("downloadData", "Download")
    ),
    
    mainPanel(
      tableOutput("contents"),
      uiOutput("choose_columns")
      
    )
    
  )
)

server <- function(input, output,session) {
  
  data2<<-data.frame()
  
  path<<-data.frame()
  path2<<-data.frame()
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$name,".csv", sep = "")
    },
    content = function(file) {
      write.csv(combine(path$Datapath,input$header,input$sep,input$quote), file, row.names = FALSE)
    
      # combine("C:/Users/Dell/Downloads/Rock.csv",TRUE,",",'"')
      }
  )
  
  output$contents<-renderTable(
    
    if(!is.null(input$file1)){
    d2<-readfiles(input$file1)
    d2<-select(d2,-Datapath)
    return(d2)
    }
    
  )
  
  
  # names<-reactive(unlist(path$Name))
  
  
  output$choose_columns <-renderUI({
    if(is.null(input$file1))
      return()
    d2<-readfiles2(input$file1)
    names<-select(d2,Name)%>%as.list()
    print(names[[1]])
    checkboxGroupInput("columns", "Check/Unchek", 
                       choices  = names[[1]],
                       selected = names[[1]])
  })
  
  
  
  
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

readfiles2<-function(test){
  test1<-test%>%as.data.frame()
  names(test1)<-c("Name","Size","Type","Datapath")
  test1$Name<-as.character(test1$Name)
  test1$Size<-as.character(test1$Size)
  test1$Type<-as.character(test1$Type)
  test1$Datapath<-as.character(test1$Datapath)
  path2<<-rbind(path2,test1)
  
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

