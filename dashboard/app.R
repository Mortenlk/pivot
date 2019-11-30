#App.R


rm(list=ls()) # Fjerner alle matriser

# Pakker ------------------------------------------------------------------


library(shiny)
library(shinydashboard)
library(RMariaDB)
library(tidyverse)
library(readxl)
library(rpivotTable)
library(rhandsontable)
library(corrplot)
library(jsonlite)


# Passwords ---------------------------------------------------------------

#id = fromJSON("~/prosjekter/pivot/dashboard/keys.json")$id  
#pw = fromJSON("~/prosjekter/pivot/dashboard/keys.json")$pw


id = fromJSON("keys.json")$id  
pw = fromJSON("keys.json")$pw


# pivot <- tribble(
#   ~v1, ~v2,
#   #--|----
#   2017, 4785,
#   2018, 5307,
#   2019, 5390
# )




ui <- dashboardPage(

#1  Header ------------------------------------------------------------------

  dashboardHeader( disable = FALSE,
    
    title = "Analysedashboard",
    titleWidth = 450


  )# Header slutt
,


# Sidebar -----------------------------------------------------------------

  dashboardSidebar(  disable = FALSE,
                     width = 250,

    
# Sidebarmenu -------------------------------------------------------------

        
    sidebarMenu(


# Sidebarmenu 1 Dashboard -------------------------------------------------

      menuItem("Last opp excelfil", tabName = "dashboard", icon = icon("dashboard")
               ),


menuItem("Pivot", tabName = "pivot", icon = icon("dashboard")
),


menuItem("Tabell", tabName = "tabell", icon = icon("dashboard")
)
,

menuItem("Variabeltype", tabName = "type", icon = icon("dashboard")
)
,


menuItem("Korrelasjon", tabName = "Korrelasjon", icon = icon("dashboard")
)



    ) #Menu slutt

    
  ) # Sidebar slutt
,


# 2 Body --------------------------------------------------------------------

  dashboardBody(
    
    tabItems(

# Body 1 Laste opp --------------------------------------------------------

      tabItem(tabName = "dashboard",
              
              
              fluidPage(
                titlePanel("Use readxl"),
                sidebarLayout(
                  sidebarPanel(
                    fileInput('file1', 'Choose xlsx file',
                              accept = c(".xlsx"),
                              
                    ),
                    actionButton("save","Lagre")
                  ),
                  mainPanel(
                    tableOutput('contents'))
                )
              )
              
              
      ) ,


# Body  Pivot --------------------------------------------------------

tabItem(tabName = "pivot",
        
        
        fluidRow(
          
          rpivotTableOutput("test")
          
          
          
        )
        
        
        
        
) 

,


# Body  Tabell --------------------------------------------------------

tabItem(tabName = "tabell",
        

          
          
          fluidPage(
            mainPanel(
              rHandsontableOutput("hot"),
              actionButton("save2","Lagre")
            )
          
          
          
        )
        
        
        
        
) 

,



# Body  Tabell --------------------------------------------------------

tabItem(tabName = "type",
        
        
        
        
        fluidPage(
          
          
          mainPanel(
            verbatimTextOutput("strfile")
          )
          
          
          
        )
        
  
        
        
) 
,

# Body Korrelasjon ---------------------------------------------------

# Second tab content
tabItem(tabName = "Korrelasjon",
        
        fluidRow(
          
            tabBox(
              width = 12,
              title = "Korrelasjon",
              tabPanel(" ", plotOutput("korrplot"))
              
            
          )
        )
        
        
) # tabItem slutt 




# Neste side kommer her (lim inn over. -----------------------------------------------
    )

    ) #  dashboardBody slutt
    
    
  ) #Body slutt



# 3 Server ------------------------------------------------------------------


server <- function(input, output) { 

  


# Data --------------------------------------------------------------------

  
  # Data --------------------------------------------------------------------
  
  con <- dbConnect(RMariaDB::MariaDB(),
                   user= id, password= "Morten1977",
                   dbname= "pivottabell", host="localhost")
  
  #dbWriteTable(con, "pivot", pivot, overwrite=T)
  
  pivot <- dbReadTable(con, "pivot", pivot, overwrite=TRUE)
  
  
  dbDisconnect(con)
  rm(con)
  
  

# Pivot -------------------------------------------------------------------

  

  output$test <- rpivotTable::renderRpivotTable({

      
    
            rpivotTable(data = pivot)
  
    
    })


  

# Upload excel ------------------------------------------------------------

  output$contents <- renderTable({
    
    req(input$file1)
    
    inFile <- input$file1
    
    read_excel(inFile$datapath, 1)
    
  
  })  


# Laste opp ---------------------------------------------------------------
  
  # You can access the value of the widget with input$file, e.g.
  observeEvent(input$save, {
    
      str(input$file1)
    
    con <- dbConnect(RMariaDB::MariaDB(),
                     user= id, password= "Morten1977",
                     dbname= "pivottabell", host="localhost")
    
    
    pivot <- read_excel(input$file1$datapath)
    
    dbWriteTable(con, "pivot", pivot, overwrite=T)
    
 #   pivot <- dbReadTable(con, "pivot", pivot, overwrite=TRUE)
    
    
    dbDisconnect(con)
    rm(con)
    
    
  })
  

# Laste ned ---------------------------------------------------------------


  # You can access the value of the widget with input$file, e.g.
  observeEvent(input$save, {
    
   str(input$file1)
    
    con <- dbConnect(RMariaDB::MariaDB(),
                     user= id, password= "Morten1977",
                     dbname= "pivottabell", host="localhost")
    
    
    pivot <- read_excel(input$file1$datapath)
    
#    dbWriteTable(con, "pivot", pivot, overwrite=T)
    
    pivot <- dbReadTable(con, "pivot", pivot, overwrite=TRUE)
    
    
    dbDisconnect(con)
    rm(con)
    
    
  })
  

  
  # Reactive ----------------------------------------------------------------
  
  
  con <- dbConnect(RMariaDB::MariaDB(),
                   user= id, password= "Morten1977",
                   dbname= "pivottabell", host= "localhost")
  
  
  
#  dbWriteTable(con, "pivot", values$data, overwrite=T)
  
 pivot <- dbReadTable(con, "pivot", pivot, overwrite=TRUE)
  
  
  dbDisconnect(con)
  rm(con)
  
  
  values <- reactiveValues(data = pivot)
  
  observe({
    if(!is.null(input$hot)){
      values$data <- as.data.frame(hot_to_r(input$hot))
      # isolate(values$data[,'conten'] <- ifelse(values$data[,'id'], values$data[,'start']-values$data[,'end'] ,0))
      print(values$data)
      output$hot <- renderRHandsontable({
        rhandsontable(values$data)
      })
    }
  })    
  
  output$hot <- renderRHandsontable({
    rhandsontable(values$data)
  })
  
  
  # When the Save button is clicked, last opp
  observeEvent(input$save2, {
    
    
    con <- dbConnect(RMariaDB::MariaDB(),
                     user= id, password= "Morten1977",
                     dbname= "pivottabell", host="localhost")
    
    
    
    dbWriteTable(con, "pivot", values$data, overwrite=T)
    
    # df <- dbReadTable(con, "df", alt, overwrite=TRUE)
    
    
    dbDisconnect(con)
    rm(con)
    
    
  })
  
  

# Type --------------------------------------------------------------------

  output$strfile <- renderPrint({str(pivot)
    
    
    })
  
 


# Correlation plot --------------------------------------------------------

  output$korrplot <- renderPlot({
    
    M <- cor(na.omit(pivot[, sapply(pivot, is.numeric)]))
    
      
    corrplot(M,
             type = "upper",
             addCoef.col = "black",
             method = "number",
             tl.col = "black"
    )  
  })
  
    
    
    
  
  } # Server slutt


# App ---------------------------------------------------------------------


shinyApp(ui, server)