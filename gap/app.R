#App.R

rm(list=ls()) # Fjerner alle matriser

# Pakker ------------------------------------------------------------------


library(shiny)
library(shinydashboard)
library(RMariaDB)
library(tidyverse)
library(ggvis)
library(jsonlite)


# Passwords ---------------------------------------------------------------


id = fromJSON("~/prosjekter/kommunedata/scripts/keys.json")$id  

pw = fromJSON("~/prosjekter/kommunedata/scripts/keys.json")$pw



# Verdier -----------------------------------------------------------------

tittel <- "økonomisk handlingsrom  "

Avh_navn <- "nettodrift"
Avh <- "nettodrift"

Uavh_navn <- "dispfond"
Uavh <- "dispfond"

min_y <- -8
max_y <- 23

min_x <- -5
max_x <- 90

first_year <- 2018
last_year <- 2018

# Laste inn data ----------------------------------------------------------


con <- dbConnect(RMariaDB::MariaDB(),
                 user= id, password= pw,
                 dbname="kommunedata", host="localhost")

#dbWriteTable(con, "data", data, overwrite=TRUE)


dat <- dbReadTable(con, "kommunedata")

#dbListTables(con)

dbDisconnect(con)
rm(con)


# Dele opp ----------------------------------------------------------------

dat <- dat %>%
  filter(!is.na(kn))

kommunedata <- dat %>%
  select(kommune,year,Avhengig = Avh_navn,Uavhengig = Uavh_navn,Sentralitet = fylke,population) %>%
  mutate(Sentralitet = ifelse(Sentralitet == "Finnmark Finnmarku","Mindre sentrale kommuner","Sentrale kommuner")) %>%
  filter(!is.na(Sentralitet)) %>%
  filter(!is.na(Avhengig))


# kommunedata <- dat %>%
#   select(kommune,year,Avhengig = Avh_navn,Uavhengig = Uavh_navn,Sentralitet = sentralitet_19,population) %>%
#   mutate(Sentralitet = ifelse(Sentralitet > 3,"Mindre sentrale kommuner","Sentrale kommuner")) %>%
#   filter(!is.na(Sentralitet)) %>%
#   filter(!is.na(Avhengig))

         

# Vectors -----------------------------------------------------------------

# Define regions vector
regions <- sort(as.vector(unique(kommunedata$Sentralitet)))
# Define countries vector
countries <- sort(as.vector(unique(kommunedata$kommune)))




ui <- dashboardPage(

#1  Header ------------------------------------------------------------------

  dashboardHeader( disable = F,
    
    title = "dashboard",
    titleWidth = 450


  )# Header slutt
,


# Sidebar -----------------------------------------------------------------

  dashboardSidebar(  disable = F,
                     width = 250,

    
# Sidebarmenu -------------------------------------------------------------

        
    sidebarMenu(


# Sidebarmenu 1 Dashboard -------------------------------------------------

      menuItem("Eneboligpris", tabName = "eneboligpris", icon = icon("dashboard")
               )# menuItem slutt
,


  menuItem("Prediksjon", tabName = "pred", icon = icon("dashboard")
)# menuItem slutt



    ) #Menu slutt

    
  ) # Sidebar slutt
,


# 2 Body --------------------------------------------------------------------

  dashboardBody(
    
    tabItems(

# Body  --------------------------------------------------------

      tabItem(tabName = "eneboligpris",
              
              
              fluidPage(
                headerPanel(tittel),
                sidebarPanel(width = 3,
                             selectizeInput("regions", "Select Region", regions, multiple = T,
                                            options = list(placeholder = 'Select regions')),
                             selectizeInput("countries", "Select kommune", countries, multiple = T,
                                            options = list(placeholder = 'Select kommune')),
                             sliderInput("year", "Select Year",
                                         min = first_year, max = last_year, value = first_year, sep = "",
                                         animate = animationOptions(interval = 100)),
                             sliderInput('pop_size', "Population",
                                         min = 500, max = 5000, value = 3000, step = 500, sep = "", ticks = F)
                ),
                mainPanel(
                  ggvisOutput("plot"),
                  uiOutput("plot_ui")
                )
              )   
      ),


# Body Machine learning --------------------------------------------------------

tabItem(tabName = "pred",
        
        
        fluidRow(
          
          
          dataTableOutput('tblMal')
          
        )   
        
        
) # tabItem slutt



        

# Neste side kommer her (lim inn over. -----------------------------------------------

    ) 

    ) #  dashboardBody slutt
    
    
  ) #Body slutt





# 3 Server ------------------------------------------------------------------


server <- function(input, output) { 



# Boblediagram  ---------------------------------------------------


  vis <- reactive({
    
    sub_df <- subset(kommunedata, year == input$year, drop=T)
    sub_df <- subset(sub_df, !is.na(sub_df$Uavhengig))
    sub_df <- subset(sub_df, !is.na(sub_df$Avhengig))
    
    regions <- input$regions
    if(!is.null(regions)){
      sub_df <- subset(sub_df, Sentralitet %in% regions, drop = T)
    }
    
    popsize <- input$pop_size
    
    selected_countries <- input$countries
    
    sub_df %>%
      ggvis(~Uavhengig, ~Avhengig, fill = ~Sentralitet,
            fillOpacity := 0.5, fillOpacity.hover := 1,
            stroke := NA, stroke.hover = ~Sentralitet, strokeWidth := 4, strokeOpacity := 0.5) %>%
      layer_text(text := ~kommune, data = subset(sub_df, kommune %in% selected_countries),
                 fontSize := 15) %>%
      
      set_options(width = 1000, height = 600, renderer = "svg") %>%
      
      add_axis("x", title = Uavh, title_offset = 50) %>%
      add_axis("y", title = Avh, title_offset = 50) %>%
      scale_numeric("x", domain = c(min_x, max_x), nice = FALSE) %>%
      scale_numeric("y", domain = c(min_y, max_y), nice = FALSE) %>%
      scale_numeric("size", range = c(10, popsize), nice = FALSE) %>%
      
      layer_points(size = ~population, key := ~kommune) %>%
      hide_legend("size") %>%
      set_options(duration = 0) %>%
      
      add_tooltip(function(data){
        paste0("Kommune: <b>", as.character(data$kommune), "</b><br>",
               "Sentralitet: <b>", as.character(data$Sentralitet), "</b><br>",
               "Population: <b>", prettyNum(data$"Population", big.mark=",", scientific=FALSE), "</b><br>",
               Uavh,": <b>", as.character(round(data$Uavhengig, 2)), "</b><br>",
               Avh,": <b>",as.character(round(data$Avhengig, 2)), "</b>")
      }, "hover")
  })
  
  vis %>%
    bind_shiny("plot", "plot_ui")
    


# Machine learning --------------------------------------------------------

  # Data output
  output$tblMal = DT::renderDataTable({
    DT::datatable(korrt, options = list(lengthChange = FALSE))
  })
  
  

  
  } # Server slutt


# App ---------------------------------------------------------------------


shinyApp(ui, server)