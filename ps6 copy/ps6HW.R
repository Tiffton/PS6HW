
library(shiny)
library(tidyverse)
covidVax <- read_delim("data/covid-data.csv")


ui <- fluidPage(
  titlePanel(title = h4("Covid Vaccination Data In The US", align = "center")),
  
  mainPanel(
    tabsetPanel(type = "tab",
                tabPanel("Introduction",
                         fluidRow(column(12,
                                         h1("Introducing The Data"),
                                         p("This app is using data from the",
                                            em("World Health Organization"),
                                                 "it contains information about the Covid Vaccination status on completion in 
                                                 different States and ages. In addition to some other factors."),
                                         br(),
                                         p("This data set is quite large, it has 3283 observations and 57 variables."),
                                         strong("Here is a sample of the data:"),
                                         br(),
                                         br(),
                                         tableOutput("sample")
                         )
                         
                         
                         )),
                tabPanel("Plot", 
                         fluidRow(sidebarPanel(width = 5,
                                               h4("Filter Through The Data"),
                                               helpText("Choose what you would like to include in the bar chart"),
                                               radioButtons("palette", "Choose a color theme",
                                                            choices = c("Palette 1", "Palette 2"),
                                                            selected = c("Palette 1")),
                                               helpText("Note to TA: Palette interacts with graph, we can see that as the fill label changes, (from red to blue) however the graph 
                                                        will not change colors. Even when inputing only a specific color (i.e. fill =red), it will not change and stays
                                                        in its default color. Another TA took a look and said that Shiny might not be able to handle the workload, that this interaction
                                                        is Shiny App's fault"),
                                               
                                               uiOutput("checkboxes")
                                               
                         ),
                         column(7, 
                                plotOutput("Plot", width = "190%"),
                                br(),
                                br(),
                                textOutput("plot_text")
                               
                         ),
                       
                         
                         )),
                
                
                tabPanel("Table",
                         fluidRow(sidebarPanel(width = 5,
                                               h4("Look Through The Age Groups"),
                                               helpText("This panel displays the number of individuals who completed their covid series by age in each County, State"),
                                               radioButtons("age_group", "Age Group",
                                                           choices = c("5Plus", "5-17", "12Plus", "18Plus", "65Plus"),
                                                           selected = c("5Plus"))
                                               ),
                                  strong(textOutput("age_text")),
                                  br(),
                           column(7, 
                                  tableOutput("dataTable")
                                  )       
                                  ))
    )
  )
)


#Intro/showing sample of data
server <- function(input, output) {
  output$sample <- renderTable({
    covidVax %>% 
      head(10)
    
  })
  
  #-----------------------------------------  
  #Plot tab interactive for States
  
  data <- reactive({
    covidVax %>% 
      group_by(Recip_State) %>% 
      select(Recip_State, Series_Complete_Yes, Series_Complete_Pop_Pct) %>% 
      filter(!is.na(Recip_State), !is.na(Series_Complete_Yes), !is.na(Series_Complete_Pop_Pct)) %>% 
      summarise(total = sum(Series_Complete_Yes)) %>% 
      filter(Recip_State %in% input$States)
  })
  
  output$checkboxes <- renderUI({
    checkboxGroupInput(
      "States", "Select States:",
      choices = unique(covidVax$Recip_State),
      select = unique(covidVax$Recip_State)
    )
  })
  
  colors <- reactive({
    if(input$palette == c("Palette 1")) {
      ("red")
    } else{
      ("blue")
    }
  })
  #-------------------------------------
  output$Plot <- renderPlot({
    data() %>%
     ggplot(aes(Recip_State, total, fill=colors())) +
      geom_col() +
      labs(title = "Total of Covid Doses Completed by State", 
           x ="State", 
           y="Number of People who Completed the Series",
           fill = "color" )
           
     
  })
  
  output$plot_text <- renderText({
    paste("You are looking at:", input$States)
  })
 
 

#------------------------------------------------------------------
  #For Table Tab
  
  
  table_data <- reactive({
   if(input$age_group %in% "5Plus") {
     covidVax %>% 
       filter(!is.na(Recip_State)) %>% 
       group_by(Recip_State) %>% 
       select(Recip_County, Series_Complete_5Plus) %>% 
       filter(!is.na(Series_Complete_5Plus)) %>% 
      arrange(rank(Recip_State))
   } else 
      if (input$age_group %in% "5-17"){
       covidVax %>% 
         filter(!is.na(Recip_State)) %>% 
         group_by(Recip_State) %>% 
         select(Recip_County, Series_Complete_5to17) %>% 
         filter(!is.na(Series_Complete_5to17)) %>% 
         arrange(rank(Recip_State))
     }
     else
      if(input$age_group %in% "12Plus"){
       covidVax %>% 
         filter(!is.na(Recip_State)) %>% 
         group_by(Recip_State) %>% 
         select(Recip_County, Series_Complete_12Plus) %>% 
         filter(!is.na(Series_Complete_12Plus)) %>% 
         arrange(rank(Recip_State))
     }
      else
        if(input$age_group %in% "18Plus"){
          covidVax %>% 
            filter(!is.na(Recip_State)) %>% 
            group_by(Recip_State) %>% 
            select(Recip_County, Series_Complete_18Plus) %>% 
            filter(!is.na(Series_Complete_18Plus)) %>% 
            arrange(rank(Recip_State))
    }
      else
        if(input$age_group %in% "65Plus"){
          covidVax %>% 
            filter(!is.na(Recip_State)) %>% 
            group_by(Recip_State) %>% 
            select(Recip_County, Series_Complete_65Plus) %>% 
            filter(!is.na(Series_Complete_65Plus)) %>% 
            arrange(rank(Recip_State))
     }
  })
  
  output$dataTable <- renderTable({
    table_data()
    
 })
  
 output$age_text <- renderText({
  paste("You have selected:", input$age_group )
 })
  
}

shinyApp(ui = ui, server = server)
