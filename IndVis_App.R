library(flexdashboard)
library(d3heatmap)
library(corrplot)
library(htmlwidgets)
library(here)
library(tseries)
library(stringr)
library(dplyr)
library(plotly)
library(heatmaply)
library(shiny)
library(miniUI)

source("load_data-02.R")

# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Tabsets"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      selectInput("Var",
                  label = "Variable",
                  choices = var_ids,
                  selected = var_ids[1],
                  selectize = TRUE,
                  multiple = TRUE),
      
      selectizeInput("EPU",
                     label = "EPU",
                     choices = epu_ids,
                     selected = epu_ids[1],
                     multiple = TRUE),
      selectInput("Var_lag",
                  label = "Lagged Variable",
                  selected = "",
                  choices = c("",as.character(var_ids)),
                  multiple = TRUE),
      selectInput("EPU_lag",
                  label = "Lagged Variable EPU",
                  selected = "",
                  choices = c("",as.character(epu_ids))),
      
      numericInput('lag', 'Series lag',value = 0,
                   min = -3, max = 3, step = 1)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Normalized Series",plotlyOutput("normseries", height  = "100%")),
                  tabPanel("Correlation Matrix",plotlyOutput("heatplot", height  = "100%"))
      )
      
    )
  )
)

# Define server logic 
server <- function(input, output) {
   
  out <- reactive({
    
    soe %>%
      dplyr::filter(Var %in% input$Var) %>% 
      {if (!input$EPU %in% 'all')
        dplyr::filter(., EPU %in% input$EPU) else .} %>% 
      group_by(Var, EPU) %>%
      mutate(count = n()) %>%
      ungroup() %>%
      dplyr::filter(count >= 10,
                    !str_detect(Var,"reference"),
                    !is.na(Value)) %>%
      tidyr::unite(., Var, Var = c("Var","EPU"), sep = " ")

  })
  
  out_lag <- reactive({
    
    if (input$EPU_lag == "" || input$Var_lag == "") {
      return(NULL)
    }
    
    soe %>%
      dplyr::filter(Var %in% input$Var_lag) %>%
      {if (!input$EPU_lag %in% 'all')
        dplyr::filter(., EPU %in% input$EPU_lag) else .} %>%
      group_by(Var, EPU) %>%
      mutate(count = n()) %>%
      do(lag_series(., k = input$lag)) %>%
      ungroup() %>%
      mutate(Var = paste(.$Var,"lag",input$lag)) %>%
      dplyr::filter(count >= 10,
                    !str_detect(Var,"reference")) %>%
      tidyr::unite(., Var, Var = c("Var","EPU"), sep = " ")
    
  })
  
   output$normseries <- renderPlotly({
     
     p <- ggplot() +
       geom_line(data = out(), aes(x = Time, y = Value, color = Var))+ #Add both data sets in one ggplot
       {if (!is.null(out_lag())) geom_line(data = out_lag(), aes(x = Time, y = Value, color = Var))}+
     ylab("Normalized Value")+
      theme_bw() 
     
     ggplotly(p)
     
   })
   
   out_corr <- reactive({

     if (input$EPU_lag == "" || input$Var_lag == "") {
       int_corr <- out()
     } else {
       int_corr <- rbind(out_lag(), out())
     }

     env_wide <- int_corr %>%
       dplyr::select(-count) %>% 
       tidyr::spread(., Var, Value) %>%
       dplyr::select(-Time)
     
     
     env_wide <- env_wide[complete.cases(env_wide),]
     
     #Find correlation matrix and order by magnitude of first principal component
     env_cor <- cor(env_wide)
     env_order <- corrMatOrder(env_cor, order = "FPC")
     env_out <- env_cor[env_order, env_order] #reorder

   })

   output$heatplot <- renderPlotly({

       heatmaply(out_corr(),
                 margins = c(200,0,0,0),
                 scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(trans = "reverse"),
                 dendrogram = 'none',
                 hide_colorbar = TRUE)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

