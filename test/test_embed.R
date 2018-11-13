library(plotly)
library(dplyr)
library(ggplot2)
library(shiny)

d <- data.frame(Group = rep(c("a","b"),each = 20),
                Value = rnorm(40),
                Location = rep(c("USA","EU"),each = 10),
                Time = rep(1:10, 4))


ui <- fluidPage(
  tags$head(
    tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                type="text/javascript")
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      #--------------------------------------------------------
      
      selectInput("Var1",
                  label = "Variable", #DATA CHOICE 1
                  selected = "a",
                  choices = c(as.character(unique(d$Group)))),
      
      selectInput("Loc1",
                  label = "Select location", #Location filter 1
                  selected = "USA",
                  choices = c(as.character(unique(d$Location)))),
      
      #--------------------------------------------------------
      
      selectInput("Var2",
                  label = "Variable2", #DATA CHOICE 2
                  selected = "",
                  choices = c("",as.character(unique(d$Group)))),
      
      selectInput("Loc2",
                  label = "Select location", #Location filter 2
                  selected = "",
                  choices = c("",as.character(unique(d$Location))))
    ),
    
    #--------------------------------------------------------
    # Show a plot of the generated distribution
    
    
    mainPanel(
      plotlyOutput('plot') #Draw figure
    ),
    position = "left"),
  HTML('<div data-iframe-height></div>')
  
)



server <- function(input, output) {
  
  out <- reactive({
    
    d %>% filter(Group == input$Var1, Location == input$Loc1)
    
  })
  
  out2 <- reactive({
    
    if (input$Var2 =="")
      return(NULL)
    
    d %>% filter(Group == input$Var2, Location == input$Loc2)
    
  })
  
  output$plot <- renderPlotly({
    p <- ggplot() +
      geom_line(data = out(), aes(x = Time, y = Value))+ #Add both data sets in one ggplot
      {if (!is.null(out2())) geom_line(data = out2(), aes(x = Time, y = Value), color = "red")}
    theme_bw()
    ggplotly(p)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
