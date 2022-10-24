

```{r}
library(shiny)


server<- shinyServer(function(input, output) {
  
  br <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/brazil_loss.csv')
  
  value <- reactive({
    br <- br %>% select(year, input$var)
    br[br$year %in% seq(from = min(input$range), to = max(input$range), by = 1), ]   
  })
  
  data <- reactive({
    x <- br %>% select(year, input$var)
    x <- x[x$year %in% seq(from = min(input$range), to = max(input$range), by = 1), ]   
    sum(x[,2])
    
  })
  
  output$plot2 <- renderPlot({
    value() %>% ggplot(aes(.[[1]],.[[2]])) + 
      geom_point(size=3, color="#69b3a2") + 
      geom_area(fill="#69b3a2", alpha=0.4) +
      geom_line(color="#69b3a2", size=2) +
      labs(x="Time period", y = "Forest loss (in Ha)") +
      theme(axis.text=element_text(size=12), axis.title=element_text(size=14)) +
      ggtitle("Loss by period")
    
  })
  
  output$range <- renderText({
    data()
  }) 
})

ui<-shinyUI(fluidPage(
  
  titlePanel("Brazil forest lost"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("range",
                  label = "Range date of interest:",
                  min = 2001,
                  max = 2013,
                  value = c(2005, 2008)),
      
      selectInput("var",
                  label = "Choose a variable",
                  choices = list("Commercial crops" = "commercial_crops",
                                 "Natural disturbances" = "natural_disturbances",
                                 "Pasture for livestock" = "pasture",
                                 "Logging for lumber" = "selective_logging",
                                 "Fire loss" = "fire"),
                  selected = "Commercial crops"),
      
      a(href = "https://rpubs.com/jepg24/751831", "Documentation")
      
    ),
    
    mainPanel(
      h4("Interactive plot"),
      plotOutput("plot2"),
      h4("Total Forest loss (in Ha)"),
      h1(textOutput("range"))
    )
  )
))
# Create Shiny app ----
shinyApp(ui = ui, server = server)
```

