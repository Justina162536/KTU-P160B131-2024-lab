
library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  titlePanel("Veiklos sritis 461000"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("Kodas", "Pasirinkite įmonę", choices = NULL)
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  data <- readRDS("../data/duomenys.rds")
  
  observe({
    updateSelectizeInput(session, "Kodas", choices = data$name, server = TRUE)
  })
  
  output$plot <- renderPlot({
    
    
    data %>%
      filter(name == input$Kodas) %>% 
      ggplot(aes(x = ym(month), y = avgWage)) +
      geom_point() +
      geom_line()+
      theme_classic() +
      labs(x = "Mėnuo", y = "Vidutinis atlyginimas")
  })
}

shinyApp(ui = ui, server = server)