library(shiny)
library(ggplot2)

# Define UI for application 
ui <- fluidPage(
    titlePanel("Edgeworth Box"),
    sidebarLayout(
        sidebarPanel(
            textInput(inputId = "E_h1", label = "Home Endowment of Good 1", value = 10),
            textInput(inputId = "E_h2", label = "Home Endowment of Good 2", value = 0),
            textInput(inputId = "E_f1", label = "Foreign Endowment of Good 1", value = 0),
            textInput(inputId = "E_f2", label = "Foreign Endowment of Good 2", value = 10),
            sliderInput("alpha", "Home Budget Share of Good 1", min = 0, max = 1, value = 0.6), 
            sliderInput("beta", "Foreign Budget Share of Good 1", min = 0, max = 1, value = 0.6)),
        mainPanel(
            plotOutput("plot", width = '600px', height = '600px'), 
            tableOutput("solution")
            )
        )
)

# Define server logic 
server <- function(input, output) {
    source('EdgeworthBox.R')
    output$solution <- renderTable({
        E_h = c(as.numeric(input$E_h1), as.numeric(input$E_h2))
        E_f = c(as.numeric(input$E_f1), as.numeric(input$E_f2))
        box <- Edgeworth(E_h = E_h, E_f = E_f, alpha = input$alpha, beta = input$beta)
        solution1 <- rbind(E_h, box$Home.Consumption, 
                           E_f, box$Foreign.Consumption, 
                           box$Terms.of.Trade)
        colnames(solution1) <- c('Good 1', 'Good 2')
        rownames(solution1) <- c('Home Endowment', 'Home Consumption', 
                                 'Foreign Endowment', 'Foreign Consumption', 
                                 'Terms of Trade (Prices)')
        solution <- solution1
        }, rownames = TRUE, colnames = TRUE)    
    output$plot <- renderPlot({
        E_h = c(as.numeric(input$E_h1), as.numeric(input$E_h2))
        E_f = c(as.numeric(input$E_f1), as.numeric(input$E_f2))
        box <- Edgeworth(E_h = E_h, E_f = E_f, alpha = input$alpha, beta = input$beta)
        plot(box$p)
    })    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
