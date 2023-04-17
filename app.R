library(shiny)
library(ggplot2)
library(DT)
library(plotly)

# Load the datasets from CSV files
training <- read.csv("train.csv")
harvest <- read.csv("harvest.csv")
packaging <- read.csv("package.csv")
research <- read.csv("market.csv")

# Define the UI
ui <- fluidPage(
  titlePanel("Harvest Management Dashboard"),
  
  navbarPage('Percpectives',
    tabPanel(
      "Innovation and Learning",
     
        plotOutput("visualization1"),
        DT::dataTableOutput("details_table")
      
    ),
    tabPanel(
      "Internal Business Process",
        plotOutput("lineChart"),
        DT::dataTableOutput("kp2details")
      
    ),
    tabPanel(
      "Customer",
        plotOutput("visualization5"),
        DT::dataTableOutput("packagingdetails")
      
    ),
    tabPanel(
      "Financial Perspective",
        plotOutput("vis6"),
        DT::dataTableOutput("financedetails")
      
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Helper function to calculate trained proportion
  trained_proportion <- reactive({
    total <- nrow(training)
    completed <- sum(training$training_status == "Completed")
    incomplete <- total - completed
    proportions <- c(completed, incomplete)
    proportions <- proportions/total
    return(proportions)
  })
  
  # Render the pie chart for employee training
  output$visualization1 <- renderPlot({
    proportions <- trained_proportion()
    labels <- c("Completed", "Incomplete")
    colors <- c("green", "red")
    
    # Create pie chart using ggplot2
    ggplot(data.frame(labels, proportions), aes(x = "", y = proportions, fill = labels)) +
      geom_col(width = 1, color = "white") +
      coord_polar("y", start = 0) +
      theme_void() +
      scale_fill_manual(values = colors) +
      labs(title = "Proportion of Employees Trained vs Not Trained", fill = "Training Status") +
      guides(fill = guide_legend(title = "Training Status")) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  # Render the details table for the dataset
  output$details_table <- DT::renderDataTable({
      DT::datatable(training, options = list(pageLength = 5),
                    rownames = FALSE,
                    caption = paste("Innovation and Learning: Details"))
    
  })

  #Render a line chart for harvest methods
  output$lineChart <- renderPlot({
    ggplot(data = harvest, aes(x = "harvest id", y = "total yield", color = "crop variety")) +
      geom_line() +
      geom_point() +
      labs(title = "Total Yield vs Harvest Methods", x = "Harvest ID", y = "Total Yield") +
      theme_minimal()
  })
  
  
  # Render the details table for the dataset
  output$kp2details <- DT::renderDataTable({
    DT::datatable(harvest, options = list(pageLength = 5),
                  rownames = FALSE,
                  caption = paste("Harvest Method Details"))
    
  })

  #Render visualization for product packaging
  output$visualization5 <- renderPlot({
    ggplot(package, aes(x = "package test", y = duration, fill = "package test")) +
      geom_bar(stat = "identity") +
      labs(title = "Shelf-Life of Products in our Store",
           x = "Package",
           y = "Duration (days)") +
      scale_fill_discrete(name = "Package")
  })

  
  # Render the details table for the customer dataset
  output$packagingdetails <- DT::renderDataTable({
    DT::datatable(package, options = list(pageLength = 5),
                  rownames = FALSE,
                  caption = paste("Shelf life of Stored Items"))
  
  })
  
# rendering visualization for financial percpective
  output$vis6 <- renderPlot({
    # Create a bar chart
    ggplot(market, aes(x = "product id", y = "revenue after", fill = "revenue before")) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_bar(aes(y = "revenue after", fill = "revenue after"), stat = "identity", position = "dodge") +
      labs(title = "Revenue Before and After Conducting Market Research", x = "Product ID", y = "Revenue") +
      scale_fill_manual(values = c("revenue before" = "lightblue", "revenue after" = "lightgreen")) +
      theme_minimal()
  })
  # Render the details table for the financial dataset
  output$financedetails <- DT::renderDataTable({
    DT::datatable(market, options = list(pageLength = 5),
                  rownames = FALSE,
                  caption = paste("Market Research vs Revenue Effects"))
    
  })
}
# Run the Shiny app
shinyApp(ui = ui, server = server)
