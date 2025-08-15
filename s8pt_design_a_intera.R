# Load required libraries
library(ethereum)
library(reactable)
library(leaflet)
library(visNetwork)

# Define blockchain API connection
api_url <- "https://mainnet.infura.io/v3/YOUR_PROJECT_ID"

# Define blockchain data retrieval function
get_blockchain_data <- function() {
  # Use ethereum package to retrieve recent blocks
  blocks <- get_block_range(api_url, from = "latest", to = "latest", full = TRUE)
  
  # Extract relevant data from blocks
  transactions <- lapply(blocks, function(x) x$transactions)
  transaction_data <- do.call(rbind, transactions)
  
  # Return transaction data
  transaction_data
}

# Define interactive dashboard UI
ui <- fluidPage(
  titlePanel("Blockchain dApp Analyzer"),
  sidebarLayout(
    sidebarPanel(
      actionButton("refresh", "Refresh Data"),
      selectInput("filter_type", "Filter by:", c("Transaction Type", "Sender", "Receiver")),
      textInput("filter_value", "Filter value:")
    ),
    mainPanel(
      reactableOutput("transactions_table"),
      leafletOutput("transactions_map")
    )
  )
)

# Define interactive dashboard server
server <- function(input, output) {
  # Get blockchain data on startup or when refresh button is clicked
  data <- eventReactive(input$refresh, {
    get_blockchain_data()
  })
  
  # Filter data based on user input
  filtered_data <- reactive({
    data <- data()
    if (input$filter_type == "Transaction Type") {
      data[data$transaction_type == input$filter_value, ]
    } else if (input$filter_type == "Sender") {
      data[data$sender == input$filter_value, ]
    } else if (input$filter_type == "Receiver") {
      data[data$receiver == input$filter_value, ]
    } else {
      data
    }
  })
  
  # Create interactive table
  output$transactions_table <- renderReactable({
    reactable(filtered_data(), columns = list(
      transaction_id = colDef(format = colFormatText()),
      transaction_type = colDef(format = colFormatText()),
      sender = colDef(format = colFormatText()),
      receiver = colDef(format = colFormatText()),
      value = colDef(format = colFormatCurrency())
    ))
  })
  
  # Create interactive map
  output$transactions_map <- renderLeaflet({
   leaflet() %>%
      addProviderTiles("OpenStreetMap") %>%
      setView(lng = 0, lat = 0, zoom_start = 2) %>%
      addMarkers(lat = filtered_data()$lat, lng = filtered_data()$lon, popup = ~filtered_data()$transaction_id)
  })
}

# Run interactive dashboard
shinyApp(ui = ui, server = server)