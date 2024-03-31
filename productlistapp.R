library(shiny)
library(shinyWidgets)
library(tidyverse)

# source("join_productlist.R")

# data --------------------------------------------------------------------

all_products <- read_rds("all_products.rds") |>
  distinct() |> arrange()

# Define UI
ui <- fluidPage(
  titlePanel("Product List App"),
  sidebarLayout(
    sidebarPanel(
      textInput("product_name", "Enter product name"),
      actionButton("add_button", "Add Product"),
      tags$script(HTML("
        $(document).on('keydown', '#product_name', function(e) {
          if(e.keyCode == 13) { // 13 is the Enter key
            $('#add_button').click();
          }
        });
      "))
    ),
    mainPanel(
      DT::dataTableOutput("products_table"),
      uiOutput("excluded"),
      actionButton("exclude_button", "Exclude"),
      downloadButton("saveRDS", "Save table as RDS")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive value to store the list of products
  products <- reactiveVal(all_products |> arrange())
  
  output$excluded <- renderUI({
    pickerInput(
      inputId = "excluded_filter",
      label = "Choose products that should be excluded", 
      choices = products()$Products |> sort() ,
      options = list(
        `actions-box` = TRUE), 
      multiple = TRUE
    )
  })
  
  observeEvent(input$add_button, {
    if (nchar(input$product_name) > 0){
      # Get current list of products
      current_products <- products() 
      # Append new product
      new_product <- data.frame(Products = input$product_name)
      updated_products <- current_products |>
        bind_rows(new_product)   |> 
        arrange()
      # Update the reactive value with new list
      products(updated_products)
      # Clear the input field after adding
      updateTextInput(session, "product_name", value = "")}
    
    
    
  })
    
  observeEvent(input$exclude_button, {
      
        # Get current list of products
        updated_products <- products() |>
          filter(!(Products %in% input$excluded_filter)) |> arrange()
          
        # Update the reactive value with new list
        products(updated_products)
    
  })
  
  
  # Render the products table
  output$products_table <- DT::renderDataTable({
    products()
  }, options = list(pageLength = 25))
  
  output$saveRDS <- downloadHandler(
    filename = function(){
    paste0(filename = "all_products.rds")
      },
    content = function(file){
      saveRDS(products() |> arrange(), file)
    }
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)
