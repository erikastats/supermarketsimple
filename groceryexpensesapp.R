library(shiny)
library(tidyverse)
library(shinyWidgets)



# Data --------------------------------------------------------------------

source("join_productlist.R")

# Ui ----------------------------------------------------------------------

ui <-  fluidPage(
  titlePanel("Supermarket expenses "),
  sidebarLayout(
    sidebarPanel(
      
      # Supermarket -------------------------------------------------------------
      radioGroupButtons(
        inputId = "supermarket", 
        label = "Choose supermarket",
        choices = c("Tesco", 
                    "Lidl", "Audi", "SuperValu"),
        justified = TRUE,
        checkIcon = list(
          yes = icon("ok", 
                     lib = "glyphicon")) 
      ),
      
      # Date --------------------------------------------------------------------
      dateInput("date", label = h3("Choose date"), value = Sys.Date()),

      # Products ----------------------------------------------------------------
      pickerInput(
        inputId = "products_filter",
        label = "Choose Product", 
        choices = product_names$Products
      ),

      # Price product -----------------------------------------------------------
      numericInput("price", label = h3("Price"), value = 0),

      # Quantity ----------------------------------------------------------------
      numericInput("quantity", label = h3("Quantity"), value = 1),

      # Discount ----------------------------------------------------------------
      numericInput("discount", label = h3("Discount"), value = 0),

      # Action button -----------------------------------------------------------
      actionButton("add_button", "Add product"),

      # Press enter do add ------------------------------------------------------
      # tags$script(HTML("
      #   $(document).on('keydown', '#product_name', function(e) {
      #     if(e.keyCode == 13) { // 13 is the Enter key
      #       $('#add_button').click();
      #     }
      #   });
      # "))

      # Exclude item ------------------------------------------------------------
      uiOutput("excluded"),
      actionButton("exclude_button", "Exclude"),

      # Save table --------------------------------------------------------------
      downloadButton("saveRDS", "Save table as RDS")
      
    ),
    mainPanel(
      DT::dataTableOutput('grocery_table')
    )
  )
)



# server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  grocery <- reactiveVal(data.frame(Supermarket = character(),
                                    Date = character(),
                                    Product = character(),
                                    Price = double(),
                                    Quantity = integer(),
                                    Discount = double(),
                                    Total = double(),
                                    id_groc = character())
                         )
  
  observeEvent(input$add_button, {
    # if (nchar(input$product_))
    current_grocery <- grocery()
    new_product <- data.frame(Supermarket = input$supermarket,
                              Date = input$date |> as.character(),
                              Product = input$products_filter,
                              Price = input$price,
                              Quantity = input$quantity,
                              Discount = input$discount) |>
      mutate(Total = (Price - Discount)*Quantity,
             id_groc = paste(Supermarket |> str_to_lower(),
                             Date,
                             Product |> str_to_lower(),
                             Price |> as.character(),
                             Quantity |> as.character(),
                             Discount |> as.character(),
                             sep = "_"
               
             ))
    
    updated_products <- current_grocery |>
      bind_rows(new_product)
    grocery(updated_products)
    
    updatePickerInput(session, "products_filter")
    updateNumericInput(session, "price", value = 0)
    updateNumericInput(session, "quantity", value = 1)
    updateNumericInput(session, "discount", value = 0)
    
  })
  
  output$grocery_table <- DT::renderDataTable({
    grocery()
  }, options = list(pageLength = 15))
  
  output$excluded <- renderUI({
    
    pickerInput(
      inputId = "excluded_filter",
      label = "Choose products that should be excluded", 
      choices = grocery()$id_groc
    )
  })
  
  observeEvent(input$exclude_button, {
    
    # Get current list of products
    updated_products <- grocery() |>
      filter(!(id_groc %in% input$excluded_filter))
    
    # Update the reactive value with new list
    grocery(updated_products)
    
  })
  
  output$saveRDS <- downloadHandler(
    filename = function(){
      paste0(filename = "grocery_", Sys.Date(), ".rds")
    },
    content = function(file){
      saveRDS(grocery(), file)
    }
  )
}


# app ---------------------------------------------------------------------

shinyApp(ui =ui, server = server)
