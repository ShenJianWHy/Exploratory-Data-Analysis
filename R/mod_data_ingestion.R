#' data_ingestion UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' 
#' @noRd 
#'
#' @importFrom shiny NS tagList \
#' @export
mod_data_ingestion_ui <- function(id){
  ns <- NS(id)
  tagList(
    checkboxInput(ns("option"), "Directly upload final data", FALSE),
    uiOutput(ns("datInges")),
    mod_datatable_ui(ns("table")),
  )
}
    
#' data_ingestion Server Function
#'
#' @noRd 
mod_data_ingestion_server <- function(id){
  moduleServer(
    id,
    
    function(input, output, session) {
      ns <- session$ns
      
      output$datInges <- renderUI({
        if(input$option) {
          tagList(
            h2("Content1"),
            mod_csv_upload_ui(ns("csvFile"),
                              label = "Plase select final data csv")
          )

        } else {
          "If FALSE: Content2"
        }
      })
      
      # Show the first 100 lines of uploaded data
      df <- mod_csv_upload_server("csvFile", stringsAsFactors = FALSE)
      mod_datatable_server("table", df)
      
      
    }
  )
  
}