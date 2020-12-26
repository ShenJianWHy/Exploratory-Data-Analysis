#' csv_upload UI Function
#'
#' @description upload only csv file, it's different from mod_upload_file_ui
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_csv_upload_ui <- function(id, label = "CSV file"){
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), label),
    checkboxInput(ns("isexcel"), "excel file", FALSE),
    checkboxInput(ns("heading"), "Has heading", TRUE),
    radioButtons(ns('sep'),'Separator', c(Comma=',',Semicolon=';',Tab='\t'), ','),
    selectInput(ns("quote"), "Quote", c(
      "None" = "",
      "Double quote" = "\"",
      "Single quote" = "'"
    ))
  )
}

#' csv_upload Server Function
#'
#' @noRd
#'
#'
mod_csv_upload_server <- function(id, stringsAsFactors = FALSE){
  moduleServer(
    id,

    function(input, output, session) {
      ns <- session$ns

      userFile <- reactive({
        # If no file is selected, don't do anything
        validate(need(input$file, message = FALSE))
        input$file
      })
      dataframe <- reactive({
        # readr::read_csv(
        #   userFile()$datapath
        #   # header = input$heading,
        #   # sep = input$sep,
        #   # quote = input$quote,
        #   # stringsAsFactors = stringsAsFactors
        # )
        if (input$isexcel) {
          readxl::read_excel(userFile()$datapath)
        } else {
          readr::read_csv(
            userFile()$datapath
            # header = input$heading,
            # sep = input$sep,
            # quote = input$quote,
            # stringsAsFactors = stringsAsFactors
          )
        }
      })
      observe({
        msg <- sprintf("File %s was uploaded", userFile()$name)
        cat(msg, "\n")
      })

      return(dataframe)
    }
  )

}

