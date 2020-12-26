#' global_vars UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' 
#' For further dev, you may replace selectinput with selectizeInput
mod_global_vars_ui <- function(id){
  ns <- NS(id)
  tagList(
    pickerInput(ns("target"), "Set your outcome/target", c("")),
    pickerInput(ns("features"), "Set your features", c(""), multiple = T,
                options = list(`actions-box` = TRUE)),
    pickerInput(ns("exposure"), "Optional: Set your exposures", c(""), multiple = T,
                options = list(`actions-box` = TRUE)),
    pickerInput(ns("date"), "Optional: Set your date", c(""))
 
  )
}
    
#' global_vars Server Function
#'
#' @noRd 
mod_global_vars_server <- function(id, rval_df){
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      ls <- reactive(colnames(rval_df()))
      type <- reactive(sapply(rval_df(), class))
      date_ls <- reactive(ls()[type() == "Date"])
      other_ls <- reactive(ls()[!type() == "Date"])
      
      observe({
        updatePickerInput(session, "target", choices = c(other_ls()))
        updatePickerInput(session, "features", choices = c(ls()))
        updatePickerInput(session, "exposure", choices = c(other_ls()))
        updatePickerInput(session, "date", choices = c(date_ls()))
      })

    }
  )
  
}


#' global_vars Server Function
#'
#' @noRd 
mod_return_global_vars_server <- function(id){
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      return(
        list(
        target = reactive({input$target }),
        features = reactive({input$features}),
        exposure = reactive({input$exposure }),
        date = reactive({input$date })
        )
      )
    }
  )
  
}


# module infocards --------------------------------------------------------

mod_global_vars_info_ui <- function(id, vec){
  ns <- NS(id)
  tagList(
    lapply(vec, function(x) {
      uiOutput(ns(x))
    })
    # uiOutput(ns("info"))
  )
}

mod_global_vars_info_server <- function(id, rval_ls, vec, icon){
  moduleServer(
    id,
    
    function(input, output, session) {
      ns <- session$ns
      
      
      mapply(vec, icon, FUN = function(x, y){
        output[[x]] <- renderUI({
          
          num <- length(rval_ls[[x]]())
          varls <- rval_ls[[x]]()

          tablerInfoCard(
            width = 10,
            value = paste(num, x),
            status = "success",
            icon = y,
            description = paste(varls, collapse=", ")
          )
          
        })
      })
      

    }
  )
  
}