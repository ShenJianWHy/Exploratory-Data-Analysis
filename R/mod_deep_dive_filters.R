# request: change modular's name for not only using in deep dive section
mod_addFilterInputs4DF_ui <- function(id, elsewhere = FALSE){
  ns <- NS(id)
  tagList(
    # this is mainly for the segement analysis/groupby
    mod_updateSelectInput_ui(
      ns("filterLS"), 
      label = "Select variables used for more granularity",
      multiple = TRUE
    ),
    if(!elsewhere) {
      uiOutput(ns("ui"))
    }
  )
}

mod_addFilterInputs4DF_putui_elsewhere_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("ui"))
  )
}

mod_addFilterSelectBoxeS_server <- function(id, rval_df, rval_df_updateselectbox) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      mod_updateSelectInput_server("filterLS", rval_df_updateselectbox)
      
      inputFunc <- function(inputId, type) {
        if(type == "Date") {
          tagList(
            h4("Date range"),
            dateRangeInput(
              ns(inputId),
              label = NULL,
              start = Sys.Date() - 20*365, end = Sys.Date() + 2 
              # TODO #### will have bugs if according to filter criteria of start and end date, the df returned is 0 length.
              # make constraints for start date and end date so that the return is normal
            )
          )
        } else if(type == "numeric" | type == "integer") {
          sliderInput(ns(inputId), "Range:",
                      min = rval_df()[[inputId]] %>% min(na.rm = TRUE), 
                      max = rval_df()[[inputId]] %>% max(na.rm = TRUE),
                      value = c(min, max))
        } else {
          selectInput(inputId = ns(inputId), 
                      label = paste0(inputId),
                      choices = rval_df()[[inputId]] %>% unique() %>% sort(),
                      multiple = T
          )
        }
      }
      output$ui <- renderUI({
        purrr::map2(input[[NS("filterLS", "inputid")]],  #inputid is id from mod_updateSelectInput_ui
                    lapply(input[[NS("filterLS", "inputid")]], function(var) {
                      class(rval_df()[[var]])
                    }),
                    inputFunc
        )
      })
    }
  )
}

mod_columnFilter_server <- function(rval_df, colName, choice_filter) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
    }
  )
}



mod_addFilterRvfilters_server <- function(id, rval_df){
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      rv <- reactiveValues()
      # input[[.x]] using the name space of session$ns
      observe({
        purrr::map(input[[NS("filterLS", "inputid")]],
                   function(.x){
                     rv[[.x]] <- input[[.x]]
                   }
        )
        # print("debug for mod_addFilterInputs4DF_server")
        # print(str(reactiveValuesToList(rv)))
      })
      return(rv)
    }
  )
}


mod_addFilterInputs4DF_server <- function(id, rval_df){
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      inputFunc <- function(inputId, type) {
        if(type == "Date") {
          tagList(
            h4("Date range"),
            dateRangeInput(
              ns(inputId),
              label = NULL,
              start = Sys.Date() - 20*365, end = Sys.Date() + 2 
              # TODO #### will have bugs if according to filter criteria of start and end date, the df returned is 0 length.
              # make constraints for start date and end date so that the return is normal
            )
          )
        } else if(type == "numeric" | type == "integer") {
          sliderInput(ns(inputId), "Range:",
                      min = rval_df()[[inputId]] %>% min(na.rm = TRUE), 
                      max = rval_df()[[inputId]] %>% max(na.rm = TRUE),
                      value = c(min, max))
        } else {
          selectInput(inputId = ns(inputId), 
                      label = paste0(inputId),
                      choices = rval_df()[[inputId]] %>% unique() %>% sort(),
                      multiple = T
          )
        }
      }
      rv <- reactiveValues()
      mod_updateSelectInput_server("filterLS", rval_df = rval_df)
      output$ui <- renderUI({
        purrr::map2(input[[NS("filterLS", "inputid")]],  #inputid is id from mod_updateSelectInput_ui
                    lapply(input[[NS("filterLS", "inputid")]], function(var) {
                      class(rval_df()[[var]])
                    }),
                    inputFunc
        )
      })
      # input[[.x]] using the name space of session$ns
      observe({
        purrr::map(input[[NS("filterLS", "inputid")]],
                   function(.x){
                     rv[[.x]] <- input[[.x]]
                   }
        )
        # print("debug for mod_addFilterInputs4DF_server")
        # print(str(reactiveValuesToList(rv)))
        
      })
      return(rv)
    }
  )
}


columnFilterUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("filter_container"))
}

columnFilter <- function(input, output, session, df, colName, choice_filter) { # df:rval
  

  output$filter_container <- renderUI({
    freezeReactiveValue(input, "filter_value")
    
    cls <- class(df()[[colName]])
    if (cls == "Date") {
      dateRangeInput(
        session$ns("filter_value"),
        label = colName,
        start = df()[[colName]] %>% min(na.rm = TRUE),
        end   = df()[[colName]] %>% max(na.rm = TRUE),
        min   = df()[[colName]] %>% min(na.rm = TRUE),
        max   = df()[[colName]] %>% max(na.rm = TRUE)
      )
    } else if (cls == "numeric") {
      sliderInput(session$ns("filter_value"), colName,
                  min   = df()[[colName]] %>% min(na.rm = TRUE),
                  max   = df()[[colName]] %>% max(na.rm = TRUE),
                  value = c(min, max))
    } else {
      selectInput(session$ns("filter_value"), colName,
                  choices = sort(unique(df()[,colName,drop=TRUE])),
                  multiple = TRUE)
    }
  })
  observeEvent(choice_filter(), {
    cls <- class(df()[[colName]])
    current_values <- input$filter_value
    surviveItems <- df()[choice_filter(),colName,drop=TRUE]
    if (cls == "Date") {
      updateDateRangeInput(
        session, "filter_value",
        start = surviveItems %>% min(na.rm = TRUE),
        end   = surviveItems %>% max(na.rm = TRUE)
      )  
    } else if (cls == "numeric") {
      updateSliderInput(
        session, "filter_value",
        min = surviveItems %>% min(na.rm = TRUE),
        max = surviveItems %>% max(na.rm = TRUE),
        value = c(min, max)
      )
    } else {
      updateSelectInput(
        session, "filter_value",
        choices = sort(unique(c(current_values, surviveItems))),
        selected = current_values
      )
    }

  })

  reactive({
    cls <- class(df()[[colName]])
    if (!isTruthy(input$filter_value)) {
      TRUE
    } else if (cls == "Date") {# TODO potential bug: cause it now compares the strings when it's date type
      df()[,colName,drop=TRUE] < input$filter_value[2] & df()[,colName,drop=TRUE] > input$filter_value[1]
    } else if (cls == "numeric") {
      df()[,colName,drop=TRUE] < input$filter_value[2] & df()[,colName,drop=TRUE] > input$filter_value[1]
    }else {
      df()[,colName,drop=TRUE] %in% input$filter_value # TODO 3 input data type
    }
  })
}



columnFilterSetUI <- function(id, maxcol, colwidth) {
  ns <- NS(id)
  fluidRow(
    lapply(1:maxcol, function(i) {
      column(colwidth,
             columnFilterUI(ns(paste0("col", i)))
      )
    })
  )
}