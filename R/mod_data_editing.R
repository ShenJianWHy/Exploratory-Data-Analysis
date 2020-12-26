# this new field could be any KPI you want to play with in deep dive section
mod_add_field_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Name"),
    textInput(ns("fieldName"), NULL, value = "", width = NULL,
              placeholder = "Field Name"),
    h4("Definition"),
    textInput(ns("def"), NULL, value = "", width = NULL,
              placeholder = "tidy eval: ex. col1/col2 - col3"),
    br(),
    fluidRow(
      column(
        6, align = "left",
        actionButton(
          inputId = ns("validate"),
          label = "Validate",
        )),
      
      column(
        6,align = "right",
        actionButton(
          inputId = ns("save"),
          label = "Save",
        ))
    ),
    hr(),
    h4("Delete Field"),
    selectInput(
      "deleteField",
      NULL,
      choices = "unfinished"
    ),
    
    actionButton(
      inputId = ns("delete"),
      label = "Delete",
    ),
    verbatimTextOutput(ns("validRes"))
  )
}

# 
# mod_add_field_server <- function(id, rvalDF){
#   moduleServer(
#     id,
#     
#     function(input, output, session) {
#       ns <- session$ns
#       
#       observeEvent(input$validate, {
#         output$validRes <- renderPrint({
#           rvalDF() %>% head(10) %>% 
#             mutate(!!input$fieldName := !!parse_quo(input$def, env = caller_env()))
#         }) 
#       })
#       
#       observeEvent(input$save, {
#         rvalDF <- reactive({rvalDF() %>% head(10) %>% 
#             mutate(!!input$fieldName := !!parse_quo(input$def, env = caller_env()))})
#       })
#     }
#   )
# }


mod_add_field_server <- function(id, rv){
  moduleServer(
    id,
    
    function(input, output, session) {
      ns <- session$ns
      
      observeEvent(input$validate, {
        output$validRes <- renderPrint({
          rvalDF() %>% head(10) %>% 
            mutate(!!input$fieldName := !!parse_quo(input$def, env = caller_env()))
        }) 
      })
      
      observeEvent(input$save, {
        rvalDF <- reactive({rvalDF() %>% head(10) %>% 
            mutate(!!input$fieldName := !!parse_quo(input$def, env = caller_env()))})
      })
    }
  )
}