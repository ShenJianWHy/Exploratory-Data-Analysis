#' data_summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_data_summary_viz_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("visdat"))
  )
}

#' data_summary Server Function
#'
#' @noRd 
#' @param vizDat rval
mod_data_summary_server_viz <- function(id, vizDat){
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      output$visdat <- renderCachedPlot({
        
        threshold <- 20000
        if (nrow(vizDat()) >= threshold) {
          truncDF <- dplyr::sample_n(vizDat(), threshold)
        } else {
          truncDF <- vizDat()
        }
        # only show sample result of the data!
        visdat::vis_dat(truncDF, palette = "cb_safe",
                        warn_large_data = F) +
          theme(axis.text.x = element_text(angle = 45))
      },
      
      cacheKeyExpr= {vizDat()}
      )
    }
  )
  
}


#' data_summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_data_summary_tbl_ui <- function(id){
  ns <- NS(id)
  tagList(
    verbatimTextOutput(ns("tbldat"))
  )
}

#' data_summary Server Function
#'
#' @noRd 
mod_data_summary_server <- function(id, tblDF){
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      output$tbldat <- renderPrint({
        skimr::skim(tblDF())
      })
    }
  )
  
}

#' data_summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_missing_data_explor_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(inputId = ns("missingVar"), "Select a variable to examine",
                choices = ""),
    plotOutput(ns("missingTab"))
  )
}

#' data_summary Server Function
#'
#' @noRd 
mod_missing_data_explor_server <- function(id, rvalDF) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      observe({
        choices <-  get_category_vars_blw20(rvalDF())
        updateSelectInput(session, "missingVar",
                          choices = choices)
      })
      # output$missingTab <- renderPlot({
      #   var <- sym(input$missingVar)
      #   
      #   rvalDF() %>%
      #     naniar::gg_miss_fct(fct = !!var)
      # })
      
    }
  )
  
}

mod_pair_vars_proportionBar_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(inputId = ns("outcomeVar"), "Select Main Interested Variable",
                choices= ""),
    selectInput(inputId = ns("condVar"), "Select Other Variable to Calculate Proportions",
                choices= ""),
    plotOutput(ns("proportionBarplot"))
  )
}

mod_pair_vars_proportionBar_server <- function(id, globalVars, rvalDF) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      # update selectinput part -------------------------------------
      
      choices1 <- reactive({
        choices <- c(globalVars$target(), 
                     globalVars$features(), 
                     globalVars$exposure(), 
                     globalVars$date()
        )
        
        choices[choices != ""]
      })
      observe({
        updateSelectInput(session, "outcomeVar", choices = choices1())
      })
      choices2 <- reactive({
        setdiff(choices1(), input$outcomeVar)
      })
      observe({
        updateSelectInput(session, "condVar", choices = choices2())
      })
      
      # bar plot part -----------------------------------------------------------
      
      output$proportionBarplot <- renderPlot({
        percent_plot(rvalDF(), input$outcomeVar, input$condVar)
      })
    }
  )
}



# One Var analysis --------------------------------------------------------
mod_oneVar_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      dropdown(
        tags$h3("List of Input"),
        selectInput(
          inputId = ns("var"), "Select a Variable",
          choices = ""),
        style = "unite", icon = icon("gear"),
        status = "success", width = "300px",
        tooltip = tooltipOptions(title = "Inputs info"),
        animate = animateOptions(
          enter = animations$fading_entrances$fadeInLeft,
          exit = animations$fading_exits$fadeOutLeft
        )
      ),
      bs4TabSetPanel(
        id = "oneVar",
        side = "left",
        tabStatus = "transparent",
        bs4TabPanel(
          tabName = "Plot",
          plotlyOutput(ns("plot"), width = "800px")
        ),
        # TODO #### It's hard to hide this tabpanel if it's the categorical var
        # bs4TabPanel(
        #   tabName = "Set Bin Breaks",
        #   plotlyOutput(ns("plotBreaks"), width = "800px")
        #   # tablerCard(
        #   #   width = 7,
        #   #   rHandsontableOutput(ns('breaksTbl')),
        #   #   overflow = TRUE
        #   # )
        # ),
        bs4TabPanel(
          tabName = "N.A. Plot",
          plotlyOutput(ns("naPlot"), width = "800px", height = "600px")
        )
      ),
    )
  )
}

mod_oneVar_server <- function(id, rvalDF) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      rv <- reactiveValues(data = NULL)
      observe({
        choices <- names(rvalDF())
        updateSelectInput(
          session, "var",
          choices = choices, 
          selected = "premNet") #request: selected = "premNet" only for testing --------------
      })
      
      # TODO #### if data type is date? integer(numeric type but takes only integer value)
      observe({
        if (is_category_var_incl_int(rvalDF()[[input$var]])| is_date_var(rvalDF()[[input$var]])) {
          output$plot <- renderPlotly({
            rvalDF() %>%
              mutate(!!sym(input$var) := ifelse(
                # TODO #### doing the following operation so many times is a waste
                as.character(!!sym(input$var)) %in% as.character(get_top_n_vars_ls(rvalDF()[[input$var]], n = 5)),
                as.character(!!sym(input$var)),
                "other"
              )) %>%
              count(!!sym(input$var)) %>%
              ggplot(aes(
                x = !!sym(input$var),
                y = n,
                fill = !!sym(input$var)
              )) +
              geom_bar(stat = "identity")
          })
          output$plotBreaks <- renderPlotly({
            p <- ggplot()
            ggplotly(p) %>% 
              layout(
                title = list(text = "No need to set bins  <br> for categorical variable",
                             y = 0.6)
              )
          })
          
        }
        if(is_numeric_var_excl_int(rvalDF()[[input$var]])) {
          output$plot <-  renderPlotly({
            obj <- histogram(rvalDF()[[input$var]])
            output$out_breaks <- renderText({obj$breaks})
            p <- rvalDF() %>%
              ggplot(aes_string(input$var)) +
              geom_histogram(aes(y = ..density..), 
                             color= "black", fill = "white", breaks = obj$breaks) +
              geom_density(fill = "red", alpha = 0.3) +
              ylim(0, 0.5) +   # TODO #### bug: set ylim(0, 0.5) will ignore some points
              scale_x_continuous(breaks = c(ifelse(min(rvalDF()[[input$var]])>0, 0, min(rvalDF()[[input$var]])),
                                            quantile(rvalDF()[[input$var]], probs = 0.25, na.rm = TRUE) %>% as.integer(),
                                            quantile(rvalDF()[[input$var]], probs = 0.5, na.rm = TRUE)%>% as.integer(),
                                            quantile(rvalDF()[[input$var]], probs = 0.75, na.rm = TRUE)%>% as.integer(),
                                            quantile(rvalDF()[[input$var]], probs = 0.9, na.rm = TRUE)%>% as.integer(),
                                            quantile(rvalDF()[[input$var]], probs = 0.95, na.rm = TRUE)%>% as.integer(),
                                            quantile(rvalDF()[[input$var]], probs = 0.99, na.rm = TRUE)%>% as.integer()
                                            ))
            ggplotly(p) %>% 
              layout(
                xaxis = list(range = c(ifelse(min(rvalDF()[[input$var]])>0, 0, min(rvalDF()[[input$var]])),
                                       quantile(rvalDF()[[input$var]], probs = 0.95, na.rm = TRUE))),
                yaxis = list(range = c(0, 0.5))  
              )
            
          })
          output$plotBreaks <- renderPlotly({
            obj <- histogram(rvalDF()[[input$var]])
            output$out_breaks <- renderText({obj$breaks})
            p1 <- rvalDF() %>%
              ggplot(aes_string(input$var)) +
              geom_histogram(aes(y = ..density..), 
                             color= "black", fill = "white", breaks = obj$breaks) +
              ylim(0, 0.5) # todo #### bug: set ylim(0, 0.5) will ignore some points
            p1 <- ggplotly(p1) %>% 
              layout(
                xaxis = list(range = c(ifelse(min(rvalDF()[[input$var]])>0, 0, min(rvalDF()[[input$var]])), 
                                       quantile(rvalDF()[[input$var]], probs = 0.95, na.rm = TRUE))),
                yaxis = list(range = c(0, 0.5))  
              )
            
            p2 <-  rvalDF() %>%
              ggplot(aes_string(input$var)) +
              geom_histogram(aes(y = ..density..), 
                             color= "white", fill = "black")
            p2 <- ggplotly(p2) %>% 
              layout(
              )
            
            subplot(p1, p2) %>% 
              layout(annotations = list(
                list(x = 0.1 , y = 1, text = "Original Chart with Default Bin Set", showarrow = F, xref='paper', yref='paper'),
                list(x = 0.9 , y = 1, text = "Chart with New Bin Set", showarrow = F, xref='paper', yref='paper'))
              )
          })
          # output$breaksTbl <- renderRHandsontable({
          #   obj <- histogram(rvalDF()[[input$var]])
          #   rhandsontable(t(data.frame("breaks" = obj$breaks, "add" = "+")))
          # })
        }
        ## TODO #### later could change the equal range cut to histogram:: histogram bin width breaks
        
        if(is_numeric_var_excl_int(rvalDF()[[input$var]])) {
          output$naPlot <- renderPlotly({
            rvalDF() %>% mutate(!!sym(input$var):= cut_number(!!sym(input$var), 5)) %>% 
              naniar::gg_miss_fct(fct = !!sym(input$var))
          }) 
        } else if (is_category_var_incl_int(rvalDF()[[input$var]]) | is_date_var(rvalDF()[[input$var]])) {
          output$naPlot <- renderPlotly({
            rvalDF() %>%
              mutate(!!sym(input$var) := ifelse(
                as.character(!!sym(input$var)) %in% as.character(get_top_n_vars_ls(rvalDF()[[input$var]], n = 5)),
                as.character(!!sym(input$var)),
                "other"
              )) %>% 
              naniar::gg_miss_fct(fct = !!sym(input$var))
          })
          
        }

      })
    }
  )
}

# Bivars ----------------------------------------------------------------
mod_biVars_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      dropdown(
        tags$h3("List of Input"),
        selectInput(ns("varX"),
                    "Select Variable",
                    choices = ""),
        selectInput(ns("varY"),
                    "Select another Variable",
                    choices = ""),
        selectInput(ns("layer"),
                    "Optional, Select Category Variable to Add a Layer",
                    choices = ""),
        selectInput(
          ns('vars'),
          "(corr matrix only)Select variables to expolore mutual correlation",
          multiple = T,
          choices = ""
        ), 
        style = "unite", icon = icon("gear"),
        status = "success", width = "300px", 
        tooltip = tooltipOptions(title = "Inputs info"),
        animate = animateOptions(
          enter = animations$fading_entrances$fadeInLeft,
          exit = animations$fading_exits$fadeOutLeft
        )
      ),
      bs4TabSetPanel(
        id = "bivar",
        side = "left",
        tabStatus = "transparent",
        bs4TabPanel(
          tabName = "Overview",
          uiOutput(ns("ui"))
        ),
        bs4TabPanel(
          tabName = "Correlation",
          br(),
          plotlyOutput(ns("corrPlot"), width = "800px")
        ),
        bs4TabPanel(
          tabName = "Correlation Matrix", 
          br(),
          plotlyOutput(ns("ggpairs"), width = "800px")
        )
      ),
    ),
  )
}
mod_biVars_server <- function(id, rvalDF) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      observe({
        choices <- union(get_numeric_variables(rvalDF()), get_category_vars_blw20(rvalDF()))
        choices <- union(choices, get_20perc_lg80perc_vars(rvalDF()))
        updateSelectInput(session, "varX", choices = choices)
        updateSelectInput(session, "varY", choices = choices)
        layerChoice <- union(get_category_vars_blw20(rvalDF()), get_20perc_lg80perc_vars(rvalDF()))
        updateSelectInput(session, "layer", choices = layerChoice)
      })
      observe({
        
        # num w.r.t catogory case
        if ((is_category_var(rvalDF()[[input$varY]]) &
            is_numeric_var(rvalDF()[[input$varX]])) | 
           
           (is_category_var(rvalDF()[[input$varX]]) &
            is_numeric_var(rvalDF()[[input$varY]]))
        ) {
          output$ui <- renderUI({plotlyOutput(ns("violinPlot"), width = "800px")})
        } # num w.r.t num case
        else if(is_numeric_var(rvalDF()[[input$varY]]) &
                is_numeric_var(rvalDF()[[input$varX]])) {
          output$ui <- renderUI({plotOutput(ns("scatterplot"), width = "800px")})
        } 
        else if(is_category_var(rvalDF()[[input$varY]]) &
                is_category_var(rvalDF()[[input$varX]])) {
          output$ui <- renderUI({plotlyOutput(ns("mosaicplot"), width = "800px")})
        }
        output$violinPlot <- renderPlotly({
          
          if (is_category_var(rvalDF()[[input$varY]])) {
            plotDF <- rvalDF() %>%
              mutate(!!sym(input$varY):= ifelse(
                !!sym(input$varY) %in% get_top_n_vars_ls(rvalDF()[[input$varY]]),
                !!sym(input$varY), "other"
              ))
            
            p <- plotDF %>%
              mutate(!!sym(input$varY):= fct_reorder(!!sym(input$varY), !!sym(input$varX), .fun='length')) %>%
              ggplot(
                aes_string(
                  x = input$varY,
                  y = input$varX,
                  fill = input$varY)) 
            
          }
          
          if (is_category_var(rvalDF()[[input$varX]])) {
            plotDF <- rvalDF() %>%
              mutate(!!sym(input$varX):= ifelse(
                !!sym(input$varX) %in% get_top_n_vars_ls(rvalDF()[[input$varX]]),
                !!sym(input$varX), "other"
              ))
            
            p <- plotDF %>%
              mutate(!!sym(input$varX):= fct_reorder(!!sym(input$varX), !!sym(input$varY), .fun='length')) %>%
              ggplot(
                aes_string(
                  x = input$varX,
                  y = input$varY,
                  fill = input$varX)) 
          }
          
          p <- p +
            geom_violin() +
            xlab(paste0(input$varY, ":\n", "from largest group to smallest")) +
            theme(
              text=element_text(),
              axis.text.x = element_text(angle = 0)) +
            theme_minimal() +
            theme(legend.position = "none")
          ggplotly(p)
        })
        output$scatterplot <- renderPlot({
          ggscatterhist(
            rvalDF(), x = input$varY, y = input$varX,
            color = input$layer, size = 3, alpha = 0.6,
            margin.params = list(fill = input$layer, color = "black", size = 0.2)
          )
        })
        output$mosaicplot <- renderPlotly({
          plotDF <- rvalDF() %>%
            mutate(!!sym(input$varY) := ifelse(!!sym(input$varY) %in% get_top_n_vars_ls(rvalDF()[[input$varY]], n = 4),
                                               !!sym(input$varY), "other"),
                   !!sym(input$varX) := ifelse(!!sym(input$varX) %in% get_top_n_vars_ls(rvalDF()[[input$varX]], n = 4),
                                               !!sym(input$varX), "other")
            )
          
          p <-ggplot(data = plotDF) +
            geom_mosaic(aes(x = product(!!sym(input$varX), !!sym(input$varY)), 
                            fill=!!sym(input$varX), conds=product(!!sym(input$layer))), 
                        na.rm=TRUE, divider=mosaic("v"),
                        show.legend = FALSE) +  
            labs(title= paste("f(", input$varX, input$varY, "|", input$layer, ")")) +
            theme(axis.text.x = element_text(angle = 45))
          ggplotly(p) %>% layout(showlegend = FALSE)
        })
      })
      # for correlation aim:
      output$corrPlot <- renderPlotly({
        mini_frame <- rvalDF() %>% 
          select(!!sym(input$varX), !!sym(input$varY)) %>%
          tidyr::drop_na()
        xcol <- mini_frame %>% pull(!!sym(input$varX))
        ycol <- mini_frame %>% pull(!!sym(input$varY))
        corval <- signif(cor(xcol, ycol), digits = 3)
        ggplot(rvalDF(), aes_string(x = input$varY, y = input$varX)) +
          geom_point() +
          stat_smooth(method = lm, se = FALSE) +
          ggtitle(paste(input$varX, "vs.", input$varY, "correlation =", corval)) +
          theme_minimal()
      })
      # correlation Matrix
      observe({
        numChoices <- get_numeric_variables(rvalDF())
        updateSelectInput(session, "vars", choices = numChoices)
      })
      output$ggpairs <- renderPlotly({
        mini_frame <-
          dplyr::sample_n(rvalDF(), size = min(nrow(rvalDF()), 1000)) %>%
          select(input$vars) %>%
          tidyr::drop_na()
        ggpairs(mini_frame,
                title = "Correlation Matrix",
                lower = list(continuous = "smooth"))  +
          theme_minimal() +
          theme(
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black")
          )
      })
    }
  )
}

# Time Series Analysis ---------------------
mod_tsAnalysis_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(
      tags$script(HTML(js4fixhightlightBug))
    ),
    fluidRow(
      dropdown(
        tags$h3("List of Input"),
        selectInput(inputId = ns("tsVar"), "Select a Date Variable",
                    choices = ""),
        selectInput(inputId = ns("group"), "Select a Goupy_by Variable (Optional)",
                    choices = ""),
        selectInput(inputId = ns("value"), "Select a Value",
                    choices = ""),
        selectInput(inputId = ns("unit"), "Select a Time Horizon",
                    choices = c("second", "minute", "hour", "day", "week", "month", "year"),
                    selected = "month"),
        tags$h5("Rolling Window Set"),
        selectInput(inputId = ns("rollSize"), "Select a Rolling Window Size",
                    choices = c(1:5), selected = 3),
        

        selectInput(inputId = ns("func"), "sum or mean", choices = c("sum", "mean")),
        
        style = "unite", icon = icon("gear"),
        status = "success", width = "300px", 
        tooltip = tooltipOptions(title = "Inputs info"),
        animate = animateOptions(
          enter = animations$fading_entrances$fadeInLeft,
          exit = animations$fading_exits$fadeOutLeft
        )
      ),
      bs4TabSetPanel(
        id = "bivar",
        side = "left",
        tabStatus = "transparent",
        bs4TabPanel(
          active = TRUE,
          tabName = "Trend Analysis",
          uiOutput(ns("tsui"))
        ),
        bs4TabPanel(
          tabName = "Rolling Window",
          uiOutput(ns("tsuiRoll"))
        )
      ),
    ),
    
  )
}

mod_tsAnalysis_server <- function(id, rvalDF){
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      observe({
        dateChoices <- get_date_variables(rvalDF()) # get_date_variables doesn't include integer type year
        updateSelectInput(session, "tsVar", choices = dateChoices)
        updateSelectInput(session, "value", choices = get_numeric_variables(rvalDF()), selected = "premNet")
        updateSelectInput(session, "group", choices = c("None", get_category_vars_blw20(rvalDF())), selected = "group") #later change it to factors or levels
      })
      
      # TODO No need for uioutput tsui/tsPlot in this way. just have a tsPlot in UI module
      observeEvent(input$tsVar, {
        output$tsui <- renderUI({
          plotlyOutput(ns("tsPlot"), width = "800px")
        })
      })
      output$tsPlot<- renderPlotly({
        if(input$group == "None") {
          plotDF <- rvalDF() %>% 
            tsagg(!!sym(input$tsVar), !!sym(input$value), unit = input$unit, func = input$func)
        } else {
          plotDF <- rvalDF() %>% 
            tsaggGrp(!!sym(input$tsVar), !!sym(input$group), !!sym(input$value), unit = input$unit, func = input$func)
        }
        if(input$group == "None") {
          baseLayer <- plot_ly(plotDF, color = I("black")) 
        } else {
          hl <- highlight_key(plotDF, plotly_eval(input$group))
          baseLayer <- plot_ly(hl, color = I("black"))  %>%
            group_by(!!sym(input$group))
        }
        tsFig <- baseLayer %>%
          add_lines(x = plotly_eval(input$tsVar), y = plotly_eval(input$value)) 
        # TODO periodslider()
        # %>%
        #   layout(
        #     title = "Time Series Analysis",
        #     xaxis = periodslider(),  
        #     yaxis = list(title = input$value,
        #                  fixedrange = FALSE)
        #   )
        expoBar <- add_bars(
          baseLayer,
          plotly_eval(input$tsVar),
          y = ~expo
        ) 
        
        subplot(tsFig, expoBar, nrows = 2) %>%
          layout(barmode = "group", showlegend = FALSE) %>%
          highlight(
            on = "plotly_click",
            dynamic = FALSE, 
            selectize = TRUE, 
            selected = attrs_selected(opacity = 1)
          )

      })
      observeEvent(input$tsVar, {
        output$tsuiRoll <- renderUI({
          plotlyOutput(ns("tsPlotRoll"), width = "800px")
        })
      })
      output$tsPlotRoll <- renderPlotly({
        if(input$group == "None") {
          plotDF <- rvalDF() %>% 
            tsaggRoll(!!sym(input$tsVar), !!sym(input$value), input$unit, as.numeric(input$rollSize), input$func)
        } else {
          plotDF <- rvalDF() %>% 
            tsaggGrpRoll(!!sym(input$tsVar), !!sym(input$group), !!sym(input$value), input$unit, as.numeric(input$rollSize), input$func)
        }
        #dropdown
        # rollFreq <- data.frame(freq = c("day", "week", "month", "year"))
        # all_buttons <- list()
        # for (i in 1:length(rollFreq[,])) { 
        #   # ref:https://plotly.com/r/sliders/
        #   all_buttons[[i]] <- list(method = "relayout",
        #                            args = list(list()),
        #                            label = rollFreq$freq[i])
        # }
        
        # slider bar for number of rolling window width
        # sliderRange <- list(
        #   list(args = list("marker.color", "red"), 
        #        label = "Red", 
        #        method = "restyle", 
        #        value = "1"
        #   ),
        #   list(args = list("marker.color", "green"), 
        #        label = "Green", 
        #        method = "restyle", 
        #        value = "2"
        #   ),
        #   list(args = list("marker.color", "blue"), 
        #        label = "Blue", 
        #        method = "restyle", 
        #        value = "3"
        #   )
        # )
        
        if(input$group == "None") {
          baseLayer <- plot_ly(plotDF, color = I("black")) 
        } else {
          hl <- highlight_key(plotDF, plotly_eval(input$group))
          baseLayer <- plot_ly(hl, color = I("black"))  %>%
            group_by(!!sym(input$group))
        }
        
        
        tsFig <- baseLayer %>%
          add_lines(x = plotly_eval(input$tsVar), y = plotly_eval(input$value)) 
        expoBar <- add_bars(
          baseLayer,
          x = plotly_eval(input$tsVar),
          y = ~expo
        ) 
        subplot(tsFig, expoBar, nrows = 2) %>%
          layout(barmode = "group", showlegend = FALSE) %>%
          highlight(
            on = "plotly_click",
            dynamic = FALSE, 
            selectize = TRUE, 
            selected = attrs_selected(opacity = 1)
          )
        
        # %>%
          # layout(
          #   title = "Moving Average Time Series",
          #   xaxis = periodslider(),
          #   yaxis = list(title = input$value,
          #                fixedrange = FALSE),
            # sliders = list(
            #   list(
            #     active = 1,
            #     currentvalue = list(prefix = "Rolling width: "),
            #     pad = list(t = 20),
            #     steps = sliderRange),
            # ),
            # updatemenus = list(
            #   list(
            #     type = "buttons",
            #     x = 1.2,
            #     y = 1,
            #     buttons = list(
            #       list(method = "restyle",
            #            args = list("line.color", "blue"),
            #            label = "Sum"),
            #       list(method = "restyle",
            #            args = list("line.color", "red"),
            #            label = "Mean"))),
            #   list(active = 0, x = 1.2, y = 0.6, 
            #        buttons=all_buttons)
            # )
          # )
        # fig %>%
        #   highlight(
        #     on = "plotly_click",
        #     selectize = FALSE,
        #     dynamic = FALSE,
        #     persistent = TRUE
        #   )
        
      })
      
      
    }
  )
}

# multiple view analysis --------
mod_mul_view_ui <- function(id){
  js <- '
$(document).on("shiny:value", function(e){
  if(e.name === "mulView-mulPlot"){
    setTimeout(function(){
      $("#mulView-mulPlot").prev().children()[1].style.display = "none";
    }, 0);
  }
});
'
  ns <- NS(id)
  tagList(
    tags$head(
      tags$script(HTML(js))
    ),
    selectInput(ns("target"), "select target", choices = ""),
    selectInput(ns("var1"), "select variable 1", choices = ""),
    selectInput(ns("var2"), "select variable 2", choices = ""),
    selectInput(ns("var3"), "select variable 3", choices = ""),
    plotlyOutput(ns("mulPlot"), height = "700px")
  )
}
mod_mul_view_server <- function(id, rvalDF){
  moduleServer(
    id,
    function(input, output, session) {
      observe({
        namels <- get_category_vars_blw20(rvalDF(), 10)
        updateSelectInput(session, "target", choices = get_numeric_variables(rvalDF()))
        updateSelectInput(session, "var1", choices = namels)
        updateSelectInput(session, "var2", choices = namels)
        updateSelectInput(session, "var3", choices = namels)
      })
      
      
      output$mulPlot <- renderPlotly({
        df500 <- sample_n(rvalDF(), 500)
        d <- highlight_key(df500)
        base <- plot_ly(d, color = I("black"), showlegend = FALSE)
        subplot(
          add_markers(base, x = plotly_eval(input$target), y = plotly_eval(input$target)),
          add_boxplot(base, x =plotly_eval(input$var1), y = plotly_eval(input$target)) %>%
            add_markers(x = plotly_eval(input$var1), y = plotly_eval(input$target), alpha = 0.1),
          add_trace(base, x = plotly_eval(input$var2), y = plotly_eval(input$target), type = "violin") %>%
            add_markers(x = plotly_eval(input$var2), y = plotly_eval(input$target), alpha = 0.1),
          shareY = TRUE,
          titleX = TRUE,
          titleY = TRUE
        ) %>%
          subplot(
            add_histogram(base, x = plotly_eval(input$var3)), 
            nrows = 2,
            titleX = TRUE,
            titleY = TRUE
          ) %>%
          # Selections are actually additional traces, and, by default,
          # plotly.js will try to dodge bars placed under the same category
          layout(barmode = "overlay") %>%
          highlight(
            "plotly_selected",
            dynamic = TRUE,
            selectize = TRUE,
            persistent = TRUE
          )
        
        ##the following part is only for test
        # df %>% 
        #   mutate(
        #     startOfPeriod = lubridate::floor_date(startOfPeriod, unit = "month")
        #   ) %>% 
        #   group_by(startOfPeriod, productTypeGroup) %>% 
        #   summarise(
        #     premNet = sum(premNet, na.rm = TRUE),
        #     productPrice = sum(productPrice, na.rm = TRUE)
        #   )
        # 
        # 
        # hl <- highlight_key(plotDF, ~productTypeGroup)
        # fig <- plot_ly(hl, color = I("black"))  %>%
        #   group_by(productTypeGroup)
        # 
        # fig <- fig %>%
        #   add_lines(x = ~startOfPeriod, y = ~premNet) %>%
        #   layout(
        #     title = "Time Series Analysis",
        #     xaxis = periodslider(),
        #     yaxis = list(title = "premNet",
        #                  fixedrange = FALSE)
        #   )
        # fig %>%
        #   highlight(
        #     on = "plotly_click",
        #     selectize = TRUE,
        #     dynamic = TRUE,
        #     persistent = TRUE
        #   )
      
        
        
        
      })
    }
  )
}



# correlation expl --------------------------------------------------------
mod_corr_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(inputId = ns("varX"), "Select X Variable",
                choices = ""),
    selectInput(inputId = ns("varY"), "Select Y Variable",
                choices = ""),
    actionButton(ns("draw"), "Draw Charts"),
    plotOutput(ns("corrPlot"))
  )
}
mod_corr_plot_server <- function(id, rvalDF) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      observe({
        numChoices <- get_numeric_variables(rvalDF())
        updateSelectInput(session, "varX", choices = numChoices)
        updateSelectInput(session, "varY", choices = numChoices)
      })
      drawPlotAct <- eventReactive(
        input$draw, 
        {mini_frame <- rvalDF() %>% 
          select(!!sym(input$varX), !!sym(input$varY)) %>%
          tidyr::drop_na()
        xcol <- mini_frame %>% pull(!!sym(input$varX))
        ycol <- mini_frame %>% pull(!!sym(input$varY))
        corval <- signif(cor(xcol, ycol), digits = 3)
        ggplot(rvalDF(), aes_string(x = input$varX, y = input$varY)) +
          naniar::geom_miss_point() + stat_smooth(method = lm, se = FALSE) +
          ggtitle(paste(input$varX, "vs.", input$varY, "correlation =", corval)) +
          theme_minimal()
        }
      )
      output$corrPlot <- renderPlot({
        drawPlotAct()
      })
      
    }
  )
}


# ggpairs for corr --------------------------------------------------------
mod_ggpairs_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    selectInput(ns('vars'), "Select variables to expolore mutual correlation", 
                multiple = T, choices = ""
    ),
    actionButton(ns("draw"), "Draw Charts"),
    plotOutput(ns("ggpairs"))
    
  )
}
mod_ggpairs_server <- function(id, rvalDF) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      observe({
        numChoices <- get_numeric_variables(rvalDF())
        updateSelectInput(session, "vars", choices = numChoices)
      })
      drawPlotAct <- eventReactive(input$draw, {
        mini_frame <- dplyr::sample_n(rvalDF(), size = min(nrow(rvalDF()), 1000)) %>% 
          select(input$vars) %>%
          tidyr::drop_na()
        
        ggpairs(mini_frame,  title="correlogram with ggpairs()",
                lower = list(continuous = "smooth"))  +
          theme_minimal() + 
          theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
      }
      )
      output$ggpairs <- renderPlot({
        drawPlotAct()
      })
    }
  )
}

# pivottable --------------------------------------------------------------
mod_pivotTbl_ui <- function(id){
  ns <- NS(id)
  
  tagList( 
    rpivotTableOutput(ns("pivot"), width = '200px',height = '200px')
  )
}
mod_pivotTbl_server <- function(id, rvalDF) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      output$pivot <- renderRpivotTable({
        rpivotTable(rvalDF(), width = '200px', height = '200px')
      })
    }
  )
}

