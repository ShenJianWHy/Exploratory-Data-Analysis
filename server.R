server <- function(input, output, session) {

  
  # global set ---------------------------------------------------------------------
  
  # Login/logout Setup -------------------------------------------------------------
  
  credentials <- mod_login_server(
    "login",
    data = user_base,
    user_col = user,
    pwd_col = password_hash,
    sodium_hashed = TRUE
  )
  observe({
    req(credentials$user_auth)
    showElement(id = "page")
  })
  
  # logout
  observeEvent(input$logout, {
    session$reload()
  })
  
  
  # Data Ingestion ----------------------------------------------------------
  
  ## 2 pushbars
  setup_pushbar(blur = TRUE, overlay = TRUE)
  observeEvent(input$opensetup, {
    pushbar_open(id = "pushbar")
  })
  observeEvent(input$datOperBttn, {
    pushbar_open(id = "datOper")
  })
  observeEvent(input$datSaveBttn, {
    pushbar_open(id = "saveDat")
  })
  
  ### uioutput conditioned in choice(sql, local file raw data, finDat)
  output$datIngesMethod <- renderUI({
    if (input$ingestMethod == "files") {
      tagList(
        h4("Upload Raw Data"),
        mod_upload_file_ui("raw", NULL),
        hr(),
        h4("Upload Other Files"),
        mod_upload_file_ui("ref", NULL),
        hr(),
        h4("Upload Data Clean Pipe Line"),
        mod_upload_file_ui("datpipe", NULL, multiple = FALSE)
      )
    } else if (input$ingestMethod == "database") {
      # database sql module
    } else {
      tagList(h4("Upload csv Data"),
              mod_csv_upload_ui("finDatCsv", NULL),)
    }
  })
  ### uioutput data editing
  output$dataEdit <- renderUI({
    mod_add_field_ui("addField")
  })
   # mod_add_field_server("addField", rv_df)
  
  ### uioutput save date
  output$dataSave <- renderUI({
    #content
    tagList(
      h4("Dowload Data"),
      radioGroupButtons(
        inputId = "saveFormat",
        label = "Saving Format",
        choices = c(".csv",
                    ".rda"),
        checkIcon = list(
          yes = tags$i(class = "fa fa-check-square",
                       style = "color: steelblue"),
          no = tags$i(class = "fa fa-square-o",
                      style = "color: steelblue")
        )
      ),
      downloadButton("downloadData", "Download"),
      hr(),
      h4("Save to Database"),
      selectInput("tblDB", "Save to existing table", c(letters[1:5])),
      textInput("newtblDB", "Create new table", placeholder = "Name table"),
      actionButton("savetoDB", "Save"),
    )
  })
  
  
  # output$downloadData <- downloadHandler(
  #   filename = function() {
  #     paste("data-", Sys.Date(), ".csv", sep="")
  #   },
  #   content = function(file) {
  #     write.csv(mtcars, file)
  #   }
  # )

  output$downloadData <- downloadHandler(
    
    
    filename = function() {
      ifelse(input$saveFormat == ".csv", "finData.csv", "finData.rData")
    },
    content = function(file) {
      if (input$saveFormat == ".csv") {
        write.csv(isolate(rval_df), file, row.names = FALSE)
      }
      else {
        saveRDS(isolate(rval_df), file)
      }
    }
  )
  
  ## upload files and run data pipe line
  mod_upload_file_server("raw", save = TRUE, "raw")
  mod_upload_file_server("ref", save = TRUE, "ref")
  mod_upload_file_server("datpipe", save = TRUE, "datpipe")
  
  ## return final data
  rv_df <- reactiveValues(data= NULL)
  rvls <- reactiveValues()
  load("insData.rda")
  
  rv_df$data <- SingaporeAuto
  # rv_df$data <- reactive(df)
  
  ###### tHIS IS FOR PIPE LINE OPERAETION ##########
  rval_df <- mod_data_pipeline_server("datpipe", rv_df)
  ###################################################
  
  rval_df <- reactive(rv_df$data)
  ############## UPLOAD ONE FILE ################
  # rval_df <- mod_csv_upload_server("finDatCsv") # TODO this doesn't return reactive
  ################################################
  
  ## Data Summary
  output$dfSummary <- renderUI({
    print(
      dfSummary(
        rval_df(),
        varnumbers = FALSE,
        valid.col = FALSE,
        graph.magnif = 0.8
      ),
      method = 'render',
      headings = FALSE,
      bootstrap.css = FALSE
    )
  })
  
  # Global Variables Definition ---------------------------------------------
  load("insData.rda")
  insData <- reactive({SingaporeAuto})
  
  mod_global_vars_server("globVars", insData)
  globVars <- mod_return_global_vars_server("globVars") 
  # target = reactive({input$target }),
  # features = reactive({input$features}),
  # exposure = reactive({input$exposure }),
  # date = reactive({input$date })  
  icons <- c("target", "layers", "cloud", "calendar")
  mod_global_vars_info_server("varinfo", globVars, varVec, icons)
  

  # Modeling ----------------------------------------------------------------
  mod_rv <- reactiveValues(model = NULL)
  observeEvent(input$runModel, ignoreInit = T, ignoreNULL = T,{
    target         <- globVars$target()
    features       <- globVars$features()
    exposure       <- globVars$exposure()
    
    mod_rv$model <- modeling(
      data     = SingaporeAuto,
      target   = target,
      features = features,
      exposure = exposure,
      family   = input$modelfamily
    )
    
    showModal(modalDialog("Model has been built", footer=NULL))
    Sys.sleep(1)
    removeModal()
  })

  observeEvent(input$saveModel,
               ignoreInit = T,
               ignoreNULL = T,
               {
                 model <- isolate(mod_rv$model)
                 
                 wd <- getwd()
                 setwd(paste0(wd, "/inst/model"))
                 save(model,
                      file = paste0(input$modelUser, "_", input$modelName, "_mod", ".rda"))
                 setwd(wd)
                 showModal(modalDialog("Model has been saved", footer=NULL))
                 Sys.sleep(1)
                 removeModal()
               })
  
  observe({
    file <- input$modelrda
    req(file)
    ext <- tools::file_ext(file$datapath)
    validate(need(ext == "rda", "Please upload a rda file"))
    load(file$datapath)
    mod_rv$model <- model # hard coded, model is obj's name from loading file
    # print(summary(model))
  })
  
  observeEvent(input$evaModel, {
    model <- isolate(mod_rv$model)
    
    output$console_text1 <- renderPrint(summary(model$model))
    output$console_text2 <- renderPrint(model$metrics)
  })
  
  observeEvent(input$combineModel, {
    file1 <- input$modelrda1
    file2 <- input$modelrda2
    #model <- isolate(mod_rv$model)
    
    load(file1$datapath)
    freq_res <- model
    load(file2$datapath)
    sev_res <- model
    
    freq <- freq_res$model
    sev <- sev_res$model
    
    freq_coef <- freq$coefficients %>% exp() %>% as.data.frame()
    freq_coef$names <- rownames(freq_coef)
    rownames(freq_coef)=NULL
    names(freq_coef) = c("coeff_freq", "names")
    
    sev_coef <- sev$coefficients %>% exp() %>% as.data.frame()
    names(sev_coef) = "coeff_sev"
    
    sev_coef <- sev$coefficients %>% exp() %>% as.data.frame()
    sev_coef$names <- rownames(sev_coef)
    rownames(sev_coef)=NULL
    names(sev_coef) = c("coeff_sev", "names")
    
    rateTbl <- freq_coef %>% full_join(sev_coef, by = "names") %>% 
      mutate(relsSev  = ifelse(is.na(coeff_sev),  1, coeff_sev),
             relsFreq = ifelse(is.na(coeff_freq), 1, coeff_freq),
             purePrem = relsSev * relsFreq) %>% 
      select(names, relsFreq, relsSev, purePrem)
    
    rateTblAdj <- rateTbl %>% 
      mutate(
        purePrem = purePrem * sev_res$adjust * freq_res$adjust
      )
    
    output$CombinerRes <- renderDataTable(rateTbl)
    output$CombinerResAdj <- renderDataTable(rateTblAdj)
  })
  # deep dive ---------------------------------------------------------------
  
  ## basic/advanced deep dive renderUI side
  observeEvent(input$adv, ignoreInit = TRUE, {
    toggle(id = "deepDiveBasic")
    toggle(id = "deepDiveAdv")
  })
  
  
  ## pushbar setup
  # rval_ls contains rv_date, rval_df_4dd, compareCond
  
  rval_ls <- mod_pushbar_server("dd_setup", rval_df, mod_udf_dd_server(id = "udf_dd", rval_df))
  
  observe({
    req(rval_df)
    updateSelectInput(session, "dd_filterItems", choices = names(rval_df()))
  })
  
  output$dd_filterSelectInputs <- renderUI({
    lapply(input$dd_filterItems, function(colName) {
      columnFilterUI(colName)
    })
  })
  
  rv_filter_dd <- reactiveValues(filters = NULL)
  
  create_choice_filter_dd <- function(colName) {
    reactive({
      col <- which(colName == input$dd_filterItems)
      filter_values_dd <- lapply(rv_filter_dd$filters[-col], do.call, args = list())
      Reduce(`&`, filter_values_dd, TRUE)
    })
  }
  
  observeEvent(input$dd_filterItems, ignoreInit = T, {
    rv_filter_dd$filters <- lapply(input$dd_filterItems, function(colName) {
      callModule(columnFilter, colName, rval_df_all, colName, create_choice_filter_dd(colName))
    })
    
  })
  
  rval_df_dd <- reactive({
    filter_values_dd <- lapply(rv_filter_dd$filters, do.call, args = list())
    selected_rows <- Reduce(`&`, filter_values_dd, TRUE)
    rval_df()[selected_rows,]
  })
  
  observe({
    print(nrow(rval_df_dd()))
  })
  
  
  
  ## basic deep dive server side
  
  lapply(1:6, function(x) {
    mod_tablerKpiCard_server(paste0("kpi", x), rval_ls)
  })
  
 
  # mod_oneVar_server("oneVar", rval_df)
  # mod_biVars_server("biVars", rval_df)
  # mod_tsAnalysis_server("tsAnalysis", rval_df)
  
  mod_oneVar_server("oneVar", rval_df_dd)
  mod_biVars_server("biVars", rval_df_dd)
  mod_tsAnalysis_server("tsAnalysis", rval_df_dd)
  
  ## advanced deep dive server side
  
  mod_pivottable_server("pvt", rval_df_dd)
  mod_data_summary_server_viz("vizDat", rval_df_dd)
  mod_data_summary_server("datSummTbl", rval_df_dd) # change name to mod_data_summary_tbl_server
  mod_mul_view_server("mulView", rval_df_dd)
  
  
  # mod_pivottable_server("pvt", rval_df)
  # mod_data_summary_server_viz("vizDat", rval_df)
  # mod_data_summary_server("datSummTbl", rval_df) # change name to mod_data_summary_tbl_server
  # mod_mul_view_server("mulView", rval_df)
  
  
  ## Monitoring Section ------
  # TODO updateTabItems could not work; Potential reason: could not work under tablerR
  
  updateTabItems(session, inputId = "monit_tabs", selected = "TAB2")
  
  rval_df_all <- reactive(SingaporeAuto)
  # rval_df_all <- reactive(allDat)
  # rv_filters_monit <- mod_addFilterInputs4DF_server("monitorDashboard", rval_df_all)
  
  observe({
    req(rval_df_all)
    updateSelectInput(session, "filterItems", choices = names(rval_df_all()))
  })
  output$filterSelectInputs <- renderUI({
    lapply(input$filterItems, function(colName) {
      columnFilterUI(colName)
    })
  })
  
  rv_filter <- reactiveValues(filters = NULL)
  
  create_choice_filter <- function(colName) {
    reactive({
      col <- which(colName == input$filterItems)
      filter_values <- lapply(rv_filter$filters[-col], do.call, args = list())
      Reduce(`&`, filter_values, TRUE)
    })
  }
  
  observeEvent(input$filterItems, ignoreInit = T, {
    rv_filter$filters <- lapply(input$filterItems, function(colName) {
      callModule(columnFilter, colName, rval_df_all, colName, create_choice_filter(colName))
    })

  })
  
  rval_df_monit <- reactive({
    filter_values <- lapply(rv_filter$filters, do.call, args = list())
    selected_rows <- Reduce(`&`, filter_values, TRUE)
    rval_df_all()[selected_rows,]
  })
  
  # observe({
  #   print(nrow(filteredDF()))
  # })
  
  # mod_addFilterSelectBoxeS_server("monitorDashboard", rval_df_monit, rval_df_all)
  # 
  # rv_filters_monit <- mod_addFilterRvfilters_server("monitorDashboard", rval_df_all)
  # 
  # rval_df_monit <- mod_rval_df4dd_server("rval_df_monit", rval_df_all, rv_filters_monit)

  observe({
    ls <- get_numeric_variables(rval_df_monit())   #TODO change assign1 2 3 to real name
    datels <- get_date_variables(rval_df_monit())
    updateSelectInput(session, "assign1", choices = ls)
    updateSelectInput(session, "assign2", choices = ls)
    updateSelectInput(session, "assign3", choices = ls)
    updateSelectInput(session, "assign4", choices = ls)
    updateSelectInput(session, "assign5", choices = datels)
    updateSelectInput(session, "assign6", choices = ls)
  })
  
  rv_monit <- reactiveValues()
  unit <- "month"
  observe({
    req(input$assign1)
    req(input$assign2)
    rv_monit$kpioverall <- rval_df_monit() %>% kpi_overall(
      input$assign1,
      input$assign6,
      input$assign2,
      input$assign3,
      input$assign4
      )#datIn, claimCount, premium, expo, cost, bc
    rv_monit$kpits <- rval_df_monit() %>% kpi_TS(unit, input$assign5)
  })

  observe({
    #update selectinput for monitGrp
    updatePickerInput(
      session = session, inputId = "monitGrp",
      # choices = setdiff(names(rval_df_all()), get_date_variables(rval_df_all()))
      choices = get_all_category_vars(rval_df_all())
    )
  })


# Data processing for TAB2

  
### TAB1 --------------------------------------------------------------------
  output$kpi_monit_infoboxs <- renderUI({
    fluidRow(
      tablerInfoCard(
        value = rv_monit$kpioverall$premNet %>% numb_format(),
        status = "success",
        icon = "dollar-sign",
        description = "Premium Net",
        width = 3
      ),
      tablerInfoCard(
        value = rv_monit$kpioverall$claimCost %>% numb_format(),
        status = "success",
        icon = "dollar-sign",
        description = "Claim Cost",
        width = 3
      ),
      tablerInfoCard(
        value = rv_monit$kpioverall$claimCount %>% numb_format(),
        status = "success",
        icon = "hash",
        description = "Claim Count",
        width = 3
      ),
      tablerInfoCard(
        value = rv_monit$kpioverall$severity %>% numb_format(),
        status = "success",
        icon = "percent",
        description = "Severity",
        width = 3
      ),
      tablerInfoCard(
        value = rv_monit$kpioverall$freq %>% scales::percent(accuracy = 0.01),
        status = "success",
        icon = "percent",
        description = "Frequency",
        width = 3
      ),
      tablerInfoCard(
        value = rv_monit$kpioverall$lossRatio %>% scales::percent(accuracy = 0.01),
        status = "success",
        icon = "percent",
        description = "Loss Ratio",
        width = 3
      ),
    )
  })
  
  # datIn, unit,  dateVar, inputVarExpo, inputVarBC, inputVarClaimCost
  output$lossRatioTS <- renderPlotly({
    LRTS(
      datIn = rval_df_monit(),
      unit = "month",
      dateVar = input$assign5,
      inputVarExpo = input$assign2,
      inputVarBC = input$assign4,
      inputVarClaimCost = input$assign3
    )
  })
  output$severityTS <- renderPlotly({
    rval_df_monit() %>% severiTS(
      unit = "month",
      dateVar = input$assign5,
      inputVarExpo = input$assign2,
      inputVarBC = input$assign4,
      inputVarClaimCost = input$assign3,
      claimCount = input$assign1
    )
  })
  output$frequencyTS <- renderPlotly({
    rval_df_monit() %>% freqTS(
      unit = "month",
      dateVar = input$assign5,
      inputVarExpo = input$assign2,
      inputVarBC = input$assign4,
      inputVarClaimCost = input$assign3,
      claimCount = input$assign1
    )
  })

  # ### TAB2 -------

  # (df, simplifyVar = NULL, findVars = 15, threshold = 10)
  
  globalVar <-  c()
  
  # claimsCostVar <- names(rval_df_monit())[1]  
  # 
  # observeEvent(input$click) {
  #   claimsCostVar <- input$assign3
  # }
  
  observeEvent(c(input$monitGrp, input$threshold_ins, input$assign1, input$assign2,input$assign3,input$assign4,input$assign5, input$assign6), ignoreInit = TRUE, {
    globalVar <<-c(input$monitGrp, input$threshold_ins, input$assign1, input$assign2,input$assign3,input$assign4,input$assign5, input$assign6)
    detailsDF <- tryCatch({
      rval_df_monit() %>%
        get_simplified_category_df(simplifyVar = input$monitGrp, overwrite = TRUE, threshold = input$threshold_ins) %>%
        group_by(!!sym(input$monitGrp)) %>%
        summarise(
          expo      = sum(!!sym(input$assign2), na.rm = TRUE),
          lossRatio = sum(!!sym(input$assign4), na.rm = TRUE) / sum(!!sym(input$assign3), na.rm = TRUE),
          severity  = sum(!!sym(input$assign3), na.rm = TRUE) / sum(!!sym(input$assign1), na.rm = TRUE),
          freq      = sum(!!sym(input$assign1), na.rm = TRUE) / sum(!!sym(input$assign2), na.rm = TRUE),
        )
    }, error = function(err){})
    
    if (!is.null(detailsDF)) {
      # Plot maing graph
      output$kpisGrp <- renderPlotly({
        detailsDF %>%
          plot_ly(x= plotly_eval(input$monitGrp)) %>%
          add_trace(y = ~expo, type = "bar", name = "Exposure") %>%
          add_trace(y = ~lossRatio, yaxis = "y2", type = "scatter", mode = "markers", name = "Loss Ratio") %>%
          add_trace(y = ~severity, yaxis = "y2", type = "scatter", mode = "markers", name = "Severity") %>%
          add_trace(y = ~freq, yaxis = "y2", type = "scatter", mode = "markers", name = "Frequency") %>%
          layout(title = 'KPIs with Subgroups',
                 xaxis = list(title = "Subgroups"),
                 yaxis = list(side = 'right', title = '', showgrid = FALSE, zeroline = FALSE),
                 yaxis2 = list(side = 'left', overlaying = "y", title = '', showgrid = FALSE, zeroline = FALSE),
                 legend = list(orientation = "h", xanchor = "center", y = - 0.2, x = 0.5))
      })
      
      # Define the plotting skeleton
      uqElements <- detailsDF %>% .[[input$monitGrp]] %>% unique()
      output$kpiGrpTS <- renderUI({
        tagList(
          fluidRow(
            lapply(
              uqElements,
              function(.x) {
                tablerCard(
                  width = 6,
                  plotlyOutput(outputId = .x)
                )
              }
            )
          )
        )
      })
    }
    
    detailsDF2 <- tryCatch({
      rval_df_monit() %>%
        get_simplified_category_df(
          simplifyVar = input$monitGrp,
          overwrite = TRUE,
          threshold = 10
        ) %>%
        mutate(roundedDate = !!sym(input$assign5) %>% lubridate::floor_date(unit = input$unit)) %>%
        group_by(!!sym(input$monitGrp), roundedDate) %>%
        summarise(
          expo      = sum(!!sym(input$assign2), na.rm = TRUE),
          lossRatio = sum(!!sym(input$assign4), na.rm = TRUE) / sum(!!sym(input$assign3), na.rm = TRUE)
          # severity  = sum(!!sym(input$assign3), na.rm = TRUE) / sum(!!sym(input$assign1), na.rm = TRUE),
          # freq      = sum(!!sym(input$assign1), na.rm = TRUE) / sum(!!sym(input$assign2), na.rm = TRUE),
        )
    }, error = function(err) {
    })
    
    
    if (!is.null(detailsDF)) {
      #Plotting the graphs
      lapply(
        uqElements, function(.x) {
          if(!is.na(.x)) {
            output[[.x]] <- renderPlotly({
              
              detailsDF2 %>% 
                filter(!!sym(paste0(input$monitGrp)) == .x) %>%
                plot_ly(x = ~roundedDate) %>%
                add_trace(y = ~expo,       yaxis = "y1", name = 'Exposure', type = 'bar', opacity = 0.4) %>%
                add_trace(y = ~lossRatio, yaxis = "y2", name = 'Loss Ratio', type = "scatter", mode = 'markers') %>%
                layout(title = paste("Group:", .x),
                       xaxis = list(title = "Time"),
                       yaxis = list(side = 'right', title = '', showgrid = FALSE, zeroline = FALSE),
                       yaxis2 = list(side = 'left', overlaying = "y", title = 'Loss Ratio', showgrid = FALSE, zeroline = FALSE),
                       legend = list(orientation = "h", xanchor = "center", y = - 0.2, x = 0.5))
            })
          }
        }
      )
    }
  })
  
}
