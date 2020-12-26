# navbar ------------------------------------------------------------------
nav <- tablerDashNav(
  id = "mymenu",
  src = "aedaSlogan.png",
  
  tags$li(class = "dropdown", style = "padding: 8px;",  
          tablerIcon("message-square")),
  tags$li(class = "dropdown", style = "padding: 8px;",  
          tags$a(href = "https://github.com/EuropAssistanceInsurance/aeda", 
                 tablerIcon("github"), target="_blank")),
  tags$li(class = "dropdown", style = "padding: 8px;",  
          tablerIcon("bell")),
  tags$li(class = "dropdown", style = "padding: 8px;",  
          actionLink("logout", tablerIcon("log-out"))),
  
  
  navMenu = tablerNavMenu(
    tablerNavMenuItem(
      tabName = "home",
      icon = "home",
      "Home",
    ),
    tablerNavMenuItem(
      tabName = "datInteg",
      icon = "database",
      "Data Integration"
    ),
    tablerNavMenuItem(
      tabName = "globVars",
      icon = "globe",
      "Global Variables"
    ),
    tablerNavMenuItem(
      tabName = "overview",
      icon = "airplay",
      "Overview"
    ),
    tablerNavMenuItem(
      tabName = "deepDive",
      icon = "search",
      "Deep Dive"
    ),
    tablerNavMenuItem(
      tabName = "geoAnalysis",
      icon = "map-pin",
      "Geographic Analysis"
    ),
    tablerNavMenuItem(
      tabName = "modeling",
      icon = "cpu",
      "Modeling"
    ),
    tablerNavMenuItem(
      tabName = "insurMonit",
      icon = "eye",
      "Insurance Monitoring"
    )
  )
)


# body --------------------------------------------------------------------

body <- tablerDashBody(
  useShinyjs(),
  chooseSliderSkin("Round"),
  
  # use shinyEffects
  setShadow(class = "galleryCard"),
  #setZoom(class = c("card")),

  # Data Ingestion ----------------------------------------------------------

  tablerTabItems(
    tablerTabItem(
      tabName = "home",
      tablerProfileCard(
        title = "Singapore Auto",
        subtitle = "Project Introduction",
        background = "https://upload.wikimedia.org/wikipedia/commons/4/48/Flag_of_Singapore.svg",
        src = NULL,
        tablerSocialLinks(
          tablerSocialLink(
            name = "facebook",
            href = "https://www.facebook.com",
            icon = "facebook"
          ),
          tablerSocialLink(
            name = "twitter",
            href = "https://www.twitter.com",
            icon = "twitter"
          )
        )
      )
    ),
    tablerTabItem(
      tabName = "datInteg",
      pushbar_deps(),
      
      fluidRow(h3("Data load, extract and transform")),
      br(),
      fluidRow(
        actionBttn(
          inputId = "opensetup",
          label = NULL,
          style = "material-circle",
          color = "success",
          icon = tablerIcon("settings")
        ),
        column(1, align = "left", h2("Setup"), style = "padding:8px;"),
        # actionBttn(
        #   inputId = "datOperBttn",
        #   label = NULL,
        #   style = "material-circle",
        #   color = "primary",
        #   icon = tablerIcon("edit")
        # ),
        # column(1, align = "left", h2("Edit"), style = "padding:8px;"),
        actionBttn(
          inputId = "datSaveBttn",
          label = NULL,
          style = "material-circle",
          color = "royal",
          icon = tablerIcon("save")
        ),
        column(4, align = "left", h2("Save"), style = "padding:8px;")
      ),
      
      pushbar(
        style = "background:#fff;padding:20px;",
        from = "left",
        id = "pushbar",
        br(), br(),
        h1("Setup"),
        radioGroupButtons(
          inputId = "ingestMethod",
          label = h4("Data ingestion method"), 
          choices = c(`<i class='fa fa-database'></i>` = "database", `<i class='fa fa-folder'></i>` = "files", 
                      `<i class='fa fa-file'></i>` = "file"),
          justified = TRUE
        ),
        uiOutput("datIngesMethod")
      ),
      pushbar(
        style = "background:#fff;padding:20px;",
        from = "left",
        id = "datOper",
        br(), br(),
        h1("Data Editing"),
        uiOutput("dataEdit")
      ),
      pushbar(
        style = "background:#fff;padding:20px;",
        from = "left",
        id = "saveDat",
        br(), br(),
        h1("Save Data"),
        uiOutput("dataSave")
      ),
      br(),
      fluidRow(
        column(
          4,
          h4("Data Flow History"),
          tablerTimeline(
            tablerTimelineItem(
              title = "Item 1",
              status = "green",
              date = "now"
            ),
            tablerTimelineItem(
              title = "Item 2",
              status = NULL,
              date = "yesterday",
              "Jian uploads the new data from Porland"
            ),
            tablerTimelineItem(
              title = "Item 3",
              status = NULL,
              date = "01-07-2020",
              "Charl modifies data schema and add a new method to define his KPI"
            )
          )
        ),
        column(
          width = 8,
          tablerCard(
            width = 12,
            title = "Data Preview",
            column(
              width = 12,
              uiOutput("dfSummary"),
              style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
            ),
            status = "success",
            statusSide = "left"
          )
        )
      )
    ),
    
    # Global Variables Definition ---------------------------------------------
    
    tablerTabItem(
      tabName = "globVars",
      fluidRow(h3("Global Variables Definitions")), 
      fluidRow("Specify key variables to study"),
      br(),
      fluidRow(
        tablerBlogCard(
          title = "Variables Selection:",
          br(),
          fluidRow(
            column(
              align="center",
              width = 6,
              mod_global_vars_info_ui("varinfo", varVec)
            ),
            # for select input
            column(
              width = 6,
              align="center",
              mod_global_vars_ui("globVars")
            )
          ),
          
          # once save or set global vars, then addonColor becoming success
          author =tablerTag(name = "status", addon = "saved", addonColor = "success"),
          date = Sys.Date(),
          href = NULL,
          src = NULL,
          avatarUrl = NULL,
          width = 12,
          "Contents"
        )
      )
    ),
    
    # Deep Dive ---------------------------------------------------------------

    tablerTabItem(
      tabName = "deepDive",
      fluidRow(
        useShinyjs(),
        h3("Deep Dive"),
        column(
          width = 12, offset = 8,
          align = "left",
          
          div(style = "display: inline-block; width: 80px;", h2("Setup")),
          hidden(div(style = "display: inline-block; width: 50px;",
              mod_pushbar_ui("dd_setup", from = "right", mod_udf_dd_ui(NS("dd_setup", "udf_dd")))
          )),
          
          div(style = "display: inline-block; width: 80px;",
              dropdown(
                size = "sm",
                tooltip = tooltipOptions(title = "Edit inputs"),
                tags$h3("List of Input"),
                # mod_addFilterInputs4DF_ui("monitorDashboard", elsewhere = T),
                selectInput(
                  "dd_filterItems", 
                  label = "Select variables used for more granularity", choices = NULL, multiple = TRUE
                ),
                uiOutput("dd_filterSelectInputs"),
                
                style = "unite",
                icon = icon("gear"),
                status = "default",
                width = "200px",
                animate = animateOptions(
                  enter = animations$fading_entrances$fadeInLeft,
                  exit = animations$fading_exits$fadeOutLeft
                )
              ),
          ),
          
          div(style = "display: inline-block; vertical-align:right; width: 120px;",
              h2("Advanced")),
          div(style = "display: inline-block; vertical-align:right; width: 100px;",
              switchInput(
                inputId = "adv",
                label = tablerIcon("bar-chart")
              )
          ),
        )
      ),
      br(),
      ## deep dive basic view -----
      div(id = "deepDiveBasic",
          tagList(
            fluidRow(
              lapply(1:6, function(x) {
                mod_tablerKpiCard_ui(paste0("kpi", x))
              })
            ),
            fluidRow(h4("One Variate Analysis")),
            fluidRow(
              column(
                width = 10,
                tablerCard(
                  title = "One Variate Analysis",
                  mod_oneVar_ui("oneVar"),
                  status = "info",
                  statusSide = "left",
                  width = 12,
                ),
              ),
              column(
                2,
              )
            ),
            fluidRow(h4("Bi-Variate Analysis")),
            fluidRow(
              column(
                width = 10,
                tablerCard(
                  title = "Bi-Variate Analysis",
                  options = tagList(
                  ),
                  mod_biVars_ui("biVars"),
                  status = "info",
                  statusSide = "left",
                  width = 12
                ),
              ),
              column(
                2
              )
            ),
            
            fluidRow(h4("Time Seriest Analysis")),
            fluidRow(
              column(
                width = 10,
                tablerCard(
                  title = "Time Seriest Analysis",
                  options = tagList(
                  ),
                  mod_tsAnalysis_ui("tsAnalysis"),
                  status = "info",
                  statusSide = "left",
                  width = 12
                ),
              ),
              column(
                2,
              )
            ),
            # bs4TabSetPanel()
          )
      ),
      ## deep dive advanced view -----
      shinyjs::hidden(
        div(
          id = "deepDiveAdv", 
          tagList(
            tablerNavMenu(
              tablerNavMenuItem(
                tabName = "datStructure",
                icon = "aperture",
                "Data Structrue"
              ),
              tablerNavMenuItem(
                tabName = "pivotTable",
                icon = "layout",
                "PivotTable"
              ),
              tablerNavMenuItem(
                tabName = "multView",
                icon = "grid",
                "Multiple View Analysis"
              ),
            ),
            tablerTabItems(
              tablerTabItem(
                tabName = "pivotTable",
                fluidRow(
                  sidebarLayout(
                    sidebarPanel(
                      mod_pivottable_row_ui("pvt"),
                      mod_pivottable_col_ui("pvt")
                    ),
                    mainPanel(
                      mod_pivottable_ui("pvt")
                    )
                  )
                )
              ),
              tablerTabItem(
                tabName = "datStructure",
                fluidRow(
                  tablerCard(
                    width = 8,
                    title = "Data Structrue Overview",
                    mod_data_summary_viz_ui("vizDat"),
                    statusSide = "left"
                  ),
                  tablerCard(
                    width = 4,
                    title = "More Details about Data...",
                    mod_data_summary_tbl_ui("datSummTbl"),
                    statusSide = "left",
                    overflow = TRUE
                  )
                )
              ),
              tablerTabItem(
                tabName = "multView",
                fluidRow(
                  mod_mul_view_ui("mulView")
                )
              )
            ), 
          )
        )
      )
    ),
    # Insurance Monitoring ----------------------------------------------------
    
    tablerTabItem(
      tabName = "insurMonit",
      fluidRow(
        column(3, selectInput("assign1", "Calim count", choices = "")),
        column(3, selectInput("assign2", "Exposure", choices = "")),
        column(3, selectInput("assign3", "Cost", choices = "")),
        column(3, selectInput("assign4", "Burning Cost", choices = "")),
        column(3, selectInput("assign5", "Date", choices = "")),
        column(3, selectInput("assign6", "Premium", choices = "")),
        column(3, selectInput("unit", "unit", choices = c("week", "month", "year"), selected = "month")),
        column(3,
               numericInput(
                 "threshold_ins",
                 "threshold",
                 10,
                 min = NA,
                 max = NA,
                 step = 1,
                 width = NULL
               )),
        column(3, actionButton("confirm_monit", "Confirm")),

      ),
      fluidRow(
        column(
          2,
          tags$div(
            # style = "position:fixed;width:200px;",
            h3("Contents"),
            
            dropdown(
              size = "sm",
              tooltip = tooltipOptions(title = "Edit inputs"),
              tags$h3("List of Input"),
              # mod_addFilterInputs4DF_ui("monitorDashboard", elsewhere = T),
              selectInput(
                "filterItems", 
                label = "Select variables used for more granularity", choices = NULL, multiple = TRUE
              ),
              
              uiOutput("filterSelectInputs"),
              
              style = "unite",
              icon = icon("gear"),
              status = "default",
              width = "200px",
              animate = animateOptions(
                enter = animations$fading_entrances$fadeInLeft,
                exit = animations$fading_exits$fadeOutLeft
              )
            ),
            
            dashboardSidebar(
              sidebarMenu(
                id = "monit_tabs",
                menuItem("Overview", tabName = "TAB1", icon = icon("dashboard")),
                menuItem("Details", tabName = "TAB2", icon = icon("dashboard"))
              )
            )
            # h3("Filters"),
            # mod_addFilterInputs4DF_putui_elsewhere_ui("monitorDashboard")
          )
        ),
        column(
          10,
          offset = 2,
          dashboardBody(
            tabItems(
              tabItem(
                tabName = "TAB1",
                h2("Dashboard tab content"),
                fluidRow(uiOutput("kpi_monit_infoboxs")),
                fluidRow(
                  tablerCard(
                    width = 12,
                    title = "Loss Ratio Over Time",
                    plotlyOutput("lossRatioTS"),
                    status = "success",
                    statusSide = "left"
                  )
                ),
                fluidRow(
                  tablerCard(
                    width = 6,
                    title = "Severity Over Time",
                    plotlyOutput("severityTS"),
                    status = "success",
                    statusSide = "left"
                  ),
                  tablerCard(
                    width = 6,
                    title = "Frequency Ratio Over Time",
                    plotlyOutput("frequencyTS"),
                    status = "success",
                    statusSide = "left"
                  )
                ),
              ), 
              tabItem(
                tabName = "TAB2",
                fluidRow(h2("Details accross subgroups")),
                fluidRow(
                  dropdown(
                    tags$h3("List of Input"),
                    pickerInput(
                      inputId = 'monitGrp',
                      label = 'Select a group by variable',
                      choices = "",
                      options = list(`style` = "btn-info")
                    ),
                    pickerInput(
                      inputId = 'monitGrp_unit',
                      label = 'Choose a period for analysis',
                      choices = c("week", "month", "year"),
                      options = list(`style` = "btn-info"),
                      selected = "month"
                    ),
                    
                    style = "unite", icon = icon("gear"),
                    status = "success", width = "300px",
                    animate = animateOptions(
                      enter = animations$fading_entrances$fadeInLeft,
                      exit = animations$fading_exits$fadeOutLeft
                    )
                  )
                ),
                fluidRow(
                  tablerCard(
                    width = 12,
                    title = "KPIs under subgroups",
                    plotlyOutput("kpisGrp", height = "650px"),
                    status = "success",
                    statusSide = "left",
                  ),
                ),
                
                fluidRow(
                  tablerCard(
                    width = 12,
                    title = "KPI time series with subgroups",
                    uiOutput("kpiGrpTS"),
                    status = "success",
                    statusSide = "left",
                  )
                )
                
              )
            )
          )
        )
      )
    ),
    
    # Overiview ---------------------------------------------------------------
    tablerTabItem(
      tabName = "overview",
      "Contents here"
    ),
    
    # Modeling ------------------------------------------------------------
    tablerTabItem(
      tabName = "modeling",
      "Contents here",
      
      div(
        id = "deepDiveAdv1", 
        tagList(
          tablerNavMenu(
            tablerNavMenuItem(
              tabName = "Builder",
              icon = "aperture",
              "Builder"
            ),
            tablerNavMenuItem(
              tabName = "Evaluator",
              icon = "layout",
              "Evaluator"
            ),
            tablerNavMenuItem(
              tabName = "Combiner",
              icon = "layout",
              "Combiner"
            )
          ),
          
          tablerTabItems(
            tablerTabItem(
              tabName = "Builder", 
              fluidRow(
                column(6,
                       shinydashboard::box(
                         "Box content here", br(), "More box content",
                         textInput("modelUser", "User:"),
                         textInput("modelName", "Name:"),
                         fileInput("modelrda", "Choose existing model", accept = ".rda")
                       )),
                column(6,
                       shinydashboard::box(
                         "Model set", br(), "Specify glmnet parameters",
                         numericInput("lambda", "lambda:", 1, min = 0, max = 100),
                         numericInput("alpha", "alpha:", 1, min = 0, max = 100),
                         selectInput("modelfamily", "Family", choice = c("poisson", "gaussian", "gamma")),
                         actionButton("runModel", "Run"),
                         actionButton("saveModel", "Save Model")
                       ))
                
              )
            ),
            tablerTabItem(
              tabName = "Evaluator", 
              h2("model summary"),
              actionButton("evaModel", "Evaluate Model"),
              fluidRow(
                verbatimTextOutput("console_text1"),
                verbatimTextOutput("console_text2")
              )
            ),
            tablerTabItem(
              tabName = "Combiner", 
              fluidRow(
                fileInput("modelrda1", "Choose model 1", accept = ".rda"),
                fileInput("modelrda2", "Choose model 2", accept = ".rda")
              ),
              fluidRow(actionButton("combineModel", "Combine Model")),
              fluidRow(dataTableOutput('CombinerRes')),
              fluidRow(dataTableOutput('CombinerResAdj'))
            )
          )
        )
      )
      
    )
    
  )
)

# Main UI -----------------------------------------------------------------

ui <- tablerDashPage(

  # log in UI
  mod_login_ui("login"),
  # main UI
  shinyjs::hidden(
    div(
      id = "page",
      tablerDashPage(
        footer = tablerDashFooter(
          copyrights = "EAH, 2020"), #?=====can not showing??=====#
        navbar = nav,
        title = "Aeda for hunter",
        body = body
      )
    )
  )


  


)
