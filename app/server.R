library(jsonlite)
library(httr)
library(shinyjs)
library(data.table)
library(tidyverse)
library(DT)
library(shinyWidgets)
library(shiny.reglog)
library(mongolite)
library(emayili)


server <- function(input, output, session){
  
  # Java Script
  jsCode <- '
        shinyjs.reset_anim = function() {
          var src = "home_page.gif";
          var div_elem = document.getElementById("homeUI");
          var img_elem = div_elem.getElementsByTagName("img")[0];
          if (!img_elem) {
            console.error("No img elements found inside homeUI");
            return;
           }
          var src_value = img_elem.getAttribute("src");
          img_elem.setAttribute("src", "");
          img_elem.setAttribute("src", src_value);
        }'
  
  # Global variables
  searchRes <- reactiveValues(dt = NULL)
  pgLoadJS <- reactiveVal(paste0('setTimeout(function() {table.page(', 0 ,').draw(false);}, 10);'))
  insertCnt <- reactiveVal() #reactive search box
  
  # User authentication/registration
  dbConnector <- RegLogMongoConnector$new(
                  Sys.getenv("MONGO_URL"),
                  "app",
                  mongo_options = mongolite::ssl_options(),
                  collections = c("account", "reset_code", "logs"),
                  custom_handlers = NULL
                 )

  mailConnector <- RegLogEmayiliConnector$new(
                    from = Sys.getenv("MAIL_USERNAME"),
                    smtp = emayili::server(host = Sys.getenv("MAIL_HOST"),
                                           port = Sys.getenv("MAIL_PORT"),
                                           username = Sys.getenv("MAIL_USERNAME"),
                                           password = Sys.getenv("MAIL_PASSWORD"))
                   )

  RegLog <- RegLogServer$new(
    dbConnector = dbConnector,
    mailConnector = mailConnector,
    app_name = "SciLitMiner",
    module_id = "cust_id",
    use_modals = list(
      login_success = FALSE,
      logout_success = FALSE
      )
  )
  
  output$authreg <- renderUI({
    RegLog_login_UI("cust_id")
    #RegLog_register_UI("cust_id")
    # tagList(
    #   RegLog_login_UI("cust_id"),
    #   br(),
    #   p(
    #     a("Imprint", href="https://www.hereon.de/innovation_transfer/communication_media/imprint/index.php.en", target="_blank"),
    #     "|",
    #     a("Data Protection", href="https://www.hereon.de/innovation_transfer/communication_media/imprint/privacy_policy/index.php.en", target="_blank")
    #   )
    # )
  })
  
  # output$authreg <- renderUI({
  #   tagList(
  #     RegLog_login_UI("cust_id"),
  #     br(),
  #     actionLink(inputId = "signup", label = "Signup"),
  #     actionLink(inputId = "resetpass", label = "Forgot Password?")
  #   )
  # })
  
  
  # observeEvent(input$login, {
  #   output$authreg <- renderUI({
  #     tagList(
  #       RegLog_login_UI("cust_id"),
  #       br(),
  #       actionLink(inputId = "signup", label = "Signup"),
  #       actionLink(inputId = "resetpass", label = "Forgot Password?")
  #     )
  #   })
  # })
  
  # observeEvent(input$signup, {
  #   output$authreg <- renderUI({
  #     tagList(
  #       RegLog_register_UI("cust_id"),
  #       br(),
  #       actionLink(inputId = "login", label = "Login"),
  #       actionLink(inputId = "resetpass", label = "Forgot Password?")
  #     )
  #   })
  # })
  
  # observeEvent(input$resetpass, {
  #   output$authreg <- renderUI({
  #     tagList(
  #       RegLog_resetPass_UI("cust_id"),
  #       br(),
  #       actionLink(inputId = "login", label = "Login"),
  #       actionLink(inputId = "signup", label = "Signup")
  #     )
  #   })
  # })
  
  
  observeEvent(input$timeOut, {
    if (RegLog$is_logged()) {
      updateTabsetPanel(session, "tabs", selected = "logout")
      showModal(modalDialog(
        title = "Timeout",
        paste("Automatically logged out due to", input$timeOut, "inactivity -", Sys.time()),
        footer = NULL,
        easyClose = TRUE
      )) 
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  ##################### input$tabs events #######################
  
  observeEvent(input$tabs, {
    req(RegLog$is_logged())
    if (input$tabs == "logout") {
      
      # Dumping elements and variables before logging out
      searchRes <- reactiveValues(dt = NULL)
      insertCnt(NULL)
      RegLog$logout()
      if (!is.null(input$tabs)) {
        removeTab("tabs", "home")
        removeTab("tabs", "Literature Collection")
        removeTab("tabs", "Literature Selection")
        removeTab("tabs", "Mining")
        #removeTab("tabs", "Analytics")
        #removeTab("tabs", "User Profile")
        removeTab("tabs", "about")
        removeTab("tabs", "logout")
      }
      #shinyjs::show("authreg")
      showTab("tabs", "login_element", session, select = TRUE)
    } else if (input$tabs == "search") {
      
      # Showing search information
      output$searchInfoTag <- renderText({
        paste("<b>INFO</b>")
      })
      
      #Search: Info
      output$searchInfo <- renderText({
        paste("Ignore: Exclude the search criteria defined in the search box",
              "Mandatory: The search criteria defined in the search box must match",
              "Phrase: Consider the search keywords defined in the search box as phrase",
              "Match All Terms: All the search keywords defined in the search box must match",
              sep = "\n"
        )
      })
      output$searchResult <- NULL
    } else if (input$tabs == "home") {
      
      # Showing home page
      output$homeUI <- renderUI({
        tags$div(
          tags$div(
            h5("NOTE: This is the demo version of SciLitMiner. Many features, such as automatic plot digitisation and named entity extraction, are still under development and/or testing. Stay tuned!"),
            style = "text-align:center;display:inline;position:relative;transition:translateY(-50%);padding-right:2%;color:#0000ff;"
          ),
          tags$br(),
          tags$div(class = "typewriter",
                   tags$h4("Let's Enhance Our Knowledge through the Synergistic Evaluation of Scientific Data"),
                   tags$style(HTML("
                    .typewriter {
                      display: flex;
                      position: relative;
                      justify-content: center;
                      text-align: center;
                      text-transform: uppercase;
                    }
                  
                    .typewriter h4 {
                      color: #3D3839;
                      width: auto;
                      display: inline-block;
                      overflow: hidden; 
                      border-right: .20em solid transparent; /* #FFC604 */ 
                      white-space: nowrap;
                      letter-spacing: .15em;
                      margin: 0;
                      margin-right auto;
                      animation: 
                        typing 5s steps(80), /* end */
                        blink-caret 6s; #step-end infinite; /* .75s */
                    }
                  
                    @keyframes typing {
                      from { max-width: 0 }
                      to { max-width: 100% }
                    }
                  
                    @keyframes blink-caret {
                      from, to { border-color: transparent }
                      50% { border-color: #FFC604 }
                    }"
                   ))
          ),
          tags$br(),
          tags$div(class = "gif-container",
                   img(src = "home_page.gif", 
                       width = "1000px"),
                   tags$style(HTML("
                      .gif-container {
                        display:flex;
                        position:relative;
                        justify-content: center;
                      }"))
          )
        )
      })
      #js$reset_anim()
      shinyjs::delay(10, js$reset_anim())
    } else if (input$tabs == "createColl") {
      disable("submitCollectionJob")
    } else if (input$tabs == "exploreColl") {
      
      # Explore Collection
      output$dtCollAll <- renderDataTable({
        
        xformJobDT <- function(dt) {
          if (nrow(dt)) {
            dt[, job_status := as.character(job_status)]
            dt[job_status == "1", job_status := "pending"]
            dt[job_status == "2", job_status := "in progress"]
            dt[job_status == "3", job_status := "completed"]
            dt[job_status == "9", job_status := "failed"]
            dt[, job_status := factor(job_status)]
            setnames(dt, c("_id"), c("coll_name"))
            setcolorder(dt, c("coll_name", "coll_desc"))
          }
          return(dt)
        }
        
        # Call REST endpoint
        tryCatch({
          getURL <- paste0(lstRESTRoutes$GET_JOBS, "?", "job_type=COLGEN")
          resp <- httr::GET(getURL)
        }, warning = function(w) {
          showNotification(paste("The following warning occurred while establishing a connection to the server:", w),
                           type = "warning",
                           duration = 7)
          return()
        }, error = function(e) {
          showNotification(paste("The following error occurred while establishing a connection to the server:", e),
                           type = "error",
                           duration = 7)
          return()
        })
        if (!exists("resp")) {
          showNotification("The Collection data couldn't be downloaded", 
                           duration = 7,
                           action = a(href = "javascript:location.reload();", "Reload page"))
        } else if (resp$status_code != 200) {
          showNotification("The Collection data couldn't be downloaded", 
                           duration = 7,
                           action = a(href = "javascript:location.reload();", "Reload page")
          )
        } else {
          content <- httr::content(resp)
          dt <- data.table::rbindlist(content) %>% 
            xformJobDT() %>% 
            .[, -"job_type"] %>%
            DT::datatable(filter = list(position = 'top', clear = FALSE), 
                          rownames = F,
                          extensions = 'FixedColumns',
                          options = list(paging = FALSE,
                                         columnDefs = list(list(width = '300px', targets = c(1,7))),
                                         pageLength = 10,
                                         autoWidth = T,
                                         scrollX = T,
                                         language = list(
                                           zeroRecords = "No records found")),
                          selection = "none",
                          escape = F)
        }
      })
      
      # Explore Collection my
      output$dtCollMy <- renderDataTable({
        
        xformJobDT <- function(dt) {
          if (nrow(dt)) {
            dt[, job_status := as.character(job_status)]
            dt[job_status == "1", job_status := "pending"]
            dt[job_status == "2", job_status := "in progress"]
            dt[job_status == "3", job_status := "successful"]
            dt[job_status == "9", job_status := "failed"]
            dt[, job_status := factor(job_status)]
            setnames(dt, c("_id"), c("coll_name"))
            setcolorder(dt, c("coll_name", "coll_desc"))
          }
          return(dt)
        }
        
        # Call REST endpoint
        tryCatch({
          getURL <- paste0(lstRESTRoutes$GET_JOBS, "?", "user_id=", RegLog$account_id(), "&job_type=COLGEN")
          resp <- GET(getURL)
        }, warning = function(w) {
          showNotification(paste("The following warning occurred while establishing a connection to the server:", w),
                           type = "warning",
                           duration = 7)
          return()
        }, error = function(e) {
          showNotification(paste("The following error occurred while establishing a connection to the server:", e),
                           type = "error",
                           duration = 7)
          return()
        })
        if (!exists("resp")) {
          showNotification("The Collection data couldn't be downloaded", 
                           duration = 7,
                           action = a(href = "javascript:location.reload();", "Reload page"))
        } else if (resp$status_code != 200) {
          showNotification("The Collection data couldn't be downloaded", 
                           duration = 7,
                           action = a(href = "javascript:location.reload();", "Reload page")
          )
        } else {
          content <- httr::content(resp)
          dt <- data.table::rbindlist(content) %>% 
            xformJobDT() %>% 
            .[, -"job_type"] %>%
            DT::datatable(filter = list(position = 'top', clear = FALSE), 
                          rownames = F,
                          extensions = 'FixedColumns',
                          options = list(paging = FALSE,
                                         columnDefs = list(list(width = '300px', targets = c(1,7))),
                                         pageLength = 10,
                                         scrollX = T,
                                         autoWidth = T,
                                         language = list(
                                           zeroRecords = "No records found")),
                          selection = "none",
                          escape = F)
        }
      })
    } else if (input$tabs == "about") {
      disable("submitContactForm")
    } else if (input$tabs == "apiKeyMgmt") {
      
      # API Keys Retrieve
      tryCatch({
        getURL <- paste0(lstRESTRoutes$GET_API_KEYS, "?", "account_id=", RegLog$account_id())
        resp <- httr::GET(getURL)
      }, warning = function(w) {
        showNotification(paste("The following warning occurred while establishing a connection to the server:", w),
                         type = "warning",
                         duration = 7)
        removeModal()
      }, error = function(e) {
        showNotification(paste("The following error occurred while establishing a connection to the server:", e),
                         type = "error",
                         duration = 7)
        removeModal()
      })
      if (exists("resp")) {
        if (resp$status_code != 200) {
          showNotification("The api keys couldn't be retrieved",
                           duration = 7,
                           action = a(href = "javascript:location.reload();", "Reload page"))
          data.table(api_keys = c(""))
        } else {
          content <- httr::content(resp)
          dt <- data.table::rbindlist(content)
          updateTextInput(session, "elsevier_api_key", value = dt$elsevier_api_key)
          updateTextInput(session, "springer_api_key", value = dt$springer_api_key)
          updateTextInput(session, "wiley_api_key", value = dt$wiley_api_key)
        }
      } else {
        data.table(api_keys = c(""))
      }
    } else if (input$tabs == "mine") {
      
      # mining domain info
      domain_info <- reactive({
        tryCatch({
          resp <- httr::GET(lstRESTRoutes$GET_DOMAIN_INFO)
        }, warning = function(w) {
          showNotification(paste("The following warning occurred while establishing a connection to the server:", w),
                           type = "warning",
                           duration = 7)
          removeModal()
        }, error = function(e) {
          showNotification(paste("The following error occurred while establishing a connection to the server:", e),
                           type = "error",
                           duration = 7)
          removeModal()
        })
        if (exists("resp")) {
          if (resp$status_code != 200) {
            showNotification("The domain info couldn't be retrieved",
                             duration = 7,
                             action = a(href = "javascript:location.reload();", "Reload page"))
            data.table(domain_name=c(""), mining_opts=c(""))
          } else {
            content <- httr::content(resp)
            data.table(domain_name=content, mining_opts=c(""))
          }
        } else {
          data.table(domain_name=c(""), mining_opts=c(""))
        }
      })
      
      # ir sub collection names
      ir_subcoll_names <- reactive({
        tryCatch({
          resp <- httr::GET(lstRESTRoutes$GET_IR_SUBCOLL_INFO)
        }, warning = function(w) {
          showNotification(paste("The following warning occurred while establishing a connection to the server:", w),
                           type = "warning",
                           duration = 7)
          removeModal()
        }, error = function(e) {
          showNotification(paste("The following error occurred while establishing a connection to the server:", e),
                           type = "error",
                           duration = 7)
          removeModal()
        })
        if (exists("resp")) {
          if (resp$status_code != 200) {
            showNotification("The IR Sub-collection names couldn't be retrieved",
                             duration = 7,
                             action = a(href = "javascript:location.reload();", "Reload page"))
            data.table(domain_name=c(), mining_opts=c())
          } else {
            content <- httr::content(resp)
            data.table::rbindlist(content)
          }
        } else {
          data.table(subcoll=c(""), description=c(""))
        }
      })
      
      # Setup Mining UI
      output$mineUI <- renderUI({
        tagList(
          fluidRow(
            column(12, textInput(inputId = "mining_job_name", 
                                 label = "Job Name", 
                                 width = "70%")),
            column(12, textAreaInput("mining_job_desc", "Job Description",
                                     placeholder = "Ex.: this job aims to mine titanium aluminide literature that includes creep dataset",
                                     width = "70%",
                                     height = "80px")),
            column(12, selectInput(inputId = "ir_subcoll", 
                                   label = "Select a Sub-Collection", 
                                   choices = ir_subcoll_names()$`_id`,
                                   width = "70%")),
            column(12, selectInput(inputId = "domain", 
                                   label = "Select a Domain", 
                                   choices = unique(domain_info()$domain_name),
                                   width = "70%")),
            column(12, pickerInput(inputId = "mining_opts",
                                   label = "Select Mining Options", 
                                   choices = NULL,
                                   options = list(
                                     `actions-box` = TRUE,
                                     `deselect-all-text` = "Unselect All",
                                     `select-all-text` = "Select All",
                                     `none-selected-text` = "Unselect All"),
                                   selected = NULL,
                                   multiple = TRUE,
                                   width = "83.7%")),
            column(12, actionButton("submitMiningJob",
                                    "Submit Job",
                                    icon = icon("paper-plane", verify_fa = FALSE),
                                    style = "bordered; white-space:normal",
                                    width = "70%")),
            column(12, br()),
            column(12, conditionalPanel(condition = "input.mining_opts.includes('miscner')",
                                        style = "border-top: 1px dotted #D3D3D3;")),
            column(12, conditionalPanel(condition = "input.mining_opts.includes('miscner')",
                                        h5(HTML("<b>Miscellaneous NER</b>")))),
            column(6, conditionalPanel(condition = "input.mining_opts.includes('miscner')",
                                       actionButton("insertMiscNER", 
                                                    "Add", 
                                                    icon = icon("plus", verify_fa = FALSE), 
                                                    style = "bordered; white-space:normal",
                                                    width = "80%"))),
            column(6, conditionalPanel(condition = "input.mining_opts.includes('miscner')",
                                       actionButton("removeMiscNER", 
                                                    "Remove", 
                                                    icon = icon("minus", verify_fa = FALSE), 
                                                    style = "bordered; white-space:normal",
                                                    width = "80%")))
          ),
          showModal(modalDialog(
            title = "Note",
            paste("This feature is still under development and/or testing. Stay tuned!"),
            footer = NULL,
            easyClose = TRUE
          )) 
        )
      })
      
      # update mining options based on domain name
      observeEvent(input$domain, {
        updatePickerInput(session, 
                          inputId = "mining_opts", 
                          label = "Select Mining Options",
                          choices = domain_info()[domain_name == input$domain][["mining_opts"]])
      }, ignoreInit = TRUE)
    } else if (input$tabs == "allminejobs") {
      # Mining Job Status
      output$dtMineJobStatus <- renderDataTable({
        
        xformMineJobDT <- function(dt) {
          if (nrow(dt)) {
            dt[, job_status := as.character(job_status)]
            dt[job_status == "1", job_status := "pending"]
            dt[job_status == "2", job_status := "in progress"]
            dt[job_status == "3", job_status := "successful"]
            dt[job_status == "9", job_status := "failed"]
            dt[, job_status := factor(job_status)]
            setnames(dt, c("_id"), c("job_name"))
            setcolorder(dt, c("job_name", "job_desc"))
          }
          return(dt)
        }
        
        # Call REST endpoint
        tryCatch({
          getURL <- paste0(lstRESTRoutes$GET_JOBS, "?", "job_type=MINING")
          resp <- GET(getURL)
        }, warning = function(w) {
          showNotification(paste("The following warning occurred while establishing a connection to the server:", w),
                           type = "warning",
                           duration = 7)
          return()
        }, error = function(e) {
          showNotification(paste("The following error occurred while establishing a connection to the server:", e),
                           type = "error",
                           duration = 7)
          return()
        })
        if (!exists("resp")) {
          showNotification(paste("The Jobs status couldn't be retrieved due to missing service REST endpoint"),
                           type = "error", 
                           duration = 7)
          return()
        } else if (resp$status_code != 200) {
          showNotification(paste("The Job status data couldn't be retrieved",
                                 httr::content(resp, type="application/json")),
                           duration = 7,
                           action = a(href = "javascript:location.reload();", "Reload page")
          )
        } else {
          content <- httr::content(resp)
          dt <- data.table::rbindlist(content) %>%
            xformMineJobDT() %>% 
            DT::datatable(filter = list(position = 'top', clear = FALSE), 
                          rownames = F,
                          extensions = 'FixedColumns',
                          options = list(paging = FALSE,
                                         pageLength = 10,
                                         scrollX = TRUE,
                                         language = list(
                                           zeroRecords = "No records found")),
                          selection = "none",
                          escape = F)
        }
      })
    } else if (input$tabs == "evaluateMine") {
      
      # Evaluation job
      # Success mine jobs name
      success_mine_job_names <- reactive({
        tryCatch({
          getURL <- paste0(lstRESTRoutes$GET_JOBS, "?", "job_type=MINING&job_status=3")
          resp <- GET(getURL)
        }, warning = function(w) {
          showNotification(paste("The following warning occurred while establishing a connection to the server:", w),
                           type = "warning",
                           duration = 7)
          removeModal()
        }, error = function(e) {
          showNotification(paste("The following error occurred while establishing a connection to the server:", e),
                           type = "error",
                           duration = 7)
          removeModal()
        })
        if (exists("resp")) {
          if (resp$status_code != 200) {
            showNotification("The successful mining jobs couldn't be retrieved",
                             duration = 7,
                             action = a(href = "javascript:location.reload();", "Reload page"))
            c()
          } else {
            content <- httr::content(resp)
            data.table::rbindlist(content)$`_id`
          }
        } else {
          c()
        }
      })
      
      # Evaluate Mining UI
      output$evalMineUI <- renderUI({
        fluidRow(
          column(12, selectInput(inputId = "eval_mine_job_name", 
                                 label = "Select a Mining Job", 
                                 choices = success_mine_job_names(),
                                 width = "50%")),
          column(12, actionButton("loadMineResults",
                                  "Load Mining Results",
                                  icon = icon("download", verify_fa = FALSE),
                                  style = "bordered; white-space:normal",
                                  width = "50%")),
          #column(12, br()),
          column(12, hr(style = "border-top: 1px solid #D3D3D3;")),
          column(12, h5(HTML("<b>Evaluation Modes</b>"))),
          disabled(column(3, actionButton("textMiningEval", 
                                          "Text", 
                                          icon = icon("file-alt", verify_fa = FALSE), 
                                          style = "bordered; white-space:normal",
                                          width = "70%"))),
          disabled(column(3, actionButton("plotMiningEval", 
                                          "Plot", 
                                          icon = icon("chart-line", verify_fa = FALSE), 
                                          style = "bordered; white-space:normal",
                                          width = "70%"))),
          disabled(column(3, actionButton("tableMiningEval", 
                                          "Table", 
                                          icon = icon("table", verify_fa = FALSE), 
                                          style = "bordered; white-space:normal",
                                          width = "70%"))),
          column(12, hr(style = "border-top: 1px solid #D3D3D3;")),
          #column(12, br()),
          column(12, h5(HTML("<b>Navigate Documents</b>"))),
          disabled(column(4, actionButton("previousDoc", 
                                          "", 
                                          icon = icon("arrow-left", verify_fa = FALSE), 
                                          style = "bordered; white-space:normal",
                                          width = "100%"))),
          disabled(column(4, actionButton("nextDoc", 
                                          "", 
                                          icon = icon("arrow-right", verify_fa = FALSE), 
                                          style = "bordered; white-space:normal",
                                          width = "100%"))),
          column(4)
        )
      })
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  observeEvent(RegLog$is_logged(), {
    
    # Features to display after logging in
    if (RegLog$is_logged()) {
      
      server_name <- Sys.getenv("REST_ENDPOINT")
      lstRESTRoutes <<- list(GET_JOBS = paste0(server_name, "getJobs"),
                            POST_JOB = paste0(server_name, "createJob"),
                            POST_IR_SEARCH = paste0(server_name, "postIRSearch"),
                            POST_IR_RESULT = paste0(server_name, "saveIRResult"),
                            GET_DOMAIN_INFO = paste0(server_name, "getDomainInfo"),
                            GET_IR_SUBCOLL_INFO = paste0(server_name, "getIRSubCollInfo"),
                            GET_MINE_RESULT = paste0(server_name, "getMineResult"),
                            POST_API_KEYS = paste0(server_name, "postAPIKeys"),
                            GET_API_KEYS = paste0(server_name, "getAPIKeys"),
                            GET_SYNONYMS = paste0(server_name, "getSynonyms"),
                            POST_SYNONYMS = paste0(server_name, "postSynonyms"),
                            GET_SYNONYM_DOMAIN_NAMES = paste0(server_name, "getSynonymDomainNames")
      )
      
      #Crete Collection: Job form validation
      output$collectionJobFormValidate <- reactive({
        shiny::validate(
          need(input$coll_name != "", label = "Collection Name"),
          need(input$coll_desc != "", label = "Collection Description"),
          need(input$search_crit_1 != "", label = "Primary Search Topic")
        )
      })
      
      output$searchInfoTag <- renderText({
        paste("<b>INFO</b>")
      })
      
      #Search: Info
      output$searchInfo <- renderText({
        paste("Mandatory: the search criteria defined in the search box must match",
              "Phrase: consider the search keywords defined in the search box as phrase",
              "Match All Terms : all the search keywords defined in the search box must match",
              sep = "\n"
        )
      })
      
      # Data Mining: Job Validation
      output$miningJobFormValidate <- reactive({
        shiny::validate(
          need(input$mining_job_name != "", label = "Job name"),
          need(input$mining_job_desc != "", label = "Job description"),
          need(input$mining_opts != "", label = "At least one mining option")
        )
      })
      
      # Data Mining: Job Info
      output$miningJobInfo <- renderText({
        paste("qty:     quantity extraction from text",
              "tbl:     table data extraction",
              "pltdgtz: plot digitization",
              "matcomp: material composition",
              "miscner: miscellaneous named entity recognition",
              sep = "\n"
        )
      })
      
      #runjs(inactivity)
      #shinyjs::hide("authreg")
      hideTab("tabs", "login_element", session)
      #removeTab("tabs", "login")
      # backgroundImageCSS <- "/* background-color: #cccccc; */
      #                  height: 85vh;
      #                  background-position: center;
      #                  background-repeat: no-repeat;
      #                  background-size: cover;
      #                  -webkit-background-size: cover;
      #                  -moz-background-size: cover;
      #                  -o-background-size: cover;
      #                  background-image: url('%s');
      #                  "
      
      appendTab("tabs",
                tabPanel("Home",
                         value = "home",
                         uiOutput("homeUI"),
                         extendShinyjs(text = jsCode, functions = c("reset_anim"))
                         #style = sprintf(backgroundImageCSS, "index.jpg"),
                ),
                select = TRUE
      )
      appendTab("tabs",
                navbarMenu(
                  # UI for Collection Menu
                  "Literature Collection",
                  tabPanel("Create Collection",
                           value = "createColl",
                           #titlePanel("Job Form for Collection Generation"),
                           sidebarLayout(
                             sidebarPanel(
                               #textInput("job_name", "Job Name:*", placeholder = "Ex.: myjob1"),
                               textInput("coll_name", "Collection Name:", placeholder = "Ex.: tialcreep"),
                               textAreaInput("coll_desc", "Collection Description:",
                                             placeholder = "Ex.: this collection contains scholarly articles on titanium aluminide with focus on creep property"),
                               textInput("search_crit_1", "Primary Search Topic:", placeholder = "Ex.: titanium aluminide"),
                               textInput("search_crit_2", "Secondary Search Topic:", placeholder = "Ex.: creep"),
                               textInput("additional_key_1", "Additional Search Key 1:", placeholder = "Ex.: TiAl"),
                               textInput("additional_key_2", "Additional Search Key 2:", placeholder = "Ex.: γ-TiAl"),
                               actionButton("submitCollectionJob",
                                            "Submit Job",
                                            icon = icon("paper-plane", verify_fa = FALSE),
                                            style = "bordered; white-space:normal"),
                               actionButton("resetCollectionJobInputs",
                                            "Reset Inputs",
                                            icon = icon("sync", verify_fa = FALSE),
                                            style = "bordered; white-space:normal")
                             ),
                             mainPanel(
                               h5(HTML("<b>VALIDATION</b>")),
                               verbatimTextOutput("collectionJobFormValidate")
                             )
                             #uiOutput("jobForm"),
                           )
                  ),
                  tabPanel("Explore Collections",
                           value = "exploreColl",
                           mainPanel(
                             tabsetPanel(
                               tabPanel("All Collections", value = "allColl", dataTableOutput("dtCollAll", width = '140%')),
                               tabPanel("My Collections", value = "mycoll", dataTableOutput("dtCollMy", width = '140%'))
                             )
                           )
                  )
                )
      )
      
      appendTab("tabs",
                navbarMenu(
                  "Literature Selection",
                  # UI for Search
                  tabPanel("Search",
                           value = "search",
                           #titlePanel("Search Interface"),
                           sidebarLayout(
                             sidebarPanel(
                               #titlePanel("Facets"),
                               #h5(HTML("<b>Facets</b>")),
                               selectInput("jrnl",
                                           "Journal Name",
                                           choices = c("all"),
                                           selected = "all",
                                           multiple = TRUE,
                                           width = "50%"),
                               selectInput("ds",
                                           "Data Source",
                                           choices = c("all", "scopus", "crossref"),
                                           selected = "all",
                                           multiple = FALSE,
                                           width = "50%"),
                               #hr(style = "border-top: 1px solid #D3D3D3;"),
                               h5(HTML("<b>Search Box</b>")),
                               actionButton("insertSearchBox",
                                            "Add",
                                            icon = icon("plus", verify_fa = FALSE),
                                            style = "bordered; white-space:normal"),
                               #style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                               actionButton("removeSearchBox",
                                            "Remove",
                                            icon = icon("minus", verify_fa = FALSE),
                                            style = "bordered; white-space:normal"),
                               #br(),
                               hr(style = "border-top: 1px solid #D3D3D3;"),
                               div(id="searchBoxInsert"),
                               actionButton('submitSearch',
                                            'Submit Search',
                                            icon = icon("paper-plane", verify_fa = FALSE),
                                            style = "bordered; white-space:normal"),
                               actionButton('irSaveResult',
                                            'Save Results',
                                            icon = icon("save", verify_fa = FALSE),
                                            style = "bordered; white-space:normal"),
                               width = 4
                             ),
                             mainPanel(
                               tags$head(
                                 tags$style(HTML("
                                            .sep {
                                              width: 25%;
                                              height: 1px;
                                              float: left;
                                            }
                                  "))
                               ),
                               htmlOutput("searchInfoTag"),
                               verbatimTextOutput("searchInfo"),
                               dataTableOutput("searchResult"),
                               width = 8
                             )
                           )
                  ),
                  tabPanel("Manage Taxonomy",
                           value = "taxonomy",
                           mainPanel(
                             tabsetPanel(id = "synonym",
                               tabPanel("View Existing", value = "existingTaxonomy", htmlOutput("synonymViewUI")),
                               tabPanel("Create New", value = "createTaxonomy", htmlOutput("synonymCreateUI"))
                             )
                           )
                  )
                )
      )

      appendTab("tabs",
                # UI for Mining
                navbarMenu(
                  # UI for Mining Menu
                  "Mining",
                  tabPanel("Create Job",
                           value = "mine",
                           sidebarLayout(
                             sidebarPanel(
                               htmlOutput("mineUI"),
                               width = 4
                             ),
                             mainPanel(
                               h5(HTML("<b>VALIDATION</b>")),
                               verbatimTextOutput("miningJobFormValidate"),
                               h5(HTML("<b>INFO</b>")),
                               verbatimTextOutput("miningJobInfo"),
                               width = 8
                             )
                           )
                           
                  ),
                  tabPanel("Job Status",
                           value = "allminejobs",
                           dataTableOutput("dtMineJobStatus")
                  ),
                  tabPanel("Evaluation",
                           value = "evaluateMine",
                           sidebarLayout(
                             sidebarPanel(
                               htmlOutput("evalMineUI"),
                               width = 4
                             ),
                             mainPanel(
                               dataTableOutput("mineResults"),
                               width = 8
                             )
                           )
                          )
                )
      )
                
      # appendTab("tabs",
      #           # UI for Analytics
      #           navbarMenu(
      #             # UI for Analytics Menu
      #             "Analytics",
      #             tabPanel("Explore Analysis Datasets"),
      #             tabPanel("Data Analysis"),
      #             tabPanel("Predictive Modeling")
      #           )
      # )
      #         
      # appendTab("tabs",
      #           # UI for User User API Management
      #           navbarMenu(
      #             "User Profile",
      #             tabPanel("Change Password",
      #                      value = "changePassword",
      #                      RegLog_credsEdit_UI("cust_id")
      #               
      #             ),
      #             tabPanel(title = "API Key Management",
      #                      value = "apiKeyMgmt",
      #                      sidebarLayout(
      #                        sidebarPanel(
      #                          textInput("elsevier_api_key", "Elsevier API Key:"),
      #                          textInput("springer_api_key", "Springer API Key:"),
      #                          textInput("wiley_api_key", "Wiley API Key:"),
      #                          actionButton("validateKeys",
      #                                       "Validate",
      #                                       style = "bordered; white-space:normal"),
      #                          actionButton("saveKeys",
      #                                       "Save",
      #                                       style = "bordered; white-space:normal")
      #                        ),
      #                        mainPanel(
      #                          h5(HTML("<b>VALIDATION</b>")),
      #                          verbatimTextOutput("apiKeyValidate")
      #                        )
      #                      )
      #             )
      #           )
      # )
        
      appendTab("tabs",
                #UI for About US
                tabPanel(title = "About Us",
                         value = "about",
                         sidebarLayout(
                           sidebarPanel(
                             h2("Contact Us"),
                             p("Should you have any questions or doubts or suggestions, feel free to contact us at", 
                               strong("scilitminer@hereon.de"), 
                               "or using the below form"),
                             selectInput("contact_anrede",
                                         "Salutation",
                                         choices = c("", "Mr.", "Ms.", "Div."),
                                         selected = NULL,
                                         width = "20%"),
                             textInput("contact_first_name", "First Name:"),
                             textInput("contact_last_name", "Last Name:"),
                             textInput("contact_email", "E-Mail:"),
                             textInput("contact_institute", "Institute/ Organization:"),
                             textAreaInput("contact_query", "Your message to us:"),
                             tags$div(class = "privacycheckbox",
                                      prettyCheckbox("privacy_consent",
                                                     label = NULL,
                                                     icon = icon("check", verify_fa = FALSE),
                                                     animation = "smooth"),
                                      tags$style(HTML(".privacycheckbox {
                                                         display: flex;
                                                         align-items: flex-start;
                                                      }")),
                                      tags$div(class = "privacycheckbox-label",
                                               "I have read the data protection notice below and
                                               agree that Hereon may store and process the
                                               information I have provided for the purpose of
                                               contacting me and responding to my inquiries.*",
                                               tags$style(HTML(".privacycheckbox-label {
                                                                  flex: 1;
                                                                  word-wrap: break-word;
                                                                  text-align: justify;
                                                                  font-size: 12px;
                                                               }"))
                                      )
                             ),
                             br(),
                             actionButton("submitContactForm",
                                          "Send",
                                          icon = icon("paper-plane", verify_fa = FALSE),
                                          style = "bordered; white-space:normal;"),
                             br(),
                             br(),
                             tags$div(HTML("<b>Data protection notice / withdrawal of consent</b></br>  
                                            <p>We process your personal data in accordance with the legal 
                                            requirements on the basis of your consent (Art. 6 para. 1 lit. a GDPR). 
                                            Your personal data will be deleted once the purpose of processing has 
                                            been achieved, provided that this does not conflict with any statutory 
                                            retention obligations.  For further information on your rights as a data
                                            subject and how to contact the data protection officer, please refer to 
                                            our general data protection information, linked in the navigation bar under 
                                            menu <i>Privacy & Legal Notices</i> of this website. You may revoke your 
                                            consent to the processing of your personal data at any time without giving 
                                            reasons. To do so, please send an email to datenschutz@hereon.de. Please 
                                            note that the withdrawal of your consent does not affect the lawfulness of 
                                            processing based on consent before its withdrawal.</p>"),
                                      style = "text-align: justify;font-size: 10px")
                           ),
                           mainPanel(
                             #QR code and github page link to be inserted
                             #img(src = "matsciminer", height = 70, width = 200),
                             h2("SciLitMiner"),
                             p(" SciLitMiner is a literature mining tool developed for the materials science field and beyond. 
                               It aims to support a multimodal data extraction as well as data modeling. 
                               SciLitMiner is hosted in the ", 
                               a("Helmholtz-Zentrum Hereon", href="https://www.hereon.de/", target="_blank"),
                               "."),
                             br(),
                             br(),
                             br(),
                             h3("Features"),
                             p("- Acquire literature from multiple data sources"),
                             p("- Search of experimental datasets embedded into different visual representations"),
                             p("- Multimodal extraction of experimental data, e.g., text, plots, and tables"),
                             p("- Modeling of extracted datasets"),
                             br(),
                             br(),
                             br(),
                             h3("Who we are"),
                             p(a("Moin", href="https://en.wikipedia.org/wiki/Moin", target="_blank"), 
                               ", SciLitMiner is developed by Vipul Gupta, a Data Scientist turned Computational Materials Scientist 
                               as part of his PhD project work carried out under the supervision of Prof. Dr. Florian Pyczak and Prof. Dr. 
                               Ingo Schmitt in the Helmholtz-Zentrum Hereon at the Institute of Materials Physics, Department of Metal Physics.")
                           )
                         )
                )
      )
                
      appendTab("tabs",
                #Logout
                tabPanel(title = "Logout",
                         value = "logout")
                # tabPanel(title = div("Logout", 
                #                      style = "position: relative; right: -700px;"),
                #          value = "logout")
      )
    }
  }, ignoreInit = TRUE)
  
  #Create Collection: Job form submission
  observeEvent(input$submitCollectionJob, {
    if (any(c(input$coll_name, input$coll_desc, input$search_crit_1) == "")) {
      showNotification("The Job couldn't be submitted. Please fill up all mandatory fields!", 
                       type = "error", 
                       duration = 7)
      
      return()
    }
    
    # lstParams <- list()
    # lstParams$user_id = "a" #based on user id from single sign on
    # lstParams$coll_name = input$coll_name
    # lstParams$job_type = "COLGEN"
    # lstParams$coll_desc = input$coll_desc
    # lstParams$search_crit_1 = input$search_crit_1
    # lstParams$search_crit_2 = switch((input$search_crit_2 == "") + 1, input$search_crit_2, NA)
    # lstParams$additional_key_1 = switch((input$additional_key_1 == "") + 1, input$additional_key_1, NA)
    # lstParams$additional_key_2 = switch((input$additional_key_2 == "") + 1, input$additional_key_2, NA)
    
    lstCollJob <- list(
      `_id`=input$coll_name,
      user_id=RegLog$account_id(),
      job_type="COLGEN",
      coll_desc=input$coll_desc,
      search_crit_1=input$search_crit_1,
      search_crit_2=switch((input$search_crit_2 == "") + 1, input$search_crit_2, NA),
      additional_key_1=switch((input$additional_key_1 == "") + 1, input$additional_key_1, NA),
      additional_key_2=switch((input$additional_key_2 == "") + 1, input$additional_key_2, NA),
      job_status=1,
      job_status_description="The Collection job is created and scheduled to be processed in the next batch",
      job_creation_date=Sys.time(),
      job_completion_date=NA
    )
    
    jsonCollJobObj <- jsonlite::toJSON(lstCollJob, auto_unbox = T)
    
    # Call REST endpoint
    tryCatch({
      resp <- POST(lstRESTRoutes$POST_JOB, body = list(jobObj = jsonCollJobObj), encode = "json")
    }, warning = function(w) {
      showNotification(paste("The following warning occurred while establishing a connection to the server:", w),
                       type = "warning",
                       duration = 7)
      return()
    }, error = function(e) {
      showNotification(paste("The following error occurred while establishing a connection to the server:", e),
                       type = "error",
                       duration = 7)
      return()
    })
    if (!exists("resp")) {
      showNotification(paste("The Collection job couldn't be submitted due to missing service REST endpoint"),
                       type = "error", 
                       duration = 7)
      return()
    } else if (!resp$status_code == 200) {
      showNotification(paste("The Collection job couldn't be created due to following error:", 
                             httr::content(resp, type="application/json"), ". Please try with another collection name value"), 
                       type = "error", 
                       duration = 7,
                       action = a(href = "javascript:location.reload();", "Reload page")
      )
      return()
    } else {
      showNotification("The Collection job has been successfully created", 
                       duration = 7,
                       action = a(href = "javascript:location.reload();", "Reload page")
      )
      updateTextInput(session, "coll_name", value = "")
      updateTextAreaInput(session, "coll_desc", value = "")
      updateTextInput(session, "search_crit_1", value = "")
      updateTextInput(session, "search_crit_2", value = "")
      updateTextInput(session, "additional_key_1", value = "")
      updateTextInput(session, "additional_key_2", value = "")
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # Create Collection: Job form reset
  observeEvent(input$resetCollectionJobInputs, {
    updateTextInput(session, "coll_name", value = "")
    updateTextAreaInput(session, "coll_desc", value = "")
    updateTextInput(session, "search_crit_1", value = "")
    updateTextInput(session, "search_crit_2", value = "")
    updateTextInput(session, "additional_key_1", value = "")
    updateTextInput(session, "additional_key_2", value = "")
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  ##############################SEARCH#####################################
  
  # Search: Search Form
  
  # Search: Insert Search Box
  observeEvent(input$insertSearchBox, {
    if (length(insertCnt()) == 3) {
      showNotification("The insertion of search boxes is limited to three", duration = 5)
      return()
    }
    
    id <- input$insertSearchBox
    insertUI(
      selector = "#searchBoxInsert",
      where = "afterEnd",
      ui = tags$div(
        shinyjs::useShinyjs(),
        fluidRow(
          column(width = 9,
                 column(width = 8, textInput(paste("keyword", id, sep = "_"), 
                                             "Query", 
                                             placeholder = "Eg.: titanium aluminide", 
                                             width = "100%"), 
                        style = "padding-right:2px;"),
                 column(width = 4, selectInput(paste("field", id, sep = "_"), 
                                               "Field", 
                                               choices = c("all", "title", "abstract", "body", "captions" = "captions.description", "references"), 
                                               selected = "all",
                                               width = "100%"), 
                        style = "padding-left:2px;padding-right:2px"),
                 style = "padding-left:0;padding-right:0;"
          ),
          conditionalPanel(condition = paste0("input.field", "_", id, " == 'captions.description'"),
                           column(width = 3, pickerInput(paste("figure_type", id, sep = "_"), 
                                                         "Visual Type", 
                                                         choices = c("3D_image", 
                                                                     "Algorithm", 
                                                                     "Area_chart", 
                                                                     "Bar_plot", 
                                                                     "Block_diagram",
                                                                     "Box_plot",
                                                                     "Bubble_Chart",
                                                                     "Natural_image",
                                                                     "Line_graph",
                                                                     "Heat_map",
                                                                     "Histogram",
                                                                     "Mask",
                                                                     "Pareto_chart",
                                                                     "Pie_chart",
                                                                     "Polar_plot",
                                                                     "Radar_chart",
                                                                     "Scatter_plot",
                                                                     "Sketch",
                                                                     "Surface_plot",
                                                                     "Table",
                                                                     "Tree_Diagram",
                                                                     "Vector_image",
                                                                     "Venn_Diagram",
                                                                     "Unknown",
                                                                     "Not Available" = "na"),
                                                         options = list(
                                                           `actions-box` = TRUE,
                                                           `deselect-all-text` = "Unselect All",
                                                           `select-all-text` = "Select All",
                                                           `none-selected-text` = "Unselect All"
                                                         ),
                                                         selected = NULL,
                                                         multiple = TRUE,
                                                         width = "100%"), 
                                  style = "padding-left:2px;"))
        ),
        fluidRow(
          column(width = 12, h5(HTML("<b>Query Settings</b>"))),
          conditionalPanel(condition = paste0(length(insertCnt()), " >= 0"),
                           column(width = 4, prettyCheckbox(paste("ignore", id, sep = "_"),
                                                            "Ignore",
                                                            icon = icon("check", verify_fa = FALSE),
                                                            animation = "smooth"),
                                  style = "padding-right:0;")),
          disabled(conditionalPanel(condition = paste0(length(insertCnt()), " == 0", " && ", "input.ignore", "_", id, " != true"),
                           column(width = 4, prettyCheckbox(paste("nature1", id, sep = "_"),
                                                           "Mandatory",
                                                           value = TRUE,
                                                           icon = icon("check", verify_fa = FALSE),
                                                           animation = "smooth"),
                                  style = "padding-left:0;padding-right:0;margin-left:-13%;"))),
          conditionalPanel(condition = paste0(length(insertCnt()), " > 0", " && ", "input.ignore", "_", id, " != true"),
                           column(width = 4, prettyCheckbox(paste("nature", id, sep = "_"),
                                                            "Mandatory",
                                                            icon = icon("check", verify_fa = FALSE),
                                                            animation = "smooth"),
                                  style = "padding-left:0;padding-right:0;margin-left:-13%;")),
          conditionalPanel(condition = paste0("input.field", "_", id, " != 'all'", " && ", "input.keyword", "_", id, ".indexOf(' ') > 0"),
                           column(width = 4, prettyCheckbox(paste("phrase", id, sep = "_"),
                                          "Phrase",
                                          icon = icon("check", verify_fa = FALSE),
                                          animation = "smooth"),
                                  style = "padding-left:0;padding-right:0;margin-left:-10%;")),
          conditionalPanel(condition = paste0("input.phrase", "_", id, " == false", " && ", "input.keyword", "_", id, ".indexOf(' ') > 0"),
                           column(width = 4, prettyCheckbox(paste("operator", id, sep = "_"),
                                                          "Match All Terms",
                                                          icon = icon("check", verify_fa = FALSE),
                                                          animation = "smooth"),
                                  style = "padding-left:0;padding-right:0;margin-left:-11%;"))
        ),
        hr(style = "border-top: 1px solid #D3D3D3;"),
        id = id
      )
    )
    insertCnt(c(insertCnt(), id))
    
    # Handle search box mandatory checkbox enable logic
    if (length(insertCnt()) == 2) {
      enable(paste("nature1", insertCnt()[[1]], sep = "_"))
      updatePrettyCheckbox(
        session = session,
        inputId = paste("nature1", insertCnt()[[1]], sep = "_"),
        value = FALSE
      )
    }

  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # Search: Remove Search Box
  observeEvent(input$removeSearchBox, {
    removeUI(
      selector = paste0("#", insertCnt()[length(insertCnt())])
    )
    #runjs(paste0('Shiny.onInputChange("field_', insertCnt[length(insertCnt)] ,'", null)'))
    insertCnt(insertCnt()[-length(insertCnt())])
    
    # Handle search box mandatory checkbox disable logic
    if (length(insertCnt()) == 1) {
      disable(paste("nature1", insertCnt()[[1]], sep = "_"))
      updatePrettyCheckbox(
        session = session,
        inputId = paste("nature1", insertCnt()[[1]], sep = "_"),
        value = TRUE
      )
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  # Search: Search form submission
  observeEvent(input$submitSearch, {
    
    output$searchInfoTag <- renderText({NULL})
    output$searchInfo <- renderText({NULL})
    
    lstQryObj <- list()
    if (length(insertCnt())) {
      
      # Code to create JSON query Object
      for (i in 1:length(insertCnt())) {
        id <- insertCnt()[i]
          
        # Check if query field is empty
        if (input[[paste("keyword", id, sep = "_")]] == "")
          next
        
        if (i == 1)
          qType = ifelse(input[[paste("nature1", id, sep = "_")]], "must", "should")
        else
          qType = ifelse(input[[paste("nature", id, sep = "_")]], "must", "should")
        
        if (input[[paste("ignore", id, sep = "_")]])
          qType = "must_not"
        
        qryObj <- list(
          type = qType,
          field = input[[paste("field", id, sep = "_")]], 
          keyword = input[[paste("keyword", id, sep = "_")]], 
          phrase = input[[paste("phrase", id, sep = "_")]],
          operator = ifelse(input[[paste("operator", id, sep = "_")]], "and", "or"),
          figuretype = switch(is.null(input[[paste("figure_type", id, sep = "_")]]) + 1, 
                              as.list(input[[paste("figure_type", id, sep = "_")]]), 
                              NA))
        lstQryObj[[length(lstQryObj) + 1]] <- qryObj
      }
    } else {
      output$searchResult <- renderDataTable(NULL)
      showNotification(paste("Please add at least one search box to perform a search"),
                       type = "error",
                       duration = 7)
      return()
    }
    
    # Check if all query fields are empty
    if (!length(lstQryObj)) {
      output$searchResult <- renderDataTable(NULL)
      showNotification(paste("Please provide a query to at least one search box to perform a search"),
                       type = "warning",
                       duration = 7)
      return()
    }
      
    # if (!input$coll == "all") {
    #   qryObj <- list(type = "filter", 
    #                   field = "collection", 
    #                   keyword = input$coll, 
    #                   phrase = "false", 
    #                   operator = "or")
    #   lstQryObj[[length(insertCnt) + 1]] = qryObj
    # }
    if (!input$jrnl == "all") {
      qryObj <- list(type = "filter", 
                      field = "journal", 
                      keyword = input$jrnl, 
                      phrase = "false", 
                      operator = "or")
      lstQryObj[[length(insertCnt()) + 2]] = qryObj
    }
    if (!input$ds == "all") {
      sQryObj <- list(type = "filter", 
                      field = "source", 
                      keyword = input$ds, 
                      phrase = "false", 
                      operator = "or")
      lstQryObj[[length(insertCnt()) + 3]] = qryObj
    }
    
    jsonQryObj <- jsonlite::toJSON(lstQryObj, auto_unbox = T)
    
    # Call REST endpoint
    tryCatch({
      resp <- httr::POST(lstRESTRoutes$POST_IR_SEARCH, body = list(qryObj = jsonQryObj), encode = "json")
    }, warning = function(w) {
      showNotification(paste("The following warning occurred while establishing a connection to the server:", w),
                       type = "warning",
                       duration = 7)
      return()
    }, error = function(e) {
      showNotification(paste("The following error occurred while establishing a connection to the server:", e),
                       type = "error",
                       duration = 7)
      return()
    })
    if (!exists("resp")) {
      showNotification(paste("The Search couldn't be performed due to missing service REST endpoint"),
                       type = "error", 
                       duration = 7)
      return()
    } else if (!resp$status_code == 200) {
        showNotification(paste("The Search couldn't be performed due to following error:", 
                               httr::content(resp, type="application/json")), 
                         type = "error", 
                         duration = 7,
                         action = a(href = "javascript:location.reload();", "Reload page"))
        return()
    }
    
    # Transform response into data table
    lstResults <- httr::content(resp)
    
    # Condition to check no result
    if (!length(lstResults)) {
      output$searchResult <- renderDataTable({
        dtEmpty <- data.table(doi=character(),
                   title=character(),
                   score=character(),
                   "caption count"=character(),
                   delete=character(),
                   summary=character())
        table <- dtEmpty %>%
          DT::datatable(filter = "none", 
                        rownames = F,
                        extensions = 'FixedColumns',
                        options = list(searching = FALSE,
                                       paging = FALSE,
                                       language = list(
                                         zeroRecords = "No match found")),
                        escape = F)
        
      })
      return()
    }
    
    pg_load_js <<- paste0('setTimeout(function() {table.page(', 0 ,').draw(false);}, 10);')
    pgLoadJS(pg_load_js)
    
    lstSimplifiedResults <- lapply(lstResults, function(doc) {
      doi_link <- paste0("https://doi.org/", doc$`_id`)
      list(doi = paste0("<a title=", doi_link, " href=", doi_link, " target='_blank'>", doc$`_id`, "</a>"), 
           title = doc$title, 
           score = doc$score, 
           "caption count" = length(doc$captions),
           #collection = doc$collection,
           abstract = doc$abstract, 
           captions = list(switch((length(doc$captions) > 0) + 1, NA, doc$captions)))
    })
    searchRes$dt <- rbindlist(lstSimplifiedResults)
    
    # Check if caption count is zero for entire data table
    # if (all(dtRes[["caption count"]] == 0))
    #   dtRes[, c("caption count") := NULL]
    
    # Round score value to two decimals
    searchRes$dt[, score := round(score, 1)]
    searchRes$dt[, REF_ID := seq.int(NROW(searchRes$dt))]
    
    # Code to make data table reactive
    
    # Auxiliary function
    shinyInput <- function(FUN, len, id, ...) {
      inputs <- character(len)
      for (i in seq_len(len)) {
        inputs[i] <- as.character(FUN(paste0(id, i), ...))
      }
      return(inputs)
    }
    
    # Reactive
    getDTReactive <- reactive({
      dt <- searchRes$dt
      dt$delete <- shinyInput(actionButton, nrow(dt), 'delete_', label = NULL, icon = icon("trash", lib = "glyphicon"),
                              #style = "color: red;background-color: white",
                              onclick = paste0('Shiny.onInputChange( \"delete_row\" , this.id, {priority: \"event\"})'))
      dt$summary<- shinyInput(actionButton, nrow(dt), 'show_', label = NULL, icon = icon("info-sign", lib = "glyphicon"),
                               #style = "inline-block",
                               #style="float:right",
                               onclick = paste0('Shiny.onInputChange( \"show_summary\" , this.id, {priority: \"event\"})'))
      return(dt)
    })
    
    # Assign reactive data.table to reactiveValues
    #searchRes$dt <- getDTReactive()
    searchRes$dt <- isolate({getDTReactive()})
    
    # paste0('setTimeout(function() {table.page(', 0 ,').draw(false);}, 10);')
    
    # Render search result to the UI
    output$searchResult <- renderDataTable({
      #dt <- searchRes$dt
      table <- searchRes$dt[, -c("REF_ID", "abstract", "captions")] %>%
        DT::datatable(filter = list(position = 'top', clear = FALSE), 
                      rownames = F,
                      extensions = c("FixedColumns", "Buttons"),
                      options = list(pageLength = 10,
                                     stateSave = TRUE,
                                     info = T,
                                     autowidth = TRUE,
                                     lengthMenu = list(c(5, 10, -1), c("5", "10", "all")),
                                     columnDefs = list(list(targets = c(4, 5), 
                                                            searchable = FALSE),
                                                       list(className = 'dt-center', 
                                                            targets = 2:5),
                                                       list(width = '1000px', targets = c(1))),
                                     scrollX = T,
                                     dom = 'l<"sep">Bfrtip', # BRSpfrti     Bfrtip     'l<"sep">Bfrtip' '     <"top"l><"top-center"B>frt<"bottom"ip><"clear">'
                                     buttons = list(list(extend = "csv",
                                                         text = "CSV",
                                                         title = "Data",
                                                         exportOptions = list(columns = c(0, 1, 2))),
                                                    list(extend = "excel",
                                                         text = "EXCEL",
                                                         title = "Data",
                                                         exportOptions = list(columns = c(0, 1, 2))),
                                                    list(extend = "pdf",
                                                         text = "PDF",
                                                         title = "Data",
                                                         exportOptions = list(columns = c(0, 1, 2)))
                                                    )
                                     ),
                      selection = "none",
                      escape = c(2,3,4),
                      caption = tags$caption(
                                style="caption-side: bottom; text-align: left; color:red",
                                "Note: Some rights reserved. This work permits non-commercial use, distribution,
                                and reproduction in any medium, provided the original author and source are credited."
                                ),
                      callback = JS(pgLoadJS())
                      )
    })
    
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # When press delete_row, remove row
  observeEvent(input$delete_row, {
    selectedRow <- as.numeric(strsplit(input$delete_row, "_")[[1]][2])
    searchRes$dt <- subset(searchRes$dt, REF_ID != selectedRow)
    if (input$searchResult_state$length == -1)
      pageNo <- -1
    else
      pageNo <- as.numeric(input$searchResult_state$start / input$searchResult_state$length)
    if (NROW(searchRes$dt) / input$searchResult_state$length == pageNo)
      pageNo <- pageNo - 1
    pg_load_js <<- paste0('setTimeout(function() {table.page.len(', input$searchResult_state$length, ').draw(false);
                          table.page(', pageNo ,').draw(false);}, 100);')
    pgLoadJS(pg_load_js)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # Show summary to the UI
  observeEvent(input$show_summary, {
    selectedRow <- as.numeric(strsplit(input$show_summary, "_")[[1]][2])
    dtS <- subset(searchRes$dt, REF_ID==selectedRow)
    showModal(modalDialog(
      title = strong("Summary"),
      div(strong("Abstract:"), 
          dtS$abstract, 
          hr(style = "border-top: 1px solid #D3D3D3;"),
          if (!is.na(dtS$captions)) {
            lapply(unlist(dtS$captions, recursive = F), function(cap) {
              str <- 
                div(strong("Caption:"), 
                    cap$caption, 
                    br(),
                    br(),
                    tryCatch({
                      cap$path <- gsub("U:/app/data/global//processed", replacement = "/data/global/processed", cap$path)
                      tags$img(src = base64enc::dataURI(file = cap$path, mime = "image/jpeg"), 
                               style="width: 480px") #640
                    }, warning = function(w) {
                      tags$img(src = "no_img.jpeg", 
                               style="width: 200px")
                    }, error = function(e) {
                      tags$img(src = "no_img.jpeg", 
                               style="width: 200px")
                    }),
                    hr(style = "border-top: 1px solid #D3D3D3;")
                )
            })
          }
      ),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # When press save results, save sub collection
  observeEvent(input$irSaveResult, {
    
    if (length(searchRes$dt) && nrow(searchRes$dt)) {
      showModal(modalDialog(
        title = strong("Save Search Result"),
        textInput("ir_result_name", "Name:*", placeholder = "Ex.: tialcreep_subcoll"),
        textAreaInput("ir_result_desc", "Description:*", placeholder = "Ex.: this sub-collection is a set of documents that includes only creep curves",
                      width = "120%"),
        actionButton("ir_save_result_final", "save"),
        modalButton("cancel"),
        easyClose = F,
        size = "m",
        footer = NULL
      ))
    } else {
      showNotification("No data found!",
                       type = "error",
                       duration = 3)
      return()
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # Save Sub Collection: Save
  observeEvent(input$ir_save_result_final, {
    if (any(c(input$ir_result_name, input$ir_result_desc) == "")) {
      showNotification("The Sub-Collection couldn't be saved. Please fill up all mandatory fields!",
                       type = "error",
                       duration = 7)
      return()
    }
    lstIRResult <- list()
    # <add user id>
    lstIRResult$`_id` <- input$ir_result_name
    lstIRResult$description <- input$ir_result_desc
    lstIRResult$dois <- searchRes$dt[, c("doi", "captions")]
    jsonIRResultObj <- jsonlite::toJSON(lstIRResult, flatten = T, auto_unbox = T)
    
    # Call REST endpoint
    tryCatch({
      resp <- httr::POST(lstRESTRoutes$POST_IR_RESULT,
                         body = list(irResultObj = jsonIRResultObj),
                         encode = "json")
    }, warning = function(w) {
      showNotification(paste("The following warning occurred while establishing a connection to the server:", w),
                       type = "warning",
                       duration = 7)
      removeModal()
    }, error = function(e) {
      showNotification(paste("The following error occurred while establishing a connection to the server:", e),
                       type = "error",
                       duration = 7)
      removeModal()
    })
    if (!exists("resp")) {
      showNotification(paste("The search results couldn't be saved due to missing service REST endpoint"),
                       type = "error",
                       duration = 7)
      removeModal()
    } else if (!resp$status_code == 200) {
      showNotification(paste("The search results couldn't be saved due to following error:",
                             httr::content(resp, type="application/json"), ". Please try with another Result Collection Name value"),
                       type = "error",
                       duration = 7)
      removeModal()
    } else {
      showNotification("The search results are successfully saved",
                       duration = 7)
      removeModal()
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # taxonomy
  # view/ update synonym
  # domain name
  
  observeEvent(input$synonym, {
    domain_names <<- reactive({
      tryCatch({
        resp <- httr::GET(lstRESTRoutes$GET_SYNONYM_DOMAIN_NAMES)
      }, warning = function(w) {
        showNotification(paste("The following warning occurred while establishing a connection to the server:", w),
                         type = "warning",
                         duration = 7)
        removeModal()
      }, error = function(e) {
        showNotification(paste("The following error occurred while establishing a connection to the server:", e),
                         type = "error",
                         duration = 7)
        removeModal()
      })
      if (exists("resp")) {
        if (resp$status_code != 200) {
          showNotification("The domain info couldn't be retrieved",
                           duration = 7,
                           action = a(href = "javascript:location.reload();", "Reload page"))
          return(c())
        } else {
          domain_names <- httr::content(resp)
          return(domain_names)
        }
      } else {
        return(c())
      }
    })
    if(input$synonym == "existingTaxonomy") {
      output$synonymViewUI <- renderUI({
        div(
          br(),
          fluidRow(
            selectInput(inputId = "syn_domain_name",
                        label = "Select a Domain",
                        choices = domain_names(),
                        width = "18%")
          ),
          br(),
          fluidRow(
            dataTableOutput("dtSynonymRules")
          )
        )
      })
      # output$synonymUpdateUI <- renderUI({
      #   div(
      #     br(),
      #     fluidRow(
      #       selectInput(inputId = "syn_domain_name",
      #                   label = "Select a Domain",
      #                   choices = domain_names(),
      #                   width = "18%")
      #     ),
      #     fluidRow(
      #       actionButton('syn_edit',
      #                    'Edit Synonyms',
      #                    icon = icon("file-pen", verify_fa = FALSE),
      #                    style = "bordered; white-space:normal"),
      #       disabled(actionButton('syn_save',
      #                             'Save Synonyms',
      #                             icon = icon("save", verify_fa = FALSE),
      #                             style = "bordered; white-space:normal"))
      #     ),
      #     br(),
      #     fluidRow(
      #       disabled(textAreaInput(inputId = "syn_display",
      #                              label = "Synonym Rules",
      #                              width = "1400px",
      #                              height = "500px"))
      #     )
      #   )
      # })
    } else if (input$synonym == "createTaxonomy") {
      output$synonymCreateUI <- renderUI({
        div(
          br(),
          fluidRow(
            selectInput(inputId = "syn_create_domain_name",
                        label = "Select a Domain",
                        choices = domain_names(),
                        width = "18%"),
            actionButton('syn_create',
                         'Create Synonyms',
                         icon = icon("paper-plane", verify_fa = FALSE),
                         style = "bordered; white-space:normal"),
          ),
          br(),
          fluidRow(  
            textAreaInput(inputId = "syn_text", 
                          label = "Define Synonym Rules",
                          placeholder = "Write synonyms seperated by comma. In case of multiple synonym rules, seperate each of them by ENTER.\nE.g., \nTitanium, Ti \nminimum creep rate, steady-state creep rate",
                          width = "1600px",
                          height = "500px")
          )
        )
      }) 
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
   
  # synonym rules as taxonomy
  # get synonyms
  observe({
    req(input$syn_domain_name, input$tabs == "taxonomy", input$synonym == "existingTaxonomy") 
    tryCatch({
      getURL <- paste0(lstRESTRoutes$GET_SYNONYMS, "?", "domain_name=", gsub(" ", "_", input$syn_domain_name))
      resp <- httr::GET(getURL)
    }, warning = function(w) {
      showNotification(paste("The following warning occurred while establishing a connection to the server:", w),
                       type = "warning",
                       duration = 7)
      removeModal()
    }, error = function(e) {
      showNotification(paste("The following error occurred while establishing a connection to the server:", e),
                       type = "error",
                       duration = 7)
      removeModal()
    })
    if (exists("resp")) {
      if (resp$status_code != 200) {
        showNotification("The synonyms couldn't be retrieved",
                         duration = 7,
                         action = a(href = "javascript:location.reload();", "Reload page"))
      } else {
        content <- httr::content(resp, as = "parsed")
      }
    } else {
      content <- character()
    }
    dtSyn <- data.table(synonym_rules = strsplit(content[[1]], "\n")[[1]])
    output$dtSynonymRules <- renderDataTable({
      table <- dtSyn %>%
        DT::datatable(filter = list(position = 'top', clear = FALSE), 
                      rownames = F,
                      extensions = 'FixedColumns',
                      options = list(paging = TRUE,
                                     pageLength = 10,
                                     lengthMenu = list(c(5, 10, -1), c("5", "10", "all")),
                                     autoWidth = F,
                                     scrollX = T,
                                     dom = 'l<"sep">Bfrtip',
                                     language = list(
                                       zeroRecords = "No records found")),
                      selection = "none")
      
    })
  })
  
  # edit synonym
  observeEvent(input$syn_edit, {
    enable("syn_display")
    enable("syn_save")
    # save synonyms
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  observeEvent(input$syn_save, {
    # save synonym rules
    tryCatch({
      lstSynObj <- list(domain_name = input$syn_domain_name, synonym_rules = input$syn_display)
      jsonSynObj <- jsonlite::toJSON(lstSynObj, auto_unbox = T)
      resp <- httr::POST(lstRESTRoutes$POST_SYNONYMS, 
                         body = list(synObj = jsonSynObj), 
                         encode = "json")
    }, warning = function(w) {
      showNotification(paste("The following warning occurred while establishing a connection to the server:", w),
                       type = "warning",
                       duration = 7)
      removeModal()
    }, error = function(e) {
      showNotification(paste("The following error occurred while establishing a connection to the server:", e),
                       type = "error",
                       duration = 7)
      removeModal()
    })
    if (!exists("resp")) {
      showNotification(paste("The synonym rules couldn't be saved due to missing service REST endpoint"),
                       type = "error", 
                       duration = 7)
      removeModal()
    } else if (!resp$status_code == 200) {
      showNotification(paste("The synonym rules couldn't be saved due to following error:", 
                             httr::content(resp, type="application/json"), ". Please contact administrator"), 
                       type = "error", 
                       duration = 7)
      removeModal()
    } else {
      showNotification("The synonym rules are successfully saved", 
                       duration = 7)
      removeModal()
    }
    disable("syn_display")
    disable("syn_save")
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # create synonym
  observeEvent(input$syn_create, {
    if (any(c(input$syn_create_domain_name, input$syn_text) == "")) {
      showNotification("The Synonym rules couldn't be created. Please fill up all fields!", 
                       type = "error", 
                       duration = 7)
      
      return()
    }
    # if (input$syn_create_domain_name %in% domain_names()) {
    #   showNotification("The domain name already exists!", 
    #                    type = "error", 
    #                    duration = 7)
    #   
    #   return()
    # }
    disable("syn_text")
    
    tryCatch({
      lstSynObj <- list(domain_name = input$syn_create_domain_name, synonym_rules = input$syn_text)
      jsonSynObj <- jsonlite::toJSON(lstSynObj, auto_unbox = T)
      resp <- httr::POST(lstRESTRoutes$POST_SYNONYMS, 
                         body = list(synObj = jsonSynObj), 
                         encode = "json")
    }, warning = function(w) {
      showNotification(paste("The following warning occurred while establishing a connection to the server:", w),
                       type = "warning",
                       duration = 7)
      removeModal()
    }, error = function(e) {
      showNotification(paste("The following error occurred while establishing a connection to the server:", e),
                       type = "error",
                       duration = 7)
      removeModal()
    })
    if (!exists("resp")) {
      showNotification(paste("The synonym rules couldn't be saved due to missing service REST endpoint"),
                       type = "error", 
                       duration = 7)
      removeModal()
    } else if (!resp$status_code == 200) {
      showNotification(paste("The synonym rules couldn't be saved due to following error:", 
                             httr::content(resp, type="application/json"), ". Please contact administrator"), 
                       type = "error", 
                       duration = 7)
      removeModal()
    } else {
      showNotification("The synonym rules are successfully saved", 
                       duration = 7)
      removeModal()
      updateTextInput(session, "syn_text", value = "")
    }
    enable("syn_text")
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  ####################################MINING####################################
  
  
  # Data Mining: submit job
  observeEvent(input$submitMiningJob, {
    if (input$mining_job_name == "" || input$mining_job_desc == "" || is.null(input$mining_opts)) {
      showNotification("The Mining job couldn't be submitted. Please fill up all mandatory fields!", 
                       type = "error", 
                       duration = 7)
      
      return()
    }
    
    lstMineJob <- list(
      `_id`=input$mining_job_name,
      user_id=RegLog$account_id(),
      job_type="MINING",
      job_desc=input$mining_job_desc,
      ir_subcoll=input$ir_subcoll,
      domain=input$domain,
      mining_opts=input$mining_opts,
      job_status=1,
      job_status_description="The Mining job is created and scheduled to be processed in the next batch",
      job_creation_date=Sys.time(),
      job_completion_date=NA
    )
    
    jsonMineJobObj <- jsonlite::toJSON(lstMineJob, auto_unbox = T)
    
    # Call REST endpoint
    tryCatch({
      resp <- httr::POST(lstRESTRoutes$POST_JOB, body = list(jobObj = jsonMineJobObj), encode = "json")
    }, warning = function(w) {
      showNotification(paste("The following warning occurred while establishing a connection to the server:", w),
                       type = "warning",
                       duration = 7)
      return()
    }, error = function(e) {
      showNotification(paste("The following error occurred while establishing a connection to the server:", e),
                       type = "error",
                       duration = 7)
      return()
    })
    if (!exists("resp")) {
      showNotification(paste("The Mining job couldn't be submitted due to missing service REST endpoint"),
                       type = "error", 
                       duration = 7)
      return()
    } else if (!resp$status_code == 200) {
      showNotification(paste("The Mining job couldn't be created due to following error:", 
                             httr::content(resp, type="application/json")), 
                       type = "error", 
                       duration = 7,
                       action = a(href = "javascript:location.reload();", "Reload page"))
      return()
    } else {
      showNotification("The Mining job has been successfully created", 
                       duration = 7,
                       action = a(href = "javascript:location.reload();", "Reload page"))
      updateTextInput(session, 
                      inputId = "mining_job_name",
                      value = "")
      updateTextAreaInput(session, 
                          inputId = "mining_job_desc",
                          value = "")
      updatePickerInput(session, 
                        inputId = "mining_opts", 
                        label = "Select Mining Options",
                        choices = domain_info[domain_name == input$domain][["mining_opts"]])
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  output$loadMineResults <- reactive({
    
  })
  
  #################################User Profile################################
  
  # Save API Keys: Save
  observeEvent(input$saveKeys, {
    if (all(c(input$elsevier_api_key, input$springer_api_key, input$wiley_api_key) == "")) {
      showNotification("Please provide at least one key!", 
                       type = "error", 
                       duration = 7)
      return()
    }
    lstAPIKey <- list()
    lstAPIKey$user_id <- RegLog$account_id()
    lstAPIKey$elsevier_api_key <- input$elsevier_api_key
    lstAPIKey$springer_api_key <- input$springer_api_key
    lstAPIKey$wiley_api_key <- input$wiley_api_key
    
    jsonAPIKeyObj <- jsonlite::toJSON(lstAPIKey, auto_unbox = T)
    
    # Call REST endpoint
    tryCatch({
      resp <- httr::POST(lstRESTRoutes$POST_API_KEYS, 
                         body = list(apiKeyObj = jsonAPIKeyObj), 
                         encode = "json")
    }, warning = function(w) {
      showNotification(paste("The following warning occurred while establishing a connection to the server:", w),
                       type = "warning",
                       duration = 7)
      removeModal()
    }, error = function(e) {
      showNotification(paste("The following error occurred while establishing a connection to the server:", e),
                       type = "error",
                       duration = 7)
      removeModal()
    })
    if (!exists("resp")) {
      showNotification(paste("The API keys couldn't be saved due to missing service REST endpoint"),
                       type = "error", 
                       duration = 7)
      removeModal()
    } else if (!resp$status_code == 200) {
      showNotification(paste("The API Key(s) couldn't be saved due to following error:", 
                             httr::content(resp, type="application/json"), ". Please contact administrator"), 
                       type = "error", 
                       duration = 7)
      removeModal()
    } else {
      showNotification("The API keys are successfully saved", 
                       duration = 7)
      removeModal()
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  #################################### Contact Form #######################################
  
  # Contact Form submit button
  observeEvent(input$submitContactForm, {
    if (!input$privacy_consent) {
      showNotification("Consent to store and process your personal data must be given", 
                       type = "error", 
                       duration = 7)
      
      return()
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  shinyjs::runjs("
      $('#loading-screen').fadeOut('slow', function() {
        $('#main-content').fadeIn('slow');
      });
    ")
  
}

# Acknowledgment page to be added
# <a href="https://www.flaticon.com/free-icons/aggregate" title="aggregate icons">Aggregate icons created by Eucalyp - Flaticon</a>
# <a href="https://www.flaticon.com/free-icons/medal" title="medal icons">Medal icons created by Octopocto - Flaticon</a>
# <a href="https://www.flaticon.com/free-icons/important-document" title="important document icons">Important document icons created by Graphix's Art - Flaticon</a>
# <a href="https://www.flaticon.com/free-icons/book" title="book icons">Book icons created by mikan933 - Flaticon</a>
# <a href="https://www.flaticon.com/free-icons/scheme" title="scheme icons">Scheme icons created by Freepik - Flaticon</a>
# <a href="https://www.flaticon.com/free-icons/logistic-regression" title="logistic regression icons">Logistic regression icons created by raidolicon - Flaticon</a>
# <a href="https://www.flaticon.com/free-icons/www" title="www icons">Www icons created by Dreamstale - Flaticon</a>