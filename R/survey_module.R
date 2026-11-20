# R/survey_module.R

# Put your service account client_email here:
BOT_EMAIL <- "survlytics-bot@survlytics.iam.gserviceaccount.com"

survey_builder_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    card(
      card_header("Step 1: Connect Your Personal Google Sheet", class = "bg-primary text-white"),
      p("Every survey builder links their own Google Sheet:"),
      tags$ol(
        tags$li("Open your Google Sheet."),
        tags$li("Click ", tags$b("Share"), " and add the robot email as ", tags$b("Editor"), ":"),
        tags$div(
          class = "alert alert-secondary p-2 my-2 user-select-all font-monospace text-break",
          BOT_EMAIL
        ),
        tags$li("Paste your Google Sheet link below and click Connect:")
      ),
      textInput(
        ns("user_sheet_url"), 
        "Your Google Sheet URL:", 
        width = "90%", 
        placeholder = "https://docs.google.com/spreadsheets/d/.../edit"
      ),
      layout_columns(
        actionButton(ns("connect_btn"), "Connect & Initialize Database", class = "btn-success"),
        actionButton(ns("sync_data_btn"), "Refresh / Sync Live Data", class = "btn-outline-primary")
      ),
      verbatimTextOutput(ns("builder_status"))
    ),
    
    layout_sidebar(
      sidebar = sidebar(
        h4("Step 2: Define Questions"),
        textInput(ns("var_name"), "Variable Name", placeholder =  "e.g., AGE"),
        textInput(ns("q_prompt"), "Question Prompt:", placeholder = "e.g., What is your age?"),
        textInput(ns("q_subtitle"), "Subtitle / Translation (Optional):", placeholder = "e.g., Berapakah umur anda?"),
        selectInput(ns("q_type"), "Input Type:", 
                    choices = c("Numeric Input", "1-10 Scale", "1-5 Scale", "Text Input", "MCQ", "Dropdown")),
        
        uiOutput(ns("conditional_options")),
        
        actionButton(ns("add_q"), "Add Question", class = "btn-primary"),
        hr(),
        h4("Step 3: Share Survey"),
        p(tags$small("Share this unique link or QR code with respondents. Submissions route directly to your sheet:")),
        verbatimTextOutput(ns("generated_survey_url")),
        uiOutput(ns("qr_ui"))
      ),
      card(
        card_header("Current Survey Schema"),
        DTOutput(ns("schema_table"))
      )
    )
  )
}

survey_active_ui <- function(id) {
  ns <- NS(id)
  
  card(
    tags$style(HTML("
      /* Expand input containers and labels to full width */
      .shiny-input-container {
        width: 100% !important;
        max-width: 100% !important;
      }
      .shiny-input-container > label {
        width: 100% !important;
        display: block !important;
      }

      /* Add spacing between inline radio options in Bootstrap 5 (bslib) */
      .shiny-options-group {
        display: flex;
        flex-wrap: wrap;
        gap: 20px;
        margin-top: 8px;
      }
      .form-check-inline, .radio-inline {
        margin-right: 0 !important;
        display: inline-flex;
        align-items: center;
        gap: 6px;
      }
    ")),
    card_header("Respondent Form", class = "bg-secondary text-white"),
    uiOutput(ns("dynamic_form")),
    hr(),
    actionButton(ns("submit_response"), "Submit Response", class = "btn-success"),
    uiOutput(ns("submit_alert"))
  )
}

survey_server <- function(id, shared_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Helper: Sync Schema & Responses tabs to a connected Google Sheet
    sync_schema_to_sheet <- function(sheet_id, schema_df) {
      req(sheet_id, nrow(schema_df) > 0)
      
      # 1. Write Schema tab
      googlesheets4::sheet_write(schema_df, ss = sheet_id, sheet = "Schema")
      
      # 2. Ensure Responses tab exists with matching headers (preserving existing rows if any)
      current_tabs <- googlesheets4::sheet_names(sheet_id)
      col_names <- c("Timestamp", schema_df$Variable)
      
      if (!"Responses" %in% current_tabs) {
        dummy_df <- as.data.frame(matrix(ncol = length(col_names), nrow = 0), stringsAsFactors = FALSE)
        colnames(dummy_df) <- col_names
        googlesheets4::sheet_write(dummy_df, ss = sheet_id, sheet = "Responses")
        shared_state$data <- dummy_df
      } else {
        existing_resp <- as.data.frame(
          googlesheets4::read_sheet(sheet_id, sheet = "Responses", col_types = "c"),
          stringsAsFactors = FALSE
        )
        # Add any newly created question columns if missing from Responses tab
        missing_cols <- setdiff(col_names, colnames(existing_resp))
        if (length(missing_cols) > 0) {
          for (mc in missing_cols) {
            existing_resp[[mc]] <- if (nrow(existing_resp) == 0) character(0) else NA_character_
          }
          existing_resp <- existing_resp[, col_names, drop = FALSE]
          googlesheets4::sheet_write(existing_resp, ss = sheet_id, sheet = "Responses")
        }
        shared_state$data <- existing_resp
      }
    }
    
    # Safely generate the respondent link
    survey_link <- reactive({
      req(shared_state$sheet_id)
      
      proto <- session$clientData$url_protocol
      host  <- session$clientData$url_hostname
      port  <- session$clientData$url_port
      path  <- session$clientData$url_pathname
      
      if (is.null(host) || !nzchar(host)) {
        base_url <- "https://naint-survlytics.share.connect.posit.cloud/"
      } else {
        port_part <- if (is.null(port) || !nzchar(port) || port %in% c("80", "443")) "" else paste0(":", port)
        base_url <- paste0(proto, "//", host, port_part, path)
      }
      
      paste0(base_url, "?mode=survey&sid=", shared_state$sheet_id)
    })
    
    output$generated_survey_url <- renderText({
      if (is.null(shared_state$sheet_id)) {
        return("Connect your Google Sheet above to generate your respondent link.")
      }
      survey_link()
    })
    
    output$qr_ui <- renderUI({
      if (is.null(shared_state$sheet_id)) {
        return(p(tags$em("QR code will appear once your Google Sheet is connected.")))
      }
      plotOutput(ns("qr_code"), width = "100%", height = "200px")
    })
    
    output$qr_code <- renderPlot({
      link <- survey_link()
      req(link, grepl("^http", link))
      par(mar = c(0, 0, 0, 0))
      plot(qrcode::qr_code(link))
    })
    
    output$conditional_options <- renderUI({
      req(input$q_type)
      if (input$q_type %in% c("MCQ", "Dropdown")) {
        textInput(ns("q_options"), "Options (comma-separated):", placeholder = "Option A, Option B")
      }
    })
    
    output$builder_status <- renderPrint({
      cat(sprintf("Questions Defined: %d\n", nrow(shared_state$schema)))
      cat(sprintf("Responses Synced: %d\n", nrow(shared_state$data)))
      cat(sprintf("Connected Sheet: %s\n", if (is.null(shared_state$sheet_id)) "Not Connected" else shared_state$sheet_id))
    })
    
    # Add question to schema (works before OR after connecting an empty sheet)
    observeEvent(input$add_q, {
      req(input$var_name, input$q_prompt)
      
      clean_var <- toupper(gsub("\\s+", "_", trimws(input$var_name)))
      clean_prompt <- trimws(input$q_prompt)
      clean_subtitle <- if (!is.null(input$q_subtitle)) trimws(input$q_subtitle) else ""
      
      if (clean_var %in% shared_state$schema$Variable) {
        showNotification(paste0("Variable '", clean_var, "' already exists!"), type = "error")
        return()
      }
      
      opts <- ""
      if (input$q_type %in% c("MCQ", "Dropdown")) {
        req(input$q_options)
        opts <- toupper(trimws(input$q_options))
      }
      
      # Ensure existing schema has Subtitle column before rbind
      if (nrow(shared_state$schema) > 0 && !"Subtitle" %in% colnames(shared_state$schema)) {
        shared_state$schema$Subtitle <- ""
      }
      
      new_q <- data.frame(
        Variable = as.character(clean_var), 
        Prompt = as.character(clean_prompt), 
        Subtitle = as.character(clean_subtitle),
        Type = as.character(input$q_type), 
        Options = as.character(opts),
        stringsAsFactors = FALSE
      )
      
      shared_state$schema <- rbind(shared_state$schema, new_q)
      
      # If a Google Sheet is already connected, immediately sync Schema + Responses headers
      if (!is.null(shared_state$sheet_id)) {
        tryCatch({
          sync_schema_to_sheet(shared_state$sheet_id, shared_state$schema)
        }, error = function(e) {
          showNotification(paste("Sheet sync warning:", e$message), type = "warning")
        })
      }
      
      updateTextInput(session, "var_name", value = "")
      updateTextInput(session, "q_prompt", value = "")
      updateTextInput(session, "q_subtitle", value = "")
      if (input$q_type %in% c("MCQ", "Dropdown")) {
        updateTextInput(session, "q_options", value = "")
      }
      
      showNotification(paste("Added variable:", clean_var), type = "message")
    })
    
    output$schema_table <- renderDT({ 
      datatable(shared_state$schema, options = list(dom = 't', pageLength = 10, scrollX = TRUE)) 
    })
    
    # Connect Google Sheet: handles existing schemas, empty schemas, AND blank sheets
    observeEvent(input$connect_btn, {
      req(input$user_sheet_url)
      
      id_notif <- showNotification("Connecting to Google Sheet...", duration = NULL)
      
      tryCatch({
        sheet_id <- googlesheets4::as_sheets_id(input$user_sheet_url)
        shared_state$sheet_id <- sheet_id
        
        sheet_names <- googlesheets4::sheet_names(sheet_id)
        schema_tab <- if ("Schema" %in% sheet_names) "Schema" else sheet_names[1]
        
        # Read candidate schema tab safely (col_types = "c" avoids errors on 0-row sheets)
        loaded_schema <- as.data.frame(
          googlesheets4::read_sheet(sheet_id, sheet = schema_tab, col_types = "c"),
          stringsAsFactors = FALSE
        )
        
        has_existing_schema <- (
          nrow(loaded_schema) > 0 &&
          all(c("Variable", "Prompt", "Type") %in% colnames(loaded_schema))
        )
        
        if (has_existing_schema) {
          # SCENARIO A: Existing spreadsheet already has a defined schema
          if (!"Subtitle" %in% colnames(loaded_schema)) loaded_schema$Subtitle <- ""
          if (!"Options" %in% colnames(loaded_schema)) loaded_schema$Options <- ""
          loaded_schema <- loaded_schema[, c("Variable", "Prompt", "Subtitle", "Type", "Options"), drop = FALSE]
          shared_state$schema <- loaded_schema
          
          # Load existing Responses tab, or create an empty one with matching headers if missing
          if ("Responses" %in% sheet_names) {
            shared_state$data <- as.data.frame(
              googlesheets4::read_sheet(sheet_id, sheet = "Responses", col_types = "c"),
              stringsAsFactors = FALSE
            )
          } else {
            col_names <- c("Timestamp", shared_state$schema$Variable)
            dummy_df <- as.data.frame(matrix(ncol = length(col_names), nrow = 0), stringsAsFactors = FALSE)
            colnames(dummy_df) <- col_names
            googlesheets4::sheet_write(dummy_df, ss = sheet_id, sheet = "Responses")
            shared_state$data <- dummy_df
          }
          
          showNotification(
            sprintf("Connected! Loaded %d questions and %d responses.", nrow(shared_state$schema), nrow(shared_state$data)),
            type = "message"
          )
          
        } else if (nrow(shared_state$schema) > 0) {
          # SCENARIO B: Sheet is empty, and user ALREADY built questions in Step 2 -> push them now
          sync_schema_to_sheet(sheet_id, shared_state$schema)
          showNotification("Empty Google Sheet initialized with your builder questions!", type = "message")
          
        } else {
          # SCENARIO C: Sheet is empty AND builder has 0 questions -> keep sheet linked so user can add questions
          shared_state$data <- data.frame()
          showNotification(
            "Empty Google Sheet connected! Now add questions in Step 2—they will save automatically to this sheet.",
            type = "message",
            duration = 8
          )
        }
        
      }, error = function(e) {
        shared_state$sheet_id <- NULL
        showNotification(paste("Connection Error:", e$message), type = "error")
      }, finally = {
        removeNotification(id_notif)
      })
    })
    
    # Sync data from sheet
    observeEvent(input$sync_data_btn, {
      req(shared_state$sheet_id)
      tryCatch({
        sheet_names <- googlesheets4::sheet_names(shared_state$sheet_id)
        if ("Responses" %in% sheet_names) {
          df <- as.data.frame(
            googlesheets4::read_sheet(shared_state$sheet_id, sheet = "Responses", col_types = "c"),
            stringsAsFactors = FALSE
          )
          shared_state$data <- df
          showNotification(paste("Synced", nrow(df), "rows from Google Sheet."), type = "message")
        } else {
          showNotification("No 'Responses' tab found yet. Add at least one question first.", type = "warning")
        }
      }, error = function(e) {
        showNotification(paste("Sync failed:", e$message), type = "error")
      })
    })
    
    # Safe Respondent Form Generation
    output$dynamic_form <- renderUI({
      schema_df <- shared_state$schema
      
      if (is.null(schema_df) || nrow(schema_df) == 0) {
        return(tags$div(
          class = "alert alert-warning",
          "No survey questions found. Add questions in Step 2 or ensure your survey link contains the full survey ID (?sid=...)."
        ))
      }
      
      tagList(
        lapply(seq_len(nrow(schema_df)), function(i) {
          var <- as.character(schema_df$Variable[i])
          prompt <- as.character(schema_df$Prompt[i])
          subtitle <- if ("Subtitle" %in% colnames(schema_df)) as.character(schema_df$Subtitle[i]) else ""
          type <- as.character(schema_df$Type[i])
          opts_raw <- if ("Options" %in% colnames(schema_df)) as.character(schema_df$Options[i]) else ""
          
          # Build label with optional bilingual subtitle underneath
          q_label <- if (!is.na(subtitle) && nzchar(trimws(subtitle))) {
            tagList(
              tags$div(prompt),
              tags$div(class = "text-muted small fw-normal fst-italic mt-1", subtitle)
            )
          } else {
            prompt
          }
          
          # Clean and parse options safely without crashing on NA
          opts <- NULL
          if (!is.na(opts_raw) && nzchar(trimws(opts_raw))) {
            opts <- trimws(unlist(strsplit(opts_raw, ",")))
            opts <- opts[nzchar(opts)]
          }
          if (is.null(opts) || length(opts) == 0) {
            opts <- c("Option 1", "Option 2")
          }
          
          if (identical(type, "Numeric Input")) {
            numericInput(inputId = ns(var), label = q_label, value = NA, width = "100%")
          } else if (identical(type, "1-5 Scale")) {
            radioButtons(inputId = ns(var), label = q_label, choices = as.character(1:5), selected = character(0), inline = TRUE, width = "100%")   
          } else if (identical(type, "1-10 Scale")) {
            radioButtons(inputId = ns(var), label = q_label, choices = as.character(1:10), selected = character(0), inline = TRUE, width = "100%")             
          } else if (identical(type, "Text Input")) {
            textInput(inputId = ns(var), label = q_label, value = "", width = "100%")
          } else if (identical(type, "MCQ")) {
            radioButtons(inputId = ns(var), label = q_label, choices = opts, width = "100%")
          } else if (identical(type, "Dropdown")) {
            selectInput(inputId = ns(var), label = q_label, choices = opts, width = "100%")
          } else {
            textInput(inputId = ns(var), label = q_label, value = "", width = "100%")
          }
        })
      )
    })
    
    # Submit Response
    observeEvent(input$submit_response, {
      req(nrow(shared_state$schema) > 0)
      
      if (is.null(shared_state$sheet_id)) {
        showNotification("Survey ID is missing. Submissions cannot be recorded.", type = "error")
        return()
      }
      
      response_list <- list(Timestamp = as.character(Sys.time()))
      
      for (i in seq_len(nrow(shared_state$schema))) {
        var <- as.character(shared_state$schema$Variable[i])
        type <- as.character(shared_state$schema$Type[i])
        val <- input[[var]]
        
        if (identical(type, "Text Input") && !is.null(val) && val != "") {
          if (grepl("^[0-9.]+$", val)) {
            showNotification(paste("Error: '", var, "' requires text, but a number was entered."), type = "error")
            return() 
          }
          val <- toupper(val)
        }
        
        response_list[[var]] <- if (is.null(val) || length(val) == 0 || is.na(val) || val == "") NA else as.character(val)
      }
      
      new_row <- as.data.frame(response_list, stringsAsFactors = FALSE)
      
      tryCatch({
        googlesheets4::sheet_append(shared_state$sheet_id, new_row, sheet = "Responses")
        
        output$submit_alert <- renderUI({
          div(class = "alert alert-success mt-2", "Thank you! Your response was recorded.")
        })
        
        # Reset form fields
        for (i in seq_len(nrow(shared_state$schema))) {
          var <- as.character(shared_state$schema$Variable[i])
          type <- as.character(shared_state$schema$Type[i])
          
          if (identical(type, "Numeric Input")) {
            updateNumericInput(session, var, value = NA)
          } else if (identical(type, "1-5 Scale")) {
            updateRadioButtons(session, var, selected = character(0))         
          } else if (identical(type, "1-10 Scale")) {
            updateRadioButtons(session, var, selected = character(0))
          } else if (identical(type, "Text Input")) {
            updateTextInput(session, var, value = "")
          } else if (type %in% c("MCQ", "Dropdown")) {
            opts_raw <- as.character(shared_state$schema$Options[i])
            if (!is.na(opts_raw) && nzchar(trimws(opts_raw))) {
              opts <- trimws(unlist(strsplit(opts_raw, ",")))
              if (identical(type, "MCQ")) updateRadioButtons(session, var, selected = character(0))
              if (identical(type, "Dropdown")) updateSelectInput(session, var, selected = opts[1])
            }
          }
        }
      }, error = function(e) {
        showNotification(paste("Submission error:", e$message), type = "error")
      })
    })
    
  })
}