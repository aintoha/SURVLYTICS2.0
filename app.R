library(shiny)
library(bslib)
library(googlesheets4)
library(googledrive)
library(dplyr)
library(ggplot2)
library(DT)
library(hunspell)
library(qrcode)
library(nortest)
library(DiagrammeR)
library(rmarkdown)
library(tinytex)
library(knitr)
library(nnet)
library(officer)
library(flextable)

if (!rmarkdown::pandoc_available()) {
  p_info <- suppressWarnings(try(rmarkdown::find_pandoc(), silent = TRUE))
  if (is.list(p_info) && !is.null(p_info$dir)) {
    Sys.setenv(RSTUDIO_PANDOC = p_info$dir)
  }
}

# 1. Authenticate with Google Sheets & Drive
JSON_KEY_FILE <- "credentials.json"
google_json_env <- Sys.getenv("GOOGLE_JSON_KEY")

if (nchar(google_json_env) > 0) {
  temp_json <- tempfile(fileext = ".json")
  writeLines(google_json_env, temp_json)
  googlesheets4::gs4_auth(path = temp_json)
  googledrive::drive_auth(path = temp_json)
} else if (file.exists(JSON_KEY_FILE)) {
  googlesheets4::gs4_auth(path = JSON_KEY_FILE)
  googledrive::drive_auth(path = JSON_KEY_FILE)
} else {
  stop("Google credentials not found! Set GOOGLE_JSON_KEY env var or provide credentials.json.")
}

# Default pre-existing Google Sheet ID (replace with your actual Sheet ID or full URL)
DEFAULT_SHEET_ID <- "YOUR_ACTUAL_SPREADSHEET_ID"

# 2. Source modules
source("R/survey_module.R")
source("R/analytics/01_summary_meta_module.R")
source("R/analytics/02_cleaning_module.R")
source("R/analytics/03_descriptive_module.R")
source("R/analytics/04_normality_module.R")
source("R/analytics/05_hypothesis_module.R")
source("R/analytics/06_linear_regression_module.R")
source("R/analytics/07_logistic_regression_module.R")
source("R/analytics/08_summary_report_module.R")

ui <- page_navbar(
  id = "main_nav",
  title = "SURVLYTICS",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  nav_panel("1. Survey Builder", survey_builder_ui("survey_mod")),
  nav_panel("2. Active Survey", survey_active_ui("survey_mod")),
  
  nav_menu("3. Analytics Engine",
    nav_panel("Metadata & Overview", summary_meta_ui("meta_mod")),
    nav_panel("Data Cleaning", cleaning_ui("clean_mod")),
    nav_panel("Descriptive & Visuals", descriptive_ui("desc_mod")),
    nav_panel("Normality Tests", normality_ui("norm_mod")),
    nav_panel("Hypothesis Testing", hypothesis_ui("ht_mod")),
    nav_panel("Linear Regression", linear_regression_ui("lm_mod")),
    nav_panel("Logistic Regression", logistic_regression_ui("glm_mod")), 
    nav_panel("Final Summary & Workflow", summary_report_ui("sum_mod"))
  )
)

server <- function(input, output, session) {

  # Session-isolated state
  session_state <- reactiveValues(
    schema = data.frame(
      Variable = character(), 
      Prompt = character(), 
      Type = character(), 
      Options = character(), 
      stringsAsFactors = FALSE
    ),
    data = data.frame(),
    sheet_id = NULL,
    final_models = list()
  )

  # Helper to load schema and existing responses (even if 0 rows) from a Google Sheet
  # Helper to load schema and existing responses (handles completely blank sheets too)
  load_sheet_into_session <- function(sid) {
    sid <- trimws(sid)
    session_state$sheet_id <- sid
    
    tryCatch({
      sheet_names <- googlesheets4::sheet_names(sid)
      
      # 1. Check Schema tab (or first tab)
      schema_tab <- if ("Schema" %in% sheet_names) "Schema" else sheet_names[1]
      schema_remote <- as.data.frame(
        googlesheets4::read_sheet(sid, sheet = schema_tab, col_types = "c"),
        stringsAsFactors = FALSE
      )
      
      if (nrow(schema_remote) > 0 && "Variable" %in% colnames(schema_remote)) {
        # Existing schema found -> load into builder & active survey
        session_state$schema <- schema_remote
        showNotification("Loaded existing survey schema from Google Sheet.", type = "message")
      } else {
        # Sheet is completely empty -> keep sheet_id linked so user can build & push new questions
        showNotification(
          "Connected to empty Google Sheet. Add questions in the Survey Builder and save to initialize it.",
          type = "message",
          duration = 8
        )
      }
      
      # 2. Load Responses tab if present (safely handles 0 response rows)
      resp_tab <- if ("Responses" %in% sheet_names) {
        "Responses"
      } else if (length(sheet_names) >= 2) {
        sheet_names[2]
      } else {
        NULL
      }
      
      if (!is.null(resp_tab)) {
        resp_remote <- as.data.frame(
          googlesheets4::read_sheet(sid, sheet = resp_tab, col_types = "c"),
          stringsAsFactors = FALSE
        )
        session_state$data <- resp_remote
      }
      
    }, error = function(e) {
      showNotification(
        paste("Google Sheet Read Error:", e$message),
        type = "error",
        duration = 15
      )
    })
  }

  # Load default sheet on startup OR switch to respondent mode if ?mode=survey is in URL
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    # Use ?sid=... from URL if provided, otherwise fall back to DEFAULT_SHEET_ID
    target_sid <- if (!is.null(query$sid) && nzchar(trimws(query$sid))) {
      query$sid
    } else if (nzchar(DEFAULT_SHEET_ID) && DEFAULT_SHEET_ID != "YOUR_ACTUAL_SPREADSHEET_ID") {
      DEFAULT_SHEET_ID
    } else {
      NULL
    }
    
    if (!is.null(target_sid)) {
      load_sheet_into_session(target_sid)
    }
    
    if (!is.null(query$mode) && query$mode == "survey") {
      nav_select(id = "main_nav", selected = "2. Active Survey")
      nav_hide(id = "main_nav", target = "1. Survey Builder")
      nav_hide(id = "main_nav", target = "3. Analytics Engine")
    }
  })
  
  # Initialize module servers with session_state
  survey_server("survey_mod", session_state)
  summary_meta_server("meta_mod", session_state)
  cleaning_server("clean_mod", session_state)
  descriptive_server("desc_mod", session_state)
  normality_server("norm_mod", session_state)
  hypothesis_server("ht_mod", session_state)
  linear_regression_server("lm_mod", session_state)
  logistic_regression_server("glm_mod", session_state)
  summary_report_server("sum_mod", session_state)
}

shinyApp(ui, server)