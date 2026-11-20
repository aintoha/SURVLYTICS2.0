# R/analytics/08_summary_report_module.R

summary_report_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      h4("Report Builder & Completion"),
      p("Select the modules you wish to include in your final Word summary report:"),
      
      checkboxGroupInput(
        ns("include_modules"),
        "Include Modules in Summary:",
        choices = c(
          "Data Preprocessing",
          "Data Wrangling",
          "Descriptive Statistics",
          "Normality Diagnostics",
          "Hypothesis Testing",
          "Linear Regression",
          "Logistic Regression"
        ),
        selected = c(
          "Data Preprocessing",
          "Data Wrangling",
          "Descriptive Statistics",
          "Normality Diagnostics",
          "Hypothesis Testing",
          "Linear Regression",
          "Logistic Regression"
        )
      ),
      
      hr(),
      actionButton(ns("finish_analysis"), "Refresh Audit & Diagram", class = "btn-success w-100 mb-3"),
      downloadButton(ns("dl_word_summary"), "Download Word Summary (.docx)", class = "btn-primary w-100")
    ),
    navset_card_tab(
      nav_panel("Narrative Builder", 
        card(
          card_header(
            class = "d-flex justify-content-between align-items-center bg-primary text-white",
            "Human-in-the-Loop Narrative Explanation",
            actionButton(ns("generate_draft"), "Auto-Generate Draft", class = "btn-sm btn-light text-primary fw-bold")
          ),
          p(tags$small("Generate a draft based on your models, then edit it to add your own expert context before downloading the report.")),
          textAreaInput(
            ns("editable_narrative"), 
            label = NULL,
            value = "Click 'Auto-Generate Draft' above to pull your findings, or type your own interpretation here...",
            width = "100%", 
            height = "350px"
          )
        )
      ),
      nav_panel("Workflow Diagram", 
        card(
          card_header("Analytical Execution Flowchart"),
          DiagrammeR::grVizOutput(ns("workflow_diagram"), height = "450px")
        )
      ),
      nav_panel("Executed Analyses Audit", 
        card(
          card_header(
            class = "d-flex justify-content-between align-items-center",
            "Session Audit Log (Select rows to remove unwanted runs)",
            actionButton(ns("delete_selected_row"), "Delete Selected Row(s)", class = "btn-sm btn-outline-danger")
          ),
          DTOutput(ns("audit_table"))
        )
      )
    )
  )
}

summary_report_server <- function(id, shared_state) {
  moduleServer(id, function(input, output, session) {
    
    # 1. Consolidate Audit Log and Final Models Safely (MERGE BOTH!)
    raw_audit_df <- reactive({
      input$finish_analysis
      
      log_list <- list()
      
      # A) Pull from General Audit Log (Descriptive, Normality, etc.)
      if (!is.null(shared_state$audit_log) && nrow(shared_state$audit_log) > 0) {
        log_list[[1]] <- shared_state$audit_log
      }
      
      # B) Pull from Final Models (Hypothesis, Regression)
      models <- shared_state$final_models
      if (!is.null(models) && length(models) > 0) {
        valid_models <- Filter(Negate(is.null), models)
        
        if (length(valid_models) > 0) {
          standardized_list <- lapply(names(valid_models), function(mod_name) {
            df <- valid_models[[mod_name]]
            if (is.null(df) || nrow(df) == 0) return(NULL)
            
            if (!"Module" %in% names(df)) {
              df$Module <- mod_name
            }
            
            data.frame(lapply(df, as.character), stringsAsFactors = FALSE, check.names = FALSE)
          })
          
          standardized_list <- Filter(Negate(is.null), standardized_list)
          
          if (length(standardized_list) > 0) {
            all_cols <- unique(unlist(lapply(standardized_list, names)))
            aligned_list <- lapply(standardized_list, function(df) {
              missing_cols <- setdiff(all_cols, names(df))
              for (col in missing_cols) df[[col]] <- ""
              df[, all_cols, drop = FALSE]
            })
            log_list[[2]] <- do.call(rbind, aligned_list)
          }
        }
      }
      
      # C) Combine Both
      if (length(log_list) == 0) return(empty_audit_df())
      
      all_final_cols <- unique(unlist(lapply(log_list, names)))
      final_aligned <- lapply(log_list, function(df) {
        missing_cols <- setdiff(all_final_cols, names(df))
        for (col in missing_cols) df[[col]] <- ""
        df[, all_final_cols, drop = FALSE]
      })
      
      do.call(rbind, final_aligned)
    })
    
    empty_audit_df <- function() {
      data.frame(
        Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        Module = "None",
        Action = "No Analysis Executed",
        Details = "No actions recorded during this session.",
        stringsAsFactors = FALSE
      )
    }

    filtered_audit_df <- reactive({
      df <- raw_audit_df()
      if ("Module" %in% names(df) && !is.null(input$include_modules)) {
        df <- df[df$Module %in% c(input$include_modules, "None"), , drop = FALSE]
      }
      df
    })
    
    # 2. Delete Selected Rows from Audit Table
    observeEvent(input$delete_selected_row, {
      req(input$audit_table_rows_selected)
      showNotification("Row deletion is restricted while viewing merged Audit & Models. To reset, start a new session.", type = "warning")
    })
    
    # 3. Auto-Generate Draft Narrative
    observeEvent(input$generate_draft, {
      df_log <- as.data.frame(filtered_audit_df())
      
      is_empty <- FALSE
      if (is.null(df_log) || nrow(df_log) == 0) {
        is_empty <- TRUE
      } else if ("Module" %in% colnames(df_log)) {
        if (!is.na(df_log$Module[1]) && df_log$Module[1] == "None") {
          is_empty <- TRUE
        }
      }
      
      if (is_empty) {
        updateTextAreaInput(session, "editable_narrative",
          value = "No analyses have been executed yet. Please run tests in the analytics engine first.")
        return()
      }
      
      draft <- "EXECUTIVE ANALYTICS SUMMARY

"
      
      n_rows <- if(!is.null(shared_state$data)) nrow(shared_state$data) else 0
      n_cols <- if(!is.null(shared_state$data)) ncol(shared_state$data) else 0
      
      draft <- paste0(draft, sprintf("Based on the dataset provided (%d rows and %d columns), the following analytical procedures were performed and interpreted:

", n_rows, n_cols))
      
      has_mod <- "Module" %in% colnames(df_log)
      has_act <- "Action" %in% colnames(df_log)
      has_det <- "Details" %in% colnames(df_log)
      
      for(i in seq_len(nrow(df_log))) {
        mod_val <- if(has_mod && !is.na(df_log$Module[i])) df_log$Module[i] else paste("Analysis", i)
        act_val <- if(has_act && !is.na(df_log$Action[i])) df_log$Action[i] else "Executed statistical test."
        det_val <- if(has_det && !is.na(df_log$Details[i])) df_log$Details[i] else "Results recorded."
        
        draft <- paste0(draft, "### ", mod_val, " ###
")
        draft <- paste0(draft, "Action: ", act_val, "
")
        draft <- paste0(draft, "Result/Details: ", det_val, "

")
      }
      
      draft <- paste0(draft, "--- ANALYST INTERPRETATION ---
")
      draft <- paste0(draft, "[Replace this text with your specific business context. E.g., The linear regression confirms a significant positive relationship (p < 0.05) between X and Y, suggesting that increasing X will lead to...]
")
      
      updateTextAreaInput(session, "editable_narrative", value = draft)
      showNotification("Draft narrative generated! You can now edit the text.", type = "message")
    })
    
    # 4. Generate Workflow Diagram (UPDATED TO HIGHLIGHT EXECUTED MODULES)
    output$workflow_diagram <- DiagrammeR::renderGrViz({
      df_log <- filtered_audit_df()
      active_modules <- if ("Module" %in% names(df_log)) df_log$Module else character(0)
      
      # Determine if a module has been executed and logged in the Audit Table
      has_clean <- "Data Cleaning" %in% active_modules || "Data Preprocessing" %in% active_modules || "Data Wrangling" %in% active_modules
      has_desc  <- "Descriptive Statistics" %in% active_modules || "Descriptives" %in% active_modules
      has_norm  <- "Normality Diagnostics" %in% active_modules || "Normality Tests" %in% active_modules
      has_ht    <- "Hypothesis Testing" %in% active_modules
      has_lm    <- "Linear Regression" %in% active_modules
      has_glm   <- "Logistic Regression" %in% active_modules
      
      total_rows <- if(!is.null(shared_state$data)) nrow(shared_state$data) else 0
      total_cols <- if(!is.null(shared_state$data)) ncol(shared_state$data) else 0
      
      # Visual definitions: Blue for executed, Grey for unexecuted
      color_executed_fill <- "#cce5ff"  # Light Blue
      color_executed_border <- "#004085" # Dark Blue
      
      color_unexecuted_fill <- "#e2e3e5" # Light Grey
      color_unexecuted_border <- "#6c757d" # Dark Grey
      
      dot_script <- paste0("
        digraph workflow {
          graph [layout = dot, rankdir = LR, nodesep = 0.5, ranksep = 0.8]
          node [shape = rectangle, style = filled, fontname = Helvetica, fontsize = 11, fixedsize = false]
          
          Start [label = 'Data Loaded
(", total_rows, " rows, ", total_cols, " cols)', fillcolor = '#d4edda', color = '#28a745']
          
          Clean [label = 'Data Wrangling', 
                 fillcolor = '", if(has_clean) color_executed_fill else color_unexecuted_fill, "', 
                 color = '", if(has_clean) color_executed_border else color_unexecuted_border, "']
                 
          Desc  [label = 'Descriptives & Visuals', 
                 fillcolor = '", if(has_desc) color_executed_fill else color_unexecuted_fill, "', 
                 color = '", if(has_desc) color_executed_border else color_unexecuted_border, "']
                 
          Norm  [label = 'Normality Tests', 
                 fillcolor = '", if(has_norm) color_executed_fill else color_unexecuted_fill, "', 
                 color = '", if(has_norm) color_executed_border else color_unexecuted_border, "']
                 
          HT    [label = 'Hypothesis Testing', 
                 fillcolor = '", if(has_ht) color_executed_fill else color_unexecuted_fill, "', 
                 color = '", if(has_ht) color_executed_border else color_unexecuted_border, "']
                 
          LM    [label = 'Linear Regression', 
                 fillcolor = '", if(has_lm) color_executed_fill else color_unexecuted_fill, "', 
                 color = '", if(has_lm) color_executed_border else color_unexecuted_border, "']
                 
          GLM   [label = 'Logistic Regression', 
                 fillcolor = '", if(has_glm) color_executed_fill else color_unexecuted_fill, "', 
                 color = '", if(has_glm) color_executed_border else color_unexecuted_border, "']
                 
          End   [label = 'Final Summary Report', fillcolor = '#f8d7da', color = '#dc3545']
          
          Start -> Clean -> Desc -> Norm -> HT
          HT -> LM -> End
          HT -> GLM -> End
          HT -> End
        }
      ")
      DiagrammeR::grViz(dot_script)
    })
    
    # 5. Render Audit Log Table
    output$audit_table <- renderDT({
      datatable(
        filtered_audit_df(), 
        rownames = FALSE, 
        selection = "multiple",
        options = list(pageLength = 10, dom = 'ftip', scrollX = TRUE)
      )
    })
    
    # 6. Native .docx Download Handler
    output$dl_word_summary <- downloadHandler(
      filename = function() { 
        paste0("Executive_Analytics_Summary_", Sys.Date(), ".docx") 
      },
      content = function(file) {
        raw_df <- filtered_audit_df()
        req(nrow(raw_df) > 0)
        
        log_data <- data.frame(
          lapply(raw_df, function(col) {
            clean_col <- as.character(col)
            clean_col[is.na(clean_col)] <- ""
            clean_col
          }),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        
        tryCatch({
          req(requireNamespace("officer", quietly = TRUE))
          
          doc <- officer::read_docx()
          
          doc <- officer::body_add_par(doc, "Executive Data Science & Statistical Analysis Report", style = "heading 1")
          doc <- officer::body_add_par(doc, paste("Generated on:", format(Sys.Date(), "%B %d, %Y")), style = "Normal")
          doc <- officer::body_add_par(doc, "", style = "Normal")
          
          doc <- officer::body_add_par(doc, "1. Executive Overview", style = "heading 2")
          doc <- officer::body_add_par(doc, paste("- Total Dataset Records (Rows):", if(!is.null(shared_state$data)) nrow(shared_state$data) else 0), style = "Normal")
          doc <- officer::body_add_par(doc, paste("- Total Attributes (Columns):", if(!is.null(shared_state$data)) ncol(shared_state$data) else 0), style = "Normal")
          doc <- officer::body_add_par(doc, paste("- Included Modules:", paste(input$include_modules, collapse = ", ")), style = "Normal")
          doc <- officer::body_add_par(doc, "", style = "Normal")
          
          doc <- officer::body_add_par(doc, "2. Analyst Interpretation & Explanations", style = "heading 2")
          
          narrative_lines <- unlist(strsplit(input$editable_narrative, "
"))
          for (line in narrative_lines) {
            if (nzchar(trimws(line))) {
              doc <- officer::body_add_par(doc, line, style = "Normal")
            } else {
              doc <- officer::body_add_par(doc, "", style = "Normal")
            }
          }
          doc <- officer::body_add_par(doc, "", style = "Normal")
          
          doc <- officer::body_add_par(doc, "3. Validated Audit Trail of Executed Models", style = "heading 2")
          doc <- officer::body_add_table(doc, value = log_data, style = "table_template")
          
          print(doc, target = file)
          
        }, error = function(e) {
          showNotification(paste("Error creating Word file:", e$message), type = "error", duration = 10)
        })
      }
    )
  })
}