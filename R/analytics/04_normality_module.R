# R/analytics/07_normality_module.R

normality_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      h4("Normality Diagnostics"),
      selectInput(ns("num_var"), "1. Select Numeric Variable:", choices = NULL),
      selectInput(ns("group_var"), "2. Group by Categorical Variable (Optional):", choices = "None"),
      
      uiOutput(ns("bin_control")),
      
      hr(),
      
      # NEW: Execute & Add to Summary Table Button
      actionButton(ns("add_to_summary"), "Execute & Add to Summary Table", class = "btn-dark w-100 mb-2", style = "background-color: #2c3e50; color: white;"),
      downloadButton(ns("dl_norm_stats"), "Download Metrics CSV", class = "btn-outline-primary btn-sm w-100")
    ),
    navset_card_tab(
      nav_panel("Statistical Tests & Metrics", 
        card(
          card_header("Normality Summary Table"),
          DTOutput(ns("norm_table")),
          card_footer("Note: Shapiro-Wilk is recommended for N <= 5000. Lilliefors (K-S) test is used as a standard continuous alternative.")
        )
      ),
      nav_panel("Histogram & Distribution", 
        plotOutput(ns("norm_hist"), height = "450px")
      ),
      nav_panel("Q-Q Plot", 
        plotOutput(ns("norm_qq"), height = "450px")
      )
    )
  )
}

normality_server <- function(id, shared_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Dynamic Dropdown Updates
    observe({
      req(shared_state$data)
      df <- as.data.frame(shared_state$data)
      req(nrow(df) > 0, ncol(df) > 0)
      
      # Detect numeric columns even if imported as character strings from Google Sheets
      is_convertible_num <- sapply(df, function(col) {
        if (is.numeric(col)) return(TRUE)
        clean_col <- col[!is.na(col) & trimws(as.character(col)) != ""]
        if (length(clean_col) == 0) return(FALSE)
        converted <- suppressWarnings(as.numeric(clean_col))
        !any(is.na(converted))
      })
      
      num_cols <- names(df)[is_convertible_num]
      cat_cols <- setdiff(names(df), num_cols)
      
      updateSelectInput(session, "num_var", 
                         choices = if (length(num_cols) > 0) num_cols else c("No numeric columns available" = ""),
                         selected = if (length(num_cols) > 0) num_cols[1] else NULL)
                         
      updateSelectInput(session, "group_var", 
                         choices = c("None", cat_cols),
                         selected = "None")
    })
    
    # Dynamic Bin Width Slider
    output$bin_control <- renderUI({
      req(input$num_var, shared_state$data)
      x_data <- shared_state$data[[input$num_var]]
      req(is.numeric(x_data))
      
      rng <- diff(range(x_data, na.rm = TRUE))
      init_bin <- round(rng / 20, 2)
      init_bin <- if(init_bin <= 0) 0.5 else init_bin
      
      sliderInput(ns("bin_width"), "Histogram Bin Width:", 
                  min = round(rng / 100, 2) + 0.01, 
                  max = round(rng / 5, 2) + 0.1, 
                  value = init_bin, 
                  step = 0.01)
    })
    
    # Helper Math Functions for Skewness & Kurtosis
    calc_skewness <- function(x) {
      n <- length(x)
      if (n < 3) return(NA)
      m3 <- sum((x - mean(x))^3) / n
      s3 <- (sd(x) * sqrt((n - 1) / n))^3
      m3 / s3
    }
    
    calc_kurtosis <- function(x) {
      n <- length(x)
      if (n < 4) return(NA)
      m4 <- sum((x - mean(x))^4) / n
      s4 <- (sd(x) * sqrt((n - 1) / n))^4
      (m4 / s4) - 3  # Excess Kurtosis
    }

    # 2. Compute Normality Statistics Table
    norm_metrics_df <- reactive({
      req(input$num_var, shared_state$data)
      df <- shared_state$data
      var_name <- input$num_var
      grp_name <- input$group_var
      
      compute_group_stats <- function(sub_x, grp_label = "All Data") {
        sub_x <- na.omit(sub_x)
        n <- length(sub_x)
        
        if (n < 3) {
          return(data.frame(
            Group = grp_label, N = n, Mean = mean(sub_x), SD = sd(sub_x),
            Skewness = NA, Kurtosis = NA, 
            `Shapiro p-value` = NA, `Lilliefors p-value` = NA,
            Interpretation = "Insufficient Data (N < 3)", check.names = FALSE
          ))
        }
        
        # Shapiro-Wilk (valid for 3 <= N <= 5000)
        sw_p <- if (n >= 3 && n <= 5000) round(shapiro.test(sub_x)$p.value, 4) else NA
        
        # Lilliefors (K-S) Test
        ks_p <- if (length(unique(sub_x)) > 1) round(nortest::lillie.test(sub_x)$p.value, 4) else NA
        
        # Normality Verdict based on Shapiro (or Lilliefors if N > 5000)
        eval_p <- if (!is.na(sw_p)) sw_p else ks_p
        verdict <- if (!is.na(eval_p) && eval_p > 0.05) "Normal (p > 0.05)" else "Non-Normal (p <= 0.05)"
        
        data.frame(
          Group = grp_label,
          N = n,
          Mean = round(mean(sub_x), 3),
          SD = round(sd(sub_x), 3),
          Skewness = round(calc_skewness(sub_x), 3),
          Kurtosis = round(calc_kurtosis(sub_x), 3),
          `Shapiro p-value` = ifelse(is.na(sw_p), "N/A", sw_p),
          `Lilliefors p-value` = ifelse(is.na(ks_p), "N/A", ks_p),
          Interpretation = verdict,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      }
      
      if (grp_name == "None") {
        compute_group_stats(df[[var_name]], "All Data")
      } else {
        split_data <- split(df[[var_name]], df[[grp_name]])
        res <- lapply(names(split_data), function(g) compute_group_stats(split_data[[g]], g))
        do.call(rbind, res)
      }
    })

    # Render Summary Table
    output$norm_table <- renderDT({
      df <- norm_metrics_df()
      req(df)
      datatable(df, rownames = FALSE, options = list(dom = 't', scrollX = TRUE))
    })

    # Download Handler
    output$dl_norm_stats <- downloadHandler(
      filename = function() { paste0("normality_analysis_", Sys.Date(), ".csv") },
      content = function(file) { write.csv(norm_metrics_df(), file, row.names = FALSE) }
    )

    # 3. Visualizations: Histogram with Fitted Normal Curve
    output$norm_hist <- renderPlot({
      req(input$num_var, shared_state$data)
      df <- shared_state$data
      x <- input$num_var
      grp <- if(input$group_var != "None") input$group_var else NULL
      bw <- if(!is.null(input$bin_width)) input$bin_width else NULL
      
      p <- ggplot(df, aes(x = .data[[x]]))
      
      if (!is.null(grp)) {
        p <- p + geom_histogram(aes(y = after_stat(density), fill = as.factor(.data[[grp]])), 
                                binwidth = bw, color = "white", alpha = 0.7) +
                 facet_wrap(vars(.data[[grp]])) +
                 labs(fill = grp)
      } else {
        p <- p + geom_histogram(aes(y = after_stat(density)), 
                                binwidth = bw, fill = "steelblue", color = "white", alpha = 0.7) +
                 stat_function(fun = dnorm, 
                               args = list(mean = mean(df[[x]], na.rm = TRUE), sd = sd(df[[x]], na.rm = TRUE)), 
                               color = "red", linewidth = 1)
      }
      
      p + theme_minimal(base_size = 14) + 
          labs(title = paste("Distribution & Normal Overlay for", x), y = "Density") +
          theme(legend.position = "top")
    })

    # Visualizations: Q-Q Plot
    output$norm_qq <- renderPlot({
      req(input$num_var, shared_state$data)
      df <- shared_state$data
      x <- input$num_var
      grp <- if(input$group_var != "None") input$group_var else NULL
      
      p <- ggplot(df, aes(sample = .data[[x]])) +
        stat_qq(color = "steelblue", size = 2) +
        stat_qq_line(color = "red", linewidth = 1)
      
      if (!is.null(grp)) {
        p <- p + facet_wrap(vars(.data[[grp]]))
      }
      
      p + theme_minimal(base_size = 14) + 
          labs(title = paste("Normal Q-Q Plot for", x), x = "Theoretical Quantiles", y = "Sample Quantiles")
    })
    
    # ---------------------------------------------------------------------
    # NEW: EXECUTE & LOG TO SUMMARY AUDIT TABLE
    # ---------------------------------------------------------------------
    observeEvent(input$add_to_summary, {
      req(input$num_var)
      
      action_desc <- "Executed Normality Diagnostics"
      
      details_desc <- paste0("Tested variable: '", input$num_var, "'")
      if (input$group_var != "None") {
        details_desc <- paste0(details_desc, ", grouped by '", input$group_var, "'")
      }
      
      # Optional: Extract the actual verdict if the table has been rendered
      df_metrics <- norm_metrics_df()
      if (!is.null(df_metrics) && nrow(df_metrics) > 0) {
        # If no grouping, grab the first interpretation. If grouped, summarize.
        if (nrow(df_metrics) == 1) {
          details_desc <- paste0(details_desc, ". Result: ", df_metrics$Interpretation[1])
        } else {
          details_desc <- paste0(details_desc, ". Results calculated across ", nrow(df_metrics), " groups.")
        }
      }
      
      new_log <- data.frame(
        Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        Module = "Normality Diagnostics",
        Action = action_desc,
        Details = details_desc,
        stringsAsFactors = FALSE
      )
      
      if (is.null(shared_state$audit_log) || nrow(shared_state$audit_log) == 0) {
        shared_state$audit_log <- new_log
      } else {
        shared_state$audit_log <- rbind(shared_state$audit_log, new_log)
      }
      
      showNotification("Normality test logged to Final Summary Workflow!", type = "message")
    })
    
  })
}