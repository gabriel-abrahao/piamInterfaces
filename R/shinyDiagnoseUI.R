#' Launch Interactive Summation Error Diagnostic Shiny App
#'
#' A Shiny-based interactive tool for visualizing and correcting summation errors.
#' Shows REMIND and PIAM variables side-by-side with their components and factors.
#'
#' @param diagnostics_list List from diagnoseSummationErrors output
#' @param mapping Optional: mapping data frame to show variable components
#' @param remind_data Optional: REMIND data frame for suggesting variables
#'
#' @details
#' The app displays:
#' - Left panel: REMIND variables with piam_factors
#' - Center: Numerical values
#' - Right panel: PIAM variables
#' - Bottom: Summation totals for both sides
#'
#' Hover over PIAM variable names to see their component REMIND variables.
#'
#' @import shiny
#' @importFrom dplyr filter pull group_by summarise arrange distinct mutate
#' @keywords internal
#' @export
launchDiagnosticUI <- function(diagnostics_list, mapping = NULL, remind_data = NULL) {

  # Load required packages
  requireNamespace("shiny", quietly = FALSE)
  library(shiny)
  library(dplyr)

  # Prepare data for Shiny
  app_data <- list(
    diagnostics = diagnostics_list,
    mapping = mapping,
    remind_all = remind_data
  )

  ui <- fluidPage(
    tags$head(
      tags$style(HTML("
        body { font-family: monospace; }
        .comparison-container { display: flex; gap: 20px; margin: 20px 0; }
        .remind-side, .piam-side { flex: 1; }
        .variable-row { display: flex; gap: 10px; padding: 5px; border-bottom: 1px solid #eee; }
        .variable-row:hover { background-color: #f9f9f9; }
        .variable-name { flex: 2; font-weight: bold; }
        .variable-value { flex: 1; text-align: right; }
        .variable-factor { flex: 0.5; text-align: center; color: #666; font-size: 0.9em; }
        .piam-variable { cursor: help; text-decoration: underline; color: #0066cc; }
        .piam-variable:hover { color: #0044aa; background-color: #eeeeee; }
        .totals-row { font-weight: bold; border-top: 2px solid black; margin-top: 10px; padding: 10px 0; }
        .missing-variable { opacity: 0.5; color: #999; }
        .zero-variable { color: #ff6600; }
        .positive-value { color: #00aa00; }
        .negative-value { color: #aa0000; }
        .tooltip { position: relative; display: inline-block; }
        .tooltip .tooltiptext {
          visibility: hidden;
          width: 300px;
          background-color: #333;
          color: #fff;
          text-align: left;
          border-radius: 6px;
          padding: 10px;
          position: absolute;
          z-index: 1;
          bottom: 125%;
          left: 50%;
          margin-left: -150px;
          opacity: 0;
          transition: opacity 0.3s;
          font-size: 0.85em;
        }
        .tooltip:hover .tooltiptext { visibility: visible; opacity: 1; }
      "))
    ),

    titlePanel("Summation Error Diagnostic Tool"),

    sidebarLayout(
      sidebarPanel(
        h3("Configuration"),

        # Equation selector
        selectInput(
          "selected_parent",
          "Select Equation:",
          choices = NULL
        ),

        # Region selector
        selectInput(
          "selected_region",
          "Select Region:",
          choices = NULL
        ),

        # Year selector
        selectInput(
          "selected_period",
          "Select Year:",
          choices = NULL
        ),

        # Summary stats
        h4("Summary"),
        uiOutput("summary_stats"),

        # Error details
        h4("Error Details"),
        uiOutput("error_details"),

        width = 3
      ),

      mainPanel(
        # Equation info
        h3(textOutput("equation_title")),

        # Data sources legend
        uiOutput("data_legend"),

        # Comparison view
        fluidRow(
          column(
            6,
            h4("REMIND Components"),
            uiOutput("remind_components")
          ),
          column(
            6,
            h4("PIAM Components"),
            uiOutput("piam_components")
          )
        ),

        # Totals comparison
        fluidRow(
          column(
            12,
            h4("Summation Totals"),
            uiOutput("totals_comparison")
          )
        ),

        # Mapping browser
        h4("Mapping Browser"),
        uiOutput("mapping_browser"),

        # Suggested REMIND variables section
        h4("Suggested REMIND Variables"),
        fluidRow(
          column(3, numericInput("prefix_level", "Prefix levels (N):", value = 2, min = 1, max = 5))
        ),
        h5("Matching prefixes:"),
        verbatimTextOutput("matching_prefixes"),
        h5("Variables in MIF:"),
        tableOutput("suggested_variables_table"),

        width = 9
      )
    )
  )

  server <- function(input, output, session) {

    # Get unique equations (full name with number suffix) from error instances
    equation_names <- unique(sapply(diagnostics_list, function(x) {
      x$error_instances$variable[1]
    }))

    updateSelectInput(session, "selected_parent",
                     choices = sort(equation_names),
                     selected = equation_names[1])

    # Reactive: get regions for selected equation
    regions_for_equation <- reactive({
      if (is.null(input$selected_parent)) return(character())

      equation <- input$selected_parent
      regions <- NULL

      for (i in seq_along(diagnostics_list)) {
        err_var <- diagnostics_list[[i]]$error_instances$variable[1]
        if (err_var == equation) {
          err <- diagnostics_list[[i]]$error_instances[1, ]
          regions <- c(regions, err$region)
        }
      }

      unique(regions)
    })

    # Reactive: get periods for selected equation and region
    periods_for_equation_region <- reactive({
      if (is.null(input$selected_parent) || is.null(input$selected_region)) return(integer())

      equation <- input$selected_parent
      region <- input$selected_region
      periods <- NULL

      for (i in seq_along(diagnostics_list)) {
        err_var <- diagnostics_list[[i]]$error_instances$variable[1]
        if (err_var == equation) {
          errs <- diagnostics_list[[i]]$error_instances
          periods <- c(periods, errs$period[errs$region == region])
        }
      }

      sort(unique(periods))
    })

    # Update region choices when equation changes
    observeEvent(input$selected_parent, {
      regions <- regions_for_equation()
      updateSelectInput(session, "selected_region",
                       choices = sort(regions),
                       selected = regions[1])
    })

    # Update period choices when equation or region changes
    observeEvent(input$selected_region, {
      periods <- periods_for_equation_region()
      updateSelectInput(session, "selected_period",
                       choices = as.character(periods),
                       selected = as.character(periods[1]))
    })

    # Reactive selected diagnostic
    selected_diag <- reactive({
      if (is.null(input$selected_parent) || is.null(input$selected_region) || is.null(input$selected_period)) {
        return(NULL)
      }

      equation <- input$selected_parent
      region <- input$selected_region
      period <- suppressWarnings(as.integer(input$selected_period))

      if (is.na(period)) return(NULL)

      for (i in seq_along(diagnostics_list)) {
        diag <- diagnostics_list[[i]]
        err_var <- diag$error_instances$variable[1]
        if (!is.na(err_var) && err_var == equation) {
          err <- diag$error_instances[1, ]
          if (!is.na(err$region) && !is.na(err$period) &&
              err$region == region && err$period == period) {
            return(diag)
          }
        }
      }

      return(NULL)
    })

    # Equation title
    output$equation_title <- renderText({
      diag <- selected_diag()
      if (is.null(diag)) return("Select an equation")
      err <- diag$error_instances[1, ]
      sprintf("%s | %s | %s | %d",
              err$model, err$scenario, err$region, err$period)
    })

    # Data sources legend
    output$data_legend <- renderUI({
      HTML('
        <div style="background-color: #f5f5f5; border: 1px solid #ddd; padding: 15px; margin: 15px 0; border-radius: 5px;">
          <h4 style="margin-top: 0;">Data Sources & Calculations</h4>
          <table style="width: 100%; border-collapse: collapse; font-size: 0.95em;">
            <tr style="background-color: #e8f4f8;">
              <td style="padding: 8px; border: 1px solid #ddd; font-weight: bold; width: 30%;">
                REMIND Components (Left)
              </td>
              <td style="padding: 8px; border: 1px solid #ddd;">
                <strong>Source:</strong> REMIND MIF file (values) + Mapping file (factors)<br/>
                <strong>Calculation:</strong> Σ(REMIND_value × mapping_factor)<br/>
                <strong>Means:</strong> What the REMIND variables compose to according to the mapping
              </td>
            </tr>
            <tr style="background-color: #f0f8f0;">
              <td style="padding: 8px; border: 1px solid #ddd; font-weight: bold;">
                PIAM Components (Right)
              </td>
              <td style="padding: 8px; border: 1px solid #ddd;">
                <strong>Source:</strong> PIAM export file (values) + Summation file (expected children)<br/>
                <strong>Calculation:</strong> Σ(PIAM_child_value)<br/>
                <strong>Means:</strong> What the PIAM parent should equal (sum of children)
              </td>
            </tr>
            <tr style="background-color: #f8f4f0;">
              <td style="padding: 8px; border: 1px solid #ddd; font-weight: bold;">
                Error Details (Sidebar)
              </td>
              <td style="padding: 8px; border: 1px solid #ddd;">
                <strong>Source:</strong> Error log file from checkSummations()<br/>
                <strong>Parent Value:</strong> Actual PIAM parent value<br/>
                <strong>Sum of Children:</strong> Sum of actual PIAM children in export<br/>
                <strong>Discrepancy:</strong> Parent_Value - Sum_of_Children (should be ~0)
              </td>
            </tr>
            <tr style="background-color: #f8e8f4;">
              <td style="padding: 8px; border: 1px solid #ddd; font-weight: bold;">
                Summation Totals (Bottom)
              </td>
              <td style="padding: 8px; border: 1px solid #ddd;">
                <strong>REMIND Sum:</strong> From error log (same as left panel total)<br/>
                <strong>Expected PIAM Sum:</strong> From summation file (same as right panel total)<br/>
                <strong>Discrepancy:</strong> Expected_PIAM_Sum - REMIND_Sum (should be ~0 if correct)
              </td>
            </tr>
          </table>
        </div>
      ')
    })

    # Summary stats
    output$summary_stats <- renderUI({
      diag <- selected_diag()
      if (is.null(diag)) return(NULL)

      n_children <- length(diag$children)
      n_errors <- nrow(diag$error_instances)
      parent_name <- diag$parent

      HTML(sprintf("
        <p><strong>Parent:</strong> %s</p>
        <p><strong>Child variables:</strong> %d</p>
        <p><strong>Error instances:</strong> %d</p>
      ", parent_name, n_children, n_errors))
    })

    # Error details
    output$error_details <- renderUI({
      diag <- selected_diag()
      if (is.null(diag)) return(NULL)

      err <- diag$error_instances[1, ]

      HTML(sprintf("
        <p><strong>Parent Value</strong><br/><span style='font-size: 0.9em;'>(from error log)</span><br/>%.4f</p>
        <p><strong>Sum of Children</strong><br/><span style='font-size: 0.9em;'>(sum of PIAM children)</span><br/>%.4f</p>
        <p><strong>Discrepancy</strong><br/><span style='font-size: 0.9em;'>(Parent - Sum)</span><br/>%.4f</p>
        <p><strong>Rel. Error</strong><br/><span style='font-size: 0.9em;'>(percent)</span><br/>%.1f%%</p>
      ", err$value, err$checkSum, err$diff, err$reldiff))
    })

    # REMIND components (sources that compose parent)
    output$remind_components <- renderUI({
      if (is.null(mapping)) {
        return(p("(Mapping not available)", style="color: #999; font-style: italic;"))
      }

      diag <- selected_diag()
      if (is.null(diag)) return(p("No data"))

      parent <- diag$parent
      remind_data <- diag$remind_data

      parent_mapping <- mapping %>%
        filter(.data$variable == parent) %>%
        distinct(.data$piam_variable, .data$piam_factor)

      if (nrow(parent_mapping) == 0) {
        return(p("(No REMIND sources found in mapping for this parent)",
                 style="color: #999; font-style: italic;"))
      }

      parent_with_values <- parent_mapping %>%
        left_join(
          remind_data %>% distinct(.data$piam_variable, .data$value),
          by = "piam_variable"
        ) %>%
        mutate(
          # Calculate adjusted value (with factor applied)
          adjusted_value = .data$value * ifelse(is.na(.data$piam_factor), 1, .data$piam_factor)
        ) %>%
        arrange(desc(.data$adjusted_value))

      rows <- lapply(seq_len(nrow(parent_with_values)), function(i) {
        var <- parent_with_values$piam_variable[i]
        raw_val <- parent_with_values$value[i]
        factor <- parent_with_values$piam_factor[i]
        adjusted_val <- parent_with_values$adjusted_value[i]

        is_missing <- is.na(raw_val)
        is_na_var <- is.na(var) || var == "" || var == "NA"
        var_class <- if (is_na_var || is_missing) "missing-variable" else ""
        var_display <- if (is_na_var) "NA" else var

        # Display adjusted value, with color based on whether it's positive or negative
        val_class <- if (!is_missing && adjusted_val >= 0) "positive-value" else if (!is_missing) "negative-value" else "missing-variable"
        val_str <- if (is_missing) "NA" else sprintf("%.4f", adjusted_val)

        factor_str <- if (is.na(factor) || factor == 1 || factor == "1") "" else sprintf(" ×%s", factor)

        HTML(sprintf(
          '<div class="variable-row %s">
             <div class="variable-name">%s%s</div>
             <div class="variable-value %s">%s</div>
           </div>',
          var_class, var_display, factor_str, val_class, val_str
        ))
      })

      # Calculate total: Σ(adjusted_value), ignoring NA values
      total_remind <- sum(parent_with_values$adjusted_value, na.rm = TRUE)
      rows[[length(rows) + 1]] <- HTML(sprintf(
        '<div class="totals-row" title="Sum of all REMIND values multiplied by their mapping factors. Source: REMIND MIF + Mapping file">
           <div class="variable-name" style="border-bottom: 1px dotted #0066cc;">
             <strong>REMIND Sum</strong><br/>
             <span style="font-weight: normal; font-size: 0.85em; color: #666;">
               Σ(value × factor)<br/>
               <span style="font-size: 0.8em; color: #999;">From: REMIND MIF + Mapping</span>
             </span>
           </div>
           <div class="variable-value">%.4f</div>
         </div>',
        total_remind
      ))

      do.call(tagList, rows)
    })

    # PIAM components from piam_data (sorted by descending value)
    output$piam_components <- renderUI({
      diag <- selected_diag()
      if (is.null(diag)) return(p("No data"))

      piam_data <- diag$piam_data
      if (nrow(piam_data) == 0) {
        return(p("(No PIAM data available)",
                 style="color: #999; font-style: italic;"))
      }

      piam_sorted <- piam_data %>% arrange(desc(.data$value))

      rows <- lapply(seq_len(nrow(piam_sorted)), function(i) {
        var <- piam_sorted$variable[i]
        val <- piam_sorted$value[i]

        is_missing <- is.na(val)
        var_class <- if (is_missing) "missing-variable" else ""
        val_class <- if (!is_missing && val >= 0) "positive-value" else if (!is_missing) "negative-value" else "missing-variable"

        val_str <- if (is_missing) "NA" else sprintf("%.4f", val)

        HTML(sprintf(
          '<div class="variable-row %s">
             <div class="variable-name">%s</div>
             <div class="variable-value %s">%s</div>
           </div>',
          var_class, var, val_class, val_str
        ))
      })

      # Calculate total: Σ(value), ignoring NA values
      total_piam <- sum(piam_sorted$value, na.rm = TRUE)
      rows[[length(rows) + 1]] <- HTML(sprintf(
        '<div class="totals-row" title="Sum of all PIAM child variables. What the parent should equal. Source: PIAM export + Summation file">
           <div class="variable-name" style="border-bottom: 1px dotted #00aa00;">
             <strong>PIAM Sum</strong><br/>
             <span style="font-weight: normal; font-size: 0.85em; color: #666;">
               Σ(child_value)<br/>
               <span style="font-size: 0.8em; color: #999;">From: PIAM export + Summation</span>
             </span>
           </div>
           <div class="variable-value">%.4f</div>
         </div>',
        total_piam
      ))

      do.call(tagList, rows)
    })

    # Totals comparison
    output$totals_comparison <- renderUI({
      diag <- selected_diag()
      if (is.null(diag)) return(NULL)

      totals <- getSummationTotals(diag, by_variables = FALSE)

      remind_total <- totals %>% filter(.data$side == "REMIND") %>% pull("total") %>% sum(na.rm = TRUE)
      piam_total <- totals %>% filter(.data$side == "PIAM") %>% pull("total") %>% sum(na.rm = TRUE)

      # Handle case where no values exist
      if (is.na(remind_total) || is.na(piam_total)) {
        return(p("(No data available for totals comparison)", style="color: #999; font-style: italic;"))
      }

      diff <- piam_total - remind_total
      pct_diff <- 100 * diff / abs(piam_total + 1e-10)

      diff_class <- if (!is.na(diff) && abs(diff) < 0.01) "positive-value" else "negative-value"

      HTML(sprintf(
        '<div style="display: flex; gap: 25px; flex-wrap: wrap; padding: 15px; background-color: #f9f9f9; border-radius: 5px; border-left: 4px solid #0066cc;">
           <div title="Sum of REMIND components (value × factor). Should match PIAM sum if correct.">
             <strong style="color: #0066cc;">REMIND Sum</strong><br/>
             <span style="font-size: 0.85em; color: #666;">Sum of REMIND<br/>components</span><br/>
             <span class="positive-value" style="font-size: 1.2em;">%.4f</span>
           </div>
           <div title="Sum of PIAM children from export. Parent should equal this sum.">
             <strong style="color: #00aa00;">PIAM Sum</strong><br/>
             <span style="font-size: 0.85em; color: #666;">Sum of PIAM<br/>children</span><br/>
             <span class="positive-value" style="font-size: 1.2em;">%.4f</span>
           </div>
           <div title="Difference between PIAM and REMIND sums. Should be ~0 if summation is correct.">
             <strong style="color: #aa0000;">Discrepancy</strong><br/>
             <span style="font-size: 0.85em; color: #666;">PIAM - REMIND<br/>(should be ~0)</span><br/>
             <span class="%s" style="font-size: 1.2em;">%.4f<br/>(%.2f%%)</span>
           </div>
         </div>',
        remind_total, piam_total, diff_class, diff, pct_diff
      ))
    })

    # Mapping browser
    output$mapping_browser <- renderUI({
      if (is.null(mapping)) return(p("Mapping not provided"))

      diag <- selected_diag()
      if (is.null(diag)) return(NULL)

      parent <- diag$parent

      # Show mapping for parent variable
      parent_mapping <- mapping %>%
        filter(.data$variable == parent) %>%
        select(.data$piam_variable, .data$piam_factor)

      if (nrow(parent_mapping) == 0) {
        return(p("No mapping found for this parent"))
      }

      # Create table
      table_html <- "<table style='width:100%; border-collapse: collapse;'>
                      <tr style='background-color: #f0f0f0;'>
                        <th style='border: 1px solid #ddd; padding: 8px; text-align: left;'>REMIND Variable</th>
                        <th style='border: 1px solid #ddd; padding: 8px; text-align: center;'>Factor</th>
                      </tr>"

      for (i in seq_len(nrow(parent_mapping))) {
        var <- parent_mapping$piam_variable[i]
        factor <- parent_mapping$piam_factor[i]

        table_html <- paste0(table_html, sprintf(
          "<tr style='border: 1px solid #ddd;'>
             <td style='border: 1px solid #ddd; padding: 8px;'>%s</td>
             <td style='border: 1px solid #ddd; padding: 8px; text-align: center;'>%s</td>
           </tr>",
          var, factor
        ))
      }

      table_html <- paste0(table_html, "</table>")

      HTML(sprintf(
        "<p><strong>Mapping for %s:</strong></p>%s",
        parent, table_html
      ))
    })

    # Suggested REMIND variables - matching prefixes
    output$matching_prefixes <- renderText({
      diag <- selected_diag()
      if (is.null(diag) || nrow(diag$remind_data) == 0) {
        return("(Select equation, region, and year)")
      }

      n <- input$prefix_level
      if (is.null(n)) n <- 2

      remind_vars <- diag$remind_data$piam_variable
      prefixes <- getUniquePrefixes(remind_vars, n = n)

      if (length(prefixes) == 0) {
        return("(No prefixes found)")
      }

      paste(prefixes, collapse = "\n")
    })

    # Suggested REMIND variables - matching table
    output$suggested_variables_table <- renderTable({
      diag <- selected_diag()
      if (is.null(diag) || is.null(app_data$remind_all)) {
        return(data.frame(Variable = "No data", Value = NA))
      }

      n <- input$prefix_level
      if (is.null(n)) n <- 2

      remind_vars <- diag$remind_data$piam_variable
      prefixes <- getUniquePrefixes(remind_vars, n = n)

      if (length(prefixes) == 0) {
        return(data.frame(Variable = "No prefixes found", Value = NA))
      }

      # Get scenario from diagnostic error instance
      err <- diag$error_instances[1, ]
      scenario <- if (!is.na(err$scenario)) err$scenario else NULL

      # Find matching variables in REMIND data
      matches <- findMatchingRemindVariables(
        app_data$remind_all,
        prefixes,
        scenario = scenario,
        region = input$selected_region,
        period = as.integer(input$selected_period)
      )

      if (nrow(matches) == 0) {
        return(data.frame(Variable = "No matching variables", Value = NA))
      }

      # Rename for display
      names(matches) <- c("Variable", "Value")
      matches
    }, digits = 4)
  }

  shinyApp(ui, server)
}
