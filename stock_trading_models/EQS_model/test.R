###############################################################################
# EQS Shiny App  | Version 1.7.1
# - Adds "id" column to track rows in the original dataframe
# - Two download buttons:
#     1) "Download EQS results"  -> only the ranked subset
#     2) "Download All + EQS"    -> original data left-joined with EQS results
# - FIX: ensure the “All + EQS” download is written as a proper XLSX file
###############################################################################

library(shiny)
library(readxl)
library(curl)
library(openxlsx)   # for writing Excel files

# Fallback data URL
fallback_excel_url <- "https://raw.githubusercontent.com/1Ramirez7/fintech-notebook/main/stock_trading_models/EQS_model/data/papersample.xlsx"

##########################
# UI Panel section
##########################
ui <- fluidPage(
  titlePanel("Equity Screening"),
  fluidRow(
    column(
      4,
      fileInput("file", "Choose Excel File", accept = ".xlsx"),
      uiOutput("var_select"),
      uiOutput("display_var_select"),
      # Updated filter selection UI
      checkboxInput("apply_filter", "Apply Group Filter", value = FALSE), # v1.6
      conditionalPanel( # v1.6
        condition = "input.apply_filter == true",
        selectInput("filter_column", "Select Column for Grouping", choices = NULL)
      ), # v1.6 end
      checkboxGroupInput(
        "exclude_options", "Exclude obs. w/",
        choices = list("Blanks" = "blanks", "0" = "zero")
      ),
      numericInput("num_results", "Number of Results per Group", 10, min = 1),
      selectInput("show", "Show", choices = c("Top" = "Top", "Bottom" = "Bottom")),
      actionButton("rank", "Calculate Rankings"),
      downloadButton("download_eqs",  "Download EQS results"),   # NEW
      downloadButton("download_full", "Download All + EQS")      # NEW
    ),
    column(8, uiOutput("ranking_controls"))
  ),
  tableOutput("resultsTable")
)

##########################
# Server Logic section
##########################
server <- function(input, output, session) {
  
  ##########################
  # Data Loading section
  ##########################
  get_data <- reactive({
    if (!is.null(input$file)) {
      df <- read_excel(input$file$datapath)
    } else {
      tmp <- tempfile(fileext = ".xlsx")
      curl_download(fallback_excel_url, tmp, mode = "wb")
      df <- read_excel(tmp)
    }
    
    # Add id column: only rows with at least one non-NA, non-blank value
    is_nonempty_row <- apply(df, 1, function(row) any(!is.na(row) & trimws(as.character(row)) != ""))
    df$id <- NA_integer_
    df$id[is_nonempty_row] <- seq_len(sum(is_nonempty_row))
    
    df
  })
  
  ##########################
  # Dynamic UI Rendering section
  ##########################
  
  ##########################
  # Sector Dropdown Update section
  # v1.6 remove filter_value_ui
  ##########################
  observe({
    df <- get_data()
    updateSelectInput(session, "filter_column", choices = names(df))
  })
  
  ##########################
  # UI for selecting ranking variables
  ##########################
  output$var_select <- renderUI({
    var_names <- names(get_data())
    selectInput("variables", "Select Variables for Ranking", var_names, multiple = TRUE)
  })
  
  # UI for selecting display variables
  output$display_var_select <- renderUI({
    var_names <- names(get_data())
    selectInput("display_vars", "Select Variables to Display", var_names, multiple = TRUE)
  })
  
  ##########################
  # UI for setting weights and order for each ranking variable
  ##########################
  output$ranking_controls <- renderUI({
    req(input$variables)
    controls <- lapply(seq_along(input$variables), function(i) {
      var <- input$variables[i]
      fluidRow(
        column(
          12,
          h4(paste("Variable:", var)),
          numericInput(paste0("weight_", i), paste("Weight for", var), 1, step = 1),
          selectInput(paste0("order_", i), paste("Order for", var),
                      choices = c("Low" = 0, "High" = 1), selected = 0)
        )
      )
    })
    do.call(tagList, controls)
  })
  
  ##########################
  # Reactive container for latest EQS results
  ##########################
  results_rv <- reactiveVal(NULL)   # holds latest ranking output
  
  ##########################
  # Ranking Engine section
  ##########################
  observeEvent(input$rank, {
    req(input$variables, input$display_vars)
    
    # v1.6 This function performs the core ranking logic on a given data frame
    perform_ranking <- function(data_subset) { # v1.6 function
      df <- data_subset
      
      # Handle cases where the subset is empty
      if (nrow(df) == 0) return(NULL)
      
      # Filter out rows with blanks or zeros based on user selection
      for (v in input$variables) {
        if ("zero"   %in% input$exclude_options) df <- df[df[[v]] != 0, ]
        if ("blanks" %in% input$exclude_options) df <- df[df[[v]] != "" & !is.na(df[[v]]), ]
      }
      
      # Return NULL if the dataframe is empty after filtering
      if (nrow(df) == 0) return(NULL)
      
      # Calculate ranks for each selected variable. lowest rank set to 1 and highest is the count of the sample.
      for (i in seq_along(input$variables)) {
        v  <- input$variables[i]
        od <- input[[paste0("order_", i)]]
        df[[paste0("Rank", v)]] <- if (od == 0) rank(-df[[v]], ties.method = "min") else rank(df[[v]], ties.method = "min")
      }
      
      # Calculate the normalized weighted rank. Normalization is based on the subset size.
      norm <- Reduce(`+`, lapply(seq_along(input$variables), function(i) {
        v   <- input$variables[i]
        wt  <- input[[paste0("weight_", i)]]
        (df[[paste0("Rank", v)]] / nrow(df)) * wt
      }))
      df$NormRank <- norm
      
      # Prepare the result data frame with selected columns
      result_df <- cbind(
        id = df$id,                          # keep identifier
        df[, input$display_vars, drop = FALSE],
        df[, input$variables,    drop = FALSE],
        df[, paste0("Rank", input$variables)],
        NormRank = df$NormRank
      )
      
      # Sort the results and select the top/bottom N
      sorted_df <- result_df[order(result_df$NormRank, decreasing = (input$show == "Top")), ]
      head(sorted_df, input$num_results)
    } # end v1.6 function
    
    df_main <- get_data()
    
    # Check if group filtering is enabled
    if (isTRUE(input$apply_filter) && !is.null(input$filter_column)) {
      # NEW LOGIC: Loop through each unique value in the filter column
      filter_col <- input$filter_column
      unique_groups <- unique(na.omit(df_main[[filter_col]]))
      
      # Apply the ranking function to each group and store results in a list
      list_of_results <- lapply(unique_groups, function(group) {
        group_subset <- df_main[df_main[[filter_col]] == group & !is.na(df_main[[filter_col]]), ]
        perform_ranking(group_subset)
      })
      
      # Combine the list of data frames into a single one for output
      final_results <- do.call(rbind, list_of_results)
      
    } else {
      # ORIGINAL LOGIC: No filter applied, run on the whole dataset
      final_results <- perform_ranking(df_main)
    }
    
    # Save results to reactive value for download/table
    results_rv(final_results)
  })
  
  ##########################
  # Results table section
  ##########################
  output$resultsTable <- renderTable({
    res <- results_rv()
    if (is.null(res) || nrow(res) == 0) {
      return(data.frame(Message = "No results to display based on current filters."))
    }
    res
  }, striped = TRUE, hover = TRUE)
  
  ##########################
  # Download handlers section
  ##########################
  ## ---- EQS-only download ----
  output$download_eqs <- downloadHandler(
    filename = function() paste0("eqs_results_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      res <- results_rv()
      if (is.null(res) || nrow(res) == 0) res <- data.frame(Message = "No EQS results.")
      openxlsx::write.xlsx(res, file, overwrite = TRUE)   # overwrite=TRUE to avoid conflicts
    }
  )
  
  ## ---- Original + EQS merged download ----
  output$download_full <- downloadHandler(
    filename = function() paste0("complete_with_eqs_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      res  <- results_rv()
      orig <- get_data()
      if (is.null(res) || nrow(res) == 0) {
        openxlsx::write.xlsx(orig, file, overwrite = TRUE)
      } else {
        merged <- merge(orig, res, by = "id", all.x = TRUE)
        openxlsx::write.xlsx(merged, file, overwrite = TRUE)  # overwrite=TRUE FIX
      }
    }
  )
}

##########################
# Shiny App Execution section
##########################
shinyApp(ui, server)

# v1.7.1
# - adding "id" column to track of all rows in the original dataframe.
# - added option to download eqs results as excel
# - also added option to download the full data frame with the eqs results merged but it is donwload as an HTML which is an issue. Got this code with gpt