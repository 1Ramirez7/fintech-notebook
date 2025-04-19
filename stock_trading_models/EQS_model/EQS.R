# ---- EQS Shiny App  | Version 1.4 + default GitHub workbook ----
library(shiny)
library(readxl)
library(curl)

# raw GitHub URL for the default/sample workbook (.xlsx)
fallback_excel_url <- "https://raw.githubusercontent.com/1Ramirez7/fintech-notebook/main/stock_trading_models/EQS_model/data/papersample.xlsx"

ui <- fluidPage(
  titlePanel("EQS"),
  fluidRow(
    column(
      4,
      fileInput("file", "Choose Excel File", accept = ".xlsx"),      # optional upload
      uiOutput("var_select"),
      uiOutput("display_var_select"),
      selectInput("sector_filter", "Filter by Sector", choices = NULL),
      checkboxGroupInput(
        "exclude_options", "Exclude obs. w/",
        choices = list("Blanks" = "blanks", "0" = "zero")
      ),
      numericInput("num_results", "Number of Results", 10, min = 1),
      selectInput("show", "Show", choices = c("Top" = "Top", "Bottom" = "Bottom")),
      actionButton("rank", "Calculate Rankings")
    ),
    column(8, uiOutput("ranking_controls"))
  ),
  tableOutput("resultsTable")
)

server <- function(input, output, session) {
  
  ## -------- data source (upload OR default) ---------------------
  get_data <- reactive({
    if (!is.null(input$file)) {
      read_excel(input$file$datapath)
    } else {
      tmp <- tempfile(fileext = ".xlsx")
      curl_download(fallback_excel_url, tmp, mode = "wb")
      read_excel(tmp)
    }
  })
  
  ## -------- update sector dropdown ------------------------------
  observe({
    df <- get_data()
    updateSelectInput(session, "sector_filter", choices = unique(df$sector))
  })
  
  ## -------- variable pickers ------------------------------------
  output$var_select <- renderUI({
    var_names <- names(get_data())
    selectInput("variables", "Select Variables for Ranking", var_names, multiple = TRUE)
  })
  
  output$display_var_select <- renderUI({
    var_names <- names(get_data())
    selectInput("display_vars", "Select Variables to Display", var_names, multiple = TRUE)
  })
  
  ## -------- per‑variable weight + order controls ----------------
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
  
  ## -------- ranking engine --------------------------------------
  observeEvent(input$rank, {
    req(input$variables, input$display_vars)
    df <- get_data()
    
    ## filters
    if (!is.null(input$sector_filter)) df <- df[df$sector == input$sector_filter, ]
    for (v in input$variables) {
      if ("zero"   %in% input$exclude_options) df <- df[df[[v]] != 0, ]
      if ("blanks" %in% input$exclude_options) df <- df[df[[v]] != "" & !is.na(df[[v]]), ]
    }
    
    ## ranks
    for (i in seq_along(input$variables)) {
      v  <- input$variables[i]
      od <- input[[paste0("order_", i)]]
      df[[paste0("Rank", v)]] <- if (od == 0) rank(-df[[v]], ties.method = "min")
      else        rank( df[[v]], ties.method = "min")
    }
    
    ## normalized weighted rank
    norm <- Reduce(`+`, lapply(seq_along(input$variables), function(i) {
      v   <- input$variables[i]
      wt  <- input[[paste0("weight_", i)]]
      (df[[paste0("Rank", v)]] / nrow(df)) * wt
    }))
    df$NormRank <- norm
    
    ## output table
    result_df <- cbind(
      df[, input$display_vars, drop = FALSE],
      df[, input$variables,    drop = FALSE],
      df[, paste0("Rank", input$variables)],
      NormRank = df$NormRank
    )
    
    sorted_df <- result_df[order(result_df$NormRank,
                                 decreasing = (input$show == "Top")), ]
    
    output$resultsTable <- renderTable(head(sorted_df, input$num_results))
  })
}

shinyApp(ui, server)


# ---- EQS Shiny App  | Version 1.4 + default GitHub workbook ----