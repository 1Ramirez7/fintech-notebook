library(shiny)
library(readxl)
library(curl)

# v1.5

fallback_excel_url <- "https://raw.githubusercontent.com/1Ramirez7/fintech-notebook/main/stock_trading_models/EQS_model/data/papersample.xlsx"

##########################
# UI Panel section
#
##########################

ui <- fluidPage(
  titlePanel("EQS"),
  fluidRow(
    column(
      4,
      fileInput("file", "Choose Excel File", accept = ".xlsx"),
      uiOutput("var_select"),
      uiOutput("display_var_select"),
      checkboxInput("apply_filter", "Apply column filter", value = FALSE), # v1.5
      conditionalPanel( # v1.5
        condition = "input.apply_filter == true", # v1.5
        selectInput("filter_column", "Select Column to Filter", choices = NULL),
        uiOutput("filter_value_ui")), # v1.5
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


##########################
# Server Logic section
#
##########################

server <- function(input, output, session) {
  
  ##########################
  # Data Loading section
  #
  ##########################
  get_data <- reactive({
    if (!is.null(input$file)) {
      read_excel(input$file$datapath)
    } else {
      tmp <- tempfile(fileext = ".xlsx")
      curl_download(fallback_excel_url, tmp, mode = "wb")
      read_excel(tmp)
    }
  })
  
  ##########################
  # Sector Dropdown Update section
  # v1.5 change from filter by sector to user pick.
  ##########################
  observe({ 
    df <- get_data()
    updateSelectInput(session, "filter_column", choices = names(df))
  })
  
  output$filter_value_ui <- renderUI({
    req(input$filter_column)
    df   <- get_data()
    selectInput("filter_value", "Select Value to Filter",
                choices = unique(df[[input$filter_column]]))
  })
  
  ##########################
  # Variable Selector UI section
  #
  ##########################
  output$var_select <- renderUI({
    var_names <- names(get_data())
    selectInput("variables", "Select Variables for Ranking", var_names, multiple = TRUE)
  })
  
  output$display_var_select <- renderUI({
    var_names <- names(get_data())
    selectInput("display_vars", "Select Variables to Display", var_names, multiple = TRUE)
  })
  
  ##########################
  # Ranking Control UI section
  #
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
  # Ranking Engine section
  #
  ##########################
  observeEvent(input$rank, {
    req(input$variables, input$display_vars)
    df <- get_data()
    
    ## filters
    if (isTRUE(input$apply_filter) && !is.null(input$filter_column) && !is.null(input$filter_value) && input$filter_value != "") { # v1.5
      df <- df[df[[input$filter_column]] == input$filter_value, ]} # v1.5 filter by user pick
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

##########################
# Shiny App Execution section
#
##########################

shinyApp(ui, server)


# v1.5
# organize the code by section
# added option to filter by any variable and filter can also be null.

# this is where I left off. 
# I went looking for my older version of EQS. I want the model that filter by sector, but also made edits only to the rows that applied for that filter. This allow to have filters by sectors
# I remember I was having some sort of trouble with the excel file modifying but I cant remember exactly. 

