library(shiny)
library(readxl)
library(openxlsx)  # Needed for writing back to Excel
library(dplyr)  # For easier data manipulation, especially for merging

# this is is working. it does not delete all data in the NormRank column but only updates
# those cells for which the observations where made. This is not good because I can have results
# for the same variables and sector but doing one value, and one high risk equities which will result
# in different and similar equities but this code will only edit new values and leave old values which
# is no bueno but I can temporarily use this for tomorrow though.
# I was pondering that I may need to revert back to doing the editing of an excel file with a smaller r and shiny app code and sample data.
# take this step by first doing baby steps to make sure it is done right because AI is now going hay wire with the responses.
# I also need to add the NormRank column if it doesn't exist or an error will happen.

ui <- fluidPage(
  titlePanel("EQS"),
  fluidRow(
    column(4,
           fileInput("file", "Choose Excel File", accept = ".xlsx"),
           uiOutput("var_select"),
           uiOutput("display_var_select"),
           selectInput("sector_filter", "Filter by Sector", choices = NULL),
           checkboxGroupInput("exclude_options", "Exclude obs. w/", choices = list("Blanks" = "blanks", "0" = "zero")),
           numericInput("num_results", "Number of Results", value = 10, min = 1),
           selectInput("show", "Show", choices = c("Top" = "Top", "Bottom" = "Bottom")),
           actionButton("rank", "Calculate Rankings"),
           downloadButton("download", "Download Updated Excel")  # Allows user to download the updated file
    ),
    column(8,
           uiOutput("ranking_controls")
    )
  ),
  tableOutput("resultsTable")
)

server <- function(input, output, session) {
  observe({
    req(input$file)
    df <- read_excel(input$file$datapath)
    updateSelectInput(session, "sector_filter", choices = unique(df$sector))
  })

  output$var_select <- renderUI({
    req(input$file)
    df <- read_excel(input$file$datapath)
    var_names <- names(df)
    selectInput("variables", "Select Variables for Ranking", choices = var_names, selected = NULL, multiple = TRUE)
  })

  output$display_var_select <- renderUI({
    req(input$file)
    df <- read_excel(input$file$datapath)
    var_names <- names(df)
    selectInput("display_vars", "Select Variables to Display", choices = var_names, selected = NULL, multiple = TRUE)
  })

  output$ranking_controls <- renderUI({
    req(input$variables)
    controls <- lapply(seq_along(input$variables), function(i) {
      var_name <- input$variables[i]
      fluidRow(
        column(12,
               h4(paste("Variable:", var_name)),
               numericInput(paste0("weight_", i), "Weight", value = .7, step = 0.01),
               selectInput(paste0("order_", i), "Order", choices = c("Low" = 0, "High" = 1), selected = 0)
        )
      )
    })
    do.call(tagList, controls)
  })

  observeEvent(input$rank, {
    req(input$file, input$variables, input$display_vars)
    df <- read_excel(input$file$datapath)  # Original DataFrame

    # Filtered DataFrame
    df_f <- df
    if (!is.null(input$sector_filter)) {
      df_f <- df_f[df_f$sector == input$sector_filter, ]
    }

    # Apply exclusions
    exclude_conditions <- input$exclude_options
    for (var in input$variables) {
      if ("zero" %in% exclude_conditions) {
        df_f <- df_f[df_f[[var]] != 0, ]
      }
      if ("blanks" %in% exclude_conditions) {
        df_f <- df_f[df_f[[var]] != "", ]
        df_f <- df_f[!is.na(df_f[[var]]), ]
      }
    }

    # Calculate ranks for each variable
    for (i in seq_along(input$variables)) {
      variable <- input$variables[i]
      order <- input[[paste0("order_", i)]]
      df_f[[paste0("Rank", variable)]] <- if(order == 0) rank(-df_f[[variable]], ties.method = "min") else rank(df_f[[variable]], ties.method = "min")
    }

    # Compute NormRank
    norm_rank_expression <- NULL
    for (i in seq_along(input$variables)) {
      variable <- input$variables[i]
      weight <- input[[paste0("weight_", i)]]
      rank_name <- paste0("Rank", variable)
      norm_rank_expression <- if(is.null(norm_rank_expression)) {
        (df_f[[rank_name]] / length(df_f[[variable]])) * weight
      } else {
        norm_rank_expression + (df_f[[rank_name]] / length(df_f[[variable]])) * weight
      }
    }
    df_f$NormRank <- norm_rank_expression

    # Merge NormRank back to original df
    df <- left_join(df, select(df_f, Ticker, NormRank), by = "Ticker", suffix = c("", ".f"))
    df$NormRank <- coalesce(df$NormRank.f, df$NormRank)
    df$NormRank.f <- NULL

    # Sort and display
    sorted_df <- df_f %>% arrange(if(input$show == "Top") desc(NormRank) else NormRank)
    output$resultsTable <- renderTable(head(sorted_df, input$num_results))

    # Allow download
    output$download <- downloadHandler(
      filename = function() { paste("Updated_Data-", Sys.Date(), ".xlsx", sep = "") },
      content = function(file) {
        write.xlsx(df, file)
      }
    )
  })
}

shinyApp(ui = ui, server = server)
