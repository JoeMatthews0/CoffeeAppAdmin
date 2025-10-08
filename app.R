# app_admin.R

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(tibble)
  library(glue)
  source("R/gs_utils.R")
})

ensure_sheets_exist()

ui <- page_fluid(
  theme = bs_theme(version = 5),
  titlePanel("☕ Coffee Club – Admin Top-ups"),
  layout_columns(
    column(6,
      card(
        card_header("Record top-up"),
        textInput("staff_id", "Staff ID", placeholder = "e.g. A1234"),
        textInput("name", "Name", placeholder = "First Last"),
        numericInput("amount", "Amount received (£)", value = 5, min = 0.5, step = 0.5),
        textInput("note", "Note (optional)", placeholder = "e.g. cash at desk"),
        actionButton("submit", "Add Top-up", class = "btn-success")
      )
    ),
    column(6,
      card(
        card_header("Current balance"),
        uiOutput("balance_ui")
      ),
      card(
        card_header("All balances (top 20)"),
        tableOutput("leaderboard")
      )
    )
  )
)

server <- function(input, output, session) {
  # Local reactive stores (no polling)
  balance_tbl     <- reactiveVal(tibble())
  leaderboard_tbl <- reactiveVal(tibble())
  
  refresh_balance <- function() {
    if (!nzchar(input$staff_id)) {
      balance_tbl(tibble())
      return(invisible(NULL))
    }
    balance_tbl(balance_of(trimws(input$staff_id)))
  }
  
  refresh_leaderboard <- function() {
    lb <- balances() |>
      arrange(desc(balance)) |>
      mutate(balance = sprintf("£%.2f", balance))
    leaderboard_tbl(lb)
  }
  
  # Refresh when Staff ID changes (and on first load of the field)
  observeEvent(input$staff_id, {
    refresh_balance()
  }, ignoreInit = FALSE)
  
  output$balance_ui <- renderUI({
    req(nzchar(input$staff_id))
    b <- balance_tbl()
    name <- if (nrow(b) == 0 || is.na(b$name[1])) input$name else b$name[1]
    bal  <- if (nrow(b) == 0) 0 else (b$balance[1] %||% 0)
    div(class = "alert alert-info",
        glue("Balance for {name} ({input$staff_id}): £{sprintf('%.2f', bal)}"))
  })
  
  observeEvent(input$submit, {
    validate(
      need(nzchar(input$staff_id), "Enter a Staff ID."),
      need(nzchar(input$name), "Enter a Name."),
      need(is.numeric(input$amount) && input$amount > 0, "Amount must be > 0")
    )
    
    append_transaction(
      staff_id     = trimws(input$staff_id),
      name         = trimws(input$name),
      type         = "topup",
      coffees      = 0L,
      amount       = as.numeric(input$amount),
      note         = input$note %||% "",
      submitted_by = paste0("admin-", session$request$REMOTE_ADDR %||% "app")
    )
    
    # Instant UI refresh after successful write
    refresh_balance()
    refresh_leaderboard()
    
    showNotification(
      glue("Added top-up £{sprintf('%.2f', input$amount)} for {input$name}"),
      type = "message"
    )
  })
  
  # First load leaderboard (run once, no dots warning)
  observeEvent(TRUE, {
    refresh_leaderboard()
  }, once = TRUE)
  
  output$leaderboard <- renderTable({
    head(leaderboard_tbl(), 20)
  })
}

shinyApp(ui, server)
