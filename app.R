# app.R  (for the ADMIN app project)

# 1) Source helpers from R/gs_utils.R (fail fast with a clear message)
helpers_path <- file.path(getwd(), "R", "gs_utils.R")
if (!file.exists(helpers_path)) {
  stop("Cannot find R/gs_utils.R in this admin project. Ensure the R/ folder is deployed.")
}
source(helpers_path, local = TRUE)

# 2) Tiny diagnostics (safe):
message("ADMIN: COFFEE_SHEET_ID nchar = ", nchar(Sys.getenv("COFFEE_SHEET_ID","")))
message("ADMIN: GSA_JSON nchar = ", nchar(Sys.getenv("GSA_JSON","")))
message("ADMIN: GSA_JSON_B64 nchar = ", nchar(Sys.getenv("GSA_JSON_B64","")))
message("ADMIN: ensure_sheets_exist exists? ", exists("ensure_sheets_exist"))

# 3) Fail clearly if the function is still missing
if (!exists("ensure_sheets_exist")) {
  stop("ensure_sheets_exist() not found after sourcing R/gs_utils.R. 
Check the file contents and function name. Did you deploy the right R/gs_utils.R?")
}

# 4) Run the admin app code (your working admin app from before)
#    If you keep your UI/server in a separate file (app_admin.R), source it now:
# source("app_admin.R", local = TRUE)

# Or paste your final admin UI/server here (short version shown):
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(dplyr); library(glue); library(tibble)
})

ensure_sheets_exist()  # <-- now safe to call

ui <- page_fluid(
  theme = bs_theme(version = 5),
  titlePanel("☕ Coffee Club – Admin Top-ups"),
  layout_columns(
    col_6(
      card(
        card_header("Record top-up"),
        textInput("staff_id", "Staff ID"),
        textInput("name", "Name"),
        numericInput("amount", "Amount received (£)", value = 5, min = 0.5, step = 0.5),
        textInput("note", "Note (optional)"),
        actionButton("submit", "Add Top-up", class = "btn-success")
      )
    ),
    col_6(
      card(card_header("Current balance"), uiOutput("balance_ui")),
      card(card_header("All balances (top 20)"), tableOutput("leaderboard"))
    )
  )
)

server <- function(input, output, session) {
  balance_tbl     <- reactiveVal(tibble())
  leaderboard_tbl <- reactiveVal(tibble())
  
  refresh_balance <- function() {
    if (!nzchar(input$staff_id)) { balance_tbl(tibble()); return(invisible(NULL)) }
    balance_tbl(balance_of(trimws(input$staff_id)))
  }
  refresh_leaderboard <- function() {
    lb <- balances() |> arrange(desc(balance)) |> mutate(balance = sprintf("£%.2f", balance))
    leaderboard_tbl(lb)
  }
  
  observeEvent(input$staff_id, { refresh_balance() }, ignoreInit = FALSE)
  
  output$balance_ui <- renderUI({
    req(nzchar(input$staff_id))
    b <- balance_tbl()
    name <- if (nrow(b) == 0 || is.na(b$name[1])) input$name else b$name[1]
    bal  <- if (nrow(b) == 0) 0 else (b$balance[1] %||% 0)
    div(class = "alert alert-info", glue("Balance for {name} ({input$staff_id}): £{sprintf('%.2f', bal)}"))
  })
  
  observeEvent(input$submit, {
    validate(
      need(nzchar(input$staff_id), "Enter a Staff ID."),
      need(nzchar(input$name), "Enter a Name."),
      need(is.numeric(input$amount) && input$amount > 0, "Amount must be > 0")
    )
    append_transaction(
      staff_id = trimws(input$staff_id),
      name     = trimws(input$name),
      type     = "topup",
      coffees  = 0L,
      amount   = as.numeric(input$amount),
      note     = input$note %||% "",
      submitted_by = paste0("admin-", session$request$REMOTE_ADDR %||% "app")
    )
    refresh_balance(); refresh_leaderboard()
    showNotification(glue("Added top-up £{sprintf('%.2f', input$amount)} for {input$name}"), type = "message")
  })
  
  observeEvent(TRUE, { refresh_leaderboard() }, once = TRUE)
  
  output$leaderboard <- renderTable(head(leaderboard_tbl(), 20))
}

shinyApp(ui, server)
