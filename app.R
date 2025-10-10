# ---- app.R (ADMIN) – robust helper sourcing + fallback ---------------------

# Try to source the helpers
helpers_path <- file.path("R", "gs_utils.R")
if (!file.exists(helpers_path)) {
  # extra diagnostics so the Connect log shows what's in your bundle
  message("ADMIN: R/gs_utils.R NOT FOUND. Bundle contents:\n",
          paste(capture.output(print(list.files(".", recursive = TRUE))), collapse = "\n"))
} else {
  source(helpers_path, local = TRUE, chdir = TRUE)
}

message("ADMIN env lengths: sheet=", nchar(Sys.getenv("COFFEE_SHEET_ID","")),
        " GSA_JSON=", nchar(Sys.getenv("GSA_JSON","")),
        " GSA_JSON_B64=", nchar(Sys.getenv("GSA_JSON_B64","")))
msg_first <- substr(trimws(Sys.getenv("GSA_JSON","")), 1, 3)
message("ADMIN GSA_JSON starts with: '", msg_first, "' (expect '{' for raw JSON, or 'eyJ' for base64)")


# Minimal deps used below
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(dplyr); library(tibble)
})

# If the helper didn’t define these yet, stub the essentials so app can run
if (!exists(".sheet", mode = "function")) {
  stop("ADMIN: .sheet() not found. This means R/gs_utils.R was not loaded. Fix the bundle/path.")
}

if (!exists("ensure_sheets_exist", mode = "function")) {
  message("ADMIN: ensure_sheets_exist() missing after sourcing; defining a fallback now.")
  ensure_sheets_exist <- function() {
    # Create tabs if missing; no-op if present
    ss <- .sheet()
    existing <- tryCatch(googlesheets4::sheet_names(ss), error = function(e) character())
    if (!"transactions" %in% existing) {
      googlesheets4::sheet_write(
        tibble(
          timestamp    = lubridate::as_datetime(character()),
          staff_id     = character(),
          name         = character(),
          type         = character(),  # "coffee" | "topup"
          coffees      = integer(),
          amount       = double(),
          note         = character(),
          submitted_by = character()
        ),
        ss = ss, sheet = "transactions"
      )
    }
    if (!"config" %in% existing) {
      googlesheets4::sheet_write(
        tibble(key = c("coffee_price","topup_threshold"),
               value = c("0.5","2.0")),
        ss = ss, sheet = "config"
      )
    }
    invisible(TRUE)
  }
}

# ---- continue with your existing admin app code ----

# (recommended) call ensure_sheets_exist() once at startup
ensure_sheets_exist()

ui <- page_fluid(
  theme = bs_theme(version = 5),
  titlePanel("☕ Coffee Club – Admin Top-ups"),
  layout_columns(
    column(6,
      card(
        card_header("Record top-up"),
        textInput("staff_id", "Staff ID"),
        textInput("name", "Name"),
        numericInput("amount", "Amount received (£)", value = 5, min = 0.5, step = 0.5),
        textInput("note", "Note (optional)"),
        actionButton("submit", "Add Top-up", class = "btn-success")
      )
    ),
    column(6,
      card(card_header("Current balance"), uiOutput("balance_ui")),
      card(card_header("All balances (top 20)"), tableOutput("leaderboard"))
    )
  )
)

server <- function(input, output, session) {
  balance_tbl     <- reactiveVal(tibble())
  leaderboard_tbl <- reactiveVal(tibble())
  
  refresh_balance <- function() {
    if (!nzchar(input$staff_id)) { balance_tbl(tibble()); return() }
    balance_tbl(balance_of(trimws(input$staff_id)))
  }
  refresh_leaderboard <- function() {
    leaderboard_tbl(balances() |> arrange(desc(balance)) |> mutate(balance = sprintf("£%.2f", balance)))
  }
  
  observeEvent(input$staff_id, { refresh_balance() }, ignoreInit = FALSE)
  
  output$balance_ui <- renderUI({
    req(nzchar(input$staff_id))
    b <- balance_tbl()
    name <- if (nrow(b) == 0 || is.na(b$name[1])) input$name else b$name[1]
    bal  <- if (nrow(b) == 0) 0 else (b$balance[1] %||% 0)
    div(class = "alert alert-info",
        glue::glue("Balance for {name} ({input$staff_id}): £{sprintf('%.2f', bal)}"))
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
    showNotification(glue::glue("Added top-up £{sprintf('%.2f', input$amount)} for {input$name}"),
                     type = "message")
  })
  
  observeEvent(TRUE, { refresh_leaderboard() }, once = TRUE)
  
  output$leaderboard <- renderTable(head(leaderboard_tbl(), 20))
}

shinyApp(ui, server)
