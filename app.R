# app_admin.R — admin name memory + dropdown for users + low-balance table

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(tibble)
  library(glue)
  library(shinythemes)
  library(shinyjs)    # for runjs / useShinyjs
  source("R/gs_utils.R")
})

ensure_sheets_exist()

ui <- fluidPage(
  theme = shinytheme("flatly"),
  useShinyjs(),
  
  # Add a container around the entire body content for padding
  div(class = "container",
      tags$head(tags$script(HTML("
    (function () {
      function trim(s){ return (s||'').replace(/^\\s+|\\s+$/g,''); }
      function setLS(val){
        try{
          val = trim(val||'');
          if (val) localStorage.setItem('coffee_admin_name', val);
          else     localStorage.removeItem('coffee_admin_name');
        }catch(e){}
      }
      function getLS(){
        try{ return trim(localStorage.getItem('coffee_admin_name') || ''); }
        catch(e){ return ''; }
      }
      function fillInput(id, val){
        var el = document.getElementById(id);
        if (!el) return;
        el.value = val || '';
        el.dispatchEvent(new Event('input', {bubbles:true}));
        el.dispatchEvent(new Event('change', {bubbles:true}));
      }

      // Server asks -> send back stored admin name and prefill DOM
      Shiny.addCustomMessageHandler('requestAdminName', function(){
        var nm = getLS();
        if (nm) fillInput('admin_name', nm);
        Shiny.setInputValue('admin_restore', { name: nm }, {priority:'event'});
      });

      // Server can persist explicitly (e.g., after submit)
      Shiny.addCustomMessageHandler('persistAdminName', function(x){
        if (x && x.name !== undefined) setLS(x.name);
      });

      // Save on any manual edits
      document.addEventListener('input', function (ev) {
        if (ev && ev.target && ev.target.id === 'admin_name') {
          setLS(ev.target.value);
        }
      }, true);
    })();
  "))),
      titlePanel("MSP Coffee Tracker -- Admin Page (BETA Version)"),
      
      fluidRow(
        h2("Top-Up Accounts"),
        column(4,
               h3("1. Select Existing Coffee Drinker"),
               selectizeInput("name_pick", "Select existing user:",
                              choices = NULL, multiple = FALSE,
                              options = list(placeholder = "Start typing staff name or ID")),
               h3("OR 1. Add New Coffee Drinker"),
                 textInput("staff_id", "Staff ID", placeholder = "nXX123"),
                 textInput("name", "Name", placeholder = "First Last")
        ),
        column(4,
               h3("2. Enter Top Up Amount"),
               fluidRow(
                 numericInput("amount", "Amount received (£)", value = 5, min = 0.5, step = 0.5),
                 textInput("admin_name", "Recorded by", placeholder = "Name")
               )
        ),
        column(4,
               h3("3. Record Top-Up"),
               actionButton("submit", "Add Top-up", class = "btn-success")
        )
      ),
      
      fluidRow(
        h2("Current Balances"),
        conditionalPanel('input.submit',
                         card(card_header("Current balance"), uiOutput("balance_ui"))
        ),
        fluidRow(
          column(6,
                 h3("All Balances"),
                 tableOutput("leaderboard")
          ),
          column(6,
                 h3("Accounts in Arrears (Balance < £0)"),
                 tableOutput("low_bal_tbl")
          )
        )
      )
  )
)

server <- function(input, output, session) {
  # Local stores
  balance_tbl     <- reactiveVal(tibble())
  leaderboard_tbl <- reactiveVal(tibble())
  
  # Load roster choices and leaderboard at startup
  refresh_roster <- function() {
    b <- balances()
    if (nrow(b) == 0) {
      updateSelectizeInput(session, "name_pick", choices = NULL, server = TRUE)
      return(invisible(NULL))
    }
    b <- b |> mutate(label = paste0(name, " (", staff_id, ")"))
    updateSelectizeInput(session, "name_pick", choices = b$label, selected = character(0), server = TRUE)
  }
  
  refresh_balance <- function() {
    if (!nzchar(input$staff_id)) { balance_tbl(tibble()); return(invisible(NULL)) }
    balance_tbl(balance_of(trimws(input$staff_id)))
  }
  
  refresh_leaderboard <- function() {
    lb <- balances() |>
      arrange(desc(balance)) |>
      mutate(balance = sprintf("£%.2f", balance))
    leaderboard_tbl(lb)
  }
  
  # Ask browser for stored admin name once UI is bound
  session$onFlushed(function() {
    session$sendCustomMessage("requestAdminName", list())
  }, once = TRUE)
  
  # When client returns stored admin name, set it authoritatively
  observeEvent(input$admin_restore, {
    nm <- input$admin_restore$name %||% ""
    if (nzchar(nm)) updateTextInput(session, "admin_name", value = nm)
  }, ignoreInit = TRUE)
  
  # Initial loads
  observeEvent(TRUE, { refresh_roster(); refresh_leaderboard() }, once = TRUE)
  
  # Dropdown selection -> prefill ID + name
  observeEvent(input$name_pick, {
    sel <- input$name_pick
    if (!nzchar(sel)) return()
    id   <- sub("^.*\\(([^()]*)\\)\\s*$", "\\1", sel)
    name <- sub("\\s*\\([^()]*\\)\\s*$", "", sel)
    if (nzchar(id))   updateTextInput(session, "staff_id", value = id)
    if (nzchar(name)) updateTextInput(session, "name",     value = name)
    refresh_balance()
  }, ignoreInit = FALSE)
  
  # Manual changes to staff_id should refresh balance
  observeEvent(input$staff_id, { if (nzchar(input$staff_id)) refresh_balance() }, ignoreInit = FALSE)
  
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
      need(nzchar(input$staff_id),  "Enter a Staff ID."),
      need(nzchar(input$name),      "Enter a Name."),
      need(is.numeric(input$amount) && input$amount > 0, "Amount must be > 0"),
      need(nzchar(input$admin_name),"Enter the admin name (Recorded by).")
    )
    
    admin_nm <- trimws(input$admin_name)
    
    append_transaction(
      staff_id     = trimws(input$staff_id),
      name         = trimws(input$name),
      type         = "topup",
      coffees      = 0L,
      amount       = as.numeric(input$amount),
      note         = "",  # note field no longer used; keep empty
      submitted_by = paste0("admin:", admin_nm)
    )
    
    # Persist admin name to localStorage for next time
    session$sendCustomMessage("persistAdminName", list(name = admin_nm))
    
    # Refresh live views and choices immediately
    refresh_balance()
    refresh_leaderboard()
    refresh_roster()
    
    showNotification(glue("Added top-up £{sprintf('%.2f', input$amount)} (recorded by {admin_nm})"),
                     type = "message")
  })
  
  # Tables
  output$leaderboard <- renderTable({leaderboard_tbl()})
  
  output$low_bal_tbl <- renderTable({
    balances() |>
      filter(balance < 0) |>
      arrange(balance) |>
      mutate(balance = sprintf("£%.2f", balance)) |>
      select(staff_id, name, balance, coffees)
  })
}

shinyApp(ui, server)
