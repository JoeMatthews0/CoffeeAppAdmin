# R/gs_utils.R
suppressPackageStartupMessages({
  library(googlesheets4)
  library(googledrive)
  library(dplyr)
  library(lubridate)
  library(tibble)
})

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (is.atomic(x) && all(is.na(x)))) y else x

cred_from_env <- function() {
  # 1) If GOOGLE_APPLICATION_CREDENTIALS points to a file, use it
  sa_path <- Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS", "")
  if (nzchar(sa_path) && file.exists(sa_path)) return(sa_path)
  
  # 2) Raw JSON pasted into GSA_JSON (your working user-app pattern)
  raw_json <- Sys.getenv("GSA_JSON", "")
  if (nzchar(raw_json) && startsWith(trimws(raw_json), "{")) {
    keyfile <- tempfile(fileext = ".json")
    writeChar(raw_json, keyfile, eos = NULL, useBytes = TRUE)
    Sys.setenv(GOOGLE_APPLICATION_CREDENTIALS = keyfile)
    return(keyfile)
  }
  
  # 3) Optional: base64 path (only if you decide to use it)
  b64 <- Sys.getenv("GSA_JSON_B64", "")
  if (nzchar(b64)) {
    if (!requireNamespace("base64enc", quietly = TRUE)) {
      stop("Missing `base64enc` (add to your deps).")
    }
    b64 <- gsub("\\s+", "", b64)
    b64 <- sub('^\"', "", sub('\"$', "", b64))
    raw <- try(base64enc::base64decode(b64), silent = TRUE)
    if (inherits(raw, "try-error")) stop("GSA_JSON_B64 not valid base64.")
    keyfile <- tempfile(fileext = ".json")
    writeBin(raw, keyfile)
    Sys.setenv(GOOGLE_APPLICATION_CREDENTIALS = keyfile)
    return(keyfile)
  }
  
  "" # nothing found -> will trigger fallback (bad on Connect)
}

.gs_auth <- function() {
  keyfile <- cred_from_env()
  if (nzchar(keyfile) && file.exists(keyfile)) {
    gs4_auth(path = keyfile, scopes = "https://www.googleapis.com/auth/spreadsheets")
    drive_auth(path = keyfile)  # needed if you use googledrive for modified time
    message(sprintf("[gs_auth] SA JSON loaded (%d bytes).", file.size(keyfile)))
  } else {
    # On Connect, this is wrong -> check env vars & deployment
    message("[gs_auth] FALLBACK to interactive OAuth (no service account found).")
    gs4_auth(cache = TRUE)
    drive_auth(cache = TRUE)
  }
}

.get_sheet_id <- function() {
  sid <- Sys.getenv("COFFEE_SHEET_ID", "")
  if (!nzchar(sid)) stop("COFFEE_SHEET_ID env var is not set.")
  sid
}

.sheet <- function() {
  .gs_auth()
  as_sheets_id(.get_sheet_id())
}

# --- the rest of your helpers (unchanged) ---
coerce_numeric_safe  <- function(x) suppressWarnings(as.numeric(x))
coerce_integer_safe  <- function(x) suppressWarnings(as.integer(x))

read_transactions <- function() {
  ss <- .sheet()
  read_sheet(ss, sheet = "transactions", col_types = "Tcccidcc") |>
    mutate(
      timestamp = with_tz(timestamp, tzone = Sys.timezone()),
      coffees   = coerce_integer_safe(coffees),
      amount    = coerce_numeric_safe(amount),
      across(c(staff_id, name, type, note, submitted_by), as.character)
    )
}

append_transaction <- function(staff_id, name, type = c("coffee","topup"),
                               coffees = 0L, amount = 0, note = "", submitted_by = "app") {
  type <- match.arg(type)
  ss <- .sheet()
  df <- tibble(
    timestamp   = lubridate::now(tzone = "UTC"),
    staff_id    = as.character(staff_id),
    name        = as.character(name),
    type        = type,
    coffees     = as.integer(coffees),
    amount      = as.numeric(amount),
    note        = as.character(note),
    submitted_by= as.character(submitted_by)
  )
  sheet_append(df, ss = ss, sheet = "transactions")
  invisible(df)
}

balances <- function() {
  read_transactions() |>
    group_by(staff_id, name) |>
    summarise(
      balance = sum(amount, na.rm = TRUE),
      coffees = sum(coffees, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(desc(balance))
}

balance_of <- function(staff_id) {
  b <- balances() |> filter(staff_id == !!staff_id)
  if (nrow(b) == 0) tibble(staff_id = staff_id, name = NA_character_, balance = 0, coffees = 0) else b
}

sheet_modified_time <- function() {
  ss <- .sheet()
  mt <- googledrive::drive_get(ss)$drive_resource[[1]]$modifiedTime
  as.numeric(lubridate::as_datetime(mt, tz = "UTC"))
}
