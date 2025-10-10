suppressPackageStartupMessages({
  library(googlesheets4)
  library(googledrive)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(lubridate)
  library(glue)
  library(base64enc)
})

suppressPackageStartupMessages({
  library(googlesheets4)
  library(googledrive)
  library(lubridate)
})

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (is.atomic(x) && all(is.na(x)))) y else x

cred_from_env <- function() {
  # 1) If GOOGLE_APPLICATION_CREDENTIALS already points to a file, use it.
  sa_path <- Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS", "")
  if (nzchar(sa_path) && file.exists(sa_path)) return(sa_path)
  
  # 2) Raw JSON in env var GSA_JSON (accepts your current value)
  raw_json <- Sys.getenv("GSA_JSON", "")
  if (nzchar(raw_json) && startsWith(trimws(raw_json), "{")) {
    keyfile <- tempfile(fileext = ".json")
    writeChar(raw_json, keyfile, eos = NULL, useBytes = TRUE)
    Sys.setenv(GOOGLE_APPLICATION_CREDENTIALS = keyfile)
    return(keyfile)
  }
  
  # 3) Base64 JSON in env var GSA_JSON_B64
  b64 <- Sys.getenv("GSA_JSON_B64", "")
  if (nzchar(b64)) {
    if (!requireNamespace("base64enc", quietly = TRUE)) {
      stop("Please add `base64enc` to your dependencies (renv::install('base64enc')).")
    }
    # tolerate accidental quotes/newlines
    b64 <- gsub("\\s+", "", b64)
    b64 <- sub('^\"', "", sub('\"$', "", b64))
    raw <- try(base64enc::base64decode(b64), silent = TRUE)
    if (inherits(raw, "try-error")) stop("GSA_JSON_B64 is not valid base64.")
    keyfile <- tempfile(fileext = ".json")
    writeBin(raw, keyfile)
    Sys.setenv(GOOGLE_APPLICATION_CREDENTIALS = keyfile)
    return(keyfile)
  }
  
  ""  # nothing found
}

.gs_auth <- function() {
  keyfile <- cred_from_env()
  if (nzchar(keyfile) && file.exists(keyfile)) {
    googlesheets4::gs4_auth(path = keyfile,
                            scopes = "https://www.googleapis.com/auth/spreadsheets")
    googledrive::drive_auth(path = keyfile)
    message(sprintf("[gs_auth] Service account loaded (size %d bytes).", file.size(keyfile)))
  } else {
    # Interactive fallback for local dev only
    googlesheets4::gs4_auth(cache = TRUE)
    googledrive::drive_auth(cache = TRUE)
    message("[gs_auth] WARNING: using interactive OAuth (likely wrong on Connect).")
  }
}

.get_sheet_id <- function() {
  sid <- Sys.getenv("COFFEE_SHEET_ID", unset = NA)
  if (is.na(sid) || sid == "") stop("COFFEE_SHEET_ID env var is not set.")
  sid
}


.sheet <- function() {
  .gs_auth()
  as_sheets_id(.get_sheet_id())
}

ensure_sheets_exist <- function() {
  ss <- .sheet()
  existing <- sheet_names(ss)
  if (!"transactions" %in% existing) {
    # Write with correct column classes hinted as text, then we coerce on read
    sheet_write(
      tibble(
        timestamp = as_datetime(character()),
        staff_id = character(),
        name = character(),
        type = character(), # "coffee" or "topup"
        coffees = integer(), # count for coffee rows
        amount = double(), # negative for coffees, positive for topups
        note = character(),
        submitted_by = character()
      ),
      ss = ss,
      sheet = "transactions"
    )
  }
  if (!"config" %in% existing) {
    sheet_write(
      tibble(key = c("coffee_price", "topup_threshold"),
             value = c("0.5", "2.0")),
      ss = ss,
      sheet = "config"
    )
  }
}

read_config <- function() {
  ss <- .sheet()
  cfg <- read_sheet(ss, sheet = "config", col_types = "cc") |>
    mutate(value = suppressWarnings(as.numeric(value)))
  coffee_price <- dplyr::coalesce(cfg$value[cfg$key == "coffee_price"], as.numeric(Sys.getenv("COFFEE_PRICE", 0.5)))
  topup_threshold <- dplyr::coalesce(cfg$value[cfg$key == "topup_threshold"], as.numeric(Sys.getenv("TOPUP_THRESHOLD", 2)))
  list(coffee_price = coffee_price, topup_threshold = topup_threshold)
}


set_config <- function(coffee_price = NULL, topup_threshold = NULL) {
  ss <- .sheet()
  cfg <- tibble(
    key = c("coffee_price", "topup_threshold"),
    value = c(
      if (!is.null(coffee_price)) as.character(coffee_price) else NA,
      if (!is.null(topup_threshold)) as.character(topup_threshold) else NA
    )
  ) |>
    filter(!is.na(value))
  if (nrow(cfg)) {
    existing <- tryCatch(read_sheet(ss, sheet = "config", col_types = "cc"), error = function(e) tibble())
    out <- existing |>
      rows_upsert(cfg, by = "key")
    sheet_write(out, ss = ss, sheet = "config")
  }
}

# ---- Robust read & type repair -------------------------------------------
coerce_numeric_safe <- function(x) suppressWarnings(as.numeric(x))
coerce_integer_safe <- function(x) suppressWarnings(as.integer(x))


read_transactions <- function() {
  ss <- .sheet()
  df <- read_sheet(ss, sheet = "transactions", col_types = "Tcccidcc") |>
    mutate(
      timestamp = with_tz(timestamp, tzone = Sys.timezone()),
      coffees = coerce_integer_safe(coffees),
      amount = coerce_numeric_safe(amount),
      staff_id = as.character(staff_id),
      name = as.character(name),
      type = as.character(type),
      note = as.character(note),
      submitted_by= as.character(submitted_by)
    )
  df
}

# One-off helper to fix any historical rows typed as text in Google Sheets
repair_transactions_types <- function() {
  ss <- .sheet()
  df <- read_sheet(ss, sheet = "transactions") |>
    mutate(
      coffees = coerce_integer_safe(coffees),
      amount = coerce_numeric_safe(amount)
    )
  sheet_write(df, ss = ss, sheet = "transactions")
  invisible(df)
}


append_transaction <- function(staff_id, name, type = c("coffee", "topup"), coffees = 0L, amount = 0, note = "", submitted_by = "app") {
  type <- match.arg(type)
  ss <- .sheet()
  now <- lubridate::now(tzone = "UTC")
  df <- tibble(
    timestamp = now,
    staff_id = as.character(staff_id),
    name = as.character(name),
    type = type,
    coffees = as.integer(coffees),
    amount = as.numeric(amount),
    note = as.character(note),
    submitted_by = as.character(submitted_by)
  )
  sheet_append(df, ss = ss, sheet = "transactions")
  invisible(df)
}

balances <- function() {
  read_transactions() |>
    mutate(
      coffees = coerce_integer_safe(coffees),
      amount = coerce_numeric_safe(amount)
    ) |>
    group_by(staff_id, name) |>
    summarise(
      balance = sum(amount, na.rm = TRUE),
      coffees = sum(coffees, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(desc(balance))
}


balance_of <- function(staff_id) {
  b <- balances() |>
    filter(staff_id == !!staff_id)
  if (nrow(b) == 0) tibble(staff_id = staff_id, name = NA_character_, balance = 0, coffees = 0) else b
}


suggest_staff <- function(prefix = "") {
  balances() |>
    mutate(label = paste0(name, " (", staff_id, ")")) |>
    arrange(name) |>
    pull(label)
}

# Cheap change detector: last modified time of the Sheet
sheet_modified_time <- function() {
  ss <- .sheet()
  # drive_get returns RFC3339; convert to numeric seconds to be stable for reactivePoll
  mt <- googledrive::drive_get(ss)$drive_resource[[1]]$modifiedTime
  as.numeric(lubridate::as_datetime(mt, tz = "UTC"))
}