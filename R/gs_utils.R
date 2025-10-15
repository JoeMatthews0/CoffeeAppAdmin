# R/gs_utils.R
suppressPackageStartupMessages({
  library(googlesheets4)
  library(googledrive)
  library(dplyr)
  library(lubridate)
  library(tibble)
})

# ---- robust credentials bootstrap ----
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (is.atomic(x) && all(is.na(x)))) y else x

write_json_to_tempfile <- function(txt_raw) {
  keyfile <- tempfile(fileext = ".json")
  writeChar(txt_raw, keyfile, eos = NULL, useBytes = TRUE)
  Sys.setenv(GOOGLE_APPLICATION_CREDENTIALS = keyfile)
  keyfile
}

decode_b64_to_tempfile <- function(b64) {
  if (!requireNamespace("base64enc", quietly = TRUE)) stop("Missing `base64enc`.")
  b64 <- gsub("\\s+", "", b64)
  b64 <- sub('^\"', "", sub('\"$', "", b64))
  raw <- try(base64enc::base64decode(b64), silent = TRUE)
  if (inherits(raw, "try-error")) stop("Provided base64 is invalid.")
  keyfile <- tempfile(fileext = ".json")
  writeBin(raw, keyfile)
  Sys.setenv(GOOGLE_APPLICATION_CREDENTIALS = keyfile)
  keyfile
}

looks_like_json <- function(txt) {
  s <- trimws(substr(txt, 1, 200))
  startsWith(s, "{") || startsWith(s, "[")
}

looks_like_b64 <- function(txt) {
  # quick heuristic: base64 uses [A-Za-z0-9+/=] and often starts with eyJ (for JSON)
  grepl("^[A-Za-z0-9+/=\\r\\n]+$", txt) && nchar(gsub("\\s", "", txt)) >= 64
}

cred_from_env <- function() {
  # 1) Explicit file path
  sa_path <- Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS", "")
  if (nzchar(sa_path) && file.exists(sa_path)) return(sa_path)
  
  # 2) GSA_JSON can be RAW JSON or BASE64 JSON
  gsa_json <- Sys.getenv("GSA_JSON", "")
  if (nzchar(gsa_json)) {
    if (looks_like_json(gsa_json)) {
      return(write_json_to_tempfile(gsa_json))
    } else if (looks_like_b64(gsa_json)) {
      return(decode_b64_to_tempfile(gsa_json))
    } else {
      message("[gs_auth] GSA_JSON present but not recognised as JSON or base64.")
    }
  }
  
  # 3) Dedicated base64 var
  gsa_b64 <- Sys.getenv("GSA_JSON_B64", "")
  if (nzchar(gsa_b64)) {
    return(decode_b64_to_tempfile(gsa_b64))
  }
  
  ""  # nothing found
}

.gs_auth <- function() {
  keyfile <- cred_from_env()
  if (nzchar(keyfile) && file.exists(keyfile)) {
    # Optionally sanity-check this is a service account
    if (requireNamespace("jsonlite", quietly = TRUE)) {
      j <- try(jsonlite::fromJSON(keyfile), silent = TRUE)
      if (!inherits(j, "try-error") && isTRUE(j$type != "service_account")) {
        stop("Credential JSON found, but type != 'service_account'. Use a service account key.")
      }
    }
    googlesheets4::gs4_deauth()  # ensure clean state
    googlesheets4::gs4_auth(path = keyfile, scopes = "https://www.googleapis.com/auth/spreadsheets")
    googledrive::drive_deauth()
    googledrive::drive_auth(path = keyfile)
    message(sprintf("[gs_auth] Service account loaded (%d bytes).", file.size(keyfile)))
  } else {
    message("[gs_auth] No service account credentials found in env; refusing interactive OAuth on Connect.")
    stop("Service account credentials not found. Set GSA_JSON (raw or base64) or GSA_JSON_B64, or a file path in GOOGLE_APPLICATION_CREDENTIALS.")
  }
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
