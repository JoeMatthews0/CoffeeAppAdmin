# init_setup.R
suppressPackageStartupMessages({
  library(googlesheets4)
  library(googledrive)
  library(dplyr)
})


# 1) Auth (interactive OAuth for setup)
gs4_auth(cache = TRUE)
drive_auth(cache = TRUE)


# 2) Create the spreadsheet
ss <- gs4_create("CoffeeClub", sheets = list(
  transactions = data.frame(
    timestamp = as.POSIXct(character()),
    staff_id = character(),
    name = character(),
    type = character(),
    coffees = integer(),
    amount = numeric(),
    note = character(),
    submitted_by = character()
  ),
  config = data.frame(
    key = c("coffee_price", "topup_threshold"),
    value = c("0.5", "2.0")
  )
))


sheet_id <- as_sheets_id(ss)
cat("Created sheet with ID:\n", sheet_id, "\n")


# 3) Share with your service account (replace email!)
drive_share(ss, role = "writer", type = "user", emailAddress = "coffeesheet@coffeeclubapp-474502.iam.gserviceaccount.com")


# 4) Set env var COFFEE_SHEET_ID in your ~/.Renviron using the printed ID