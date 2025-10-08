# generate_qr.R
# Generates a QR that opens your deployed User App URL


# install.packages("qrcode")
library(qrcode)


user_app_url <- "https://your-deployment.example.com/coffee-user" # <-- replace
outfile <- "qr_coffee_user.png"


qr_code(user_app_url, ecl = "M", plotQRcode = FALSE, dataOutput = FALSE,
        mask = TRUE, filename = outfile)


message("Saved ", outfile, ". Print and stick it by the machine.")