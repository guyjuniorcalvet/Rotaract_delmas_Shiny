# Script: create_credentials.R
library(shinymanager)
credentials_data <- data.frame(
  user = c("dev@test.com", "pres@test.com"),
  password = c("dev123", "pres123"),
  role = c("Développeur", "Président"), # Assurez-vous que l'orthographe correspond exactement à roles_config
  stringsAsFactors = FALSE
)
create_db(
  credentials_data = credentials_data,
  sqlite_path = "credentials.sqlite", 
  passphrase = "THINKDIFFERENTTHINKBIG"
)

