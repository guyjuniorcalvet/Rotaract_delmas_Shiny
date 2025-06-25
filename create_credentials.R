# Script: create_credentials.R
library(shinymanager)
credentials_data <- data.frame(
  user = c("dev@test.com", "pres@test.com","pastpres@test.com", "vice@test.com", "secretariat@test.com"),
  password = c("dev123", "pres123","past123", "vice123", "secret123"),
  role = c("Développeur", "Président(e)","PastPrésident(e)", "Vice-président(e)", "Secretaire"), # Assurez-vous que l'orthographe correspond exactement à roles_config
  stringsAsFactors = FALSE
)
create_db(
  credentials_data = credentials_data,
  sqlite_path = "credentials.sqlite", 
  passphrase = "THINKDIFFERENTTHINKBIG"
)

