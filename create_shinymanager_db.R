# Script pour créer la base de données shinymanager (ex: create_shinymanager_db.R)
# install.packages("shinymanager") # Si pas déjà installé
library(shinymanager)

# Définir les informations des utilisateurs initiaux
# Notez que les mots de passe sont en clair ici, shinymanager les hashera lors de la création de la DB
initial_credentials <- data.frame(
  user = c("guyjuniorcalvet@gmail.com", "president@example.com"),
  password = c("dev9710", "preS2526"),
  # Colonnes optionnelles que shinymanager peut gérer :
  admin = c(TRUE, FALSE), # L'utilisateur "admin" peut gérer les autres utilisateurs via l'interface d'admin de shinymanager
  role = c("Développeur", "Président"), # Votre colonne personnalisée
  # start = c("2020-12-31"), # Optionnel: date de début de validité
  # expire = c(NA, "2025-12-31"), # Optionnel: date d'expiration
  # comment = c("Compte développeur", "Compte président"), # Optionnel
  stringsAsFactors = FALSE
)

# Créer la base de données SQLite (elle sera chiffrée)
# Le fichier s'appellera "credentials.sqlite" et sera créé dans le répertoire de travail actuel.
# Choisissez une passphrase FORTE et conservez-la précieusement !
create_db(
  credentials_data = initial_credentials,
  sqlite_path = "credentials.sqlite", # Chemin vers le fichier DB à créer
  passphrase = "THINKDIFFERENTTHINKBIG" # CHANGEZ CECI !
)

cat("Base de données 'credentials.sqlite' créée avec succès.\n")
cat("N'oubliez pas d'utiliser la MEME passphrase dans votre application Shiny.\n")

