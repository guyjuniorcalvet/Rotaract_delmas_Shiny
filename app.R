# 0. PACKAGES & SETUP
# ==========================================================================
library(shiny)
library(DBI); library(RMariaDB); library(pool); library(ggplot2); library(dplyr); library(shinyjs)
library(DT); library(bslib); library(fontawesome); library(shinymanager); library(lubridate); library(sodium)
library(forcats)
library(plotly)
library(thematic)
library(scales)
library(RColorBrewer)

# --- FONCTIONS ET CONSTANTES ----
# ==========================================================================
get_db_config <- function() {
  list(
    dbname   = "Rotaract_de_Delmas",
    host     = "34.148.67.245",
    user     = "shiny_app_user_rtcdelmas'@'%",
    password = "rtcDelmas@25",
    port     = 3306
  )
}

APP_VERSION <- "3.0.0"
LAST_UPDATE_DATE <- "24 mai 2024 à 13:00"

# --- THÈME BSLIB ----
# ==========================================================================
app_light_theme <- bs_theme(
  version = 5, 
  bg = "#FFFFFF", 
  fg = "#212529", 
  primary = "#D32F2F", 
  "navbar-bg" = "#D32F2F",
  "navbar-dark-color" = "rgba(255,255,255,0.9)", 
  "navbar-dark-hover-color" = "#FFFFFF",
  "navbar-dark-active-color" = "#FFFFFF", 
  "font-family-sans-serif" = "'Segoe UI', sans-serif"
)

# 1. UI DEFINITION
# ==========================================================================
ui_content <- page_navbar(
  id = "main_navbar",
  title = tagList(
    tags$img(src = "logo_rotaract.png", height = "30px", style = "margin-right: 10px;"), 
    "Rotaract Club Delmas"
  ),
  theme = app_light_theme, 
  collapsible = TRUE,
  header = tagList(
    useShinyjs(),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"))
  ),
  
  nav_panel(
    title = "Accueil", 
    icon = icon("home"),
    div(
      class = "home-content", 
      style = "text-align: center; padding-top: 50px;",
      tags$img(src = "logo_rotaract.png", height = "150px", style="margin-bottom: 20px;"),
      tags$h2("Interface Shiny pour l'exploitation et l'alimentation de la base de données du Club Rotaract de Delmas", style="color: #D32F2F; margin-bottom: 30px; font-weight: bold;"),
      hr(),
      p("Conçu et développé par : Guy Junior CALVET", style="font-style: italic; font-size: 0.9em;"),
      uiOutput("footer_last_update_text")
    )
  )
)

ui <- secure_app(ui_content, language = "fr")

# 2. SERVER LOGIC
# ==========================================================================
server <- function(input, output, session) {
  
  thematic::thematic_shiny()
  
  # --- 1. GESTION DE SESSION ET AUTHENTIFICATION ---
  raw_credentials_val <- reactiveVal(NULL)
  
  # Lecture de la base de données des utilisateurs chiffrée
  passphrase_shinymanager_db <- "THINKDIFFERENTTHINKBIG"
  df_for_check_credentials <- tryCatch(
    shinymanager::read_db_decrypt("credentials.sqlite", passphrase = passphrase_shinymanager_db), 
    error = function(e) { NULL }
  )
  raw_credentials_val(df_for_check_credentials)
  
  # Lancement du module d'authentification
  res_auth <- secure_server(
    check_credentials = shinymanager::check_credentials(df_for_check_credentials)
  )
  
  # Récupération du rôle de l'utilisateur connecté
  user_role <- reactive({
    req(res_auth$user)
    current_user <- res_auth$user
    current_raw_credentials <- raw_credentials_val()
    req(current_raw_credentials)
    
    user_data_row <- current_raw_credentials[current_raw_credentials$user == current_user, , drop = FALSE]
    role_col_name <- grep("^role$", names(user_data_row), ignore.case = TRUE, value = TRUE)
    
    role_candidate <- if (length(role_col_name) > 0) trimws(as.character(user_data_row[[role_col_name[1]]][1])) else "Visiteur"
    if (is.na(role_candidate) || !nzchar(role_candidate)) "Visiteur" else role_candidate
  })
  
  # --- 2. GESTION DE LA CONNEXION BDD AVEC UN POOL ---
  db_pool <- tryCatch({ 
    config <- get_db_config()
    pool <- dbPool(
      drv = RMariaDB::MariaDB(), 
      dbname = config$dbname, 
      host = config$host, 
      user = config$user,
      password = config$password, 
      port = config$port
    )
    message("✅ Pool de connexions à la base de données créé avec succès.")
    pool 
  }, error = function(e) { 
    message("❌ ÉCHEC CRITIQUE DE LA CRÉATION DU POOL: ", e$message)
    showNotification("Erreur irrécupérable de connexion à la base de données.", type="error", duration=NULL)
    NULL 
  })
  
  onSessionEnded(function() { 
    if (!is.null(db_pool)) { 
      message("🚪 Session terminée. Fermeture du pool de connexions.")
      poolClose(db_pool) 
    }
  })
  
  data_trigger <- reactiveVal(Sys.time())
  
  # --- 3. CONSTRUCTION DYNAMIQUE DU MENU ---
  menu_inserted <- reactiveVal(FALSE)
  observeEvent(res_auth$user, {
    req(res_auth$user, !menu_inserted())
    current_user_role <- user_role()
    
    roles_config <- list(
      "Développeur"       = c("vues", "saisie", "requetes_simples", "requetes_sql"), 
      "Président(e)"      = c("vues", "saisie", "requetes_simples", "requetes_sql"), 
      "PastPrésident(e)"  = c("vues", "saisie", "requetes_simples"), 
      "Vice-président(e)" = c("vues", "saisie"), 
      "Secretaire"        = c("saisie"), # Accepte le rôle sans accent (problème d'encodage)
      "Secrétaire"        = c("saisie")  # Gardé par sécurité
    )
    user_permissions <- roles_config[[current_user_role]]
    
    if (is.null(user_permissions)) return()
    
    menu_items <- list()
    if ("vues" %in% user_permissions) {
      menu_items <- c(menu_items, list(
        nav_panel(title = tagList(icon("chart-pie"), "Vues d'informations"), value = "vue_info_tab", 
                  fluidPage(
                    titlePanel("Visualisation des Données"), 
                    p("Choisissez une table, puis un type de visualisation pour générer un graphique interactif."), 
                    fluidRow(
                      column(4, selectInput("table_select_vue", "1. Choisir une table :", c("Membres", "Activites", "Cotisations", "Presence"))),
                      column(4, uiOutput("graph_type_ui_vue")),
                      column(4, uiOutput("chart_type_ui_vue"))
                    ),
                    hr(), 
                    conditionalPanel(
                      condition = "input.graph_type_select_vue != null && input.graph_type_select_vue != '' && input.chart_type_select_vue != null",
                      h4("Graphique Généré"),
                      plotlyOutput("main_plot_vue"),
                      hr()
                    ), 
                    h4("Données brutes (aperçu)"),
                    DT::dataTableOutput("table_view_dt_vue")
                  )
        )
      ))
    }
    if ("saisie" %in% user_permissions) {
      menu_items <- c(menu_items, list(
        nav_panel(title = tagList(icon("keyboard"), "Saisie de nouvelles données"), value = "saisie_donnees_tab",
                  fluidPage(
                    br(),
                    h3("Ajouter de Nouvelles Entrées"),
                    sidebarLayout(
                      sidebarPanel(width = 4, 
                                   selectInput("table_select_saisie", "Choisir une table pour l'ajout :", choices = c("Membres", "Activites", "Cotisations", "Presence")),
                                   uiOutput("form_ui_ajout"),
                                   actionButton("submit_ajout", "Ajouter à la base de données", icon = icon("plus"), class = "btn-success w-100"),
                                   hr(),
                                   h5("Aperçu (5 dernières entrées)"),
                                   tableOutput("table_view_ajout_preview")
                      ),
                      mainPanel(width = 8,
                                h4("Référence (10 dernières entrées)"),
                                DT::dataTableOutput("table_view_saisie_dt_ref")
                      )
                    )
                  )
        )
      ))
    }
    if ("requetes_simples" %in% user_permissions) {
      menu_items <- c(menu_items, list(
        nav_panel(title = tagList(icon("search"), "Requêtes Simples"), value = "requetes_simples_tab",
                  fluidPage(
                    titlePanel("Exécuter des Requêtes Prédéfinies"),
                    p("Posez une question simple à la base de données sans écrire de code SQL."),
                    selectInput("simple_query_type", "1. Choisir le type de recherche :", c("", "Membres par Statut" = "membres_statut", "Activités dans une période" = "activites_periode", "Cotisations d'un membre" = "cotisations_membre")),
                    uiOutput("simple_query_params_ui"),
                    actionButton("run_simple_query", "Exécuter la recherche", icon = icon("play")),
                    hr(),
                    h4("Résultats de la recherche"),
                    DT::dataTableOutput("simple_query_result_table"),
                    uiOutput("simple_query_graph_ui")
                  )
        )
      ))
    }
    if ("requetes_sql" %in% user_permissions) {
      menu_items <- c(menu_items, list(
        nav_panel(title = tagList(icon("code-branch"), "Requêtes Complexes (SQL)"), value = "requetes_specifiques_tab",
                  fluidPage(
                    titlePanel("Exécuter des Requêtes SQL"),
                    textAreaInput("custom_query_input", "Entrez votre requête SQL SELECT ici:", rows = 5, width = "100%"),
                    actionButton("run_custom_query", "Exécuter la requête", icon = icon("play")),
                    hr(),
                    h4("Résultats de la requête:"),
                    DT::dataTableOutput("custom_query_output")
                  )
        )
      ))
    }
    
    if (length(menu_items) > 0) {
      full_menu <- nav_menu(title = "Menu", icon = icon("bars"), align = "right", !!!menu_items)
      nav_insert(id = "main_navbar", nav = full_menu, target = "Accueil", position = "after")
      menu_inserted(TRUE)
    }
  })
  
  # --- 4. FONCTIONS D'AIDE UTILISANT LE POOL ---
  get_membres_choices <- reactive({ req(db_pool); data_trigger(); tryCatch({ membres <- dbGetQuery(db_pool, "SELECT ID_Membre, Nom, Prenom FROM Membres ORDER BY Nom, Prenom"); setNames(membres$ID_Membre, paste(membres$Nom, membres$Prenom)) }, error = function(e) c("Erreur" = "")) })
  get_activites_choices <- reactive({ req(db_pool); data_trigger(); tryCatch({ activites <- dbGetQuery(db_pool, "SELECT ID_activite, Theme, Date_activite FROM Activites ORDER BY Date_activite DESC, Theme"); setNames(activites$ID_activite, paste(activites$Theme, "- (", format(as.Date(activites$Date_activite), "%d/%m/%Y"), ")")) }, error = function(e) c("Erreur" = "")) })
  
  # --- 5. PAGE : VUES D'INFORMATIONS ---
  output$graph_type_ui_vue <- renderUI({ req(input$table_select_vue); choices <- switch(input$table_select_vue, "Membres" = c("", "Répartition par Genre" = "membres_genre", "Répartition par Poste" = "membres_poste", "Répartition par Statut" = "membres_statut", "Membres par Année d'entrée" = "membres_annee"), "Activites" = c("", "Activités par Type" = "activites_type", "Activités par Mode (Présentiel/Distanciel)" = "activites_mode"), "Cotisations" = c("", "Total par Type de cotisation" = "cotisations_type", "Evolution des cotisations" = "cotisations_temps"), "Presence" = c("", "Répartition par Statut de présence" = "presence_statut", "Participation par activité" = "presence_activite")); selectInput("graph_type_select_vue", "2. Choisir la visualisation :", choices) })
  output$chart_type_ui_vue <- renderUI({ req(input$graph_type_select_vue); choices <- switch(input$graph_type_select_vue, "membres_genre" = c("Barres" = "bar", "Camembert" = "pie"), "membres_poste" = c("Barres" = "bar", "Camembert" = "pie"), "membres_statut" = c("Barres" = "bar", "Camembert" = "pie"), "membres_annee" = c("Lignes" = "line", "Barres" = "bar"), "activites_type" = c("Barres" = "bar", "Camembert" = "pie"), "activites_mode" = c("Barres" = "bar", "Camembert" = "pie"), "cotisations_type" = c("Barres" = "bar"), "cotisations_temps" = c("Lignes" = "line", "Barres" = "bar"), "presence_statut" = c("Barres" = "bar", "Camembert" = "pie"), "presence_activite" = c("Barres" = "bar"), NULL); if (!is.null(choices)) { selectInput("chart_type_select_vue", "3. Choisir le type de graphique :", choices) } })
  
  vue_data <- reactive({ data_trigger(); req(db_pool, input$table_select_vue); dbReadTable(db_pool, input$table_select_vue) })
  
  plot_vue_reactive <- reactive({ 
    req(input$graph_type_select_vue, input$graph_type_select_vue != "", input$chart_type_select_vue, nrow(vue_data()) > 0)
    data <- vue_data()
    chart_type <- input$chart_type_select_vue
    
    plot_data <- switch( input$graph_type_select_vue, "membres_genre" = data %>% count(Genre, name="value") %>% rename(category = Genre), "membres_poste" = data %>% count(Poste, name="value") %>% rename(category = Poste), "membres_statut" = data %>% count(Statut, name="value") %>% rename(category = Statut), "membres_annee" = data %>% mutate(Annee_entree = year(as.Date(Date_debut))) %>% count(Annee_entree, name="value") %>% rename(category = Annee_entree), "activites_type" = data %>% count(Type_activite, name="value") %>% rename(category = Type_activite), "activites_mode" = data %>% mutate(Mode = if_else(Présentiel == 1, "Présentiel", "Distanciel")) %>% count(Mode, name="value") %>% rename(category = Mode), "cotisations_type" = data %>% group_by(Type_cotisation) %>% summarise(value = sum(Montant, na.rm=TRUE)) %>% rename(category = Type_cotisation), "cotisations_temps" = data %>% mutate(AnneeMois = floor_date(as.Date(Date_cotisation), "month")) %>% group_by(AnneeMois) %>% summarise(value = sum(Montant, na.rm=TRUE)) %>% rename(category = AnneeMois), "presence_statut" = data %>% count(Statut, name="value") %>% rename(category = Statut), "presence_activite" = { data_activites <- dbReadTable(db_pool, "Activites"); data %>% filter(Statut == "Présent") %>% count(ID_activite, name="value") %>% left_join(select(data_activites, ID_activite, Theme), by="ID_activite") %>% filter(!is.na(Theme)) %>% rename(category = Theme) })
    
    all_choices <- switch(isolate(input$table_select_vue), "Membres" = c("Répartition par Genre" = "membres_genre", "Répartition par Poste" = "membres_poste", "Répartition par Statut" = "membres_statut", "Membres par Année d'entrée" = "membres_annee"), "Activites" = c("Activités par Type" = "activites_type", "Activités par Mode (Présentiel/Distanciel)" = "activites_mode"), "Cotisations" = c("Total par Type de cotisation" = "cotisations_type", "Evolution des cotisations" = "cotisations_temps"), "Presence" = c("Répartition par Statut de présence" = "presence_statut", "Participation par activité" = "presence_activite")); 
    plot_title <- names(all_choices)[all_choices == input$graph_type_select_vue]; 
    if (length(plot_title) == 0) plot_title <- "Graphique"
    
    if (chart_type == "pie") {
      plot_ly(plot_data, labels = ~category, values = ~value, type = 'pie', textinfo = 'label+percent', hoverinfo = 'text', text = ~paste(category, ":", scales::comma(value)), marker = list(colors = RColorBrewer::brewer.pal(max(3, nrow(plot_data)), "Set2"))) %>% layout(title = list(text = plot_title, x = 0.5), legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.1), showlegend = TRUE) %>% config(displaylogo = FALSE)
    } else { 
      is_flipped <- input$graph_type_select_vue %in% c("membres_poste", "activites_type", "cotisations_type", "presence_activite")
      p_aes <- if (is_flipped) { aes(x = reorder(category, value), y = value, fill = category, text = paste(category, ":", scales::comma(value))) } else { aes(x = category, y = value, fill = category, text = paste(category, ":", scales::comma(value))) }
      p <- ggplot(plot_data, p_aes)
      if (chart_type == "bar") { p <- p + geom_col() } else if (chart_type == "line") { p <- p + geom_line(aes(group = 1), color = "#D32F2F") + geom_point(color = "#D32F2F") }
      p <- p + labs(title = plot_title, x = "", y = "Valeur") + theme_minimal(base_size = 14) + theme(legend.position = "none")
      if (is_flipped) p <- p + coord_flip()
      ggplotly(p, tooltip = "text") %>% config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d", "sendDataToCloud"))
    } 
  })
  
  output$main_plot_vue <- renderPlotly({ plot_vue_reactive() })
  output$table_view_dt_vue <- DT::renderDataTable({ DT::datatable(vue_data(), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) })
  
  # --- 6. PAGE : SAISIE DE DONNÉES ---
  output$form_ui_ajout <- renderUI({ req(input$table_select_saisie); switch(input$table_select_saisie, "Membres" = tagList( textInput("ajout_nom", "Nom *"), textInput("ajout_prenom", "Prénom *"), selectInput("ajout_genre", "Genre *", choices = c("", "Homme", "Femme")), dateInput("ajout_date_naissance", "Date de naissance *", format = "dd-mm-yyyy", language="fr"), selectInput("ajout_poste", "Poste *", choices = c("", 'Président(e)', 'Past-Président(e)', 'Vice-président(e)', 'Secrétaire', 'Secrétaire-adjoint', 'Trésorier(e)', 'Membre', 'Responsable-pro', 'Responsable-int', 'Responsable-comu', 'Responsable-in', 'Invité(e)', 'Ami(e)')), selectInput("ajout_statut", "Statut *", choices = c("", "Actif", "Non-Actif")), dateInput("ajout_date_debut", "Date d'entrée *", format = "dd-mm-yyyy", language="fr", value = Sys.Date()), selectInput("ajout_profession", "Profession *", choices = c("", 'Etudiant(e)', 'Professionnel(le)-actif', 'Etudiant-Professionnel(le)')), textInput("ajout_ville", "Ville *"), textInput("ajout_etat", "État/Département"), textInput("ajout_pays", "Pays *", value="Haïti"), textInput("ajout_zip_code", "Code Postal"), textInput("ajout_email", "Email *"), textInput("ajout_telephone", "Téléphone *"), selectInput("ajout_groupe_sanguin", "Groupe sanguin", choices = c("", "A+", "A-", "B+", "B-", "AB+", "AB-", "O+", "O-", "Inconnu")), textInput("ajout_facebook", "Lien Facebook"), textInput("ajout_instagram", "Lien Instagram"), textInput("ajout_twitter", "Lien Twitter/X"), textInput("ajout_tiktok", "Lien Tiktok"), textInput("ajout_linkedln", "Lien LinkedIn") ), "Activites" = tagList( textInput("ajout_theme", "Thème *"), dateInput("ajout_date_activite", "Date *", format = "dd-mm-yyyy", language="fr"), textInput("ajout_heure_debut", "Heure début (HH:MM) *"), textInput("ajout_heure_fin", "Heure fin (HH:MM) *"), checkboxInput("ajout_presentiel", "Présentiel", TRUE), textInput("ajout_lieu", "Lieu *"), textInput("ajout_ville_activite", "Ville"), textInput("ajout_pays_activite", "Pays"), selectInput("ajout_type_activite", "Type *", choices = c("", "Reunion_statutaire", "Reunion_extraordianire", "Causerie/Conférence", "Formation", "Communautaire", "Sorties")) ), "Cotisations" = tagList( selectInput("ajout_id_membre_cot", "Membre concerné *", choices = get_membres_choices()), dateInput("ajout_date_cotisation", "Date *", format="dd-mm-yyyy", language="fr"), numericInput("ajout_montant", "Montant (HTG) *", value = 0, min = 0), selectInput("ajout_type_cotisation", "Type *", choices = c("", 'Happy_gourdes', 'Amende', 'Frais_district', 'Dons')), textInput("ajout_mode_paiement", "Mode de paiement") ), "Presence" = tagList( selectInput("ajout_id_membre_pres", "Membre concerné *", choices = get_membres_choices()), selectInput("ajout_id_activite_pres", "Activité concernée *", choices = get_activites_choices()), selectInput("ajout_statut_pres", "Statut de présence *", choices = c("", "Présent", "Absent", "Excusé")) ) ) })
  
  observeEvent(input$submit_ajout, { 
    req(db_pool, input$table_select_saisie)
    table_name <- input$table_select_saisie
    is_valid <- TRUE
    error_messages_list <- c()
    
    if (table_name == "Membres") {
      if (any(sapply(c(input$ajout_nom, input$ajout_prenom, input$ajout_genre, input$ajout_poste, input$ajout_statut, input$ajout_profession, input$ajout_ville, input$ajout_pays, input$ajout_email, input$ajout_telephone), function(x) x == ""))) {
        is_valid <- FALSE
      } else {
        query <- sqlInterpolate(db_pool, "INSERT INTO Membres (Nom, Prenom, Genre, Date_naissance, Poste, Statut, Date_debut, Profession, Ville, Etat, Pays, Zip_code, Email, Telephone, Groupe_sanguin, Facebook, Instagram, Twitter, Tiktok, Linkedln) VALUES (?nom, ?prenom, ?genre, ?date_naissance, ?poste, ?statut, ?date_debut, ?profession, ?ville, ?etat, ?pays, ?zip_code, ?email, ?telephone, ?groupe_sanguin, ?facebook, ?instagram, ?twitter, ?tiktok, ?linkedin)",
                                nom = input$ajout_nom, prenom = input$ajout_prenom, genre = input$ajout_genre, date_naissance = input$ajout_date_naissance, poste = input$ajout_poste, statut = input$ajout_statut, date_debut = input$ajout_date_debut, profession = input$ajout_profession, ville = input$ajout_ville, etat = input$ajout_etat, pays = input$ajout_pays, zip_code = input$ajout_zip_code, email = input$ajout_email, telephone = input$ajout_telephone, groupe_sanguin = input$ajout_groupe_sanguin, facebook = input$ajout_facebook, instagram = input$ajout_instagram, twitter = input$ajout_twitter, tiktok = input$ajout_tiktok, linkedin = input$ajout_linkedln)
      }
    } else if (table_name == "Activites") {
      if (any(sapply(c(input$ajout_theme, input$ajout_heure_debut, input$ajout_heure_fin, input$ajout_lieu, input$ajout_type_activite), function(x) x == ""))) {
        is_valid <- FALSE
      } else {
        query <- sqlInterpolate(db_pool, "INSERT INTO Activites (Theme, Date_activite, Heure_debut, Heure_fin, Présentiel, Lieu, Ville_activite, Pays_activite, Type_activite) VALUES (?theme, ?date_activite, ?heure_debut, ?heure_fin, ?presentiel, ?lieu, ?ville_activite, ?pays_activite, ?type_activite)",
                                theme = input$ajout_theme, date_activite = input$ajout_date_activite, heure_debut = input$ajout_heure_debut, heure_fin = input$ajout_heure_fin, presentiel = as.integer(input$ajout_presentiel), lieu = input$ajout_lieu, ville_activite = input$ajout_ville_activite, pays_activite = input$ajout_pays_activite, type_activite = input$ajout_type_activite)
      }
    } else if (table_name == "Cotisations") {
      if (is.null(input$ajout_id_membre_cot) || input$ajout_id_membre_cot == "" || is.null(input$ajout_montant) || input$ajout_type_cotisation == "") {
        is_valid <- FALSE
      } else {
        query <- sqlInterpolate(db_pool, "INSERT INTO Cotisations (ID_membre, Date_cotisation, Montant, Type_cotisation, Mode_paiement) VALUES (?id_membre, ?date_cotisation, ?montant, ?type_cotisation, ?mode_paiement)",
                                id_membre = input$ajout_id_membre_cot, date_cotisation = input$ajout_date_cotisation, montant = input$ajout_montant, type_cotisation = input$ajout_type_cotisation, mode_paiement = input$ajout_mode_paiement)
      }
    } else if (table_name == "Presence") {
      if (is.null(input$ajout_id_membre_pres) || input$ajout_id_membre_pres == "" || is.null(input$ajout_id_activite_pres) || input$ajout_id_activite_pres == "" || input$ajout_statut_pres == "") {
        is_valid <- FALSE
      } else {
        query <- sqlInterpolate(db_pool, "INSERT INTO Presence (ID_membre, ID_activite, Statut) VALUES (?id_membre, ?id_activite, ?statut)",
                                id_membre = input$ajout_id_membre_pres, id_activite = input$ajout_id_activite_pres, statut = input$ajout_statut_pres)
      }
    }
    
    if (!is_valid) {
      showModal(modalDialog(title = "Champs Invalides", p("Les champs avec * sont requis."), easyClose = TRUE))
      return()
    }
    
    tryCatch({ 
      dbExecute(db_pool, query)
      showNotification(paste("Ajout réussi à la table", table_name), type = "message")
      data_trigger(Sys.time())
    }, error = function(e) {
      showModal(modalDialog(title = "Erreur SQL", p("L'ajout a échoué. Détails de l'erreur :"), tags$pre(e$message), easyClose = TRUE))
    })
  })
  
  saisie_ref_data <- reactive({ data_trigger(); req(db_pool, input$table_select_saisie); table_q <- dbQuoteIdentifier(db_pool, input$table_select_saisie); query <- paste("SELECT * FROM", table_q, "ORDER BY 1 DESC LIMIT 10"); tryCatch(dbGetQuery(db_pool, query), error=function(e) data.frame()) })
  output$table_view_ajout_preview <- renderTable({ head(saisie_ref_data(), 5) })
  output$table_view_saisie_dt_ref <- DT::renderDataTable({ DT::datatable(saisie_ref_data(), options=list(pageLength=5, scrollX=T, rownames=F, searching=F, lengthChange=F)) })
  
  # --- 7. PAGE : REQUÊTES SIMPLES ---
  output$simple_query_params_ui <- renderUI({ req(input$simple_query_type); switch(input$simple_query_type, "membres_statut" = selectInput("param_statut", "Choisir un statut :", choices = c("Actif", "Non-Actif"), multiple=TRUE), "activites_periode" = dateRangeInput("param_periode", "Choisir une période :", start = Sys.Date() - 30, end = Sys.Date(), language="fr"), "cotisations_membre" = selectInput("param_membre_id", "Choisir un membre :", choices = get_membres_choices())) })
  simple_query_result_data <- eventReactive(input$run_simple_query, { req(input$simple_query_type, db_pool); query <- ""; if (input$simple_query_type == "membres_statut" && !is.null(input$param_statut)) { query <- sqlInterpolate(db_pool, "SELECT Nom, Prenom, Statut, Email, Telephone FROM Membres WHERE Statut IN (?statuts)", statuts = list(input$param_statut)) } else if (input$simple_query_type == "activites_periode" && !is.null(input$param_periode)) { query <- sqlInterpolate(db_pool, "SELECT Theme, Date_activite, Type_activite, Lieu FROM Activites WHERE Date_activite BETWEEN ?start AND ?end ORDER BY Date_activite DESC", start = input$param_periode[1], end = input$param_periode[2]) } else if (input$simple_query_type == "cotisations_membre" && !is.null(input$param_membre_id)) { query <- sqlInterpolate(db_pool, "SELECT c.Date_cotisation, c.Montant, c.Type_cotisation, c.Mode_paiement FROM Cotisations c WHERE c.ID_membre = ?id ORDER BY c.Date_cotisation DESC", id = input$param_membre_id) }; if (nchar(query) > 0) tryCatch(dbGetQuery(db_pool, query), error=function(e) data.frame(Erreur=e$message)) else data.frame() })
  output$simple_query_result_table <- DT::renderDataTable({ DT::datatable(simple_query_result_data(), options = list(pageLength = 10, scrollX = TRUE), rownames=FALSE) })
  output$simple_query_graph_ui <- renderUI({ if (nrow(simple_query_result_data()) > 0 && input$simple_query_type %in% c("cotisations_membre", "membres_statut")) { tagList(hr(), h4("Visualisation des résultats"), plotlyOutput("simple_query_plot")) } })
  plot_req_reactive <- reactive({ req(nrow(simple_query_result_data()) > 0); data <- simple_query_result_data(); p <- NULL; if (input$simple_query_type == "cotisations_membre") { p <- ggplot(data, aes(x=as.Date(Date_cotisation), y=Montant, fill=Type_cotisation, text = paste("Date:", as.Date(Date_cotisation), "<br>Montant:", Montant, "HTG<br>Type:", Type_cotisation))) + geom_col(position="stack") + labs(title="Cotisations pour le membre sélectionné", x="Date", y="Montant") } else if (input$simple_query_type == "membres_statut") { p <- data %>% count(Statut) %>% ggplot(aes(x=fct_reorder(Statut, -n), y=n, fill=Statut, text=paste("Statut:", Statut, "<br>Nombre:", n))) + geom_col() + labs(title="Répartition des membres trouvés par statut", x="Statut", y="Nombre") }; if (!is.null(p)) { p <- p + theme_minimal(base_size = 14) + theme(legend.position = "bottom"); ggplotly(p, tooltip = "text") %>% config(displaylogo = FALSE) } })
  output$simple_query_plot <- renderPlotly({ plot_req_reactive() })
  
  # --- 8. PAGE : REQUÊTES COMPLEXES (SQL) ---
  custom_query_result <- eventReactive(input$run_custom_query, { req(db_pool, input$custom_query_input); if (!grepl("^\\s*SELECT", input$custom_query_input, ignore.case = TRUE)) { return(data.frame(Message="Action non autorisée. Seules les requêtes SELECT sont permises.")) }; tryCatch( dbGetQuery(db_pool, input$custom_query_input), error=function(e){ data.frame(Erreur=e$message) } ) })
  output$custom_query_output <- DT::renderDataTable({ DT::datatable(custom_query_result(), options=list(pageLength=10, scrollX=T), rownames=F)})
  
  # --- 9. PIED DE PAGE ET FIN DE SESSION ---
  output$footer_last_update_text <- renderText({ 
    paste("Version", APP_VERSION, "| Dernière mise à jour :", LAST_UPDATE_DATE) 
  })
}

# 3. RUN APP
# ==========================================================================
shinyApp(ui = ui, server = server)