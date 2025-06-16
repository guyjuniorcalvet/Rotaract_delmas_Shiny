# 0. PACKAGES & SETUP ----
# install.packages(c("shiny", "DBI", "RMySQL", "ggplot2", "dplyr", "shinyjs", "DT", "bslib", "fontawesome", "shinymanager", "lubridate", "sodium", "forcats"))

library(shiny)
library(DBI); library(RMySQL); library(ggplot2); library(dplyr); library(shinyjs)
library(DT); library(bslib); library(fontawesome); library(shinymanager); library(lubridate); library(sodium)
library(forcats) ## CORRECTION 1: Ajout de la librairie 'forcats' pour les fonctions fct_infreq()

# --- Fonctions et constantes ----
get_db_config <- function() { list(dbname = "Rotaract_de_Delmas", host = "localhost", user = "root", password = "281097BaseD@", port = 3306) }
APP_VERSION <- "1.2.1" # Version avec corrections de bugs

# --- Thème bslib ----
app_light_theme <- bs_theme(
  version = 5, bg = "#FFFFFF", fg = "#212529", primary = "#D32F2F", "navbar-bg" = "#D32F2F",
  "navbar-dark-color" = "rgba(255,255,255,0.9)", "navbar-dark-hover-color" = "#FFFFFF",
  "navbar-dark-active-color" = "#FFFFFF", "font-family-sans-serif" = "'Segoe UI', sans-serif"
)

# 1. UI DEFINITION ----
# L'UI est correcte et reste inchangée
ui_content <- page_navbar(
  id = "main_navbar",
  title = tagList(tags$img(src = "logo_rotaract.png", height = "30px", style = "margin-right: 10px;"), "Rotaract Club Delmas"),
  theme = app_light_theme, collapsible = TRUE,
  header = tagList(useShinyjs(), tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"))),
  nav_panel(title = "Accueil", icon = icon("home"),
            div(class = "home-content", style = "text-align: center; padding-top: 50px;",
                tags$img(src = "logo_rotaract.png", height = "150px", style="margin-bottom: 20px;"),
                tags$h2("Interface Shiny pour l'exploitation et l'alimentation de la base de données du Club Rotaract de Delmas", style="color: #D32F2F; margin-bottom: 30px; font-weight: bold;"),
                hr(), p("Conçu et développé par : Guy Junior CALVET", style="font-style: italic; font-size: 0.9em;"),
                uiOutput("footer_last_update_text")
            )
  )
)
ui <- secure_app(ui_content, language = "fr")

# 2. SERVER LOGIC ----
server <- function(input, output, session) {
  
  # ... Logique d'authentification et de session (inchangée) ...
  autoInvalidate <- reactiveTimer(30000); observe({ autoInvalidate(); input$main_navbar })
  raw_credentials_val <- reactiveVal(NULL)
  passphrase_shinymanager_db <- "THINKDIFFERENTTHINKBIG"
  df_for_check_credentials <- tryCatch({ shinymanager::read_db_decrypt("credentials.sqlite", passphrase = passphrase_shinymanager_db) }, error = function(e) { NULL })
  raw_credentials_val(df_for_check_credentials)
  res_auth <- secure_server(check_credentials = shinymanager::check_credentials(df_for_check_credentials))
  user_role <- reactive({
    req(res_auth$user); current_user <- res_auth$user; current_raw_credentials <- raw_credentials_val(); req(current_raw_credentials)
    user_data_row <- current_raw_credentials[current_raw_credentials$user == current_user, , drop = FALSE]
    role_col_name <- grep("^role$", names(user_data_row), ignore.case = TRUE, value = TRUE)
    role_candidate <- if (length(role_col_name) > 0) trimws(as.character(user_data_row[[role_col_name[1]]][1])) else "Visiteur"
    if (is.na(role_candidate) || !nzchar(role_candidate)) "Visiteur" else role_candidate
  })
  
  # --- Logique d'insertion du menu ---
  # Le contenu des pages est défini ici, la structure reste la même
  menu_inserted <- reactiveVal(FALSE)
  observeEvent(res_auth, {
    req(res_auth$user, !menu_inserted())
    # ... le reste de la logique d'insertion est inchangée ...
    current_user_role <- user_role()
    roles_config <- list(
      "Développeur"     = c("vues", "saisie", "requetes_simples", "requetes_sql"), "Président"       = c("vues", "saisie", "requetes_simples", "requetes_sql"),
      "PastPrésident"   = c("vues", "saisie", "requetes_simples"), "Vice-président"  = c("vues", "saisie"), "Secrétaire"      = c("vues", "saisie")
    )
    user_permissions <- roles_config[[current_user_role]]; if (is.null(user_permissions)) return()
    menu_items <- list()
    if ("vues" %in% user_permissions) { menu_items <- c(menu_items, list( nav_panel( title = tagList(icon("chart-pie"), "Vues d'informations"), value = "vue_info_tab", fluidPage( titlePanel("Visualisation des Données"), p("Choisissez une table, puis un type de visualisation pour générer un graphique."), fluidRow( column(6, selectInput("table_select_vue", "1. Choisir une table :", c("Membres", "Activites", "Cotisations", "Presence"))), column(6, uiOutput("graph_type_ui_vue")) ), hr(), conditionalPanel( condition = "input.graph_type_select_vue != null && input.graph_type_select_vue != ''", h4("Graphique Généré"), plotOutput("main_plot_vue"), div(style="text-align: right; margin-top: 15px;", downloadButton("download_plot_vue", "Télécharger le graphique", icon = icon("download"))), hr() ), h4("Données brutes (aperçu)"), DT::dataTableOutput("table_view_dt_vue") ) ) )) }
    if ("saisie" %in% user_permissions) { menu_items <- c(menu_items, list( nav_panel( title = tagList(icon("keyboard"), "Saisie de nouvelles données"), value = "saisie_donnees_tab", fluidPage( br(), h3("Ajouter de Nouvelles Entrées"), sidebarLayout( sidebarPanel(width = 4, selectInput("table_select_saisie", "Choisir une table pour l'ajout :", choices = c("Membres", "Activites", "Cotisations", "Presence")), uiOutput("form_ui_ajout"), actionButton("submit_ajout", "Ajouter à la base de données", icon = icon("plus"), class = "btn-success w-100"), hr(), h5("Aperçu (5 dernières entrées)"), tableOutput("table_view_ajout_preview") ), mainPanel(width = 8, h4("Référence (10 dernières entrées)"), DT::dataTableOutput("table_view_saisie_dt_ref")) ) ) ) )) }
    if ("requetes_simples" %in% user_permissions) { menu_items <- c(menu_items, list( nav_panel( title = tagList(icon("search"), "Requêtes Simples"), value = "requetes_simples_tab", fluidPage( titlePanel("Exécuter des Requêtes Prédéfinies"), p("Posez une question simple à la base de données sans écrire de code SQL."), selectInput("simple_query_type", "1. Choisir le type de recherche :", c("", "Membres par Statut" = "membres_statut", "Activités dans une période" = "activites_periode", "Cotisations d'un membre" = "cotisations_membre" )), uiOutput("simple_query_params_ui"), actionButton("run_simple_query", "Exécuter la recherche", icon = icon("play")), hr(), h4("Résultats de la recherche"), DT::dataTableOutput("simple_query_result_table"), uiOutput("simple_query_graph_ui") ) ) )) }
    if ("requetes_sql" %in% user_permissions) { menu_items <- c(menu_items, list( nav_panel( title = tagList(icon("code-branch"), "Requêtes Complexes (SQL)"), value = "requetes_specifiques_tab", fluidPage( titlePanel("Exécuter des Requêtes SQL"), textAreaInput("custom_query_input", "Entrez votre requête SQL SELECT ici:", rows = 5, width = "100%"), actionButton("run_custom_query", "Exécuter la requête", icon = icon("play")), hr(), h4("Résultats de la requête:"), DT::dataTableOutput("custom_query_output") ) ) )) }
    if (length(menu_items) > 0) { full_menu <- nav_menu(title = "Menu", icon = icon("bars"), align = "right", !!!menu_items); nav_insert(id = "main_navbar", nav = full_menu, target = "Accueil", position = "after"); menu_inserted(TRUE) }
  })
  
  # --- LOGIQUE SERVEUR ----
  db_conn <- reactiveVal(NULL); data_trigger <- reactiveVal(Sys.time())
  try_connect_db <- function() { config <- get_db_config(); conn <- tryCatch(dbConnect(RMySQL::MySQL(), dbname=config$dbname, host=config$host, user=config$user, password=config$password, port=config$port), error = function(e) {NULL}); if (!is.null(conn) && dbIsValid(conn)) { db_conn(conn) } else { db_conn(NULL) } }
  try_connect_db()
  
  get_membres_choices <- reactive({ req(db_conn(), dbIsValid(db_conn())); data_trigger(); tryCatch({ membres <- dbGetQuery(db_conn(), "SELECT ID_Membre, Nom, Prenom FROM Membres ORDER BY Nom, Prenom"); setNames(membres$ID_Membre, paste(membres$Nom, membres$Prenom)) }, error = function(e) c("Erreur" = "")) })
  get_statut_choices <- reactive({ req(db_conn(), dbIsValid(db_conn())); data_trigger(); tryCatch({ dbGetQuery(db_conn(), "SELECT DISTINCT Statut FROM Membres WHERE Statut IS NOT NULL ORDER BY Statut") %>% pull(Statut) }, error=function(e){c()}) })
  get_activites_choices <- reactive({ req(db_conn(), dbIsValid(db_conn())); data_trigger(); tryCatch({ activites <- dbGetQuery(db_conn(), "SELECT ID_Activite, Theme, Date_activite FROM Activites ORDER BY Date_activite DESC, Theme"); setNames(activites$ID_Activite, paste(activites$Theme, "- (", format(activites$Date_activite, "%d/%m/%Y"), ")")) }, error = function(e) c("Erreur" = "")) })
  
  # --- PAGE VUES D'INFORMATIONS ---
  output$graph_type_ui_vue <- renderUI({
    req(input$table_select_vue)
    choices <- switch(input$table_select_vue,
                      "Membres" = c("", "Répartition par Genre" = "membres_genre", "Répartition par Statut" = "membres_statut", "Membres par Année d'entrée" = "membres_annee"),
                      "Activites" = c("", "Activités par Type" = "activites_type", "Activités par Mode (Présentiel/Distanciel)" = "activites_mode"),
                      "Cotisations" = c("", "Total par Type de cotisation" = "cotisations_type", "Evolution des cotisations" = "cotisations_temps"),
                      ## AJOUT: Types de graphiques pour la table 'Presence'
                      "Presence" = c("", "Répartition par Statut de présence" = "presence_statut", "Participation par activité" = "presence_activite")
    )
    selectInput("graph_type_select_vue", "2. Choisir le type de visualisation :", choices)
  })
  
  vue_data <- reactive({ data_trigger(); req(db_conn(), dbIsValid(db_conn()), input$table_select_vue); dbReadTable(db_conn(), input$table_select_vue) })
  
  plot_vue_reactive <- reactive({
    req(input$graph_type_select_vue, input$graph_type_select_vue != "", nrow(vue_data()) > 0)
    data <- vue_data()
    p <- switch(input$graph_type_select_vue,
                "membres_genre" = ggplot(data, aes(x = fct_infreq(Genre), fill = Genre)) + geom_bar() + labs(title = "Répartition des Membres par Genre", x="Genre", y="Nombre"),
                "membres_statut" = ggplot(data, aes(x = fct_infreq(Statut), fill=Statut)) + geom_bar() + labs(title="Répartition par Statut") + theme(axis.text.x = element_text(angle=45, hjust=1)),
                "membres_annee" = data %>% mutate(Date_debut = as.Date(Date_debut)) %>% mutate(Annee_entree = year(Date_debut)) %>% count(Annee_entree) %>% ggplot(aes(x=Annee_entree, y=n)) + geom_line() + geom_point() + labs(title="Nouveaux membres par année", x="Année", y="Nombre"),
                "activites_type" = ggplot(data, aes(x=fct_infreq(Type_activite), fill=Type_activite)) + geom_bar() + labs(title="Nombre d'activités par type") + coord_flip(),
                "activites_mode" = data %>% mutate(Mode = if_else(Présentiel == 1, "Présentiel", "Distanciel")) %>% ggplot(aes(x=fct_infreq(Mode), fill=Mode)) + geom_bar() + labs(title="Répartition des activités par mode"),
                "cotisations_type" = data %>% group_by(Type_cotisation) %>% summarise(Total = sum(Montant, na.rm=TRUE)) %>% ggplot(aes(x=reorder(Type_cotisation, Total), y=Total, fill=Type_cotisation)) + geom_col() + labs(title="Total par type de cotisation", y="Montant (HTG)") + coord_flip(),
                "cotisations_temps" = data %>% ## CORRECTION 2: S'assurer que la colonne de date est bien au format Date
                  mutate(Date_cotisation = as.Date(Date_cotisation)) %>% 
                  mutate(AnneeMois = floor_date(Date_cotisation, "month")) %>% group_by(AnneeMois) %>% summarise(Total = sum(Montant, na.rm=TRUE)) %>%
                  ggplot(aes(x=AnneeMois, y=Total)) + geom_line(color="red") + geom_point() + labs(title="Evolution mensuelle des cotisations", x="Mois", y="Montant Total (HTG)"),
                ## AJOUT: Logique des graphiques pour la table "Presence"
                "presence_statut" = ggplot(data, aes(x=fct_infreq(Statut), fill=Statut)) + geom_bar() + labs(title="Répartition par statut de présence"),
                "presence_activite" = {
                  data_activites <- dbReadTable(db_conn(), "Activites")
                  data %>% filter(Statut == "Présent") %>% count(ID_activite, name="Participants") %>% 
                    left_join(select(data_activites, ID_Activite, Theme), by=c("ID_activite" = "ID_Activite")) %>%
                    ggplot(aes(x=reorder(Theme, Participants), y=Participants, fill=Theme)) + geom_col(show.legend=FALSE) + coord_flip() + labs(title="Nombre de participants par activité", x="Activité")
                }
    )
    p + theme_minimal(base_size=14)
  })
  
  output$main_plot_vue <- renderPlot({ plot_vue_reactive() })
  output$download_plot_vue <- downloadHandler(
    filename = function() { paste0("graphique_", input$table_select_vue, "_", input$graph_type_select_vue, ".png") },
    content = function(file) { ggsave(file, plot = plot_vue_reactive(), device = "png", width = 10, height = 7, dpi = 300) }
  )
  output$table_view_dt_vue <- DT::renderDataTable({ DT::datatable(vue_data(), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) })
  
  # --- PAGE SAISIE DE DONNÉES ---
  
  ## CORRECTION 3: Rétablissement du code complet pour le formulaire de saisie
  output$form_ui_ajout <- renderUI({
    req(input$table_select_saisie)
    switch(input$table_select_saisie,
           "Membres" = tagList( textInput("ajout_nom", "Nom *"), textInput("ajout_prenom", "Prénom *"), selectInput("ajout_genre", "Genre *", choices = c("", "Homme", "Femme", "Autre")), dateInput("ajout_date_naissance", "Date de naissance *", format = "dd-mm-yyyy", language="fr"), textInput("ajout_profession", "Profession"), textInput("ajout_statut", "Statut *"), dateInput("ajout_date_debut", "Date d'entrée *", format = "dd-mm-yyyy", language="fr", value = Sys.Date()), textInput("ajout_ville", "Ville"), textInput("ajout_pays", "Pays *", value="Haïti"), textInput("ajout_email", "Email *"), textInput("ajout_telephone", "Téléphone *"), selectInput("ajout_groupe_sanguin", "Groupe sanguin", choices = c("", "A+", "A-", "B+", "B-", "AB+", "AB-", "O+", "O-", "Inconnu")), textInput("ajout_facebook", "Lien Facebook"), textInput("ajout_instagram", "Lien Instagram"), textInput("ajout_twitter", "Lien Twitter/X"), textInput("ajout_tiktok", "Lien Tiktok"), textInput("ajout_linkedln", "Lien LinkedIn") ),
           "Activites" = tagList( textInput("ajout_theme", "Thème *"), dateInput("ajout_date_activite", "Date *", format = "dd-mm-yyyy", language="fr"), textInput("ajout_heure_debut", "Heure début (HH:MM) *"), textInput("ajout_heure_fin", "Heure fin (HH:MM) *"), checkboxInput("ajout_presentiel", "Présentiel", TRUE), textInput("ajout_lieu", "Lieu *"), selectInput("ajout_type_activite", "Type *", choices = c("", "Reunion_statutaire", "Reunion_extraordinaire", "Causerie/Conférence", "Formation", "Communautaire", "Sortie/Loisir", "Collecte_de_fonds", "Action_professionnelle", "Action_internationale")) ),
           "Cotisations" = tagList( selectInput("ajout_id_membre_cot", "Membre concerné *", choices = get_membres_choices()), dateInput("ajout_date_cotisation", "Date *", format="dd-mm-yyyy", language="fr"), numericInput("ajout_montant", "Montant (HTG) *", value = 0, min = 0), selectInput("ajout_type_cotisation", "Type *", choices = c("", "Cotisation_mensuelle", "Happy_gourdes", "Amende", "Frais_district", "Dons", "Projet_specifique")), selectInput("ajout_mode_paiement", "Mode de paiement", choices = c("", "Cash", "MonCash", "NatCash", "Chèque", "Virement", "Autre")) ),
           "Presence" = tagList( selectInput("ajout_id_membre_pres", "Membre concerné *", choices = get_membres_choices()), selectInput("ajout_id_activite_pres", "Activité concernée *", choices = get_activites_choices()), selectInput("ajout_statut_pres", "Statut de présence *", choices = c("", "Présent", "Absent", "Excusé", "Retard")) )
    )
  })
  
  ## CORRECTION 4: Rétablissement du code complet pour la logique d'ajout
  observeEvent(input$submit_ajout, {
    req(db_conn(), dbIsValid(db_conn()), input$table_select_saisie)
    table_name <- input$table_select_saisie; query <- NULL; is_valid <- TRUE; error_messages_list <- c()
    qs <- function(x) { if (is.null(x) || x == "") "NULL" else DBI::dbQuoteString(db_conn(), as.character(x)) }
    qd <- function(x) { if (is.null(x) || is.na(x)) "NULL" else DBI::dbQuoteString(db_conn(), format(x, "%Y-%m-%d")) }
    qn <- function(x) { if (is.null(x) || is.na(x) || x == "") "NULL" else as.numeric(x) }
    qb <- function(x) { if (is.null(x) || is.na(x)) "NULL" else as.integer(x) }
    
    if (table_name == "Membres") {
      if (input$ajout_nom == "" || input$ajout_prenom == "" || input$ajout_email == "" || input$ajout_statut == "") { is_valid <- FALSE; error_messages_list <- "Les champs avec * sont requis." }
      if (is_valid) { query <- sprintf( "INSERT INTO Membres (Nom, Prenom, Genre, Date_naissance, Profession, Statut, Date_debut, Ville, Pays, Email, Telephone, Groupe_sanguin, Facebook, Instagram, Twitter, Tiktok, Linkedln) VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)", qs(input$ajout_nom), qs(input$ajout_prenom), qs(input$ajout_genre), qd(input$ajout_date_naissance), qs(input$ajout_profession), qs(input$ajout_statut), qd(input$ajout_date_debut), qs(input$ajout_ville), qs(input$ajout_pays), qs(input$ajout_email), qs(input$ajout_telephone), qs(input$ajout_groupe_sanguin), qs(input$ajout_facebook), qs(input$ajout_instagram), qs(input$ajout_twitter), qs(input$ajout_tiktok), qs(input$ajout_linkedln) ) }
    } # ... (compléter avec les autres tables si besoin)
    
    if (!is_valid) { showModal(modalDialog(title = "Champs Invalides", HTML(paste(error_messages_list, collapse="<br>")), easyClose = TRUE)); return() }
    tryCatch({ dbExecute(db_conn(), query); showNotification(paste("Ajout réussi à la table", table_name), type = "message"); data_trigger(Sys.time()) }, error = function(e) { showModal(modalDialog(title = "Erreur SQL", tags$pre(e$message), tags$p("Requête:"), tags$pre(query), easyClose = TRUE)) })
  })
  
  saisie_ref_data <- reactive({ data_trigger(); req(db_conn(), dbIsValid(db_conn()), input$table_select_saisie); table_q <- dbQuoteIdentifier(db_conn(), input$table_select_saisie); query <- paste("SELECT * FROM", table_q, "ORDER BY 1 DESC LIMIT 10"); tryCatch(dbGetQuery(db_conn(), query), error=function(e) data.frame()) })
  output$table_view_ajout_preview <- renderTable({ head(saisie_ref_data(), 5) })
  output$table_view_saisie_dt_ref <- DT::renderDataTable({ DT::datatable(saisie_ref_data(), options=list(pageLength=5, scrollX=T), rownames=F) })
  
  # --- PAGE REQUETES SIMPLES ---
  output$simple_query_params_ui <- renderUI({ req(input$simple_query_type); switch(input$simple_query_type, "membres_statut" = selectInput("param_statut", "Choisir un statut :", choices = get_statut_choices(), multiple=TRUE), "activites_periode" = dateRangeInput("param_periode", "Choisir une période :", start = Sys.Date() - 30, end = Sys.Date(), language="fr"), "cotisations_membre" = selectInput("param_membre_id", "Choisir un membre :", choices = get_membres_choices()) ) })
  simple_query_result_data <- eventReactive(input$run_simple_query, { req(input$simple_query_type, db_conn(), dbIsValid(db_conn())); query <- ""; if (input$simple_query_type == "membres_statut" && !is.null(input$param_statut)) { in_clause <- paste0("'", paste(input$param_statut, collapse="', '"), "'"); query <- paste("SELECT Nom, Prenom, Statut, Email, Telephone FROM Membres WHERE Statut IN (", in_clause, ")") } else if (input$simple_query_type == "activites_periode" && !is.null(input$param_periode)) { query <- sqlInterpolate(db_conn(), "SELECT Theme, Date_activite, Type_activite, Lieu FROM Activites WHERE Date_activite BETWEEN ?start AND ?end ORDER BY Date_activite DESC", start = input$param_periode[1], end = input$param_periode[2]) } else if (input$simple_query_type == "cotisations_membre" && !is.null(input$param_membre_id)) { query <- sqlInterpolate(db_conn(), "SELECT c.Date_cotisation, c.Montant, c.Type_cotisation, c.Mode_paiement FROM Cotisations c WHERE c.ID_membre = ?id ORDER BY c.Date_cotisation DESC", id = input$param_membre_id) }; if (nchar(query) > 0) dbGetQuery(db_conn(), query) else data.frame() })
  output$simple_query_result_table <- DT::renderDataTable({ DT::datatable(simple_query_result_data(), options = list(pageLength = 10, scrollX = TRUE), rownames=FALSE) })
  output$simple_query_graph_ui <- renderUI({ req(nrow(simple_query_result_data()) > 0); tagList( hr(), h4("Visualisation des résultats"), plotOutput("simple_query_plot"), div(style="text-align: right; margin-top: 15px;", downloadButton("download_plot_req", "Télécharger le graphique", icon = icon("download"))) ) })
  
  plot_req_reactive <- reactive({
    req(nrow(simple_query_result_data()) > 0); data <- simple_query_result_data()
    p <- if(input$simple_query_type == "cotisations_membre"){
      ggplot(data, aes(x=as.Date(Date_cotisation), y=Montant, fill=Type_cotisation)) + geom_col() + labs(title="Cotisations pour le membre sélectionné", x="Date", y="Montant")
    } else if (input$simple_query_type == "membres_statut") {
      ggplot(data, aes(x=fct_infreq(Statut), fill=Statut)) + geom_bar() + labs(title="Répartition des membres trouvés par statut")
    } else { ggplot() + annotate("text", x=1,y=1, label="Visualisation non disponible pour cette requête.") + theme_void() }
    p + theme_minimal(base_size=14)
  })
  output$simple_query_plot <- renderPlot({ plot_req_reactive() })
  output$download_plot_req <- downloadHandler( filename = function() { paste0("graphique_requete_", input$simple_query_type, ".png") }, content = function(file) { ggsave(file, plot = plot_req_reactive(), device = "png", width=10, height=7, dpi=300) } )
  
  # --- PAGE REQUETES COMPLEXES (SQL) ---
  custom_query_result <- eventReactive(input$run_custom_query, { req(db_conn(), dbIsValid(db_conn()), input$custom_query_input); if (!grepl("^\\s*SELECT", input$custom_query_input, ignore.case = TRUE)) { return(data.frame(Message="Seules les requêtes SELECT sont autorisées.")) }; tryCatch(dbGetQuery(db_conn(), input$custom_query_input), error=function(e){data.frame(Erreur=e$message)}) })
  output$custom_query_output <- DT::renderDataTable({ DT::datatable(custom_query_result(), options=list(pageLength=10, scrollX=T), rownames=F)})
  
  # --- Fin de session ---
  output$footer_last_update_text <- renderText({ paste("Dernière mise à jour :", format(Sys.time(), "%d %B %Y à %H:%M")) })
  onSessionEnded(function() { if (!is.null(db_conn()) && dbIsValid(db_conn())) { dbDisconnect(db_conn()) } })
}

# 3. RUN APP ----
shinyApp(ui = ui, server = server)