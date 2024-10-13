library(shiny)
library(leaflet)
library(dplyr)
library(DT)
library(shinydashboard)
library(ggplot2)
library(shinyauthr)
library(sodium)


setwd("C:/Users/isaaq/Downloads/R")

logements = read.csv("logements_69.csv", header = TRUE, sep = ";", dec = ".", fileEncoding = "UTF-8")
logements_echantillon = logements[sample(nrow(logements), 10000), ]

# Fonction pour définir la couleur des étiquettes DPE
getCouleur=function(dpe) {
  if (dpe == "A") {
    return("green")
  } else if (dpe == "B") {
    return("lightgreen")
  } else if (dpe == "C") {
    return("yellow")
  } else if (dpe == "D") {
    return("orange")
  } else if (dpe == "E") {
    return("orangered")
  } else if (dpe == "F") {
    return("red")
  } else {
    return("darkred")
  }
}

# Ajout des couleurs au dataframe
logements_echantillon = logements_echantillon %>%
  mutate(couleur = sapply(Etiquette_DPE, getCouleur))

# Base des utilisateurs pour l'authentification
base_utilisateurs = data.frame(
  utilisateur = c("admin", "invite"), 
  mot_de_passe = sapply(c("admin2024", "invite2024"), sodium::password_store),
  stringsAsFactors = FALSE
)

# Interface utilisateur
ui = dashboardPage(
  dashboardHeader(title = "Carte des Logements dans le Rhône (Échantillon)"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Carte", tabName = "carte"),
      menuItem("KPI", tabName = "kpi"),
      menuItem("Base de données", tabName = "contexte"),
      menuItem("Datavisualisation", tabName = "datavisualisation")
    )
  ),
  
  dashboardBody(
    shinyauthr::loginUI(id = "login"),  # Interface de connexion
    uiOutput("app_content")
  )
)

# Serveur
server = function(input, output, session) {
# Authentification utilisateur 
identifiants = shinyauthr::loginServer(
id = "login",
data = base_utilisateurs,
user_col = utilisateur,
pwd_col = mot_de_passe,
sodium_hashed = TRUE,  
log_out = reactive(input$logout))
  
 # Contenuapplication après authentification
output$app_content = renderUI({
req(identifiants()$user_auth)
    
  
  
tabItems(tabItem(tabName = "carte",
sidebarLayout(
sidebarPanel(
selectInput(inputId = "departement", label = "Choisissez un département",
            choices = unique(logements_echantillon$Code_postal_.BAN.), 
            selected = unique(logements_echantillon$Code_postal_.BAN.)[1])),
mainPanel(leafletOutput("carte"),DTOutput("stats"),downloadButton("downloadData", "Télécharger les données")))),
      



tabItem(tabName = "kpi",
fluidPage(titlePanel("Indicateurs Clés de Performance (KPI)"),
fluidRow(valueBoxOutput("kpi_total_logements"),valueBoxOutput("kpi_score_moyen")))),
      


tabItem(tabName = "contexte",
fluidPage(titlePanel("Base de données (Contexte)"),
          h4("Présentation et Visualisation des Données Disponibles"),
          DTOutput("resume_donnees"))),



tabItem(tabName = "datavisualisation",
fluidPage(titlePanel("Visualisations des Données"),
sidebarLayout(
sidebarPanel(selectInput(inputId = "departement_viz", label = "Choisissez un département pour la visualisation",
                         choices = unique(logements_echantillon$Code_postal_.BAN.),
                         selected = unique(logements_echantillon$Code_postal_.BAN.)[1]),downloadButton("downloadDataViz", "Télécharger les données")),
mainPanel(plotOutput("camembert", height = "600px"),plotOutput("boite_a_moustaches"))))
      )
    )
  })
  
  


# Filtrer les données pour la carte et les statistiques
donnees_filtrees = reactive({logements_echantillon %>% filter(Code_postal_.BAN. == input$departement)})
  
  
# Filtrer les données pour la visualisation
donnees_filtrees_viz = reactive({logements_echantillon %>% filter(Code_postal_.BAN. == input$departement_viz)})
  
  # Carte Leaflet
  output$carte = renderLeaflet({
    leaflet(data = donnees_filtrees()) %>%
      addTiles() %>%
      addCircleMarkers(~lon, ~lat, color = ~couleur, radius = 5, fillOpacity = 0.8,
                       popup = ~paste(
                         "<b>Identifiant BAN:</b>", Identifiant__BAN, "<br>",
                         "<b>Étiquette DPE:</b>", Etiquette_DPE, "<br>",
                         "<b>Date Réception DPE:</b>", Date_réception_DPE, "<br>",
                         "<b>Code Postal:</b>", Code_postal_.BAN.
                       ),
                       clusterOptions = markerClusterOptions()) %>%
      addLegend(
        "bottomright",
        colors = c("green", "lightgreen", "yellow", "orange", "orangered", "red", "darkred"),
        labels = c("A", "B", "C", "D", "E", "F", "G"),
        title = "Étiquette DPE",
        opacity = 0.8
      )
  })
  
  # Télécharger les données filtrées en CSV
  output$downloadData = downloadHandler(
    filename = function() { paste("logements_filtrees_", Sys.Date(), ".csv", sep = "") },
    content = function(file) {
      write.csv(donnees_filtrees(), file, row.names = FALSE)
    }
  )
  
  output$downloadDataViz = downloadHandler(
    filename = function() { paste("logements_visualisation_", Sys.Date(), ".csv", sep = "") },
    content = function(file) {
      write.csv(donnees_filtrees_viz(), file, row.names = FALSE)
    }
  )
  
  # KPI
  output$kpi_total_logements = renderValueBox({
    valueBox(value = nrow(donnees_filtrees()), subtitle = "Total de logements", icon = icon("home"), color = "blue")
  })
  
  output$kpi_score_moyen = renderValueBox({
    valueBox(value = round(mean(donnees_filtrees()$X_score, na.rm = TRUE), 2), subtitle = "Score énergétique moyen", icon = icon("trophy"), color = "green")
  })
  
  # Résumé des données
  output$resume_donnees = renderDT({
    datatable(logements_echantillon, options = list(searching = FALSE, paging = FALSE))
  })
  
  # Statistiques filtrées
  output$stats = renderDT({
    donnees_filtrees() %>%
      summarise(
        Total_Logements = n(),
        Score_Moyen = round(mean(X_score, na.rm = TRUE), 2),
        Code_Postal = unique(Code_postal_.BAN.)
      ) %>%
      datatable(options = list(pageLength = 10, dom = 't'))
  })
  
  # Camembert
  output$camembert = renderPlot({
    donnees_camembert = donnees_filtrees_viz() %>%
      count(Etiquette_DPE) %>%
      mutate(fraction = n / sum(n),
             ypos = cumsum(fraction) - 0.5 * fraction)
    
    ggplot(donnees_camembert, aes(x = "", y = fraction, fill = Etiquette_DPE)) +
      geom_bar(stat = "identity", color = "white", width = 1) +
      coord_polar("y", start = 0) +
      labs(title = "Distribution des Étiquettes DPE", fill = "Étiquette DPE") +
      geom_text(aes(label = paste0(round(fraction * 100), "%")),
                position = position_stack(vjust = 0.5), size = 5, color = "black") +
      theme_void() +
      theme(
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        legend.position = "right",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold")
      ) +
      scale_fill_manual(values = c("A" = "green", "B" = "lightgreen", "C" = "yellow",
                                   "D" = "orange", "E" = "orangered", "F" = "red", "G" = "darkred"))
  })
  
  # Boîte à moustaches
  output$boite_a_moustaches = renderPlot({
    ggplot(donnees_filtrees_viz(), aes(x = Etiquette_DPE, y = X_score, fill = Etiquette_DPE)) +
      geom_boxplot() +
      labs(title = "Boîte à Moustaches des Scores Énergétiques par Étiquette DPE",
           x = "Étiquette DPE", y = "Score Énergétique") +
      theme_minimal() +
      theme(legend.position = "none")
  })
}

# Lancer l'application Shiny
shinyApp(ui, server)
