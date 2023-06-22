library(shiny)
library(readxl)
library(tm)
library(topicmodels)
library(DT)
library(cluster)
library(ggplot2)
library(udpipe)
library(dplyr)
library(Rtsne)
library(umap)
library(FactoMineR)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(), # Activer shinyjs
  titlePanel("Topic Modeling avec LDA"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choisissez un fichier XLSX",
                accept = c(".xlsx")
      ),
      tags$hr(),
      actionButton("do_analysis", "Effectuer l'analyse de sujets"),
      hidden( # Utilisez hidden pour masquer initialement les éléments
        div(id = "selectors",
            selectInput("select_dim_red", "Choisissez une méthode de réduction de la dimensionnalité", 
                        choices = c("PCA", "t-SNE", "UMAP"), 
                        selected = "PCA"),
            conditionalPanel(
              condition = "input.select_dim_red == 't-SNE'",
              sliderInput("slider_tsne", "Perplexité pour t-SNE", min = 1, max = 100, value = 30)
            )
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Tableau", DTOutput("table")),
        tabPanel("Graphique", plotOutput("plot")),
        tabPanel("Termes par Topic",
                 fluidRow(
                   column(3, selectInput("selected_topic", "Choisissez un sujet:", choices = 1:10)), # Vous pouvez régler les choix selon le nombre de sujets que vous avez.
                   column(9, plotOutput("terms_plot"))
                 )
      )
      )
    )
  )
)

server <- function(input, output) {
  
  data <- reactive({
    req(input$file)
    read_xlsx(input$file$datapath)
  })
  
  analysis_results <- reactiveVal(data.frame())
  tokenized_data <- reactiveVal(data.frame()) # Stocker les tokens
  
  observeEvent(input$do_analysis, {
    dt <- data()
    
    if (is.null(dt) || ncol(dt) < 10) {
      return("Fichier non valide.")
    }
    
    # Sélection des colonnes pertinentes et combinaison en un seul texte
    selected_data <- dt[, c(6, 10)]
    selected_data$combined_text <- paste(selected_data[[1]], selected_data[[2]], sep = " ")
    
    # Nettoyage du texte
    cat("Cleaning text...\n")
    docs <- Corpus(VectorSource(selected_data$combined_text))
    toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
    docs <- tm_map(docs, toSpace, "/")
    docs <- tm_map(docs, toSpace, "@")
    docs <- tm_map(docs, toSpace, "\\|")
    docs <- tm_map(docs, content_transformer(tolower))
    docs <- tm_map(docs, removeNumbers)
    docs <- tm_map(docs, removeWords, stopwords("fr"))
    docs <- tm_map(docs, removePunctuation)
    docs <- tm_map(docs, stripWhitespace)
    
    # Conversion en texte brut pour la tokenisation
    cleaned_text <- sapply(docs, as.character)
    
    # Tokenisation avec udpipe
    cat("Tokenizing...\n")
    ud_model <- udpipe_download_model(language = "french") # Modèle de langue française
    ud_model <- udpipe_load_model(ud_model$file_model)
    tokens <- udpipe_annotate(ud_model, x = cleaned_text)
    tokens <- as.data.frame(tokens)
    
    # Stockage des tokens
    tokenized_data(tokens)
    cat("Tokenized.\n")
    
    
    # Création de la matrice document-terme
    dtm <- DocumentTermMatrix(Corpus(VectorSource(cleaned_text)))
    
    # Vérifier si DTM a au moins deux dimensions
    if (nrow(dtm) < 1 || ncol(dtm) < 1) {
      return("La matrice document-terme est vide après le prétraitement.")
    }
    
    # Filtrer les lignes avec uniquement des zéros dans la DTM
    dtm <- dtm[rowSums(as.matrix(dtm)) > 0, ]
    
    # Application de LDA
    cat("Applying LDA...\n")
    lda_model <- LDA(dtm, k = 10)
    topics <- as.matrix(topics(lda_model))
    
    # Extraction des termes les plus probables pour chaque sujet
    terms <- as.matrix(terms(lda_model, 10))
    terms_per_topic <- data.frame()
    for (i in 1:ncol(terms)) {
      terms_per_topic <- rbind(terms_per_topic, data.frame(Topic=i, Terms=paste(terms[, i], collapse=", ")))
    }
    
    # Combine les deux dans un seul tableau pour l'affichage
    combined_output <- data.frame(DocIndex = 1:nrow(topics), Topic = topics[,1])
    
    # Joindre les termes par sujet avec le tableau
    combined_output <- merge(combined_output, terms_per_topic, by="Topic")
    
    analysis_results(combined_output)
    
    # Obtenir la distribution des sujets pour chaque document
    cat("Obtaining topic distributions...\n")
    gamma <- posterior(lda_model)$topics
    
    if (is.null(dim(gamma))) {
      cat("Gamma has no dimensions. Exiting...\n")
      return()
    }
    
    # Appliquer le clustering K-means sur les distributions de sujet
    cat("Applying K-means clustering...\n")
    set.seed(42)
    kmeans_result <- kmeans(gamma, centers = 10)
    
    plot_data <- reactive({
      if (input$select_dim_red == "PCA") {
        cat("Applying PCA...\n")
        pca_result <- PCA(gamma)
        data.frame(
          x = pca_result$ind$coord[, 1],
          y = pca_result$ind$coord[, 2],
          cluster = as.factor(kmeans_result$cluster)
        )
      } else if (input$select_dim_red == "t-SNE") {
        cat("Applying t-SNE...\n")
        tsne_result <- Rtsne(gamma, perplexity = input$slider_tsne, learning_rate = 200, check_duplicates = FALSE)
        data.frame(
          x = tsne_result$Y[, 1],
          y = tsne_result$Y[, 2],
          cluster = as.factor(kmeans_result$cluster)
        )
      } else if (input$select_dim_red == "UMAP") {
        cat("Applying UMAP...\n")
        umap_result <- umap(gamma, n_neighbors = 15, min_dist = 0.1)
        data.frame(
          x = umap_result$layout[, 1],
          y = umap_result$layout[, 2],
          cluster = as.factor(kmeans_result$cluster)
        )
      }
    })
    
    # Créer un graphique avec ggplot2
    output$plot <- renderPlot({
      cat("Rendering plot...\n")
      plot_data <- plot_data() # utiliser des parenthèses pour appeler la valeur réactive
      
      ggplot(plot_data, aes(x = x, y = y, color = cluster)) +
        geom_point() +
        labs(title = "Clustering K-means des documents basé sur les sujets LDA",
             x = "t-SNE dimension 1",
             y = "t-SNE dimension 2") +
        theme_minimal()
    })
    
    output$terms_plot <- renderPlot({
      # Assurons-nous que le modèle lda_model est bien disponible
      if (!exists("lda_model")) {
        cat("lda_model does not exist. Exiting...\n")
        return()
      }
      
      # Extraire les termes du modèle
      terms_mat <- as.matrix(terms(lda_model, 10)) # Changer 10 à un autre nombre si vous voulez plus/moins de termes
      
      # Extraire les termes et les probabilités pour le sujet sélectionné
      topic_idx <- as.numeric(input$selected_topic)
      terms_for_topic <- terms_mat[, topic_idx]
      probabilities <- colSums(as.matrix(posterior(lda_model)$terms))[terms_for_topic]
      
      # Convertir les probabilités en pourcentages
      percentages <- probabilities * 100
      
      # Créer un dataframe pour le graphique
      top_terms <- data.frame(Term = terms_for_topic, Percentage = percentages)
      
      # Créer un graphique en bâtons
      p <- ggplot(top_terms, aes(x = reorder(Term, -Percentage), y = Percentage)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(title = paste("Termes du sujet", topic_idx),
             x = "Termes",
             y = "Pourcentage") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      # Afficher le graphique
      print(p)
    })
    
    
    
    cat("Finished analysis.\n")
    # Afficher les sélecteurs après avoir terminé l'analyse
    shinyjs::show("selectors")
    
  })
  
  output$table <- renderDT({
    analysis_results()
  })
  
  # Rendu de la table de tokens
  output$tokens <- renderDT({
    tokenized_data()
  })
}

shinyApp(ui, server)
