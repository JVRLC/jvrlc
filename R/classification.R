#' Calculer la distance entre deux visages dans l'espace des eigenfaces
#'
#' @param projection1 Vecteur de projection du premier visage.
#' @param projection2 Vecteur de projection du deuxième visage.
#' @param methode Méthode de calcul de distance ("euclidienne" ou "mahalanobis").
#' @param n_composantes Nombre de composantes à utiliser (par défaut: toutes).
#'
#' @return La distance entre les deux visages.
#' @export
#'
#' @examples
#' \dontrun{
#' dist <- distance_faces(acp$projections[1,], acp$projections[2,])
#' }
distance_faces <- function(projection1, projection2, methode = "euclidienne", 
                           n_composantes = NULL) {
  
  # Limiter aux n premières composantes
  if (!is.null(n_composantes)) {
    n <- min(n_composantes, length(projection1), length(projection2))
    projection1 <- projection1[1:n]
    projection2 <- projection2[1:n]
  }
  
  if (methode == "euclidienne") {
    sqrt(sum((projection1 - projection2)^2))
  } else if (methode == "manhattan") {
    sum(abs(projection1 - projection2))
  } else if (methode == "cosine") {
    1 - sum(projection1 * projection2) / 
      (sqrt(sum(projection1^2)) * sqrt(sum(projection2^2)))
  } else {
    stop("Méthode non reconnue: ", methode)
  }
}

#' Classifier une nouvelle image
#'
#' Identifie à quel sujet appartient une nouvelle image en utilisant les k plus proches voisins.
#'
#' @param acp Objet ACP contenant les projections de référence.
#' @param nouvelle_image Vecteur de la nouvelle image à classifier.
#' @param labels Vecteur des labels pour chaque image de référence.
#' @param k Nombre de voisins à considérer (par défaut 1).
#' @param n_composantes Nombre de composantes à utiliser.
#'
#' @return Liste contenant:
#'   \itemize{
#'     \item \code{prediction}: Le label prédit.
#'     \item \code{distances}: Les distances aux k plus proches voisins.
#'     \item \code{voisins}: Les indices des k plus proches voisins.
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' # Extraire les labels des noms de fichiers
#' labels <- gsub("\\..*", "", noms_fichiers)
#' resultat <- classifier_image(acp, nouvelle_image, labels, k = 3)
#' }
classifier_image <- function(acp, nouvelle_image, labels, k = 1, n_composantes = NULL) {
  if (!inherits(acp, "jvrlc_acp")) {
    stop("L'argument acp doit être un objet de classe 'jvrlc_acp'")
  }
  
  # Projeter la nouvelle image
  if (acp$centrer) {
    image_centree <- nouvelle_image - acp$moyenne
  } else {
    image_centree <- nouvelle_image
  }
  
  if (acp$reduire && !is.null(acp$ecart_type)) {
    image_centree <- image_centree / acp$ecart_type
  }
  
  nouvelle_projection <- as.vector(t(acp$composantes) %*% image_centree)
  
  # Limiter le nombre de composantes
  if (is.null(n_composantes)) {
    n_composantes <- length(nouvelle_projection)
  }
  n_composantes <- min(n_composantes, ncol(acp$composantes))
  
  # Calculer les distances à toutes les images de référence
  n_refs <- nrow(acp$projections)
  distances <- numeric(n_refs)
  
  for (i in 1:n_refs) {
    distances[i] <- distance_faces(
      nouvelle_projection[1:n_composantes], 
      acp$projections[i, 1:n_composantes]
    )
  }
  
  # Trouver les k plus proches voisins
  ordre <- order(distances)
  k_voisins <- ordre[1:min(k, length(ordre))]
  k_distances <- distances[k_voisins]
  k_labels <- labels[k_voisins]
  
  # Vote majoritaire
  prediction <- names(sort(table(k_labels), decreasing = TRUE))[1]
  
  list(
    prediction = prediction,
    distances = k_distances,
    voisins = k_voisins,
    voisins_labels = k_labels,
    projection = nouvelle_projection
  )
}

#' Évaluer la performance de classification par validation croisée leave-one-out
#'
#' @param acp Objet ACP.
#' @param labels Vecteur des labels pour chaque image.
#' @param k Nombre de voisins pour kNN.
#' @param n_composantes Nombre de composantes à utiliser.
#'
#' @return Liste contenant:
#'   \itemize{
#'     \item \code{accuracy}: Taux de bonne classification.
#'     \item \code{predictions}: Prédictions pour chaque image.
#'     \item \code{confusion}: Matrice de confusion.
#'   }
#' @export
evaluer_classification <- function(acp, labels, k = 1, n_composantes = NULL) {
  n <- nrow(acp$projections)
  predictions <- character(n)
  
  if (is.null(n_composantes)) {
    n_composantes <- ncol(acp$composantes)
  }
  
  for (i in 1:n) {
    # Distances aux autres images
    distances <- numeric(n)
    for (j in 1:n) {
      if (i != j) {
        distances[j] <- distance_faces(
          acp$projections[i, 1:n_composantes],
          acp$projections[j, 1:n_composantes]
        )
      } else {
        distances[j] <- Inf
      }
    }
    
    # k plus proches voisins
    ordre <- order(distances)
    k_voisins <- ordre[1:min(k, n - 1)]
    k_labels <- labels[k_voisins]
    
    # Vote majoritaire
    predictions[i] <- names(sort(table(k_labels), decreasing = TRUE))[1]
  }
  
  # Calculer l'accuracy
  accuracy <- mean(predictions == labels)
  
  # Matrice de confusion
  confusion <- table(Réel = labels, Prédit = predictions)
  
  message(sprintf("Accuracy: %.2f%% (%d/%d)", accuracy * 100, sum(predictions == labels), n))
  
  list(
    accuracy = accuracy,
    predictions = predictions,
    confusion = confusion
  )
}
