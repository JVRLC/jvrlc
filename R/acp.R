#' Effectuer une Analyse en Composantes Principales (ACP)
#'
#' Réalise une ACP sur une matrice d'images pour extraire les eigenfaces.
#'
#' @param matrice_images Matrice où chaque ligne représente une image aplatie.
#' @param n_composantes Nombre de composantes principales à conserver (par défaut: toutes).
#' @param centrer Centrer les données (par défaut TRUE).
#' @param reduire Réduire les données (diviser par l'écart-type, par défaut FALSE).
#'
#' @return Une liste de classe "jvrlc_acp" contenant:
#'   \itemize{
#'     \item \code{composantes}: Les composantes principales (eigenfaces).
#'     \item \code{valeurs_propres}: Les valeurs propres.
#'     \item \code{variance_expliquee}: Pourcentage de variance expliquée par composante.
#'     \item \code{variance_cumulee}: Pourcentage de variance cumulée.
#'     \item \code{moyenne}: L'image moyenne.
#'     \item \code{projections}: Les projections des images originales.
#'     \item \code{ecart_type}: Écart-type utilisé pour la réduction.
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' data <- charger_images_dossier("Yale A/")
#' acp <- effectuer_acp(data$matrice, n_composantes = 50)
#' }
effectuer_acp <- function(matrice_images, n_composantes = NULL, centrer = TRUE, reduire = FALSE) {
  if (!is.matrix(matrice_images)) {
    matrice_images <- as.matrix(matrice_images)
  }
  
  n_images <- nrow(matrice_images)
  n_pixels <- ncol(matrice_images)
  
  message("Réalisation de l'ACP sur ", n_images, " images de ", n_pixels, " pixels...")
  
  # Calculer la moyenne
  moyenne <- colMeans(matrice_images)
  
  # Centrer les données
  if (centrer) {
    matrice_centree <- sweep(matrice_images, 2, moyenne)
  } else {
    matrice_centree <- matrice_images
  }
  
  # Réduire si demandé
  ecart_type <- NULL
  if (reduire) {
    ecart_type <- apply(matrice_images, 2, stats::sd)
    ecart_type[ecart_type == 0] <- 1  # Éviter division par zéro
    matrice_centree <- sweep(matrice_centree, 2, ecart_type, "/")
  }
  
  # ACP via SVD (plus efficace pour les grandes dimensions)
  if (n_images < n_pixels) {
    # Méthode optimisée: calculer la matrice de covariance réduite
    cov_reduite <- matrice_centree %*% t(matrice_centree) / (n_images - 1)
    decomposition <- eigen(cov_reduite, symmetric = TRUE)
    
    # Récupérer les vecteurs propres dans l'espace original
    vecteurs_propres <- t(matrice_centree) %*% decomposition$vectors
    
    # Normaliser
    for (i in seq_len(ncol(vecteurs_propres))) {
      norme <- sqrt(sum(vecteurs_propres[, i]^2))
      if (norme > 0) {
        vecteurs_propres[, i] <- vecteurs_propres[, i] / norme
      }
    }
    
    valeurs_propres <- decomposition$values * (n_images - 1)
  } else {
    # SVD directe
    svd_result <- svd(matrice_centree)
    vecteurs_propres <- svd_result$v
    valeurs_propres <- svd_result$d^2 / (n_images - 1)
  }
  
  # Filtrer les valeurs propres négatives ou nulles
  idx_valides <- valeurs_propres > 1e-10
  valeurs_propres <- valeurs_propres[idx_valides]
  vecteurs_propres <- vecteurs_propres[, idx_valides, drop = FALSE]
  
  # Limiter le nombre de composantes
  if (!is.null(n_composantes) && n_composantes < length(valeurs_propres)) {
    valeurs_propres <- valeurs_propres[1:n_composantes]
    vecteurs_propres <- vecteurs_propres[, 1:n_composantes, drop = FALSE]
  }
  
  # Calculer la variance expliquée
  variance_totale <- sum(valeurs_propres)
  variance_expliquee <- valeurs_propres / variance_totale * 100
  variance_cumulee <- cumsum(variance_expliquee)
  
  # Projeter les images originales
  projections <- matrice_centree %*% vecteurs_propres
  
  message("ACP terminée. ", length(valeurs_propres), " composantes retenues.")
  message("Variance expliquée par les 5 premières composantes: ", 
          round(variance_cumulee[min(5, length(variance_cumulee))], 2), "%")
  
  resultat <- list(
    composantes = vecteurs_propres,
    valeurs_propres = valeurs_propres,
    variance_expliquee = variance_expliquee,
    variance_cumulee = variance_cumulee,
    moyenne = moyenne,
    projections = projections,
    ecart_type = ecart_type,
    centrer = centrer,
    reduire = reduire
  )
  
  class(resultat) <- c("jvrlc_acp", "list")
  return(resultat)
}

#' Résumé d'un objet ACP
#'
#' @param object Un objet de classe "jvrlc_acp".
#' @param ... Arguments additionnels (ignorés).
#'
#' @return Invisible NULL.
#' @export
summary.jvrlc_acp <- function(object, ...) {
  cat("=== Résumé ACP (jvrlc) ===\n\n")
  cat("Nombre de composantes:", length(object$valeurs_propres), "\n")
  cat("Nombre de pixels par image:", length(object$moyenne), "\n\n")
  
  cat("Variance expliquée (%):\n")
  n_show <- min(10, length(object$variance_expliquee))
  for (i in 1:n_show) {
    cat(sprintf("  CP%d: %.2f%% (cumulée: %.2f%%)\n", 
                i, object$variance_expliquee[i], object$variance_cumulee[i]))
  }
  
  if (length(object$variance_expliquee) > 10) {
    cat("  ...\n")
  }
  
  invisible(NULL)
}

#' Afficher un objet ACP
#'
#' @param x Un objet de classe "jvrlc_acp".
#' @param ... Arguments additionnels (ignorés).
#'
#' @return Invisible x.
#' @export
print.jvrlc_acp <- function(x, ...) {
  cat("Objet ACP (jvrlc)\n")
  cat("- Composantes:", length(x$valeurs_propres), "\n")
  cat("- Variance totale expliquée:", round(max(x$variance_cumulee), 2), "%\n")
  invisible(x)
}
