#' jvrlc: Analyse d'Images et ACP pour la Reconnaissance Faciale
#'
#' Ce package fournit des outils pour l'analyse d'images faciales utilisant
#' l'Analyse en Composantes Principales (ACP). Il permet de:
#' 
#' \itemize{
#'   \item Charger des images depuis différents formats
#'   \item Effectuer une ACP pour extraire les eigenfaces
#'   \item Visualiser les résultats (images, eigenfaces, variance)
#'   \item Reconstruire des images avec un nombre réduit de composantes
#'   \item Classifier de nouvelles images par similarité
#' }
#'
#' @section Fonctions principales:
#' \describe{
#'   \item{\code{\link{charger_image}}}{Charge une image depuis un fichier}
#'   \item{\code{\link{charger_images_dossier}}}{Charge toutes les images d'un dossier}
#'   \item{\code{\link{effectuer_acp}}}{Réalise l'ACP sur les images}
#'   \item{\code{\link{afficher_image}}}{Affiche une image}
#'   \item{\code{\link{afficher_eigenfaces}}}{Affiche les eigenfaces}
#'   \item{\code{\link{reconstruire_image}}}{Reconstruit une image}
#'   \item{\code{\link{classifier_image}}}{Classifie une nouvelle image}
#' }
#'
#' @section Exemple d'utilisation:
#' \preformatted{
#' # Charger les images
#' data <- charger_images_dossier("Yale A/")
#' 
#' # Effectuer l'ACP
#' acp <- effectuer_acp(data$matrice, n_composantes = 50)
#' 
#' # Afficher les eigenfaces
#' afficher_eigenfaces(acp, n_faces = 9, hauteur = 320, largeur = 243)
#' 
#' # Visualiser la variance
#' plot_variance(acp)
#' }
#'
#' @docType package
#' @name jvrlc-package
#' @aliases jvrlc
"_PACKAGE"
