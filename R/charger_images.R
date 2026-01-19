#' Charger une image en niveaux de gris
#'
#' Cette fonction charge une image depuis un fichier et la convertit en matrice/vecteur.
#' Utilise le package magick pour supporter de nombreux formats (GIF, PNG, JPG, PGM, etc.).
#'
#' @param chemin Chemin vers le fichier image.
#' @param as_vector Retourner l'image comme vecteur (TRUE) ou matrice (FALSE). Par défaut TRUE.
#'
#' @return Un vecteur ou une matrice représentant l'image en niveaux de gris.
#' @export
#'
#' @examples
#' \dontrun{
#' img <- charger_image("subject01.normal")
#' img_mat <- charger_image("subject01.normal", as_vector = FALSE)
#' }
charger_image <- function(chemin, as_vector = TRUE) {
  if (!file.exists(chemin)) {
    stop("Le fichier n'existe pas: ", chemin)
  }
  
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Le package 'magick' est requis. Installez-le avec: install.packages('magick')")
  }
  
  # Lire l'image avec magick
  img <- magick::image_read(chemin)
  
  # Convertir en niveaux de gris
  img_gray <- magick::image_convert(img, colorspace = "gray")
  
  # Extraire les données (format: channel x width x height)
  img_data <- magick::image_data(img_gray)
  
  # Convertir en matrice d'entiers (width x height)
  img_matrix <- as.integer(img_data[1,,])
  
  if (as_vector) {
    return(as.vector(img_matrix))
  } else {
    return(img_matrix)
  }
}

#' Obtenir les dimensions d'une image
#'
#' @param chemin Chemin vers le fichier image.
#'
#' @return Un vecteur nommé avec largeur et hauteur.
#' @export
#'
#' @examples
#' \dontrun{
#' dims <- dimensions_image("subject01.normal")
#' }
dimensions_image <- function(chemin) {
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Le package 'magick' est requis.")
  }
  
  img <- magick::image_read(chemin)
  info <- magick::image_info(img)
  
  c(largeur = info$width, hauteur = info$height)
}

#' Charger toutes les images d'un dossier
#'
#' Cette fonction charge toutes les images d'un dossier et les retourne
#' sous forme de matrice où chaque ligne représente une image aplatie.
#'
#' @param dossier Chemin vers le dossier contenant les images.
#' @param pattern Pattern regex pour filtrer les fichiers (par défaut "subject").
#'
#' @return Une liste contenant:
#'   \itemize{
#'     \item \code{matrice}: Matrice des images (une image par ligne).
#'     \item \code{noms}: Noms des fichiers.
#'     \item \code{dimensions}: Dimensions originales des images (largeur, hauteur).
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' data <- charger_images_dossier("Yale A/", pattern = "subject")
#' }
charger_images_dossier <- function(dossier, pattern = "subject") {
  if (!dir.exists(dossier)) {
    stop("Le dossier n'existe pas: ", dossier)
  }
  
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Le package 'magick' est requis. Installez-le avec: install.packages('magick')")
  }
  
  # Lister les fichiers
  fichiers <- list.files(dossier, pattern = pattern, full.names = TRUE)
  
  if (length(fichiers) == 0) {
    stop("Aucun fichier trouvé dans le dossier avec le pattern: ", pattern)
  }
  
  message("Chargement de ", length(fichiers), " images...")
  
  # Obtenir les dimensions de la première image
  dims <- dimensions_image(fichiers[1])
  
  # Charger toutes les images
  images <- lapply(fichiers, function(f) {
    tryCatch({
      charger_image(f, as_vector = TRUE)
    }, error = function(e) {
      warning("Erreur lors du chargement de ", f, ": ", e$message)
      NULL
    })
  })
  
  # Filtrer les images valides
  valides <- !sapply(images, is.null)
  images <- images[valides]
  fichiers <- fichiers[valides]
  
  if (length(images) == 0) {
    stop("Aucune image n'a pu être chargée.")
  }
  
  # Créer la matrice
  matrice <- do.call(rbind, images)
  
  message("Images chargées avec succès: ", nrow(matrice))
  
  list(
    matrice = matrice,
    noms = basename(fichiers),
    dimensions = dims
  )
}
