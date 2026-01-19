#' Reconstruire une image à partir de ses composantes principales
#'
#' Cette fonction reconstruit une image en utilisant un nombre limité de composantes principales.
#'
#' @param acp Objet ACP retourné par \code{effectuer_acp}.
#' @param projection Vecteur de projection de l'image (ou index de l'image originale).
#' @param n_composantes Nombre de composantes à utiliser pour la reconstruction.
#'
#' @return Vecteur représentant l'image reconstruite.
#' @export
#'
#' @examples
#' \dontrun{
#' acp <- effectuer_acp(matrice_images)
#' img_reconstruite <- reconstruire_image(acp, 1, n_composantes = 50)
#' }
reconstruire_image <- function(acp, projection, n_composantes = NULL) {
  if (!inherits(acp, "jvrlc_acp")) {
    stop("L'argument acp doit être un objet de classe 'jvrlc_acp'")
  }
  
  # Si projection est un entier, récupérer la projection correspondante
  if (length(projection) == 1 && is.numeric(projection)) {
    if (projection < 1 || projection > nrow(acp$projections)) {
      stop("Index de projection invalide")
    }
    projection <- acp$projections[projection, ]
  }
  
  # Limiter le nombre de composantes
  if (is.null(n_composantes)) {
    n_composantes <- length(projection)
  }
  n_composantes <- min(n_composantes, ncol(acp$composantes), length(projection))
  
  # Reconstruction
  reconstruction <- acp$moyenne + 
    as.vector(acp$composantes[, 1:n_composantes, drop = FALSE] %*% 
                projection[1:n_composantes])
  
  return(reconstruction)
}

#' Comparer les reconstructions avec différents nombres de composantes
#'
#' Affiche une grille comparant la reconstruction d'une image avec différents nombres de composantes.
#'
#' @param acp Objet ACP retourné par \code{effectuer_acp}.
#' @param index Index de l'image à reconstruire.
#' @param n_composantes_liste Vecteur des nombres de composantes à tester.
#' @param hauteur Hauteur des images.
#' @param largeur Largeur des images.
#' @param image_originale Image originale pour comparaison (optionnel).
#'
#' @return Invisible NULL.
#' @export
#'
#' @examples
#' \dontrun{
#' comparer_reconstructions(acp, 1, c(5, 10, 25, 50, 100))
#' }
comparer_reconstructions <- function(acp, index, n_composantes_liste = c(5, 10, 25, 50, 100),
                                      hauteur = 320, largeur = 243, image_originale = NULL) {
  
  n_images <- length(n_composantes_liste) + 1  # +1 pour l'originale ou moyenne
  n_cols <- ceiling(sqrt(n_images))
  n_rows <- ceiling(n_images / n_cols)
  
  oldpar <- graphics::par(mfrow = c(n_rows, n_cols), mar = c(1, 1, 2, 1))
  on.exit(graphics::par(oldpar))
  
  palette <- grDevices::gray.colors(256, start = 0, end = 1)
  
  # Afficher l'image de référence
  if (!is.null(image_originale)) {
    if (is.vector(image_originale)) {
      img_ref <- matrix(image_originale, nrow = hauteur, ncol = largeur, byrow = TRUE)
    } else {
      img_ref <- image_originale
    }
    img_norm <- normaliser_image(img_ref)
    graphics::image(t(img_norm[nrow(img_norm):1, ]), 
                    col = palette, axes = FALSE, main = "Originale",
                    useRaster = TRUE)
  } else {
    # Afficher la moyenne
    img_moyenne <- matrix(acp$moyenne, nrow = hauteur, ncol = largeur, byrow = TRUE)
    img_norm <- normaliser_image(img_moyenne)
    graphics::image(t(img_norm[nrow(img_norm):1, ]), 
                    col = palette, axes = FALSE, main = "Moyenne",
                    useRaster = TRUE)
  }
  
  # Afficher les reconstructions
  for (n_cp in n_composantes_liste) {
    n_cp_effectif <- min(n_cp, ncol(acp$composantes))
    reconstruction <- reconstruire_image(acp, index, n_composantes = n_cp_effectif)
    img_rec <- matrix(reconstruction, nrow = hauteur, ncol = largeur, byrow = TRUE)
    img_norm <- normaliser_image(img_rec)
    
    titre <- sprintf("%d CP", n_cp_effectif)
    graphics::image(t(img_norm[nrow(img_norm):1, ]), 
                    col = palette, axes = FALSE, main = titre,
                    useRaster = TRUE)
  }
  
  invisible(NULL)
}

#' Calculer l'erreur de reconstruction
#'
#' Calcule l'erreur quadratique moyenne entre l'image originale et reconstruite.
#'
#' @param originale Vecteur de l'image originale.
#' @param reconstruite Vecteur de l'image reconstruite.
#'
#' @return Erreur quadratique moyenne (RMSE).
#' @export
erreur_reconstruction <- function(originale, reconstruite) {
  sqrt(mean((originale - reconstruite)^2))
}
