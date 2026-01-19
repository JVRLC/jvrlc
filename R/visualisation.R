#' Afficher une image
#'
#' Affiche une image représentée par une matrice ou un vecteur.
#'
#' @param image Image sous forme de matrice ou vecteur.
#' @param hauteur Hauteur de l'image (requis si image est un vecteur).
#' @param largeur Largeur de l'image (requis si image est un vecteur).
#' @param titre Titre à afficher au-dessus de l'image.
#' @param palette Palette de couleurs (par défaut: niveaux de gris).
#'
#' @return Invisible NULL.
#' @export
#'
#' @examples
#' \dontrun{
#' img <- charger_image("subject01.normal")
#' afficher_image(img, titre = "Sujet 01 - Normal")
#' }
afficher_image <- function(image, hauteur = NULL, largeur = NULL, titre = "", 
                           palette = grDevices::gray.colors(256, start = 0, end = 1)) {
  
  # Convertir en matrice si nécessaire
  if (is.vector(image)) {
    if (is.null(hauteur) || is.null(largeur)) {
      stop("hauteur et largeur sont requis pour un vecteur image")
    }
    image <- matrix(image, nrow = hauteur, ncol = largeur, byrow = TRUE)
  }
  
  # Normaliser les valeurs entre 0 et 1
  img_min <- min(image, na.rm = TRUE)
  img_max <- max(image, na.rm = TRUE)
  
  if (img_max > img_min) {
    image_norm <- (image - img_min) / (img_max - img_min)
  } else {
    image_norm <- image * 0
  }
  
  # Afficher l'image (rotation pour affichage correct)
  oldpar <- graphics::par(mar = c(1, 1, 2, 1))
  on.exit(graphics::par(oldpar))
  
  graphics::image(t(image_norm[nrow(image_norm):1, ]), 
                  col = palette, 
                  axes = FALSE, 
                  main = titre,
                  useRaster = TRUE)
  
  invisible(NULL)
}

#' Afficher les eigenfaces
#'
#' Affiche une grille des premières eigenfaces (composantes principales).
#'
#' @param acp Objet ACP retourné par \code{effectuer_acp}.
#' @param n_faces Nombre d'eigenfaces à afficher (par défaut 9).
#' @param hauteur Hauteur des images.
#' @param largeur Largeur des images.
#' @param inclure_moyenne Inclure l'image moyenne (par défaut TRUE).
#'
#' @return Invisible NULL.
#' @export
#'
#' @examples
#' \dontrun{
#' data <- charger_images_dossier("Yale A/")
#' acp <- effectuer_acp(data$matrice)
#' afficher_eigenfaces(acp, n_faces = 9, hauteur = 320, largeur = 243)
#' }
afficher_eigenfaces <- function(acp, n_faces = 9, hauteur = 320, largeur = 243, 
                                 inclure_moyenne = TRUE) {
  
  if (!inherits(acp, "jvrlc_acp")) {
    stop("L'argument doit être un objet de classe 'jvrlc_acp'")
  }
  
  # Calculer la grille
  n_total <- if (inclure_moyenne) n_faces + 1 else n_faces
  n_cols <- ceiling(sqrt(n_total))
  n_rows <- ceiling(n_total / n_cols)
  
  oldpar <- graphics::par(mfrow = c(n_rows, n_cols), mar = c(1, 1, 2, 1))
  on.exit(graphics::par(oldpar))
  
  palette <- grDevices::gray.colors(256, start = 0, end = 1)
  
  # Afficher l'image moyenne
  if (inclure_moyenne) {
    img_moyenne <- matrix(acp$moyenne, nrow = hauteur, ncol = largeur, byrow = TRUE)
    img_norm <- normaliser_image(img_moyenne)
    graphics::image(t(img_norm[nrow(img_norm):1, ]), 
                    col = palette, axes = FALSE, main = "Moyenne",
                    useRaster = TRUE)
  }
  
  # Afficher les eigenfaces
  n_disponibles <- min(n_faces, ncol(acp$composantes))
  
  for (i in 1:n_disponibles) {
    eigenface <- matrix(acp$composantes[, i], nrow = hauteur, ncol = largeur, byrow = TRUE)
    img_norm <- normaliser_image(eigenface)
    
    titre <- sprintf("CP%d (%.1f%%)", i, acp$variance_expliquee[i])
    graphics::image(t(img_norm[nrow(img_norm):1, ]), 
                    col = palette, axes = FALSE, main = titre,
                    useRaster = TRUE)
  }
  
  invisible(NULL)
}

#' Normaliser une image pour l'affichage
#'
#' @param image Matrice représentant l'image.
#'
#' @return Matrice normalisée entre 0 et 1.
#' @keywords internal
normaliser_image <- function(image) {
  img_min <- min(image, na.rm = TRUE)
  img_max <- max(image, na.rm = TRUE)
  
  if (img_max > img_min) {
    (image - img_min) / (img_max - img_min)
  } else {
    image * 0 + 0.5
  }
}

#' Afficher la variance expliquée
#'
#' Crée un graphique montrant la variance expliquée par chaque composante.
#'
#' @param acp Objet ACP retourné par \code{effectuer_acp}.
#' @param n_composantes Nombre de composantes à afficher (par défaut 20).
#'
#' @return Invisible NULL.
#' @export
#'
#' @examples
#' \dontrun{
#' acp <- effectuer_acp(matrice_images)
#' plot_variance(acp, n_composantes = 30)
#' }
plot_variance <- function(acp, n_composantes = 20) {
  if (!inherits(acp, "jvrlc_acp")) {
    stop("L'argument doit être un objet de classe 'jvrlc_acp'")
  }
  
  n <- min(n_composantes, length(acp$variance_expliquee))
  
  oldpar <- graphics::par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
  on.exit(graphics::par(oldpar))
  
  # Graphique 1: Variance individuelle
  graphics::barplot(acp$variance_expliquee[1:n],
                    names.arg = 1:n,
                    main = "Variance expliquée par composante",
                    xlab = "Composante principale",
                    ylab = "Variance expliquée (%)",
                    col = "steelblue")
  
  # Graphique 2: Variance cumulée
  graphics::plot(1:n, acp$variance_cumulee[1:n],
                 type = "b",
                 main = "Variance cumulée",
                 xlab = "Nombre de composantes",
                 ylab = "Variance cumulée (%)",
                 col = "darkred",
                 pch = 19)
  graphics::abline(h = 90, lty = 2, col = "gray50")
  graphics::abline(h = 95, lty = 2, col = "gray50")
  graphics::text(n * 0.8, 91, "90%", col = "gray50")
  graphics::text(n * 0.8, 96, "95%", col = "gray50")
  
  invisible(NULL)
}
