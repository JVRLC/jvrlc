# jvrlc 📸

Un package R pour l'analyse d'images faciales et la reconnaissance par eigenfaces.

## Installation

```r
# Installation depuis le dossier local
install.packages("path/to/jvrlc", repos = NULL, type = "source")

# Ou avec devtools
devtools::install_local("path/to/jvrlc")
```

## Fonctionnalités

- 📷 **Chargement d'images** : Support des formats PGM et fichiers bruts
- 🔬 **Analyse en Composantes Principales** : Extraction des eigenfaces
- 📊 **Visualisation** : Affichage des images, eigenfaces et graphiques de variance
- 🔄 **Reconstruction** : Reconstruction d'images avec réduction de dimension
- 🎯 **Classification** : Reconnaissance faciale par k-NN

## Utilisation rapide

```r
library(jvrlc)

# Charger les images du dataset Yale
data <- charger_images_dossier("Yale A/", pattern = "subject")

# Effectuer l'ACP
acp <- effectuer_acp(data$matrice, n_composantes = 50)

# Voir le résumé
summary(acp)

# Afficher les eigenfaces
afficher_eigenfaces(acp, n_faces = 9, hauteur = 320, largeur = 243)

# Visualiser la variance expliquée
plot_variance(acp, n_composantes = 20)

# Reconstruire une image
img_rec <- reconstruire_image(acp, 1, n_composantes = 25)
afficher_image(img_rec, hauteur = 320, largeur = 243, titre = "Reconstruction 25 CP")

# Comparer les reconstructions
comparer_reconstructions(acp, 1, c(5, 10, 25, 50, 100), hauteur = 320, largeur = 243)
```

## Classification

```r
# Extraire les labels des noms de fichiers (ex: "subject01" depuis "subject01.happy")
labels <- gsub("\\..*", "", data$noms)

# Évaluer par validation croisée leave-one-out
resultats <- evaluer_classification(acp, labels, k = 1, n_composantes = 50)
print(resultats$confusion)

# Classifier une nouvelle image
nouvelle_img <- charger_image("nouvelle_image.pgm")
resultat <- classifier_image(acp, as.vector(nouvelle_img), labels, k = 3)
print(paste("Prédiction:", resultat$prediction))
```

## Structure du package

```
jvrlc/
├── R/
│   ├── charger_images.R   # Fonctions de chargement
│   ├── acp.R              # Analyse en composantes principales
│   ├── visualisation.R    # Affichage et graphiques
│   ├── reconstruction.R   # Reconstruction d'images
│   ├── classification.R   # Classification et distance
│   └── jvrlc-package.R    # Documentation du package
├── man/                   # Documentation (générée)
├── tests/                 # Tests unitaires
├── DESCRIPTION
├── NAMESPACE
├── LICENSE
└── README.md
```

## Auteur

**Serigne MBAYE**

## Licence

MIT
