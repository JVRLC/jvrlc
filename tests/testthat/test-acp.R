test_that("effectuer_acp fonctionne avec une matrice simple", {
  # Créer une matrice de test
  set.seed(42)
  matrice_test <- matrix(rnorm(100 * 50), nrow = 10, ncol = 500)
  
  # Effectuer l'ACP
  acp <- effectuer_acp(matrice_test, n_composantes = 5)
  
  # Vérifications

expect_s3_class(acp, "jvrlc_acp")
  expect_equal(length(acp$valeurs_propres), 5)
  expect_equal(ncol(acp$composantes), 5)
  expect_equal(length(acp$moyenne), 500)
  expect_true(all(acp$variance_expliquee >= 0))
  expect_true(all(acp$variance_cumulee <= 100))
})

test_that("la variance cumulée est croissante", {
  set.seed(42)
  matrice_test <- matrix(rnorm(500), nrow = 10, ncol = 50)
  acp <- effectuer_acp(matrice_test)
  
  # La variance cumulée doit être croissante
  expect_true(all(diff(acp$variance_cumulee) >= 0))
})

test_that("reconstruire_image fonctionne", {
  set.seed(42)
  matrice_test <- matrix(rnorm(500), nrow = 10, ncol = 50)
  acp <- effectuer_acp(matrice_test)
  
  # Reconstruire avec toutes les composantes
  reconstruction <- reconstruire_image(acp, 1)
  
  expect_equal(length(reconstruction), 50)
})
