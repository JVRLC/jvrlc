test_that("distance_faces calcule correctement la distance euclidienne", {
  v1 <- c(0, 0, 0)
  v2 <- c(3, 4, 0)
  
  dist <- distance_faces(v1, v2, methode = "euclidienne")
  expect_equal(dist, 5)
})

test_that("distance_faces calcule correctement la distance manhattan", {
  v1 <- c(0, 0, 0)
  v2 <- c(1, 2, 3)
  
  dist <- distance_faces(v1, v2, methode = "manhattan")
  expect_equal(dist, 6)
})

test_that("distance_faces respecte n_composantes", {
  v1 <- c(0, 0, 0, 100)
  v2 <- c(3, 4, 0, 200)
  
  # Avec 3 composantes, la 4ème est ignorée
  dist <- distance_faces(v1, v2, n_composantes = 3)
  expect_equal(dist, 5)
})
