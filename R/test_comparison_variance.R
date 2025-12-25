#' Test de comparaison de variances
#'
#' Compare les variances de deux échantillons.
#'
#' @param x,y vecteurs numériques
#'
#' @return objet htest (var.test)
#'
#' @examples
#' x <- rnorm(20, sd = 2)
#' y <- rnorm(25, sd = 3)
#' test_variance(x, y)
test_variance <- function(x, y) {
   stats::var.test(x, y)
  }
