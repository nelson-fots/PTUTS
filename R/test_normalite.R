#' Test de normalité (Shapiro-Wilk)
#'
#' Vérifie si un échantillon suit une loi normale.
#'
#' @param x vecteur numérique
#'
#' @return objet htest (shapiro.test)
#'
#' @examples
#' x <- rnorm(30)
#' test_normalite(x)
test_normalite <- function(x) {
   stats::shapiro.test(x)
  }
