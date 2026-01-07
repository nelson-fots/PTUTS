#' Test de probabilité (binomial exact)
#'
#' Teste si une proportion est égale à une valeur donnée.
#'
#' @param x nombre de succès
#' @param n nombre d'essais
#' @param p0 probabilité sous H0
#'
#' @return objet htest (binom.test)
#'
#'@export
#'
#' @examples
#' test_probabilite(35, 50, p0 = 0.5)
test_probabilite <- function(x, n, p0 = 0.5) {
   stats::binom.test(x, n, p = p0)
  }
