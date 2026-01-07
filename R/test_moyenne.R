#' Test de moyenne (1 échantillon)
#'
#' Teste si la moyenne d’un échantillon est égale à une valeur donnée.
#'
#' @param x vecteur numérique
#' @param mu0 moyenne sous l’hypothèse nulle H0
#' @param alternative "two.sided", "less" ou "greater"
#'
#' @return objet htest (t.test)
#'
#'@export
#'
#' @examples
#' x <- rnorm(30, mean = 10)
#' test_moyenne(x, mu0 = 10)
test_moyenne <- function(x, mu0 = 0,
                           alternative = c("two.sided", "less", "greater")) {
   alternative <- match.arg(alternative)
   stats::t.test(x, mu = mu0, alternative = alternative)
  }
