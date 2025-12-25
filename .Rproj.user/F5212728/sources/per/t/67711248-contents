#' Test de comparaison de moyennes (2 échantillons)
#'
#' Compare les moyennes de deux échantillons indépendants.
#'
#' @param x,y vecteurs numériques
#' @param var.equal TRUE si variances supposées égales
#' @param alternative "two.sided", "less" ou "greater"
#'
#' @return objet htest (t.test)
#'
#' @examples
#' x <- rnorm(20, mean = 5)
#' y <- rnorm(25, mean = 6)
#' test_comparaison_moyenne(x, y)
test_comparaison_moyenne <- function(x, y, var.equal = FALSE,
                                       alternative = c("two.sided", "less", "greater")) {
   alternative <- match.arg(alternative)
   stats::t.test(x, y, var.equal = var.equal, alternative = alternative)
  }
