#' Test de comparaison de moyennes (2 échantillons)
#'
#' Compare les moyennes de deux échantillons indépendants.
#'
#' @details
#' Le test de comparaison de moyennes permet d'évaluer si deux échantillons
#' indépendants ont des moyennes significativement différentes.
#'
#' Il est couramment utilisé pour comparer deux groupes, par exemple avant/après
#' une intervention ou entre un groupe témoin et un groupe test.
#'
#' Selon les hypothèses d'égalité des variances, différentes versions du test
#' peuvent être appliquées.
#'
#' L'interprétation repose sur la p-valeur, qui indique si la différence observée
#' entre les moyennes est statistiquement significative.
#' @param x,y vecteurs numériques
#' @param var.equal TRUE si variances supposées égales
#' @param alternative "two.sided", "less" ou "greater"
#'
#' @return objet htest (t.test)
#'
#'@export
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
