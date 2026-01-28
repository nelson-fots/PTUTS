#' Test de moyenne (1 échantillon)
#'
#' Teste si la moyenne d’un échantillon est égale à une valeur donnée.
#'
#' @details
#' Le test de moyenne permet de vérifier si la moyenne d'un échantillon est
#' statistiquement différente d'une valeur de référence donnée.
#'
#' Ce test est fréquemment utilisé pour comparer une moyenne observée à une
#' valeur théorique ou attendue.
#'
#' Selon les hypothèses sur la distribution des données et la taille de l'échantillon,
#' le test peut être basé sur la loi normale ou la loi de Student.
#'
#' Le résultat du test repose sur la p-valeur, qui permet de décider si la différence
#' observée est significative ou non au seuil choisi.

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
