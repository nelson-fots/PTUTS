#' Test du Khi-deux
#'
#' Test d’indépendance ou d’adéquation.
#'
#' @details
#' Le test du khi-deux est utilisé pour analyser la relation entre deux variables
#' qualitatives à partir d'un tableau de contingence.
#'
#' Il permet de tester l'indépendance entre les variables ou l'adéquation
#' entre des fréquences observées et des fréquences théoriques.
#'
#' Ce test est largement utilisé dans les études descriptives et exploratoires
#' impliquant des données catégorielles.
#'
#' @param x table de contingence ou vecteur de comptes
#' @param p probabilités théoriques (optionnel)
#'
#' @return objet htest (chisq.test)
#'
#'@export
#'
#' @examples
#' tab <- matrix(c(20, 10, 15, 25), nrow = 2)
#' test_khi_deux(tab)
test_khi_deux <- function(x, p = NULL) {
   stats::chisq.test(x, p = p)
  }
