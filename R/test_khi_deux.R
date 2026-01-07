#' Test du Khi-deux
#'
#' Test d’indépendance ou d’adéquation.
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
