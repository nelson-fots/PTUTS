#' Interprétation automatique d’un test
#'
#' Fournit une conclusion simple à partir de la p-value.
#'
#' @param p_value p-value du test
#' @param alpha niveau de risque (par défaut 5%)
#'
#' @return caractère
#'
#' @examples
#' interpretation_test(0.03)
interpretation_test <- function(p_value, alpha = 0.05) {
   if (p_value < alpha) {
     "Rejet de l'hypothèse nulle (H0)"
     } else {
       "On ne rejette pas l'hypothèse nulle (H0)"
       }
  }
