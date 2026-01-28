#' Interpretation automatique d'un test
#'
#' Fournit une conclusion simple a partir de la p-value.
#'
#' @details
#' L'interprétation automatique vise à fournir une lecture simplifiée des résultats
#' d'un test statistique à partir de la p-valeur et du seuil de significativité.
#'
#' Elle permet de traduire les résultats numériques en conclusions compréhensibles
#' pour des étudiants ou des utilisateurs débutants.
#'
#' Cette approche favorise l'autonomie dans l'analyse statistique tout en
#' rappelant que l'interprétation doit toujours être replacée dans le contexte
#' de l'étude.
#'
#' @param p_value p-value du test (numerique)
#' @param alpha niveau de risque, par defaut 0.05
#'
#' @return Une chaine de caracteres indiquant le resultat du test.
#' @export
#'
#' @examples
#' interpretation_test(0.03)
interpretation_test <- function(p_value, alpha = 0.05) {
  if (p_value < alpha) {
    return("Rejet de l'hypothese nulle (H0)")
  } else {
    return("On ne rejette pas l'hypothese nulle (H0)")
  }
}
