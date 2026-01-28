#' Test de normalité (Shapiro-Wilk)
#'
#' Vérifie si un échantillon suit une loi normale.
#'
#' @details
#' Le test de normalité permet de vérifier si un échantillon peut être considéré
#' comme issu d'une loi normale.
#'
#' Cette étape est importante car de nombreux tests statistiques reposent
#' sur l'hypothèse de normalité des données.
#'
#' En cas de rejet de l'hypothèse de normalité, il peut être préférable d'utiliser
#' des méthodes non paramétriques.
#'
#' @param x vecteur numérique
#'
#' @return objet htest (shapiro.test)
#'
#'@export
#'
#' @examples
#' x <- rnorm(30)
#' test_normalite(x)
test_normalite <- function(x) {
   stats::shapiro.test(x)
  }
