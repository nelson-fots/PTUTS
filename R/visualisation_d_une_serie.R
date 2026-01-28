#' Visualisation simple d’une serie temporelle
#'
#' Trace la serie avec un titre automatique.
#' @details
#' La visualisation est une étape clé dans l'analyse des séries chronologiques.
#' Elle permet d'observer l'évolution des données dans le temps et d'identifier
#' visuellement des phénomènes tels que la tendance, la saisonnalité ou les ruptures.
#'
#' Une représentation graphique claire facilite l'interprétation des résultats
#' et guide le choix des méthodes d'analyse à appliquer par la suite.
#'
#' @param x objet ts
#'
#'@export
#'
#' @examples
#' x <- ts(rnorm(30), frequency = 12)
#' visualisation_serie(x)
visualisation_serie <- function(x) {
   plot(x, main = "Serie temporelle", ylab = "Valeurs")
  }
