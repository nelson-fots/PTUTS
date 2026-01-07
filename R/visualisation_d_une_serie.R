#' Visualisation simple d’une serie temporelle
#'
#' Trace la serie avec un titre automatique.
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
