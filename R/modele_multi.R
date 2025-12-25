#' Décomposition multiplicative
#'
#' Décompose une série en tendance, saisonnalité et résidu (forme multiplicative).
#'
#' @param x objet ts strictement positif
#'
#' @return objet decompose
#'
#' @examples
#' x <- ts(abs(rnorm(120)) + 10, frequency = 12)
#' modele_multiplicatif(x)
modele_multiplicatif <- function(x) {
    stopifnot(inherits(x, "ts"))
    stats::decompose(x, type = "multiplicative")
  }
