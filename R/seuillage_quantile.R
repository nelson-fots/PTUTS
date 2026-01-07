
#' Seuillage par quantiles
#'
#' Identifie ou corrige les valeurs extrêmes d’une série.
#'
#' @param x vecteur numérique
#' @param q_low quantile bas
#' @param q_high quantile haut
#'
#' @return liste avec bornes et indicateur d’extrêmes
#'@export
#'
#' @examples
#' x <- c(rnorm(100), 10)
#' seuillage_quantile(x)
seuillage_quantile <- function(x, q_low = 0.05, q_high = 0.95) {
    bas <- quantile(x, q_low)
    haut <- quantile(x, q_high)
    outlier <- (x < bas) | (x > haut)
    list(borne_basse = bas, borne_haute = haut, outlier = outlier)
  }

