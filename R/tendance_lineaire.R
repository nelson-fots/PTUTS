#' Tendance linéaire
#'
#' Ajuste une droite de régression sur une série temporelle.
#'
#' @details
#' La tendance représente l'évolution globale et de long terme d'une série
#' chronologique. Elle peut être croissante, décroissante ou stable.
#'
#' Selon les données, la tendance peut être modélisée par une relation linéaire
#' ou par une relation non linéaire plus flexible.
#'
#' Identifier la tendance permet de mieux comprendre la dynamique générale
#' de la série et constitue une étape importante dans la modélisation.
#' @param x vecteur numérique ou ts
#'
#' @return liste avec pente et tendance estimée
#'
#'@export
#'
#' @examples
#' x <- ts(1:20 + rnorm(20))
#' tendance_lineaire(x)
tendance_lineaire <- function(x) {
    is_ts <- inherits(x, "ts")
    xx <- as.numeric(x)
    t <- seq_along(xx)
    mod <- lm(xx ~ t)
    trend <- predict(mod)
    if (is_ts) trend <- ts(trend, start = start(x), frequency = frequency(x))
    list(trend = trend, slope = coef(mod)[2])
  }
