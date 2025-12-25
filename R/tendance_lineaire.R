#' Tendance linéaire
#'
#' Ajuste une droite de régression sur une série temporelle.
#'
#' @param x vecteur numérique ou ts
#'
#' @return liste avec pente et tendance estimée
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
