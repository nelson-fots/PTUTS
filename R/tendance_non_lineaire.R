#' Tendance non linéaire (exponentielle simple)
#'
#' Ajuste une tendance exponentielle simple sur une série.
#'
#' @param x vecteur numérique ou ts (valeurs positives conseillées)
#'
#' @return liste avec paramètres et tendance estimée
#'
#'@export
#'@importFrom stats nls predict ts start frequency coef
#'
#' @examples
#' t <- 1:30
#' x <- 2 * exp(0.05 * t) + rnorm(30)
#' tendance_non_lineaire(x)
tendance_non_lineaire <- function(x) {
    is_ts <- inherits(x, "ts")
    xx <- as.numeric(x)
    t <- seq_along(xx)
    mod <- nls(xx ~ a * exp(b * t), start = list(a = xx[1], b = 0.01))
    trend <- predict(mod)
    if (is_ts) trend <- ts(trend, start = start(x), frequency = frequency(x))
    list(a = coef(mod)[1], b = coef(mod)[2], trend = trend)
}
