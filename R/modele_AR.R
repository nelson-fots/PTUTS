#' Modèle auto-régressif AR(p)
#'
#' Ajuste un modèle AR simple sur une série temporelle.
#'
#' @param x vecteur numérique ou ts
#' @param p ordre du modèle (entier)
#'
#' @return liste avec coefficients et série ajustée
#'
#'@export
#'
#' @examples
#' x <- arima.sim(list(ar = 0.6), n = 100)
#' modele_auto_regressif(x, p = 1)
modele_auto_regressif <- function(x, p = 1) {
    stopifnot(p >= 1)
    is_ts <- inherits(x, "ts")
    xx <- as.numeric(x)
    mod <- stats::ar(xx, order.max = p, aic = FALSE)
    fitted <- xx - mod$resid
    if (is_ts) fitted <- ts(fitted, start = start(x), frequency = frequency(x))
    list(order = mod$order, coef = mod$ar, fitted = fitted)
  }
