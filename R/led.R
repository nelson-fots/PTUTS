#' Lissage exponentiel double (Holt)
#'
#' Modélise le niveau et la tendance d’une série sans saisonnalité.
#'
#' @param x vecteur numérique ou ts
#' @param alpha lissage du niveau
#' @param beta lissage de la tendance
#'
#' @return liste avec niveau, tendance et série ajustée
#'
#'@export
#'
#' @examples
#' x <- c(5, 6, 8, 9, 11)
#' lissage_exp_double(x, alpha = 0.4, beta = 0.2)
lissage_exp_double <- function(x, alpha, beta) {
    stopifnot(alpha > 0, alpha <= 1, beta > 0, beta <= 1)
    is_ts <- inherits(x, "ts")
    xx <- as.numeric(x)
    n <- length(xx)

      L <- T <- fitted <- numeric(n)
      L[1] <- xx[1]
      T[1] <- xx[2] - xx[1]

        for (i in 2:n) {
            L[i] <- alpha * xx[i] + (1 - alpha) * (L[i - 1] + T[i - 1])
            T[i] <- beta * (L[i] - L[i - 1]) + (1 - beta) * T[i - 1]
            fitted[i] <- L[i] + T[i]
          }

        if (is_ts) {
            fitted <- ts(fitted, start = start(x), frequency = frequency(x))
          }
      list(level = L, trend = T, fitted = fitted)
  }

