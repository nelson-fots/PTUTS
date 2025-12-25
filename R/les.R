
#' Lissage exponentiel simple (SES)
#'
#' Lisse une série à l’aide d’un paramètre alpha.
#'
#' @param x vecteur numérique ou ts
#' @param alpha paramètre de lissage (0 < alpha <= 1)
#'
#' @return liste contenant la série lissée
#'
#' @examples
#' x <- c(10, 12, 13, 12, 14)
#' lissage_exp_simple(x, alpha = 0.3)
lissage_exp_simple <- function(x, alpha) {
    stopifnot(alpha > 0, alpha <= 1)
    is_ts <- inherits(x, "ts")
    xx <- as.numeric(x)
    n <- length(xx)
     s <- numeric(n)
    s[1] <- xx[1]

      for (i in 2:n) {
          s[i] <- alpha * xx[i] + (1 - alpha) * s[i - 1]
        }

      if (is_ts) s <- ts(s, start = start(x), frequency = frequency(x))
      list(fitted = s, alpha = alpha)
    }
