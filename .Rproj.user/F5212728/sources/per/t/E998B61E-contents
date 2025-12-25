
#' Moyenne mobile simple
#'
#' Calcule une moyenne mobile afin de lisser les fluctuations de court terme
#' d’une série temporelle.
#'
#' @param x vecteur numérique ou objet ts
#' @param k taille de la fenêtre (entier >= 2)
#' @param centre logique, TRUE pour moyenne centrée
#'
#' @return vecteur numérique ou objet ts de même longueur
#'
#' @examples
#' x <- 1:12
#' moyenne_mobile(x, k = 3)
#'
#' ts1 <- ts(rnorm(24), frequency = 12)
#' moyenne_mobile(ts1, k = 5)
moyenne_mobile <- function(x, k, centre = TRUE) {
    stopifnot(k >= 2)
    is_ts <- inherits(x, "ts")
    xx <- as.numeric(x)
    n <- length(xx)
    res <- rep(NA, n)

      if (centre) {
          h <- floor(k / 2)
          for (i in (h + 1):(n - h)) {
              res[i] <- mean(xx[(i - h):(i + h)])
            }
        } else {
            for (i in k:n) {
                res[i] <- mean(xx[(i - k + 1):i])
              }
          }

      if (is_ts) res <- ts(res, start = start(x), frequency = frequency(x))
      res
    }
