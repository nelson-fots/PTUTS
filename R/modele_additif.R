  #' Décomposition additive
  #'
  #' Sépare une série en tendance, saisonnalité et résidu.
  #'
  #' @param x objet ts (fréquence >= 2)
  #'
  #' @return objet decompose
  #'
  #' @examples
  #' x <- ts(rnorm(120) + sin(2*pi*(1:120)/12), frequency = 12)
  #' modele_additif(x)
  modele_additif <- function(x) {
      stopifnot(inherits(x, "ts"))
      stats::decompose(x, type = "additive")
    }
