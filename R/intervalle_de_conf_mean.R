#' Intervalle de confiance pour la moyenne
#'
#' Calcule un intervalle de confiance pour la moyenne d'un echantillon
#' en utilisant la loi de Student.
#'
#' @param x Numeric vector.
#' @param conf.level Niveau de confiance (par defaut 0.95).
#'
#' @return Liste contenant :
#' \describe{
#'   \item{mean}{moyenne de l'echantillon}
#'   \item{conf.int}{intervalle de confiance}
#'   \item{n}{taille de l'echantillon}
#' }
#'
#' @export
#' @importFrom stats qt sd
#'
#' @examples
#' x <- stats::rnorm(30, mean = 10, sd = 2)
#' ic_mean(x)
ic_mean <- function(x, conf.level = 0.95) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 2) stop("Il faut au moins 2 observations.")

  m <- mean(x)
  s <- stats::sd(x) # Utilisation du prefixe pour etre sur
  alpha <- 1 - conf.level
  t_crit <- stats::qt(1 - alpha / 2, df = n - 1)
  marge <- t_crit * s / sqrt(n)

  list(
    mean = m,
    conf.int = c(m - marge, m + marge),
    n = n,
    conf.level = conf.level
  )
}
