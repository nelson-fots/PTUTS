#' Intervalle de confiance pour la variance
#'
#' Calcule un intervalle de confiance pour la variance d'un echantillon
#' en supposant une distribution normale.
#'
#' @param x Numeric vector.
#' @param conf.level Niveau de confiance (par defaut 0.95).
#'
#' @return Liste contenant :
#' \describe{
#'   \item{variance}{variance empirique}
#'   \item{conf.int}{intervalle de confiance}
#'   \item{n}{taille de l'echantillon}
#' }
#'
#' @export
#' @importFrom stats var qchisq
#'
#' @examples
#' x <- stats::rnorm(25, sd = 2)
#' ic_variance(x)
ic_variance <- function(x, conf.level = 0.95) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 2) stop("Il faut au moins 2 observations.")

  s2 <- stats::var(x)
  df <- n - 1
  alpha <- 1 - conf.level

  lower <- df * s2 / stats::qchisq(1 - alpha / 2, df)
  upper <- df * s2 / stats::qchisq(alpha / 2, df)

  list(
    variance = s2,
    conf.int = c(lower, upper),
    n = n,
    conf.level = conf.level
  )
}
