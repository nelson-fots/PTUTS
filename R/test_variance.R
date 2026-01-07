#' Test de variance (1 échantillon)
#'
#' Teste si la variance d’un échantillon est égale à une valeur donnée.
#'
#' @param x vecteur numérique
#' @param sigma0 écart-type sous H0
#'
#' @return liste avec statistique et p-value
#'
#'@export
#'
#' @examples
#' x <- rnorm(25, sd = 2)
#' test_variance_simple(x, sigma0 = 2)
test_variance_simple <- function(x, sigma0) {
   n <- length(x)
   s2 <- var(x)
   chi2 <- (n - 1) * s2 / sigma0^2
   pval <- 2 * min(pchisq(chi2, n - 1), 1 - pchisq(chi2, n - 1))
   list(statistic = chi2, p_value = pval)
  }
