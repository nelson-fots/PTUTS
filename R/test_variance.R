#' Test de variance (1 échantillon)
#'
#' Teste si la variance d’un échantillon est égale à une valeur donnée.
#'
#' @details
#' Le test de variance permet de vérifier si la dispersion d'un échantillon
#' est conforme à une valeur de référence ou de comparer les variances de deux
#' échantillons.
#'
#' Il est utile pour analyser la stabilité ou la variabilité des données.
#'
#' Ce test repose généralement sur des hypothèses de normalité des données.
#' Une variance significativement différente peut indiquer une hétérogénéité
#' importante dans les observations.
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
