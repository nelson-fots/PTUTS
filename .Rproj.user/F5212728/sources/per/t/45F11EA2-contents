#' Résumé d’une série temporelle
#'
#' Fournit des statistiques descriptives simples.
#'
#' @param x vecteur numérique ou ts
#'
#' @return liste de statistiques
#'
#' @examples
#' x <- ts(rnorm(50))
#' resume_serie(x)
resume_serie <- function(x) {
   xx <- as.numeric(x)
   list(
     moyenne = mean(xx),
     ecart_type = sd(xx),
     min = min(xx),
     max = max(xx)
     )
  }
