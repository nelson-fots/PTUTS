  #' Test de Chow (cassure structurelle)
  #'
  #' Teste un changement de relation linéaire à une date donnée.
  #'
  #' @param y variable expliquée
  #' @param x variable explicative
  #' @param breakpoint position de la cassure
  #'
  #' @return liste avec statistique F et p-value
  #'
  #' @examples
  #' n <- 60
  #' x <- 1:n
  #' y <- c(1 + 0.2*x[1:30], 1 + 0.8*x[31:60]) + rnorm(n)
  #' test_chow_cassure(y, x, breakpoint = 30)
  test_chow_cassure <- function(y, x, breakpoint) {
      n <- length(y)
      mod1 <- lm(y[1:breakpoint] ~ x[1:breakpoint])
      mod2 <- lm(y[(breakpoint+1):n] ~ x[(breakpoint+1):n])
      mod_all <- lm(y ~ x)

        rss_all <- sum(resid(mod_all)^2)
        rss_1 <- sum(resid(mod1)^2)
        rss_2 <- sum(resid(mod2)^2)

          k <- length(coef(mod_all))
          Fstat <- ((rss_all - (rss_1 + rss_2)) / k) / ((rss_1 + rss_2) / (n - 2*k))
          pval <- 1 - pf(Fstat, k, n - 2*k)

            list(F = Fstat, p_value = pval)
        }

