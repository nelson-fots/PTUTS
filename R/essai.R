set.seed(123)

# 6 ans de données mensuelles
n <- 72
t <- 1:n

# Composantes simples
tendance <- 1000 + 5 * t
saison <- 120 * sin(2 * pi * t / 12)
bruit <- rnorm(n, mean = 0, sd = 60)

# Série finale
visiteurs <- tendance + saison + bruit

# Série temporelle mensuelle
ts_visiteurs <- ts(
  visiteurs,
  frequency = 12,
  start = c(2019, 1)
)

plot(ts_visiteurs,
     main = "Nombre mensuel de visiteurs d’un site web",
     ylab = "Nombre de visiteurs",
     xlab = "Temps")
