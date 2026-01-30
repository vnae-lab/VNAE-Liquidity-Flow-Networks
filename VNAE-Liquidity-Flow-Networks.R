# ------------------------------------------------------------
# VNAE Asymmetric Liquidity Flow Networks
# Funding Stress Propagation in Financial Systems
# ------------------------------------------------------------

rm(list = ls())
set.seed(28)

# ------------------------------------------------------------
# Parameters
# ------------------------------------------------------------

n <- 20                 # number of financial institutions
T <- 30
dt <- 0.1
time <- seq(0, T, by = dt)

# ------------------------------------------------------------
# Asymmetric liquidity frictions (Theta) - MANUAL
# ------------------------------------------------------------

theta <- c(
  1.8, 1.5, 1.2, 2.0, 1.7,
  0.9, 0.8, 1.1, 0.7, 1.0,
  0.6, 0.5, 0.9, 0.4, 0.8,
  1.3, 1.6, 1.4, 1.9, 1.1
)

Theta <- diag(theta)

# ------------------------------------------------------------
# Directed weighted interbank exposure network
# ------------------------------------------------------------

A <- matrix(rexp(n * n, rate = 2), n, n)
diag(A) <- 0

# Directed Laplacian
L <- diag(rowSums(A)) - A

# ------------------------------------------------------------
# VNAE geometric rigidity
# ------------------------------------------------------------

beta <- 0.15

# ------------------------------------------------------------
# External funding stress (shock vector)
# ------------------------------------------------------------

p <- rnorm(n, mean = 0, sd = 0.2)

# ------------------------------------------------------------
# Initial liquidity imbalances
# ------------------------------------------------------------

omega0 <- rnorm(n, 0, 1)

# ------------------------------------------------------------
# VNAE liquidity dynamics
# ------------------------------------------------------------

liquidity_dynamics <- function(t, omega, parms) {
  with(parms, {
    domega <- -L %*% omega - Theta %*% omega + p
    list(as.vector(domega))
  })
}

# ------------------------------------------------------------
# Simulation
# ------------------------------------------------------------

library(deSolve)

params <- list(L = L, Theta = Theta, p = p)

sol <- ode(
  y = omega0,
  times = time,
  func = liquidity_dynamics,
  parms = params
)

# ------------------------------------------------------------
# VNAE effective geometry (quadratic proxy)
# ------------------------------------------------------------

g <- diag(n) + beta * (Theta + A)

# Scalar curvature proxy (interpretive, not Riemannian)
K <- 0
for (i in 1:(n - 1)) {
  for (j in (i + 1):n) {
    K <- K +
      abs(theta[i] - theta[j]) * abs(A[i, j]) /
      (1 + beta * (theta[i] + theta[j]))
  }
}
K <- 2 * K / (n * (n - 1))

cat("VNAE curvature proxy K =", round(K, 4), "\n")

# ------------------------------------------------------------
# Visualization
# ------------------------------------------------------------

matplot(
  sol[, 1],
  sol[, -1],
  type = "l",
  lty = 1,
  col = rgb(0.1, 0.4, 0.7, 0.4),
  xlab = "Time",
  ylab = "Liquidity stress ω_i(t)",
  main = "Liquidity Flow under Asymmetric Dissipation (VNAE)"
)
abline(h = 0, lty = 2)
grid()
