f0_cop_pos <- alist(
  A ~ bernoulli(p),
  logit(p) <- a + b_c * Cop + b_p * P,
  a ~ normal(0, 1),
  b_c ~ normal(0, 1),
  b_p ~ normal(0, 1)
)

m0 <- ulam(
  flist = f0_cop_pos,
  data = data,
  chains = 4, cores = 4,
  log_lik = TRUE
)

f1_cop_pos <- alist(
  A ~ bernoulli(p),
  logit(p) <- a + b_c * Cop + b_p * sum(delta_j[1:P]),
  a ~ normal(0, 1),
  b_c ~ normal(0, 1),
  b_p ~ normal(0, 1),
  vector[3]:delta_j <<- append_row(0, delta),
  simplex[2]:delta ~ dirichlet(alpha)
)

m1 <- ulam(
  flist = f1_cop_pos,
  data = data,
  chains = 4, cores = 4,
  log_lik = TRUE
)

f2_cop_pos <- alist(
  A ~ bernoulli(p),
  logit(p) <- a + b_c * Cop + b_p * sum(delta_j[1:P]),
  a ~ normal(0, 1),
  b_c ~ normal(0, 1),
  b_p ~ normal(0, 1),
  vector[3]:delta_j <<- append_row(0, delta),
  simplex[2]:delta ~ dirichlet(alpha),
  P ~ dordlogit(phi, cutpoints),
  phi <- u + g_c * Cop,
  g_c ~ dnorm(0, 0.5),
  u ~ dnorm(0, 1),
  cutpoints ~ dnorm(0, 1.5)
)

m2 <- ulam(
  flist = f2_cop_pos,
  data = data,
  chains = 4, cores = 4,
  log_lik = TRUE
)

compare(m0, m1, m2)
