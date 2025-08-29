library(tidyverse)
library(rethinking)
library(kableExtra)

source("./fix_pairs_rethinking.R")
# Loading data
dados <- read_csv("./data.csv") %>%
  mutate(pos_pred_a = factor(pos_pred_a, levels = c("A", "N", "P")),
    OBJ_TH = factor(
      ifelse(
        VM_LEMMA == "δοκέω",
        "Experiencer",
        ifelse(VM_MOD, ifelse(Vinf_Cop, "Experiencer", "Agent"), "Recipient")
      ), levels = c("Recipient", "Experiencer", "Agent")
    )
  )

dat <- list(
  "A" = dados$Attr,
  "Th" = dados$OBJ_TH,
  "PoS" = dados$pos_pred_a,
  "Cop" = dados$Vinf_Cop,
  "Auth" = as.factor(dados$AUTHOR),
  "Dia" = as.factor(dados$DIALECT),
  "Prec" = (dados$DIST_OBJ_PRED < 0),
  "Dist" = abs(dados$DIST_OBJ_PRED),
  alpha_pos = rep(3, 2),
  alpha_th = rep(3, 2)
)

flist <- alist(
  # Attraction Model
  A ~ bernoulli(p),
  logit(p) <- a +
    b_c * Cop +
    b_pos * sum(delta_j_pos[1:PoS]) +
    b_th * sum(delta_j_th[1:Th]) +
    z_auth[Auth] * s_auth +
    b_dia_bar + z_dia[Dia] * s_dia +
    b_dist * Dist +
    b_wo * Prec,
  # base
  a ~ normal(0, 1),
  # copula
  b_c ~ normal(0, 1),
  # pos
  b_pos ~ lognormal(0, 1),
  vector[3]:delta_j_pos <<- append_row(0, delta_pos),
  simplex[2]:delta_pos ~ dirichlet(alpha_pos),
  transpars > real[1]:d_part <<- b_pos * 0,
  transpars > real[1]:d_adj <<- b_pos * delta_pos[1],
  transpars > real[1]:d_noun <<- b_pos * (delta_pos[1] + delta_pos[2]),
  # th
  b_th ~ lognormal(0, 1),
  vector[3]:delta_j_th <<- append_row(0, delta_th),
  simplex[2]:delta_th ~ dirichlet(alpha_th),
  transpars > real[1]:d_recip <<- b_th * 0,
  transpars > real[1]:d_exp <<- b_th * delta_th[1],
  transpars > real[1]:d_agent <<- b_th * (delta_th[1] + delta_th[2]),
  # auth
  z_auth[Auth] ~ normal(0, 1),
  s_auth ~ exponential(1),
  gq > vector[Auth]:b_auth <<- z_auth * s_auth,
  # dia
  b_dia_bar ~ normal(0, 1),
  z_dia[Dia] ~ normal(0, 1),
  s_dia ~ exponential(1),
  gq > vector[Dia]:b_dia <<- b_dia_bar + z_dia * s_dia,
  # Dist
  b_dist ~ normal(0, 1),
  # WO
  b_wo ~ normal(0, 1)
)

model <- ulam(
  flist = flist,
  data = dat,
  chains = 4, cores = 4,
  log_lik = TRUE,
  file = "./model_glm"
)
