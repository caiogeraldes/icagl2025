require("tidyverse")
library(kableExtra)
require("rethinking")
require("dagitty")
require("ggdag")

source("./fix_pairs_rethinking.R")
source("./pred_a_info.R")

# Loading data
dados <- read_csv("./data_animacy.csv") %>%
  mutate(pos_pred_a = factor(pred_a_pos, levels = c("A", "N", "P")),
    pred_a = pred_a,
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
  alpha_pos = rep(2, 2),
  alpha_th = rep(2, 2)
)

flist <- alist(
  # Attraction Model
  A ~ bernoulli(p),
  logit(p) <- a +
    b_c * Cop +
    b_pos * sum(delta_j_pos[1:PoS]) +
    b_th * sum(delta_j_th[1:Th]) +
    z_auth[Auth] * s_auth +
    z_dia[Dia] * s_dia +
    b_dist * Dist +
    b_wo * Prec,
  # base
  a ~ normal(0, 1),
  # copula
  b_c ~ normal(0, 1),
  # pos
  b_pos ~ normal(0, 0.5),
  vector[3]:delta_j_pos <<- append_row(0, delta_pos),
  simplex[2]:delta_pos ~ dirichlet(alpha_pos),
  transpars > real[1]:d_adj <<- b_pos + delta_pos[1],
  transpars > real[1]:d_noun <<- b_pos + delta_pos[1] + delta_pos[2],
  # th
  b_th ~ normal(0, 0.5),
  vector[3]:delta_j_th <<- append_row(0, delta_th),
  simplex[2]:delta_th ~ dirichlet(alpha_th),
  transpars > real[1]:d_exp <<- b_th + delta_th[1],
  transpars > real[1]:d_agent <<- b_th + delta_th[1] + delta_th[2],
  # auth
  z_auth[Auth] ~ normal(0, 1),
  s_auth ~ exponential(1),
  gq > vector[Auth]:b_auth <<- z_auth * s_auth,
  # dia
  z_dia[Dia] ~ normal(0, 1),
  s_dia ~ exponential(1),
  gq > vector[Dia]:b_dia <<- z_dia * s_dia,
  # Dist
  b_dist ~ normal(0, 1),
  # WO
  b_wo ~ normal(0, 1)
)

model <- ulam(
  flist = flist,
  data = dat,
  chains = 4, cores = 4,
  log_lik = TRUE
)

precis(model, depth = 2)

# PoS
(a <- precis(model, pars = c("b_pos", "delta_pos"), depth = 2))
sink("./tables/model_domain_effects.tex")
tibble(
  Effect = a@row.names,
  mean = round(a@.Data[[1]], 2),
  sd = round(a@.Data[[2]], 2),
  `5.5\\%` = round(a@.Data[[3]], 2),
  `94.5\\%` = round(a@.Data[[4]], 2),
  rhat = round(a@.Data[[5]], 2),
) %>%
  mutate(Effect = c(
    "$\\beta_{\\text{PoS}}$",
    "$\\delta_{\\text{Adj}}$",
    "$\\delta_{\\text{Noun}}$"
  )) %>%
  kbl(format = "latex", booktabs = TRUE, escape = FALSE)
sink()

(a <- precis(model, pars = c("b_pos", "d_adj", "d_noun"), depth = 2))
sink("./tables/model_domain_cummulative_effects.tex")
tibble(
  `Cummulative effect` = a@row.names,
  mean = round(a@.Data[[1]], 2),
  sd = round(a@.Data[[2]], 2),
  `5.5\\%` = round(a@.Data[[3]], 2),
  `94.5\\%` = round(a@.Data[[4]], 2),
  rhat = round(a@.Data[[5]], 2),
) %>%
  mutate(
    PoS = c("Participle", "Adjective", "Noun"),
    `Cummulative effect` = c(
      "$\\beta_{\\text{PoS}}$",
      "$\\beta_{\\text{PoS}} + \\delta_{\\text{Adj}}$",
      "$\\beta_{\\text{PoS}} + \\delta_{\\text{Adj}} + \\delta_{\\text{Noun}}$"
    )
  ) %>%
  relocate("PoS") %>%
  kbl(format = "latex", booktabs = TRUE, escape = FALSE)
sink()


png(
  "./figs/model_domain_banddelta.png",
  units = "px", width = 1600, height = 1600, res = 300
)
pairs(
  model,
  pars = c("b_pos", "delta_pos"), labels = c("Participle", "Adjective", "Noun")
)
dev.off()


# Theta
(a <- precis(model, pars = c("b_th", "delta_th"), depth = 2))
sink("./tables/model_theta_effects.tex")
tibble(
  Effect = a@row.names,
  mean = round(a@.Data[[1]], 2),
  sd = round(a@.Data[[2]], 2),
  `5.5\\%` = round(a@.Data[[3]], 2),
  `94.5\\%` = round(a@.Data[[4]], 2),
  rhat = round(a@.Data[[5]], 2),
) %>%
  mutate(Effect = c(
    "$\\beta_{\\text{Theta}}$",
    "$\\delta_{\\text{Exp}}$",
    "$\\delta_{\\text{Agent}}$"
  )) %>%
  kbl(format = "latex", booktabs = TRUE, escape = FALSE)
sink()

(a <- precis(model, pars = c("b_th", "d_exp", "d_agent"), depth = 2))
sink("./tables/model_theta_cummulative_effects.tex")
tibble(
  `Cummulative effect` = a@row.names,
  mean = round(a@.Data[[1]], 2),
  sd = round(a@.Data[[2]], 2),
  `5.5\\%` = round(a@.Data[[3]], 2),
  `94.5\\%` = round(a@.Data[[4]], 2),
  rhat = round(a@.Data[[5]], 2),
) %>%
  mutate(
    Theta = c("Recipient", "Experiencer", "Agent"),
    `Cummulative effect` = c(
      "$\\beta_{\\text{Rec}}$",
      "$\\beta_{\\text{Rec}} + \\delta_{\\text{Exp}}$",
      "$\\beta_{\\text{Rec}} + \\delta_{\\text{Exp}} + \\delta_{\\text{Agent}}$"
    )
  ) %>%
  relocate("Theta") %>%
  kbl(format = "latex", booktabs = TRUE, escape = FALSE)
sink()


png(
  "./figs/model_theta_banddelta.png",
  units = "px", width = 1600, height = 1600, res = 300
)
pairs(
  model,
  pars = c("b_th", "delta_th"), labels = c("Recipient", "Experiencer", "Agent")
)
dev.off()

# Copula and Distance
(a <- precis(model, pars = c("b_c", "b_dist"), depth = 2))
sink("./tables/model_copula_dist_effects.tex")
tibble(
  Effect = a@row.names,
  mean = round(a@.Data[[1]], 2),
  sd = round(a@.Data[[2]], 2),
  `5.5\\%` = round(a@.Data[[3]], 2),
  `94.5\\%` = round(a@.Data[[4]], 2),
  rhat = round(a@.Data[[5]], 2),
) %>%
  mutate(Effect = c(
    "$\\beta_{\\text{Copula}}$",
    "$\\beta_{\\text{Dist}}$"
  )) %>%
  kbl(format = "latex", booktabs = TRUE, escape = FALSE)
sink()


# Dialect
(a <- precis(model, pars = c("b_dia"), depth = 2))
sink("./tables/model_dialect_effects.tex")
tibble(
  Effect = a@row.names,
  mean = round(a@.Data[[1]], 2),
  sd = round(a@.Data[[2]], 2),
  `5.5\\%` = round(a@.Data[[3]], 2),
  `94.5\\%` = round(a@.Data[[4]], 2),
  rhat = round(a@.Data[[5]], 2),
) %>%
  mutate(Effect = c(
    "$\\beta_{\\text{Attic}}$",
    "$\\beta_{\\text{Jonic}}$"
  )) %>%
  kbl(format = "latex", booktabs = TRUE, escape = FALSE)
sink()

# Author
(a <- precis(model, pars = c("b_auth"), depth = 2))
sink("./tables/model_author_effects.tex")
tibble(
  Effect = a@row.names,
  mean = round(a@.Data[[1]], 2),
  sd = round(a@.Data[[2]], 2),
  `5.5\\%` = round(a@.Data[[3]], 2),
  `94.5\\%` = round(a@.Data[[4]], 2),
  rhat = round(a@.Data[[5]], 2),
) %>%
  mutate(Effect = c(
    "$\\beta_{\\text{Aeschines}}$",
    "$\\beta_{\\text{Aeschylus}}$",
    "$\\beta_{\\text{Andocides}}$",
    "$\\beta_{\\text{Antiphon}}$",
    "$\\beta_{\\text{Aristophanes}}$",
    "$\\beta_{\\text{Demosthenes}}$",
    "$\\beta_{\\text{Euripides}}$",
    "$\\beta_{\\text{Herodotus}}$",
    "$\\beta_{\\text{Isaeus}}$",
    "$\\beta_{\\text{Isocrates}}$",
    "$\\beta_{\\text{Lycurgus}}$",
    "$\\beta_{\\text{Lysias}}$",
    "$\\beta_{\\text{Plato}}$",
    "$\\beta_{\\text{Sophocles}}$",
    "$\\beta_{\\text{Thucydides}}$",
    "$\\beta_{\\text{Xenophon}}$"
  )) %>%
  kbl(format = "latex", booktabs = TRUE, escape = FALSE)
sink()

png(
  "./figs/precis_author.png",
  units = "px", width = 1600, height = 1600, res = 300
)
plot(precis(model, pars = "b_auth", depth = 2),
  labels = c(
    "Aeschines",
    "Aeschylus",
    "Andocides",
    "Antiphon",
    "Aristophanes",
    "Demosthenes",
    "Euripides",
    "Herodotus",
    "Isaeus",
    "Isocrates",
    "Lycurgus",
    "Lysias",
    "Plato",
    "Sophocles",
    "Thucydides",
    "Xenophon"
  )
)
dev.off()
