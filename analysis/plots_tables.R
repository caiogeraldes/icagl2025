library(tidyverse)
library(kableExtra)
library(rethinking)
library(dagitty)
library(ggdag)

source("./fix_pairs_rethinking.R")
source("./glm.R")

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

samples <- extract.samples(model)


png(
  "./figs/posterior_b_copula.png",
  units = "px", width = 1600, height = 1600, res = 300
)
density_b_c <- density(samples$b_c, bw = 0.036)
density_b_c <- tibble(x = density_b_c$x, y = density_b_c$y)
boundaries <- HPDI(samples$b_c)
density_b_c %>%
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  geom_area(
    data = subset(
      density_b_c,
      x > boundaries[1] & x < boundaries[2]
    ),
    mapping = aes(x = x, y = y),
    fill = "purple",
    alpha = 0.5
  ) +
  xlab("b_cop") +
  ylab("density") +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20)
  )
dev.off()

png(
  "./figs/posterior_b_dist.png",
  units = "px", width = 1600, height = 1600, res = 300
)
density_b_dist <- density(samples$b_dist, bw = 0.0035)
density_b_dist <- tibble(x = density_b_dist$x, y = density_b_dist$y)
boundaries <- HPDI(samples$b_dist)
density_b_dist %>%
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  geom_area(
    data = subset(
      density_b_dist,
      x > boundaries[1] & x < boundaries[2]
    ),
    mapping = aes(x = x, y = y),
    fill = "purple",
    alpha = 0.5
  ) +
  xlab("b_dist") +
  ylab("density") +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20)
  )
dev.off()


# Dialect
(a <- precis(model, pars = c("b_dia_bar", "s_dia", "b_dia"), depth = 2))
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
    "$\\overline{\\beta}_{\\text{Dia}}$",
    "$\\sigma_{\\text{Dia}}$",
    "$\\beta_{\\text{Attic}}$",
    "$\\beta_{\\text{Jonic}}$"
  )) %>%
  rows_append(tibble(
    Effect =
      "$\\beta_{\\text{Attic}} - \\beta_{\\text{Jonic}}$",
    mean = round(mean(samples$b_dia[, 1] - samples$b_dia[, 2]), 2),
    sd = round(sd(samples$b_dia[, 1] - samples$b_dia[, 2]), 2),
    `5.5\\%` = round(HPDI(samples$b_dia[, 1] - samples$b_dia[, 2])[1], 2),
    `94.5\\%` = round(HPDI(samples$b_dia[, 1] - samples$b_dia[, 2])[2], 2),
    rhat = NA
  )) %>%
  kbl(format = "latex", booktabs = TRUE, escape = FALSE)
sink()

png(
  "./figs/posterior_b_attic.png",
  units = "px", width = 1600, height = 1600, res = 300
)
density_b_attic <- density(samples$b_dia[, 1], bw = 0.06)
density_b_attic <- tibble(x = density_b_attic$x, y = density_b_attic$y)
boundaries <- HPDI(samples$b_dia[, 1])
density_b_attic %>%
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  geom_area(
    data = subset(
      density_b_attic,
      x > boundaries[1] & x < boundaries[2]
    ),
    mapping = aes(x = x, y = y),
    fill = "purple",
    alpha = 0.5
  ) +
  xlab("b_attic") +
  ylab("density") +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20)
  )
dev.off()

png(
  "./figs/posterior_b_jonic.png",
  units = "px", width = 1600, height = 1600, res = 300
)
density_b_jonic <- density(samples$b_dia[, 2], bw = 0.06)
density_b_jonic <- tibble(x = density_b_jonic$x, y = density_b_jonic$y)
boundaries <- HPDI(samples$b_dia[, 2])
density_b_jonic %>%
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  geom_area(
    data = subset(
      density_b_jonic,
      x > boundaries[1] & x < boundaries[2]
    ),
    mapping = aes(x = x, y = y),
    fill = "purple",
    alpha = 0.5
  ) +
  xlab("b_jonic") +
  ylab("density") +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20)
  )
dev.off()

png(
  "./figs/posterior_diff_dia.png",
  units = "px", width = 1600, height = 1600, res = 300
)
density_diff_dia <- density(samples$b_dia[, 1] - samples$b_dia[, 2], bw = 0.06)
density_diff_dia <- tibble(x = density_diff_dia$x, y = density_diff_dia$y)
boundaries <- HPDI(samples$b_dia[, 1] - samples$b_dia[, 2])
density_diff_dia %>%
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  geom_area(
    data = subset(
      density_diff_dia,
      x > boundaries[1] & x < boundaries[2]
    ),
    mapping = aes(x = x, y = y),
    fill = "purple",
    alpha = 0.5
  ) +
  geom_vline(aes(xintercept = 0)) +
  xlab("b_attic - b_jonic") +
  ylab("density") +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20)
  )
dev.off()


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

png(
  "./figs/dens_authors.png",
  units = "px", width = 1600, height = 1600, res = 300
)
as_tibble(samples$b_auth) %>%
  rename(
    "Aeschines" = V1,
    "Aeschylus" = V2,
    "Andocides" = V3,
    "Antiphon" = V4,
    "Aristophanes" = V5,
    "Demosthenes" = V6,
    "Euripides" = V7,
    "Herodotus" = V8,
    "Isaeus" = V9,
    "Isocrates" = V10,
    "Lycurgus" = V11,
    "Lysias" = V12,
    "Plato" = V13,
    "Sophocles" = V14,
    "Thucydides" = V15,
    "Xenophon" = V16
  ) %>%
  pivot_longer(cols = everything()
  ) %>%
  ggplot(aes(x = value)) +
  geom_density(aes(color = name)) +
  facet_wrap(vars(name)) +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    legend.position = "none"
  )
dev.off()
