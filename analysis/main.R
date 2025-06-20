require("tidyverse")
library(kableExtra)
require("rethinking")
require("dagitty")
require("ggdag")

source("./fix_pairs_rethinking.R")
source("./pred_a_info.R")

png(
  "./figs/dag_non_canonical_agreement.png",
  units = "px", width = 800, height = 800, res = 200
)
dagitty("dag{
 C -> A <- D
 T -> A
}") %>% tidy_dagitty() %>% ggdag(text_size = 10) + theme_dag_blank()
dev.off()

# Loading data
dados <- read_csv("./data_animacy.csv") %>%
  mutate(pos_pred_a = factor(pred_a_pos, levels = c("A", "N", "P")),
    pred_a = pred_a
  )

# Domain
dag_domain <- dagitty("
    dag {
    A
    D
    PoS
    C
    D -> A
    D -> C -> A
    PoS -> A <- PoS
    C -> PoS
    }
  ") %>%
  tidy_dagitty()


dag_cop <- dag_domain %>%
  dag_label(labels = c("A" = "outcome", "D" = "latent", "C" = "exposure"))
png("./figs/dag_cop.png", units = "px", width = 1600, height = 1600, res = 300)
dag_cop %>% ggdag_paths_fan(
  from = "PoS", to = "A", shadow = TRUE, node = FALSE, text = FALSE
) +
  geom_dag_point(aes(color = label)) +
  geom_dag_text(size = 5) +
  theme_dag_blank(legend.position = "none")
dev.off()

png("./figs/dag_pos.png", units = "px", width = 1600, height = 1600, res = 300)
dag_pos <- dag_domain %>%
  dag_label(labels = c("A" = "outcome", "D" = "latent", "PoS" = "exposure"))
dag_pos %>% ggdag_paths_fan(
  from = "PoS", to = "A", shadow = TRUE, node = FALSE, text = FALSE
) +
  geom_dag_point(aes(color = label)) +
  geom_dag_text(size = 5) +
  theme_dag_blank(legend.position = "none")
dev.off()

# Controller
dag_controller <- dagitty::dagitty("
  dag{
    A
    ModV
    Aut -> Dia
    Aut -> Genre
    Dia -> A
    Genre -> ModV
    ModV -> A
  }
")


# Independence tests
chisq.test(dados$GENRE, dados$Attr)

chisq.test(dados$GENRE, dados$Attr)

dados_modal_vm <- dados %>% filter(VM_MOD)
dados_not_modal_vm <- dados %>% filter(!VM_MOD)


chisq.test(dados_modal_vm$GENRE, dados_modal_vm$Attr)
chisq.test(dados_not_modal_vm$GENRE, dados_not_modal_vm$Attr)

chisq.test(dados_modal_vm$AUTHOR, dados_modal_vm$Attr)
chisq.test(dados_not_modal_vm$AUTHOR, dados_not_modal_vm$Attr)


dados_juridical <- dados %>% filter(GENRE == "juridical")
dados_drama <- dados %>% filter(GENRE == "drama")
dados_historiography <- dados %>% filter(GENRE == "historiography")
dados_philosophical <- dados %>% filter(GENRE == "philosophical_dialogue")
dados_other <- dados %>% filter(GENRE == "other")

chisq.test(dados_juridical$AUTHOR, dados_juridical$Attr)
chisq.test(dados_drama$AUTHOR, dados_drama$Attr)
chisq.test(dados_historiography$AUTHOR, dados_historiography$Attr)
chisq.test(dados_philosophical$AUTHOR, dados_philosophical$Attr)
chisq.test(dados_other$AUTHOR, dados_other$Attr)

chisq.test(dados$VM_MOD, dados$Vinf_Cop)
chisq.test(dados$AUTHOR, dados$Cop)


a <- table(
  dados$DIALECT,
  dados$AUTHOR,
  dados$VM_MOD,
  dados$Attr
)
# Attic
chisq.test(a[1, , 1, ], correct = TRUE)
chisq.test(a[1, , 2, ], correct = TRUE)
# Jonic (Only Herodotus)
chisq.test(a[2, , 1, ], correct = TRUE)
chisq.test(a[2, , 2, ], correct = TRUE)

dag_general <- dagitty::dagitty("
  dag{
    A
    PoS
    ModV
    WO
    Aut -> Dia
    Aut -> Genre
    Aut -> WO
    Cop -> A
    Cop -> PoS
    Dia -> A
    Genre -> ModV
    PoS -> A
    ModV -> A
    WO -> A
  }
")



dag_a <- dagitty::dagitty("
  dag{
    A
    PoS
    Cop
    D
    D -> A <- PoS <- Cop
    Cop -> A
  }
") %>%
  tidy_dagitty() %>%
  dag_label(
    labels = c(
      "A" = "outcome",
      "D" = "unobserved",
      "Cop" = "observed",
      "PoS" = "observed"
    )
  )
png("./figs/dag_a.png", units = "px", width = 1600, height = 1600, res = 300)
dag_a %>%
  ggdag() +
  geom_dag_node(aes(color = label)) +
  geom_dag_text() +
  theme_dag_blank()
dev.off()

dag_b <- dagitty::dagitty("
  dag{
    A
    PoS
    Cop
    D
    C -> A
    D -> A <- PoS <- Cop
    Cop -> A
    Cop -> Th -> A
  }
") %>%
  tidy_dagitty() %>%
  dag_label(
    labels = c(
      "A" = "outcome",
      "D" = "unobserved",
      "C" = "unobserved",
      "Cop" = "observed",
      "PoS" = "observed",
      "Th" = "observed"
    )
  )

png("./figs/dag_b.png", units = "px", width = 1600, height = 1600, res = 300)
dag_b %>%
  ggdag() +
  geom_dag_node(aes(color = label)) +
  geom_dag_text() +
  theme_dag_blank()
dev.off()


dag_c <- dagitty::dagitty("
  dag{
    A
    PoS
    Cop
    D
    C -> A
    D -> A <- PoS <- Cop
    Cop -> A
    Cop -> Th -> A
    T -> A
    Auth -> Genre -> Th
    Auth -> A <- Dia <- Auth
  }
") %>%
  tidy_dagitty() %>%
  dag_label(
    labels = c(
      "A" = "outcome",
      "D" = "unobserved",
      "T" = "unobserved",
      "C" = "unobserved",
      "Cop" = "observed",
      "PoS" = "observed",
      "WO" = "observed",
      "Th" = "observed",
      "Auth" = "observed",
      "Genre" = "observed",
      "Dia" = "observed"
    )
  )

png("./figs/dag_c.png", units = "px", width = 1600, height = 1600, res = 300)
dag_c %>%
  ggdag() +
  geom_dag_node(aes(color = label)) +
  geom_dag_text() +
  theme_dag_blank()
dev.off()


dag_d <- dagitty::dagitty("
  dag{
    A
    PoS
    Cop
    D
    C -> A
    D -> A <- PoS <- Cop
    Cop -> A
    Cop -> Th -> A
    T -> A
    Auth -> Genre -> Th
    Auth -> A <- Dia <- Auth
    T <- WO <- Dist -> A
  }
") %>%
  tidy_dagitty() %>%
  dag_label(
    labels = c(
      "A" = "outcome",
      "D" = "unobserved",
      "T" = "unobserved",
      "C" = "unobserved",
      "Cop" = "observed",
      "PoS" = "observed",
      "WO" = "observed",
      "Th" = "observed",
      "Auth" = "observed",
      "Genre" = "observed",
      "Dia" = "observed",
      "Dist" = "observed"
    )
  )

png("./figs/dag_d.png", units = "px", width = 1600, height = 1600, res = 300)
dag_d %>%
  ggdag() +
  geom_dag_node(aes(color = label)) +
  geom_dag_text() +
  theme_dag_blank()
dev.off()

source("./glm.R")
