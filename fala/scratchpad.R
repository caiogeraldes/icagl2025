require("dagitty")
require("tidyverse")
require("ggdag")
require("ggplot2")
set.seed(1)

# Full model
dag <- dagitty::dagitty("
  dag{
    A[outcome]
    PoS[exposure]
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
    ModV -> PoS
    WO -> A
  }
") %>%
  tidy_dagitty() %>%
  dag_label(
    labels = c(
      "A" = "outcome",
      "PoS" = "exposure"
    )
  ) %>%
  mutate(
    label_as_fact =
      factor(
        ifelse(is.na(label), "unadjusted", label),
        levels = c("outcome", "exposure", "adjusted", "unadjusted")
      )
  )

png("figs/dag3.png")
dag %>%
  ggdag_paths_fan(
    from = "PoS",
    to = "A",
    shadow = TRUE,
    node_size = 0,
    spread = 2,
  ) +
  geom_dag_point(
    aes(
      color = label,
      shape = (name == "PoS")
    ),
    show.legend = FALSE
  ) +
  geom_dag_text() +
  theme_dag_blank()
dev.off()
dag %>%
  adjust_for("Aut") %>%
  mutate(
    label_as_fact = ifelse(adjusted == "adjusted", "adjusted", label_as_fact)
  ) %>%
  ggdag_paths_fan(
    from = "PoS",
    to = "A",
    shadow = TRUE,
    node_size = 0,
    spread = 2,
  ) +
  geom_dag_point(
    aes(color = label, shape = (name == "PoS")),
    show.legend = FALSE
  ) +
  geom_dag_text() +
  theme_dag_blank()


dag %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_point(show.legend = FALSE) +
  geom_dag_text() +
  geom_dag_edges() +
  theme_dag()

dag %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, text = name)) +
  geom_dag_point(aes(color = label), show.legend = FALSE) +
  geom_dag_text() +
  geom_dag_edges() +
  theme_dag()
