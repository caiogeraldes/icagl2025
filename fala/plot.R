require("dagitty")
require("tidyverse")
require("ggdag")
require("ggplot2")
set.seed(1)

png("figs/dag1.png")
ggdag(
  dag <- dagitty('
    dag {
      C -> A <- T
      D -> A
      C [pos="0,0"]
      T [pos="0,2"]
      D [pos="-1,1"]
      A [pos="1,1"]
    }
  '),
  text_size = 10
) + theme_dag_blank()
dev.off()

png("figs/dag2a.png")
dag <- dagitty('
    dag {
    A [outcome,pos="0,0"]
    D [latent,pos="0.0,-1"]
    PoS [exposure,pos="-1,0"]
    C [pos="-1,-1"]
    D -> A
    D -> C -> A
    PoS -> A <- PoS
    C -> PoS
  }') %>%
  tidy_dagitty() %>%
  dag_label(
    labels = c(
      "A" = "outcome",
      "D" = "latent",
      "PoS" = "exposure"
    )
  )
dag %>% ggdag_paths_fan(
  from = "PoS",
  to = "A",
  shadow = TRUE,
  node = FALSE,
  text = FALSE,
) + geom_dag_point(aes(color = label)) +
  geom_dag_text(text_size = 5) +
  theme_dag_blank(legend.position = "none")
dev.off()

png("./figs/dag2b.png")
dag <- dagitty('
    dag {
    A [outcome,pos="0,0"]
    D [latent,pos="0.0,-1"]
    PoS [exposure,pos="-1,0"]
    C [pos="-1,-1"]
    D -> A
    D -> C -> A
    PoS -> A <- PoS
    C -> PoS
  }') %>%
  tidy_dagitty() %>%
  dag_label(
    labels = c(
      "A" = "outcome",
      "D" = "latent",
      "C" = "exposure"
    )
  )
dag %>% ggdag_paths_fan(
  from = "C",
  to = "A",
  shadow = TRUE,
  node = FALSE,
  text = FALSE,
) + geom_dag_point(aes(color = label)) +
  geom_dag_text(text_size = 5) +
  theme_dag_blank(legend.position = "none")
dev.off()
