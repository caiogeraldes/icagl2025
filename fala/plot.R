require("dagitty")
require("ggdag")
require("ggplot2")

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

png("figs/dag2.png")
dag <- dagitty('
    dag {
    A [outcome,pos="0,0"]
    D [latent,pos="0.0,-1"]
    P [exposure,pos="-1,0"]
    C [pos="-1,-1"]
    D -> A
    D -> C -> A
    P -> A <- P
    C -> P
  }') %>%
  tidy_dagitty() %>%
  dag_label(
    labels = c(
      "A" = "outcome",
      "D" = "latent",
      "P" = "exposure",
      "C" = "adjusted"
    )
  )
dag %>% ggdag_paths_fan(
  from = "P",
  to = "A",
  spread = 1.5,
  text_size = 10
) + theme_dag_blank()
dev.off()

dag %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_point(aes(color = label), show.legend = FALSE) +
  geom_dag_text(aes(color = label)) +
  geom_dag_edges() +
  geom_dag_label_repel(aes(label = label), colour = "black", box.padding = 3) +
  theme_dag()
