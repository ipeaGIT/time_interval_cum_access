library(data.table)
library(ggplot2)
library(cowplot)
library(ggtext)
library(latex2exp)

set.seed(0)

dist <- data.table(tt = 1:120)
dist[, access := abs(rnorm(120))]
dist[, cum_access := cumsum(access)]

tema <- theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(arrow = arrow(type = "closed", length = unit(5, "points"))),
    axis.title.x = element_text(hjust = 1, margin = margin(t = -10)),
    axis.text.x = element_markdown(size = 7.5, margin = margin())
  )

# grafico convencional ----------------------------------------------------

pontos <- c(
  "t<sub>1</sub>" = 15,
  "t<sub>2</sub>" = 30,
  "t<sub>3</sub>" = 45,
  "..." = 70,
  "t<sub>n</sub>" = 95
)
access_pontos <- dist[tt %in% pontos]$cum_access
names(access_pontos) <- c(
  "a<sub>1</sub>",
  "a<sub>2</sub>",
  "a<sub>3</sub>",
  "...",
  "a<sub>n</sub>"
)

segmentos_verticais <- data.table(
  x = pontos,
  xend = pontos,
  y = 0,
  yend = access_pontos
)
segmentos_verticais <- segmentos_verticais[-4]

segmentos_horizontais <- data.table(
  x = 0,
  xend = pontos,
  y = access_pontos,
  yend = access_pontos
)
segmentos_horizontais <- segmentos_horizontais[-4]

p1 <- ggplot(dist) +
  geom_line(aes(tt, cum_access), color = "gray40") +
  geom_segment(
    data = segmentos_verticais,
    aes(x = x, xend = xend, y = y, yend = yend),
    linetype = "dotted",
    alpha = 0.5
  ) +
  geom_segment(
    data = segmentos_horizontais,
    aes(x = x, xend = xend, y = y, yend = yend),
    linetype = "dotted",
    alpha = 0.5
  ) +
  scale_x_continuous(
    name = "T",
    breaks = pontos,
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_y_continuous(
    name = "A",
    breaks = access_pontos,
    expand = expansion(mult = c(0, 0.05))
  ) +
  tema +
  theme(
    axis.text.y = element_markdown(size = 7.5),
    axis.title.y = element_text(hjust = 1, angle = 0, margin = margin(r = -10)),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 18)
  )

# grafico com janelas -----------------------------------------------------

tempos_janelas <- c(
  "t<sub>w1<sub>min</sub></sub>" = 15,
  "t<sub>w1<sub>max</sub></sub>" = 30,
  "t<sub>w2<sub>min</sub></sub>" = 45,
  "t<sub>w2<sub>max</sub></sub>" = 60,
  "t<sub>wn<sub>min</sub></sub>" = 80,
  "t<sub>wn<sub>max</sub></sub>" = 105
)
access_janelas <- dist[tt %in% tempos_janelas]$cum_access
names(access_janelas) <- rep("", 6)

janelas_verticais <- data.table(
  x = tempos_janelas,
  xend = tempos_janelas,
  y = 0,
  yend = access_janelas
)
janelas_horizontais <- data.table(
  x = 0,
  xend = tempos_janelas,
  y = access_janelas,
  yend = access_janelas
)
nomes_janelas <- data.table(
  x = c(tempos_janelas[c(1, 3, 5)] + 1, 68),
  y = 1,
  label = c("w1", "w2", "wn", "...")
)

x_colchetes <- -3
colchetes <- data.table(
  x = x_colchetes,
  xend = x_colchetes,
  y = access_janelas[c(1, 3, 5)],
  yend = access_janelas[c(2, 4, 6)]
)
barrinhas_colchetes <- data.table(
  x = x_colchetes - 1,
  xend = x_colchetes + 1,
  y = access_janelas,
  yend = access_janelas
)
breaks_y = c(
  (access_janelas[1] + access_janelas[2]) / 2,
  (access_janelas[3] + access_janelas[4]) / 2,
  (access_janelas[5] + access_janelas[6]) / 2
)
labels_y = c(
  TeX("\\bar{$a_{w1}$}"),
  TeX("\\bar{$a_{w2}$}"),
  TeX("\\bar{$a_{wn}$}")
)

p2 <- ggplot(dist) +
  geom_line(aes(tt, cum_access), color = "gray40") +
  geom_segment(
    data = janelas_verticais,
    aes(x = x, xend = xend, y = y, yend = yend),
    linetype = "longdash",
    color = rep(c("firebrick3", "dodgerblue3", "goldenrod3"), each = 2)
  ) +
  geom_segment(
    data = janelas_horizontais,
    aes(x = x, xend = xend, y = y, yend = yend),
    linetype = "dotted",
    color = rep(c("firebrick3", "dodgerblue3", "goldenrod3"), each = 2),
    alpha = 0.5
  ) +
  geom_text(
    data = nomes_janelas,
    aes(x = x, y = y, label = label),
    hjust = 0,
    vjust = 0,
    color = c("firebrick3", "dodgerblue3", "goldenrod3", "gray20"),
    size = 2.5
  ) +
  geom_segment(
    data = colchetes,
    aes(x = x, xend = xend, y = y, yend = yend),
    linetype = "solid",
    color = "gray30"
  ) +
  geom_segment(
    data = barrinhas_colchetes,
    aes(x = x, xend = xend, y = y, yend = yend),
    linetype = "solid",
    color = "gray30"
  ) +
  scale_x_continuous(
    name = "T",
    breaks = tempos_janelas,
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_y_continuous(
    name = "A",
    breaks = breaks_y,
    labels = labels_y,
    expand = expansion(mult = c(0, 0.05))
  ) +
  coord_cartesian(xlim = c(0, 120), clip = "off") +
  tema +
  theme(
    axis.text.y = element_text(size = 7.5, margin = margin(r = 5)),
    axis.title.y = element_text(hjust = 1, angle = 0, margin = margin(r = -15)),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 18)
  )

seta <- ggplot() +
  geom_segment(
    aes(x = 0, xend = 1, y = 0, yend = 0),
    arrow = arrow(length = unit(10, "points"))
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank()
  )

pf <- plot_grid(
  p1, p2,
  nrow = 1,
  align = "h",
  labels = c("a", "b")
)

ggsave(
  "./figures/figura_esquematica_artigo_for.jpg",
  plot = pf,
  width = 16,
  height = 5,
  units = "cm", dpi = 300
)
