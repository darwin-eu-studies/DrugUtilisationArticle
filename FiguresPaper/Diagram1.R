
exposures <- "#66A3FF"
era <- "#003366"

x0 <- dplyr::tibble(start = c(0, 5, 24), end = c(11, 14, 30), y = c(5, 4, 4.5)) |>
  dplyr::mutate(group = dplyr::row_number()) |>
  tidyr::pivot_longer(c("start", "end"), names_to = NULL, values_to = "x")
x1 <- dplyr::tibble(xmin = c(0, 24, 0), xmax = c(14, 30, 30), y = c(1.3, 1.3, 0)) |>
  dplyr::mutate(group = dplyr::row_number())
x2 <- dplyr::tibble(x = c(14, 24), y = c(4.5, 5.5))

p <- ggplot2::ggplot() +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = x, y = y, group = group),
    data = x0,
    size = 2,
    colour = exposures
  ) +
  ggplot2::geom_errorbar(
    mapping = ggplot2::aes(xmax = xmax, xmin = xmin, y = y, group = group),
    data = x1,
    size = 0.7,
    colour = era,
    width = 0.4
  ) +
  ggplot2::geom_segment(
    mapping = ggplot2::aes(x = 0, y = -1, xend = 35, yend = -1),
    arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "cm")),
    size = 0.4,
    color = "black"
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = 35/2, y = -1.4, label = "Time"),
    size = 6,
    family = "Graphik"
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = 0, y = 5.6, label = "Exposures"),
    size = 4.5,
    family = "Graphik",
    hjust = 0
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = 0, y = 2.2, label = "Eras"),
    size = 4.5,
    family = "Graphik",
    hjust = 0
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = -0.5, y = 1.3, label = "gapEra ≤ 7 days"),
    size = 4,
    family = "Graphik",
    hjust = 1
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = -0.5, y = 0, label = "gapEra ≥ 8 days"),
    size = 4,
    family = "Graphik",
    hjust = 1
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = 19, y = 6.5, label = "gap = 8 days"),
    size = 4,
    family = "Graphik"
  ) +
  ggimage::geom_image(
    mapping = ggplot2::aes(x = -3, y = 4.5, image = "./person.png"),
    size = 0.3
  ) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = c(14, 14, 24, 24), y = c(4, 5.7, 4.5, 5.7), group = c(1, 1, 2, 2)),
    linetype = "dashed"
  ) +
  ggbrace::stat_brace(mapping = aes(x = x, y = y), data = x2) +
  ggplot2::xlim(c(-8, 35)) +
  ggplot2::ylim(c(-2, 7)) +
  #ggplot2::theme_void() +
  ggplot2::theme(
    legend.position = "top",
    plot.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.background = ggplot2::element_rect(fill = "white", color = NA)
  )

p

ggplot2::ggsave(
  filename = "./Figures/Diagram1.png",
  plot = p,
  width = 740*3,
  height = 344*3,
  units = "px",
  dpi = 300
)
