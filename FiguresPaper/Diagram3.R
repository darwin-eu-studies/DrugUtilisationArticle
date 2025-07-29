
col0 <- "black"
colc <- "#003366"
col1 <- "#1f77b4"
col21 <- "#ffd199"
col22 <- "#b35c00"
col31 <- "#a6e3a6"
col32 <- "#1b661b"
col4 <- "#d62728"
col5 <- "#9467bd"
col6 <- "#8c564b"
col7 <- "#e377c2"

sep1

ggplot2::ggplot() +
  # cohort
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = 0, y = 10.5, label = "cohort"),
    family = "Graphik",
    hjust = 0
  ) +
  ggplot2::geom_errorbar(
    mapping = ggplot2::aes(xmin = 0, xmax = 45, y = 10),
    colour = colc,
    size = 1,
    width = 0.24
  ) +
  # exposures
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = 0, y = 9.2, label = "exposures"),
    family = "Graphik",
    hjust = 0
  ) +
  ggplot2::geom_line(
    data = dplyr::tibble(
      x = c(0, 20, 15, 25, 30, 45),
      y = c(0, 0, -0.25, -0.25, -0.125, -0.125) + 8.7,
      group = c(1, 1, 2, 2, 3, 3)
    ),
    mapping = ggplot2::aes(x = x, y = y, group = group),
    colour = col1,
    size = 4,
    alpha = 0.5
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = 12.5, y = 8.7, label = "Q: 21; DD: 500"),
    size = 3.5
  ) +
  ggplot2::geom_errorbar(
    mapping = ggplot2::aes(xmin = 25, xmax = 30, y = 9),
    colour = col0,
    size = 0.5,
    width = 0.3
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = 27.5, y = 9.3, label = "5 days"),
    family = "Graphik",
    size = 3
  ) +
  # number eras
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = 0, y = 8, label = "number eras"),
    family = "Graphik",
    hjust = 0
  ) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = c(0, 25, 30, 45), y = 7.3, group = c(1, 1, 2, 2)),
    colour = col21,
    size = 1
  ) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = c(0, 45), y = 6.6),
    colour = col22,
    size = 1
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = c(12.5, 37.5, 22.5), y = c(7.55, 7.55, 6.85), label = "1"),
    family = "Graphik",
    size = 3.5
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = c(45, 45), y = c(7.55, 6.85), label = c(" = 2", " = 1")),
    family = "Graphik",
    size = 3.5,
    hjust = 0
  ) +
  # days exposed
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = 0, y = 8 - 2, label = "days exposed"),
    family = "Graphik",
    hjust = 0
  ) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = c(0, 25, 30, 45), y = 7.3 - 2, group = c(1, 1, 2, 2)),
    colour = col31,
    size = 1
  ) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = c(0, 45), y = 6.6 - 2),
    colour = col32,
    size = 1
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = c(12.5, 37.5, 22.5), y = c(7.55, 7.55, 6.85) - 2, label = c("31", "11", "46")),
    family = "Graphik",
    size = 3.5
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = c(45, 45), y = c(7.55, 6.85) - 2, label = c(" = 42", " = 46")),
    family = "Graphik",
    size = 3.5,
    hjust = 0
  ) +
  # initial quantity
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = 0, y = 6 - 2, label = "initial quantity"),
    family = "Graphik",
    hjust = 0
  ) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = c(0, 25), y = 7.3 - 4),
    colour = col4,
    size = 1
  ) +
  # cumulative quantity
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = 0, y = 6 - 3, label = "cumulative quantity"),
    family = "Graphik",
    hjust = 0
  ) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = c(0, 25, 30, 45), y = 7.3 - 5, group = c(1, 1, 2, 2)),
    colour = col4,
    size = 1
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = c(12.5, 37.5, 22.5), y = c(7.55, 7.55, 6.85) - 5, label = c("31", "11", "46")),
    family = "Graphik",
    size = 3.5
  ) +
  # initial daily dose
  # cumulative dose
  # axis
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = c(-3, 53), y = -0.55),
    data = NULL,
    colour = "black",
    inherit.aes = FALSE,
    size = 0.4
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = 25, y = -1.5, label = "Time (days)"),
    data = NULL,
    size = 5,
    family = "Graphik"
  ) +
  ggplot2::coord_cartesian(xlim = c(0, 75), ylim = c(0, 11), clip = "off") +
  ggplot2::scale_y_continuous(breaks = NULL, name = "") +
  ggplot2::scale_x_continuous(name = "", breaks = seq(0, 50, by = 10)) +
  ggplot2::theme(
    legend.position = "none",
    axis.line.x = ggplot2::element_line(linewidth = 0),
    legend.text = ggplot2::element_text(size = 12),
    plot.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.background = ggplot2::element_rect(fill = "white", color = NA),
    text = ggplot2::element_text(size = 14, family = "Graphik")
  )
  # persons
  ggimage::geom_image(
    mapping = ggplot2::aes(x = -70, y = 1:5, image = "./person.png"),
    data = dplyr::tibble(),
    inherit.aes = FALSE,
    size = 0.12
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = -70, y = 1:5, label = 1:5),
    data = dplyr::tibble(),
    inherit.aes = FALSE,
    color = "white",
    nudge_y = 0.08,
    size = 3.2,
    family = "Graphik"
  ) +
  # vertical lines
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = x, y = y, group = group),
    data = dplyr::tibble(
      x = c(-60.5, -30.5, -0.5, 0.5, 30.5, 60.5),
      y1 = 0.5,
      y2 = 5.8,
    ) |>
      dplyr::mutate(group = dplyr::row_number()) |>
      tidyr::pivot_longer(c("y1", "y2"), names_to = NULL, values_to = "y"),
    linetype = "dashed",
    colour = "black",
    inherit.aes = FALSE,
    alpha = 0.5
  ) +
  # legend
  ggplot2::scale_color_manual(
    name = NULL,
    values = c("Treatment A" = colA, "Treatment B" = colB, "Treatment C" = colC, "Not in observation" = col0)
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = x, y = y, label = lab),
    data = legendlabs,
    hjust = 0
  ) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = x, y = y, colour = type),
    data = legendLines |>
      dplyr::filter(id == 1),
    size = 5,
    alpha = 0.5
  ) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = x, y = y, colour = type),
    data = legendLines |>
      dplyr::filter(id != 1),
    size = 3,
    alpha = 0.8
  ) +
  # tables
  ggimage::geom_image(
    mapping = ggplot2::aes(x = 120, y = 6, image = "./table1.png"),
    data = dplyr::tibble(),
    inherit.aes = FALSE,
    size = 0.65,
    nudge_x = 0,
    nudge_y = 0
  ) +
  ggimage::geom_image(
    mapping = ggplot2::aes(x = 120, y = 2, image = "./table2.png"),
    data = dplyr::tibble(),
    inherit.aes = FALSE,
    size = 0.65,
    nudge_x = 0,
    nudge_y = 0
  )

p

ggplot2::ggsave(
  filename = "./Figures/Diagram3.png",
  plot = p,
  width = 974*3,
  height = 426*3,
  units = "px",
  dpi = 300
)
