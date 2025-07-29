
col0 <- "black"
colc <- "#003366"
col1 <- "#1f77b4"

y0 <- 0.5
y00 <- 0.385
y1 <- 1
y2 <- 1.5
y3 <- 2
y5 <- 2.7
ym <- y5

cohort <- dplyr::tibble(xmin = 0, xmax = 45, y = y5, type = "cohort")
exposures <- dplyr::tibble(
  x = c(0, 20, 15, 25, 30, 45),
  y = c(y3, y3, y1, y1, y2, y2),
  group = c(1, 1, 2, 2, 3, 3),
  type = "exposures"
)

p <- ggplot2::ggplot() +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = x, y = y, group = group),
    data = dplyr::tibble(
      x = c(15, 15, 20, 20, 25, 25, 30, 30, 45, 45),
      y = c(y00, y1, y00, y3, y00, y1, y00, y2, y00, y2),
      group = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
    ),
    colour = "grey",
    alpha = 0.7,
    linetype = "dashed"
  ) +
  # cohort
  ggplot2::geom_errorbar(
    mapping = ggplot2::aes(xmin = xmin, xmax = xmax, y = y, colour = type),
    data = cohort,
    size = 1,
    width = 0.24
  ) +
  # exposures
  ggplot2::geom_line(
    data = exposures,
    mapping = ggplot2::aes(x = x, y = y, group = group, colour = type),
    size = 2,
    alpha = 0.75
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = 10, y = y3, label = "Acetaminophen 500 mg Oral Tablet (19020053)\nquantity = 63"),
    size = 4.5
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = 20, y = y1, label = "Acetaminophen 300 mg Oral Tablet (19096574)\nquantity = 22"),
    size = 4.5
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = 37.5, y = y2, label = "Acetaminophen 750 mg Oral Tablet (19107439)\nquantity = 32"),
    size = 4.5
  ) +
  # person
  ggimage::geom_image(
    mapping = ggplot2::aes(x = -4, y = 1.6, image = "./person.png"),
    data = dplyr::tibble(),
    inherit.aes = FALSE,
    size = 0.7
  ) +
  # axis
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = c(-3, 48), y = 0.385),
    data = NULL,
    colour = "black",
    inherit.aes = FALSE,
    size = 0.4
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = 22.5, y = 0, label = "Time (days)"),
    data = NULL,
    size = 5,
    family = "Graphik"
  ) +
  ggplot2::scale_color_manual(
    values = c("cohort" = colc, "exposures" = col1), name = ""
  ) +
  ggplot2::coord_cartesian(xlim = c(-3, 48), ylim = c(y0, ym), clip = "off") +
  ggplot2::scale_y_continuous(breaks = NULL, name = "") +
  ggplot2::scale_x_continuous(name = "", breaks = c(0, 10, 15, 20, 25, 30, 40, 45)) +
  ggplot2::theme(
    legend.position = "top",
    axis.line.x = ggplot2::element_line(linewidth = 0),
    legend.text = ggplot2::element_text(size = 12),
    plot.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.background = ggplot2::element_rect(fill = "white", color = NA),
    text = ggplot2::element_text(size = 14, family = "Graphik")
  )

p

ggplot2::ggsave(
  filename = "./Figures/Diagram3.png",
  plot = p,
  width = 943*3,
  height = 286*3,
  units = "px",
  dpi = 300
)
