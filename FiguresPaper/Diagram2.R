
cole <- "#66A3FF"
colw <- "black"
colh <- "#2A9D8F"
colp <- "#F4A261"
colu <- "#8E44AD"

exposures <- dplyr::tibble(
  person_id = 1:5L,
  start = c(0, 0, 0, 0, 0),
  end = c(20, 10, 10, 25, 15),
  type = "Exposures"
)
headache <- dplyr::tibble(
  person_id = c(4, 2),
  x = c(3, -32)
)
pain <- dplyr::tibble(
  person_id = c(2, 4, 5),
  x = c(0, -10, 1)
)
unknown <- dplyr::tibble(
  person_id = c(2, 3, 3, 4, 5),
  x = c(-15, -10, -20, -36, 20)
)

headache <- headache |>
  dplyr::inner_join(exposures, by = "person_id") |>
  dplyr::mutate(x = x + start, type = "Headache") |>
  dplyr::select(!c("start", "end"))
pain <- pain |>
  dplyr::inner_join(exposures, by = "person_id") |>
  dplyr::mutate(x = x + start, type = "Pain") |>
  dplyr::select(!c("start", "end"))
unknown <- unknown |>
  dplyr::inner_join(exposures, by = "person_id") |>
  dplyr::mutate(x = x + start, type = "Unknown") |>
  dplyr::select(!c("start", "end"))

window <- c(-30, 7)
windows <- exposures |>
  dplyr::select("person_id", x = "start") |>
  dplyr::mutate(
    xmin = x + window[1],
    xmax = x + window[2],
    type = "Indication window",
    person_id = person_id + 0.1
  )

exposures <- exposures |>
  tidyr::pivot_longer(c("start", "end"), names_to = NULL, values_to = "x")

p <- ggplot2::ggplot() +
  # exposures
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = x, y = person_id, group = person_id, color = type),
    data = exposures,
    size = 2,
    inherit.aes = FALSE
  ) +
  # windows
  ggplot2::geom_point(
    mapping = ggplot2::aes(x = x, y = person_id, group = person_id, color = type),
    data = windows,
    size = 1,
    inherit.aes = FALSE
  ) +
  ggplot2::geom_errorbar(
    mapping = ggplot2::aes(xmin = xmin, xmax = xmax, y = person_id, group = person_id, color = type),
    data = windows,
    size = 0.5,
    width = 0.1,
    inherit.aes = FALSE
  ) +
  # headache
  ggplot2::geom_point(
    mapping = ggplot2::aes(x = x, y = person_id, group = person_id, fill = type),
    data = headache,
    size = 4,
    shape = 23,
    stroke = 0.3,
    colour = "black",
    inherit.aes = FALSE
  ) +
  # pain
  ggplot2::geom_point(
    mapping = ggplot2::aes(x = x, y = person_id, group = person_id, fill = type),
    data = pain,
    size = 4,
    shape = 23,
    stroke = 0.3,
    colour = "black",
    inherit.aes = FALSE
  ) +
  # unknown
  ggplot2::geom_point(
    mapping = ggplot2::aes(x = x, y = person_id, group = person_id, fill = type),
    data = unknown,
    size = 4,
    shape = 23,
    stroke = 0.3,
    colour = "black",
    inherit.aes = FALSE
  ) +
  # persons
  ggimage::geom_image(
    mapping = ggplot2::aes(x = -40, y = 1:5, image = "./person.png"),
    data = dplyr::tibble(),
    inherit.aes = FALSE,
    size = 0.15
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = -40, y = 1:5, label = 1:5),
    data = dplyr::tibble(),
    inherit.aes = FALSE,
    color = "white",
    nudge_y = 0.08,
    size = 3.5,
    family = "Graphik"
  ) +
  # legend
  ggplot2::scale_color_manual(
    name = NULL,
    values = c("Exposures" = cole, "Indication window" = colw)
  ) +
  ggplot2::scale_fill_manual(
    name = NULL,
    values = c("Headache" = colh, "Pain" = colp, "Unknown" = colu)
  ) +
  ggplot2::scale_y_continuous(breaks = NULL, name = "", limits = c(0.5, 5.5)) +
  ggplot2::scale_x_continuous(name = "Time (days)") +
  ggplot2::theme(
    legend.position = "top",
    axis.line.x = ggplot2::element_line(linewidth = 0.5),
    legend.text = ggplot2::element_text(size = 12),
    plot.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.background = ggplot2::element_rect(fill = "white", color = NA),
    text = ggplot2::element_text(size = 14, family = "Graphik")
  )

p

ggplot2::ggsave(
  filename = "./Figures/Diagram2.png",
  plot = p,
  width = 662*3,
  height = 426*3,
  units = "px",
  dpi = 300
)
