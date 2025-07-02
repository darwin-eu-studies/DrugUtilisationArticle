
cole <- "#66A3FF"
colw <- "black"
colh <- "#279122"
pain <- ""
unknown <- ""

exposures <- dplyr::tibble(
  person_id = 1:5L,
  start = c(0, 0, 0, 0, 0),
  end = c(20, 10, 10, 25, 15),
  type = "Exposures"
)
headache <- dplyr::tibble(
  person_id = c(4, 2),
  x = c(3, -35)
)
pain <- dplyr::tibble(
  person_id = c(2, 4, 5),
  x = c(0, -10, 1)
)

headache <- headache |>
  dplyr::inner_join(exposures, by = "person_id") |>
  dplyr::mutate(x = x + start, type = "Headache") |>
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

ggplot2::ggplot() +
  # exposures
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = x, y = person_id, group = person_id, color = type),
    data = exposures,
    size = 2,
  ) +
  # windows
  ggplot2::geom_point(
    mapping = ggplot2::aes(x = x, y = person_id, group = person_id, color = type),
    data = windows,
    size = 1
  ) +
  ggplot2::geom_errorbar(
    mapping = ggplot2::aes(xmin = xmin, xmax = xmax, y = person_id, group = person_id, color = type),
    data = windows,
    size = 0.5,
    width = 0.1
  ) +
  # headache
  ggplot2::geom_point(
    mapping = ggplot2::aes(x = x, y = person_id, group = person_id, color = type),
    data = headache,
    shape = 13,
    size = 4
  ) +
  # legend
  ggplot2::scale_color_manual(
    name = NULL,
    values = c("Exposures" = cole, "Indication window" = colw, "Headache" = colh)
  ) +
  ggplot2::scale_y_continuous(breaks = NULL, name = "") +
  ggplot2::scale_x_continuous(name = "Time (days)") +
  ggplot2::theme(
    legend.position = "top",
    axis.line.x = ggplot2::element_line(linewidth = 0.5),
    legend.text = ggplot2::element_text(size = 14),
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
