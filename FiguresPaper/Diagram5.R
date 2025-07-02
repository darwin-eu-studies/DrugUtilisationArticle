
colour <- "#66A3FF"

eras <- dplyr::tibble(
  person_id = c(1L, 1L, 2L, 3L, 4L, 4L, 5L),
  start = c(0L, 240L, 0L, 0L, 0L, 235L, 0L),
  end = c(170L, 365L, 80L, 180L, 60L, 365L, 120L)
)
obs <- dplyr::tibble(
  person_id = 1:5L,
  start = 0L,
  end = c(365L, 100L, 365L, 365L, 270L)
)
t <- 0:365
x <- purrr::map_df(t, \(x) {
  num <- eras |>
    dplyr::filter(.data$start <= .env$x & .env$x <= .data$end) |>
    dplyr::tally() |>
    dplyr::pull()
  den <- obs |>
    dplyr::filter(.data$start <= .env$x & .env$x <= .data$end) |>
    dplyr::tally() |>
    dplyr::pull()
  alpha <- 0.05
  p <- num / den
  q <- 1 - p
  z <- stats::qnorm(1 - alpha / 2)
  t1 <- (num + z^2 / 2) / (den + z^2)
  t2 <- z * sqrt(den) / (den + z^2) * sqrt(p * q + z^2 / (4 * den))
  upper <- t1 + t2
  upper[upper > 1] <- 1
  lower <- t1 - t2
  lower[lower < 0] <- 0
  dplyr::tibble(t = x, ppc = p, ppc_lower = lower, ppc_upper = upper, type = "Exposures")
})

person_y <- \(x) 1.1*(x/5) + 0.975
erasToPlot <- eras |>
  dplyr::mutate(era_id = dplyr::row_number(), y = person_y(person_id)) |>
  tidyr::pivot_longer(c("start", "end"), names_to = NULL, values_to = "x")
lines <- ppc |>
  dplyr::mutate(next_ppc = dplyr::lead(ppc), t1 = t, t2 = t + 1) |>
  dplyr::filter(ppc != next_ppc) |>
  dplyr::left_join(
    eras |>
      dplyr::union_all(obs) |>
      tidyr::pivot_longer(c("start", "end"), names_to = NULL, values_to = "t1") |>
      dplyr::rename(id1 = person_id),
    by = "t1"
  ) |>
  dplyr::left_join(
    eras |>
      dplyr::union_all(obs) |>
      tidyr::pivot_longer(c("start", "end"), names_to = NULL, values_to = "t2") |>
      dplyr::rename(id2 = person_id),
    by = "t2"
  ) |>
  dplyr::mutate(
    person_id = dplyr::coalesce(id1, id2),
    y = person_y(person_id),
    id = dplyr::row_number(),
    t = 0.5 * (t1 + t2),
    y2 = 0.5 * (ppc + next_ppc),
  ) |>
  dplyr::select("t", "y", "y2", "id") |>
  dplyr::union_all(
    dplyr::tibble(t = c(0, 365), y = person_y(5), y2 = c(1, 2/3), id = c(-1, -2))
  ) |>
  tidyr::pivot_longer(c("y", "y2"), names_to = NULL, values_to = "y")
notObs <- obs |>
  dplyr::mutate(
    start = end,
    end = 365L,
    y = person_y(person_id)
  ) |>
  tidyr::pivot_longer(c("start", "end"), names_to = NULL, values_to = "x")
l0 <- 80
w <- 25
s1 <- 3
s2 <- 62
leg0 <- dplyr::tibble(x = l0 + c(w + s1, 2 * w + 2 * s1 + s2), y = 2.3, lab = c("Exposed", "Not in observation"))
leg1 <- dplyr::tibble(x = c(0, w) + l0, y = 2.3)
leg2 <- dplyr::tibble(x = c(0, w) + l0 + s2 + w + s1, y = 2.3)

p <- ggplot2::ggplot(data = x, mapping = ggplot2::aes(x = t, y = ppc, color = type, ymin = ppc_lower, ymax = ppc_upper)) +
  ggplot2::geom_line(size = 1) +
  ggplot2::geom_ribbon(alpha = 0.3, show.legend = FALSE, linewidth = 0, fill = colour) +
  ggplot2::scale_color_manual(
    name = NULL,
    values = c("Exposures" = colour)
  ) +
  ggplot2::geom_line(
    data = erasToPlot,
    mapping = ggplot2::aes(x = x, y = y, group = era_id),
    colour = colour,
    inherit.aes = FALSE,
    size = 1.5
  ) +
  ggplot2::geom_segment(
    data = dplyr::tibble(),
    mapping = ggplot2::aes(x = c(-18, 383), xend = c(-18, 383), y = -0.115, yend = 1),
    inherit.aes = FALSE,
    color = "black",
    size = 0.5
  ) +
  ggplot2::geom_segment(
    data = dplyr::tibble(),
    mapping = ggplot2::aes(x = seq(0, 350, by = 50), xend = seq(0, 350, by = 50), y = -0.115, yend = 1),
    inherit.aes = FALSE,
    color = "grey",
    size = 0.25
  ) +
  ggplot2::geom_segment(
    data = dplyr::tibble(),
    mapping = ggplot2::aes(x = -18, xend = 383, y = -0.115, yend = -0.115),
    inherit.aes = FALSE,
    color = "black",
    size = 0.5
  ) +
  ggplot2::geom_line(
    data = lines,
    mapping = ggplot2::aes(x = t, y = y, group = id),
    inherit.aes = FALSE,
    linetype = "dashed"
  ) +
  ggplot2::geom_line(
    data = lines,
    mapping = ggplot2::aes(x = t, y = y, group = id),
    inherit.aes = FALSE,
    linetype = "dashed"
  ) +
  ggplot2::geom_line(
    data = notObs,
    mapping = ggplot2::aes(x = x, y = y, group = person_id),
    inherit.aes = FALSE,
    color = "black",
    size = 4,
    alpha = 0.5
  ) +
  ggimage::geom_image(
    mapping = ggplot2::aes(x = -10, y = person_y(1:5), image = "./person.png"),
    data = dplyr::tibble(),
    inherit.aes = FALSE,
    size = 0.08
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = -10, y = person_y(1:5), label = c(1:5)),
    data = dplyr::tibble(),
    inherit.aes = FALSE,
    color = "white",
    nudge_y = 0.02,
    size = 3.5,
    family = "Graphik"
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = -73, y = 0.5, label = "Proportion of Patients Covered"),
    data = dplyr::tibble(),
    inherit.aes = FALSE,
    color = "black",
    size = 4.3,
    family = "Graphik",
    angle = 90
  ) +
  # legend
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = x, y = y, label = lab),
    data = leg0,
    inherit.aes = FALSE,
    color = "black",
    size = 4.3,
    hjust = 0
  ) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = x, y = y),
    data = leg1,
    inherit.aes = FALSE,
    color = colour,
    size = 1.5
  ) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = x, y = y),
    data = leg2,
    inherit.aes = FALSE,
    color = "black",
    size = 4,
    alpha = 0.5
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::percent_format(accuracy = 0.1),
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    minor_breaks = seq(0, 1, by = 1/8),
    name = ""
  ) +
  ggplot2::scale_x_continuous(name = "Time (days)", breaks = seq(0, 350, by = 50)) +
  ggplot2::coord_cartesian(ylim = c(0, 2.3), xlim = c(0, 365), clip = "off") +
  ggplot2::theme(
    legend.position = "none",
    plot.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.background = ggplot2::element_rect(fill = "white", color = NA),
    axis.line.y = ggplot2::element_blank(),
    panel.grid.minor.y = ggplot2::element_line(colour = "grey"),
    panel.grid.major.y = ggplot2::element_line(colour = "grey"),
    text = ggplot2::element_text(size = 14, family = "Graphik")
  )

p

ggplot2::ggsave(
  filename = "./Figures/Diagram5.png",
  plot = p,
  width = 630*3,
  height = 691*3,
  units = "px",
  dpi = 300
)
