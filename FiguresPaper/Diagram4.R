
col0 <- "black"
col1 <- "#1f77b4"
col2 <- "#ff7f0e"
col3 <- "#2ca02c"
col4 <- "#d62728"

exposures <- dplyr::tribble(
  ~person_id, ~start, ~end, ~type, ~offset,
  1, 0, 100, "A", 0,
  1, 200, 295, "A", 0,
  1, 296, 350, "B", 0,
  2, 13, 150, "A", 1,
  2, 105, 200, "B", -1,
  2, 201, 240, "C", 0,
  2, 241, 280, "A", 0,
  3, 150, 210, "C", 1,
  3, 190, 245, "A", -1
) |>
  dplyr::mutate(exposure_id = dplyr::row_number(), type = paste0("Treatment ", type))

off <- 0.1
y1 <- 11
y2 <- 4.5
y3 <- 0
min <- -10

# discontinuations
disc1 <- exposures |>
  dplyr::filter(type == "Treatment A") |>
  dplyr::mutate(
    x = end,
    y = person_id + y1,
    id = dplyr::row_number(),
    type = "Discontinuation"
  ) |>
  dplyr::select("x", "y", "id", "type")
disc2 <- exposures |>
  dplyr::filter(type == "Treatment A") |>
  dplyr::arrange(person_id, -end) |>
  dplyr::mutate(
    x = 0,
    id = dplyr::row_number(),
    y = id + y2,
    type = "Discontinuation"
  ) |>
  dplyr::select("x", "y", "id", "type")
disc3 <- dplyr::tibble(
  x = 0,
  y = 1:3 + y3,
  id = c(1, 3, 5),
  type = "Discontinuation"
)
disc <- disc1 |>
  dplyr::union_all(disc2) |>
  dplyr::union_all(disc3)

# exposures
exp1 <- exposures |>
  tidyr::pivot_longer(c("start", "end"), names_to = NULL, values_to = "x") |>
  dplyr::mutate(y = person_id + y1 + offset * off)
exp2 <- exposures |>
  dplyr::filter(type == "Treatment A") |>
  dplyr::select("person_id", "t0" = "end") |>
  dplyr::arrange(person_id, t0) |>
  dplyr::mutate(disc_id = dplyr::row_number()) |>
  dplyr::left_join(exposures, by = "person_id", relationship = "many-to-many") |>
  dplyr::mutate(start = start - t0, end = end - t0) |>
  dplyr::filter(end >= min) |>
  dplyr::mutate(
    end = pmax(end, min),
    start = pmax(start, min),
    offset = dplyr::if_else(person_id == 3, 0, offset)
  ) |>
  dplyr::mutate(exposure_id = dplyr::row_number()) |>
  tidyr::pivot_longer(c("start", "end"), names_to = NULL, values_to = "x") |>
  dplyr::mutate(y = disc_id + y2 + offset * off) |>
  dplyr::select(!c("disc_id", "t0"))
exp3 <- exposures |>
  dplyr::filter(type == "Treatment A") |>
  dplyr::group_by(person_id) |>
  dplyr::filter(exposure_id == min(exposure_id)) |>
  dplyr::ungroup() |>
  dplyr::select("person_id", "t0" = "end") |>
  dplyr::arrange(person_id, -t0) |>
  dplyr::mutate(disc_id = dplyr::row_number()) |>
  dplyr::left_join(exposures, by = "person_id", relationship = "many-to-many") |>
  dplyr::mutate(start = start - t0, end = end - t0) |>
  dplyr::filter(end >= min) |>
  dplyr::mutate(
    end = pmax(end, min),
    start = pmax(start, min),
    offset = dplyr::if_else(person_id == 3, 0, offset)
  ) |>
  dplyr::mutate(exposure_id = dplyr::row_number()) |>
  tidyr::pivot_longer(c("start", "end"), names_to = NULL, values_to = "x") |>
  dplyr::mutate(y = disc_id + y3 + offset * off) |>
  dplyr::select(!c("disc_id", "t0"))
exp <- exp1 |>
  dplyr::mutate(group = 1) |>
  dplyr::union_all(exp2 |> dplyr::mutate(group = 2)) |>
  dplyr::union_all(exp3 |> dplyr::mutate(group = 3)) |>
  dplyr::group_by(exposure_id, group) |>
  dplyr::mutate(exposure_id = dplyr::cur_group_id())

# not in observation
notObs <- dplyr::tibble(
  y = c(3 + y1, 5 + y2, 3 + y3),
  start = c(325, 105, 105),
  end = 350,
  type = "Not in observation"
) |>
  dplyr::mutate(group = dplyr::row_number()) |>
  tidyr::pivot_longer(c("start", "end"), names_to = NULL, values_to = "x")

# tables
dplyr::tibble(
  "Treatment" = rep(c("restart", "switch", "restart and switch", "untreated"), 2),
  "follow-up (days)" = rep(c(90, 180), each = 4),
  estimate_name = "rand",
  estimate_type = "character",
  estimate_value = paste0(c("0", "40", "0", "60", "20", "20", "20", "40"), "%")
) |>
  visOmopResults::visTable(header = "follow-up (days)", hide = c("estimate_name", "estimate_type"))
dplyr::tibble(
  "Treatment" = rep(c("restart", "switch", "restart and switch", "untreated"), 2),
  "follow-up (days)" = rep(c(90, 180), each = 4),
  estimate_name = "rand",
  estimate_type = "character",
  estimate_value = paste0(c("0", "33.3", "0", "66.7", "33.3", "0", "33.3", "33.3"), "%")
) |>
  visOmopResults::visTable(header = "follow-up (days)", hide = c("estimate_name", "estimate_type"))

# persons
persons <- dplyr::tibble(
  x = -20,
  y = c(1:3 + y3, 1:5 + y2, 1:3 + y1),
  lab = c(1, 2, 3, 1, 1, 2, 2, 3, 1, 2, 3)
)

# legend
y <- 17
x0 <- 0
w <- 16
w1 <- 6
s <- 2
s1 <- 77
s2 <- 55
legendlabs <- dplyr::tribble(
  ~x, ~lab,
  w + s, "Not in observation",
  2 * w + 2 * s + s1, "Treatment A",
  3 * w + 3 * s + s1 + s2, "Treatment B",
  4 * w + 4 * s + s1 + 2 * s2, "Treatment C",
  4 * w + w1 + 5 * s + s1 + 3 * s2, "Discontinuation"
) |>
  dplyr::mutate(x = x + x0, y = y)
legendLines <- dplyr::tibble(
  x1 = c(0, w + s + s1, 2 * w + 2 * s + s1 + s2, 3 * w + 3 * s + s1 + 2 * s2),
  type = c("Not in observation", "Treatment A", "Treatment B", "Treatment C")
) |>
  dplyr::mutate(x1 = x1 + x0, x2 = x1 + w, y = y, id = dplyr::row_number()) |>
  tidyr::pivot_longer(c("x1", "x2"), names_to = NULL, values_to = "x")
legendDot <- dplyr::tibble(x = x0 + 4 * w + 5 * s + s1 + 3 * s2, y = y)

p <- ggplot2::ggplot() +
  # exposures
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = x, y = y, colour = type, group = exposure_id),
    data = exp,
    size = 2
  ) +
  # not observation
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = x, y = y, colour = type, group = group),
    data = notObs,
    size = 3.5,
    alpha = 0.5
  ) +
  # disc
  ggplot2::geom_point(
    mapping = ggplot2::aes(x = x, y = y, colour = type),
    data = disc,
    size = 4
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = x, y = y, label = id),
    data = disc,
    size = 3.5,
    family = "Graphik",
    colour = "white"
  ) +
  # persons
  ggimage::geom_image(
    mapping = ggplot2::aes(x = x, y = y, image = "./person.png"),
    data = persons,
    inherit.aes = FALSE,
    size = 0.06
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = x, y = y, label = lab),
    data = persons,
    inherit.aes = FALSE,
    color = "white",
    nudge_y = 0.08,
    size = 2.5,
    family = "Graphik"
  ) +
  # vertical lines
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = x, y = y, group = group),
    data = dplyr::tibble(
      x = c(rep(90, 4), rep(180, 4)),
      y = rep(c(c(0, 3) + y3, c(0, 5) + y2) + 0.5, 2),
      group = rep(1:4, each = 2),
    ),
    linetype = "dashed",
    colour = "black",
    inherit.aes = FALSE
  ) +
  # tables
  ggimage::geom_image(
    mapping = ggplot2::aes(x = 460, y = 11.5, image = "./table3.png"),
    data = dplyr::tibble(),
    inherit.aes = FALSE,
    size = 0.55,
    nudge_x = 0,
    nudge_y = 0
  ) +
  ggimage::geom_image(
    mapping = ggplot2::aes(x = 460, y = 3, image = "./table4.png"),
    data = dplyr::tibble(),
    inherit.aes = FALSE,
    size = 0.55,
    nudge_x = 0,
    nudge_y = 0
  ) +
  # legend
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
    size = 3
  ) +
  ggplot2::geom_point(
    mapping = ggplot2::aes(x = x, y = y),
    data = legendDot,
    size = 4,
    colour = col4
  ) +
  # axis
  # ggplot2::geom_text(
  #   mapping = ggplot2::aes(x = 0, y = c(15, 10.5, 4), label = c("Exposures", "restrictToFirstDiscontinuation = FALSE", "restrictToFirstDiscontinuation = TRUE")),
  #   data = NULL,
  #   size = 4.5,
  #   family = "Graphik",
  #   hjust = 0
  # ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = c(-10, -10, -10, 375, 375), y = c(15, 10.5, 4, 15, 6.7), label = c("A", "B", "C", "D", "E")),
    data = NULL,
    size = 8,
    family = "Graphik",
  ) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = c(-10, 360), y = -0.05),
    data = NULL,
    colour = "black",
    inherit.aes = FALSE,
    size = 0.4
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = 350/2, y = -1.2, label = "Time (days)"),
    data = NULL,
    size = 5,
    family = "Graphik"
  ) +
  ggplot2::scale_color_manual(
    name = NULL,
    values = c("Treatment A" = col1, "Treatment B" = col2, "Treatment C" = col3, "Discontinuation" = col4, "Not in observation" = col0)
  ) +
  ggplot2::coord_cartesian(xlim = c(0, 520), ylim = c(0.75, 17), clip = "off") +
  ggplot2::scale_y_continuous(breaks = NULL, name = "") +
  ggplot2::scale_x_continuous(name = "", breaks = seq(0, 350, by = 50)) +
  ggplot2::theme(
    legend.position = "none",
    axis.line.x = ggplot2::element_line(linewidth = 0),
    legend.text = ggplot2::element_text(size = 12),
    plot.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.background = ggplot2::element_rect(fill = "white", color = NA),
    text = ggplot2::element_text(size = 14, family = "Graphik")
  )

p

ggplot2::ggsave(
  filename = "./Figures/Diagram4.png",
  plot = p,
  width = 982*3,
  height = 505*3,
  units = "px",
  dpi = 300
)
