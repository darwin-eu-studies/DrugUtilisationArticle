
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
  2, 113, 250, "A", 1,
  2, 205, 300, "B", -1,
  2, 301, 340, "C", 0,
  2, 341, 350, "A", 0,
  3, 150, 210, "C", 1,
  3, 190, 245, "A", -1
) |>
  dplyr::mutate(exposure_id = dplyr::row_number(), type = paste0("Treatment ", type))

notObs <- dplyr::tibble(
  person_id = 5, start = 325, end =
)

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
    y = person_id + y1 + offset * off,
    id = dplyr::row_number(),
    type = "Discontinuation"
  ) |>
  dplyr::select("x", "y", "id", "type")
disc2 <- exposures |>
  dplyr::filter(type == "Treatment A") |>
  dplyr::arrange(person_id, -end) |>
  dplyr::mutate(
    x = 0,
    offset = dplyr::if_else(person_id == 3, 0, offset),
    id = dplyr::row_number(),
    y = id + y2 + offset * off,
    type = "Discontinuation"
  ) |>
  dplyr::select("x", "y", "id", "type")

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

ggplot2::ggplot() +
  # initial exposures
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = x, y = y, colour = type, group = exposure_id),
    data = exp,
    size = 2
  ) +
  ggplot2::geom_point(
    mapping = ggplot2::aes(x = x, y = y, colour = type),
    data = disc1,
    size = 4
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = x, y = y, label = id),
    data = disc1,
    size = 3.5,
    family = "Graphik",
    colour = "white"
  ) +
  # restrictToFirstDiscontinuation = FALSE
  ggplot2::geom_point(
    mapping = ggplot2::aes(x = x, y = y, colour = type),
    data = disc2,
    size = 4
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = x, y = y, label = id),
    data = disc2,
    size = 3.5,
    family = "Graphik",
    colour = "white"
  ) +
  # axis
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = 0, y = c(15, 10.5, 4), label = c("Exposures", "restrictToFirstDiscontinuation = FALSE", "restrictToFirstDiscontinuation = TRUE")),
    data = NULL,
    size = 4.5,
    family = "Graphik",
    hjust = 0
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(x = -10, y = c(15, 10.5, 4), label = c("A", "B", "C")),
    data = NULL,
    size = 8,
    family = "Graphik",
  ) +
  # ggplot2::geom_line(
  #   mapping = ggplot2::aes(x = c(-3, 53), y = -0.55),
  #   data = NULL,
  #   colour = "black",
  #   inherit.aes = FALSE,
  #   size = 0.4
  # ) +
  # ggplot2::geom_text(
  #   mapping = ggplot2::aes(x = 25, y = -1.5, label = "Time (days)"),
  #   data = NULL,
  #   size = 5,
  #   family = "Graphik"
  # ) +
  ggplot2::scale_color_manual(
    name = NULL,
    values = c("Treatment A" = col1, "Treatment B" = col2, "Treatment C" = col3, "Discontinuation" = col4, "Not in observation" = col0)
  ) +
  ggplot2::coord_cartesian(xlim = c(0, 500), ylim = c(0.75, 15.5), clip = "off") +
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
  filename = "./Figures/Diagram4.png",
  plot = p,
  width = 974*3,
  height = 426*3,
  units = "px",
  dpi = 300
)
