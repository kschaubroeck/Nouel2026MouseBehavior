graphics <- function(...) enquos(...)

plot_emm <- function(
  .results,
  .covariates,
  ...,
  .color_by = NULL,
  .size = 3,
  .bar_width = 0.1
) {
  if (length(.results) == 0L) {
    cli_warn("No results to plot.")
    return(invisible(NULL))
  }

  quo_covariates <- enquo(.covariates)
  quo_color_by <- enquo(.color_by)

  if (quo_is_null(quo_covariates) || quo_is_missing(quo_covariates)) {
    cli_abort("{.arg {.covariates}} must be specified to plot EMM results.")
  }

  make_plot <- function(data, nm) {
    cols <- names(tidyselect::eval_select(quo_covariates, data)) |>
      map(~ sym(.x))

    lvls <- eval_tidy(purrr::reduce(cols, ~ call2(":", .x, .y)), data = data)

    lvls_fac <- factor(lvls, levels = unique(lvls))

    if (quo_is_null(quo_color_by) || quo_is_missing(quo_color_by)) {
      p <- ggplot2::ggplot(
        data,
        ggplot2::aes(x = lvls_fac, y = adjusted)
      )
      color_scale <- NULL
    } else {
      color_cols <- names(tidyselect::eval_select(quo_color_by, data)) |>
        map(~ sym(.x))
      colors <- eval_tidy(
        purrr::reduce(color_cols, ~ call2(":", .x, .y)),
        data = data
      )
      p <- ggplot2::ggplot(
        data,
        ggplot2::aes(x = lvls_fac, y = adjusted, color = colors)
      )
      color_names <- names(tidyselect::eval_select(quo_color_by, data))
      color_scale <- ggplot2::scale_color_brewer(
        palette = "Set2",
        name = paste(color_names, collapse = ", ")
      )
    }

    dodge <- ggplot2::position_dodge(width = 0.6)

    p <- p +
      ggplot2::geom_point(size = .size, position = dodge) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
        width = .2,
        position = dodge
      ) +
      ggplot2::labs(
        x = paste(
          names(tidyselect::eval_select(quo_covariates, data)),
          collapse = " \u00d7 "
        ),
        y = "Adjusted Mean"
      ) +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      )

    if (!is.null(color_scale)) {
      p <- p + color_scale
    }

    p
  }

  map_plots(.results, make_plot, ...)
}
