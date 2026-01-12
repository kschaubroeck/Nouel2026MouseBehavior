graphics <- function(...) enquos(...)


#' Plot Estimated Marginal Means
#'
#' Creates visualizations of estimated marginal means with error bars,
#' integrating style themes, faceting, and custom graphics layers.
#'
#' @param .results A named list of data frames containing EMM results
#' @param .covariates A tidyselect specification of covariate columns for x-axis
#' @param ... Additional arguments (must be empty)
#' @param .color Optional tidyselect specification for coloring points by variable
#' @param .layout Optional facet specification created by `facet()`
#' @param .styles Optional styles object created by `new_styles()` for styling
#' @param .extra Optional list of quosures for additional ggplot2 layers
#' @param .writer Optional PlotOutput object created by `plot_writer()` for saving plots
#'
#' @return If `.writer` is provided, invisibly returns saved plot paths.
#'   Otherwise, returns a list of ggplot objects.
#' @export
plot_emm <- function(
  .results,
  .covariates,
  ...,
  .color = NULL,
  .layout = NULL,
  .styles = NULL,
  .extra = NULL,
  .writer = NULL
) {
  check_dots_empty0(...)

  if (length(.results) == 0L) {
    cli_warn("No results to plot.")
    return(invisible(NULL))
  }

  quo_covariates <- enquo(.covariates)
  quo_color <- enquo(.color)

  assert(
    "{.arg .covariates} must be specified to plot EMM results.",
    !(quo_is_null(quo_covariates) || quo_is_missing(quo_covariates))
  )

  assert(
    "{.arg .layout} must be a Facet object or NULL.",
    is_null(.layout) || is_facet(.layout)
  )

  assert(
    "{.arg .styles} must be a Styles object or NULL.",
    is_null(.styles) || is_styles(.styles)
  )

  assert(
    "{.arg .extra} must be a list of quosures or NULL.",
    is_null(.extra) || is_list(.extra)
  )

  assert(
    "{.arg .writer} must be a PlotOutput object or NULL.",
    is_null(.writer) || is_plot_output(.writer)
  )

  make_plot <- function(data, nm) {
    # Select covariate columns for x-axis and build interaction (handle single)
    covariate_names <- names(tidyselect::eval_select(quo_covariates, data))
    cols <- map(covariate_names, ~ sym(.x))

    if (length(cols) == 1L) {
      lvls <- eval_tidy(cols[[1]], data = data)
    } else {
      lvls <- eval_tidy(purrr::reduce(cols, ~ call2(":", .x, .y)), data = data)
    }
    lvls_fac <- factor(lvls, levels = unique(lvls))

    # Base plot setup with or without color aesthetic; support single or multiple color cols
    if (quo_is_null(quo_color) || quo_is_missing(quo_color)) {
      p <- ggplot2::ggplot(
        data,
        ggplot2::aes(y = lvls_fac, x = adjusted)
      )
    } else {
      # Color by specific variable(s)
      color_names <- names(tidyselect::eval_select(quo_color, data))
      color_cols <- map(color_names, ~ sym(.x))

      if (length(color_cols) == 1L) {
        colors <- eval_tidy(color_cols[[1]], data = data)
      } else {
        colors <- eval_tidy(
          purrr::reduce(color_cols, ~ call2(":", .x, .y)),
          data = data
        )
      }

      colors_fac <- factor(colors, levels = unique(colors))
      p <- ggplot2::ggplot(
        data,
        ggplot2::aes(y = lvls_fac, x = adjusted, color = colors_fac)
      )

      # Add the theme/styles/colors
      if (!is_null(.styles)) {
        p <- p +
          scale_color_style(.styles, keys = !!quo_color)
      }
    }

    # Add the theme/styles/colors
    if (!is_null(.styles)) {
      p <- p +
        scale_y_discrete_style(.styles, keys = !!quo_covariates)
    }

    p <- p +
      ggplot2::geom_point(size = 3) +
      ggplot2::geom_errorbar(
        ggplot2::aes(xmin = ci_lower, xmax = ci_upper),
        width = 0.2
      ) +
      ggplot2::labs(
        y = "Variables",
        x = "Adjusted Mean"
      )

    # Apply faceting if specified
    if (!is_null(.layout)) {
      p <- p + as_facet_layer(.layout)
    }

    # Apply default theme
    p <- p +
      ggplot2::theme_bw(base_size = 13) +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        legend.position = "none"
      )

    p
  }

  # Save plots if writer provided, otherwise just generate them
  if (!is_null(.writer)) {
    map_plots(
      .results,
      make_plot,
      .writer = .writer,
      .extra = .extra,
      .data = NULL
    )
  } else {
    imap(.results, make_plot)
  }
}


#' Plot Pairwise Comparisons
#'
#' Creates visualizations of pairwise comparisons (estimates with CIs),
#' where the user can select one or more covariate columns to form the
#' y-axis grouping (interaction of selected covariates).
#'
#' @param .results A named list of data frames containing comparison results
#' @param .covariates A tidyselect specification of covariate columns for y-axis
#' @param ... Additional arguments (must be empty)
#' @param .color Optional tidyselect specification for coloring points by variable
#' @param .layout Optional facet specification created by `facet()`
#' @param .styles Optional styles object created by `new_styles()` for styling
#' @param .extra Optional list of quosures for additional ggplot2 layers
#' @param .writer Optional PlotOutput object created by `plot_writer()` for saving plots
#'
#' @return If `.writer` is provided, invisibly returns saved plot paths.
#'   Otherwise, returns a list of ggplot objects.
#' @export
plot_compare <- function(
  .results,
  .covariates,
  ...,
  .color = .covariates,
  .layout = NULL,
  .styles = NULL,
  .extra = NULL,
  .writer = NULL,
  .ref_line = TRUE
) {
  check_dots_empty0(...)

  if (length(.results) == 0L) {
    cli_warn("No results to plot.")
    return(invisible(NULL))
  }

  quo_covariates <- enquo(.covariates)
  quo_color <- enquo(.color)

  assert(
    "{.arg .covariates} must be specified to plot comparison results.",
    !(quo_is_null(quo_covariates) || quo_is_missing(quo_covariates))
  )

  assert(
    "{.arg .layout} must be a Facet object or NULL.",
    is_null(.layout) || is_facet(.layout)
  )

  assert(
    "{.arg .styles} must be a Styles object or NULL.",
    is_null(.styles) || is_styles(.styles)
  )

  assert(
    "{.arg .extra} must be a list of quosures or NULL.",
    is_null(.extra) || is_list(.extra)
  )

  assert(
    "{.arg .writer} must be a PlotOutput object or NULL.",
    is_null(.writer) || is_plot_output(.writer)
  )

  assert(
    "{.arg .ref_line} must be a single logical value.",
    is_bool(.ref_line)
  )

  make_plot <- function(data, nm) {
    covariate_names <- names(tidyselect::eval_select(quo_covariates, data))
    cols <- map(covariate_names, ~ sym(.x))

    # build interaction levels robustly for 1+ covariates
    if (length(cols) == 1L) {
      lvls <- eval_tidy(cols[[1]], data = data)
    } else {
      lvls <- eval_tidy(purrr::reduce(cols, ~ call2(":", .x, .y)), data = data)
    }
    lvls_fac <- factor(lvls, levels = unique(lvls))

    # Determine which x-variable to use (`estimate` preferred, fallback to `ratio`)
    assert(
      "Comparison data must contain either `estimate` or `ratio` column.",
      any(c("estimate", "ratio") %in% names(data))
    )

    xvar <- if ("estimate" %in% names(data)) "estimate" else "ratio"
    x_label <- if (xvar == "estimate") "Estimate" else "Ratio"

    if (quo_is_null(quo_color) || quo_is_missing(quo_color)) {
      p <- ggplot2::ggplot(
        data,
        ggplot2::aes(y = lvls_fac, x = !!sym(xvar))
      )
    } else {
      color_names <- names(tidyselect::eval_select(quo_color, data))
      color_cols <- map(color_names, ~ sym(.x))

      if (length(color_cols) == 1L) {
        colors <- eval_tidy(color_cols[[1]], data = data)
      } else {
        colors <- eval_tidy(
          purrr::reduce(color_cols, ~ call2(":", .x, .y)),
          data = data
        )
      }

      colors_fac <- factor(colors, levels = unique(colors))
      p <- ggplot2::ggplot(
        data,
        ggplot2::aes(y = lvls_fac, x = !!sym(xvar), color = colors_fac)
      )

      if (!is_null(.styles)) {
        p <- p + scale_color_style(.styles, keys = !!quo_color)
      }
    }

    if (!is_null(.styles)) {
      p <- p + scale_y_discrete_style(.styles, keys = !!quo_covariates)
    }

    p <- p +
      ggplot2::geom_point(size = 3) +
      ggplot2::geom_errorbar(
        ggplot2::aes(xmin = ci_lower, xmax = ci_upper),
        width = 0.2
      ) +
      ggplot2::labs(
        y = "Variables",
        x = x_label
      )

    if (.ref_line) {
      threshold <- if (xvar == "estimate") 0 else 1
      p <- p +
        ggplot2::geom_vline(
          xintercept = threshold,
          linetype = "dashed",
          color = "grey50"
        )
    }

    if (!is_null(.layout)) {
      p <- p + as_facet_layer(.layout)
    }

    p <- p +
      ggplot2::theme_bw(base_size = 13) +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        legend.position = "none"
      )

    p
  }

  if (!is_null(.writer)) {
    map_plots(
      .results,
      make_plot,
      .writer = .writer,
      .extra = .extra,
      .data = NULL
    )
  } else {
    imap(.results, make_plot)
  }
}
