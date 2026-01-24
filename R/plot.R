#' Create a List of Quosures for Graphics Layers
#'
#' @description
#' Helper function to capture additional ggplot2 layers as quosures. This is useful
#' for passing extra graphics elements to plotting functions.
#'
#' @param ... Additional ggplot2 layer expressions to be captured as quosures
#'
#' @return A list of quosures containing the unevaluated expressions
#' @export
graphics <- function(...) enquos(...)

#' Plot Estimated Marginal Means
#'
#' @description
#' Creates visualizations of estimated marginal means (EMMs) with confidence intervals.
#' The function supports custom styling, faceting, coloring by variables, and additional
#' ggplot2 layers for flexible, publication-ready plots.
#'
#' @param .results A named list of data frames containing EMM results. Each data frame
#'   should include columns for adjusted means and confidence intervals
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
    is_null(.layout) || is_facet_class(.layout)
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
      .f = make_plot,
      .writer = .writer,
      .extra = .extra,
      ...
    )
  } else {
    imap(.results, make_plot)
  }
}


#' Plot Pairwise Comparisons
#'
#' @description
#' Creates visualizations of pairwise comparisons with confidence intervals. Handles
#' both difference estimates and ratios, automatically detecting which type is present.
#' The user can select one or more covariate columns to form the y-axis grouping
#' (interaction of selected covariates). Supports custom styling, faceting, and
#' reference lines for easier interpretation.
#'
#' @param .results A named list of data frames containing comparison results. Each data
#'   frame should include either `estimate` or `ratio` column along with confidence intervals
#' @param .covariates A tidyselect specification of covariate columns for y-axis
#' @param ... Additional arguments (must be empty)
#' @param .color Optional tidyselect specification for coloring points by variable
#' @param .layout Optional facet specification created by `facet()`
#' @param .styles Optional styles object created by `new_styles()` for styling
#' @param .extra Optional list of quosures for additional ggplot2 layers
#' @param .writer Optional PlotOutput object created by `plot_writer()` for saving plots
#' @param .ref_line Logical indicating whether to add a reference line at 0 (for estimates)
#'   or 1 (for ratios). Default is `TRUE`
#'
#' @return If `.writer` is provided, invisibly returns saved plot paths.
#'   Otherwise, returns a list of ggplot objects.
#' @export
plot_compare <- function(
  .results,
  .covariates,
  ...,
  .color = NULL,
  .layout = NULL,
  .styles = NULL,
  .extra = NULL,
  .writer = NULL,
  .ref_line = TRUE
) {
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
    is_null(.layout) || is_facet_class(.layout)
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
      .f = make_plot,
      .writer = .writer,
      .extra = .extra,
      ...
    )
  } else {
    imap(.results, make_plot)
  }
}


#' Plot Analysis Results
#'
#' @description
#' Visualize model-adjusted results with optional raw data overlay, faceting,
#' styling, and the ability to split result sets and write output to disk.
#'
#' @param .results Named list of data frames with analysis results. Each data frame
#'   must contain an `adjusted` column and typically includes `ci_lower` and
#'   `ci_upper` for confidence intervals.
#' @param .covariates Tidyselect specification (one or more columns) used to form the x-axis.
#' @param ... Reserved for future arguments; currently unused.
#' @param .with_data Logical; if TRUE, raw data points from the original `data`
#'   attribute are plotted (default FALSE).
#' @param .without_mean Logical; if TRUE, the estimated means and their errorbars are omitted.
#' @param .layout Optional Facet object (from `facet()`) used for faceting.
#' @param .split Optional column name(s) used to split each result data frame into multiple plots.
#' @param .styles Optional Styles object from `new_styles()` to control scales and appearance.
#' @param .extra Optional list of quosures containing additional ggplot2 layers to add.
#' @param .writer Optional PlotOutput object from `plot_writer()`; when provided,
#'   plots are written to disk and file paths are returned invisibly.
#' @param .alpha Numeric alpha transparency for raw data points; default depends on `.with_data`.
#' @param .seed Integer seed used for reproducible jittering of raw points (default 12542).
#'
#' @return If `.writer` is provided, invisibly returns saved file paths; otherwise
#'   returns a named list of `ggplot` objects corresponding to `.results`.
#' @export
plot_analysis <- function(
  .results,
  .covariates,
  ...,
  .with_data = FALSE,
  .without_mean = FALSE,
  .layout = NULL,
  .split = NULL,
  .styles = NULL,
  .extra = NULL,
  .writer = NULL,
  .alpha = if (.with_data) 0.4 else 0.7,
  .seed = 12542
) {
  if (length(.results) == 0L) {
    cli_warn("No results to plot.")
    return(invisible(NULL))
  }

  covariates <- sym_names({{ .covariates }})
  x_val <- as_interaction(covariates)
  splits <- sym_names({{ .split }}, allow_null = TRUE)

  make_plot <- function(data, nm) {
    if (!all(covariates %in% names(data))) {
      cli_warn(c(
        "{.emph {nm}} is missing required variables.",
        "i" = "Required variables: {.var {covariates}}",
        "i" = "Available variables: {.var {names(data)}}"
      ))
      return(NULL)
    }
    if (!any("adjusted" == names2(data))) {
      cli_warn("{.emph {nm}} must contain an `adjusted` column.")
      return(NULL)
    }

    # Default
    y_min <- 0

    # Check and see if eitehr confidence interval goes below zero to
    if ("ci_lower" %in% names(data)) {
      if (any(data[["ci_lower"]] < 0, na.rm = TRUE)) {
        y_min <- NA # let ggplot do the work
      }
    }

    if ("ci_upper" %in% names(data)) {
      if (any(data[["ci_upper"]] < 0, na.rm = TRUE)) {
        y_min <- NA # let ggplot do the work
      }
    }

    # get response variable name from formula attribute if available
    frmla <- attr_or(data, "model_formula", default = NULL)
    df <- attr_or(data, "data", default = NULL)

    if (!is_null(frmla)) {
      response_var <- paste(deparse(f_lhs(frmla)), collapse = "")
    } else {
      response_var <- nm
    }

    # compute limits with existing data
    # This next segment could be wrapped in a check for y_min is na
    # but since it only ever sets y_min to NA if needed, it's fine as is.
    if (!is_null(df) && response_var %in% names(df) && is_true(.with_data)) {
      if (min(df[[response_var]], na.rm = TRUE) < 0) {
        y_min <- NA # let ggplot do the work
      }
    }

    p <- ggplot2::ggplot(
      data,
      ggplot2::aes(x = !!x_val, y = adjusted, color = !!x_val)
    )

    if (is_true(.with_data)) {
      if (!is_null(df) && !is_null(frmla)) {
        lhs <- sym(paste(deparse(f_lhs(frmla)), collapse = ""))
        p <- p +
          ggplot2::geom_point(
            data = df,
            ggplot2::aes(
              x = !!x_val,
              y = !!lhs,
              color = !!x_val,
              fill = !!x_val
            ),
            size = 2.4,
            alpha = .alpha,
            position = ggplot2::position_jitter(width = 0.12)
          )
      }
    }

    if (is_false(.without_mean)) {
      p <- p +
        ggplot2::geom_errorbar(
          ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
          width = 0.25,
          linewidth = 1.1,
          position = ggplot2::position_dodge2(width = 0.6, padding = 0.4)
        ) +
        ggplot2::geom_point(
          size = 4,
          stroke = 1.2
        )
    }

    if (!is_null(.styles)) {
      p <- p +
        scale_color_style(.styles, keys = {{ .covariates }}) +
        scale_x_discrete_style(.styles, keys = {{ .covariates }})

      if (is_true(.with_data)) {
        p <- p +
          scale_fill_style(.styles, keys = {{ .covariates }})
      }
    }

    if (!is_null(.layout)) {
      p <- p + as_facet_layer(.layout)
    }

    if (!is.na(y_min)) {
      p <- p + ggplot2::coord_cartesian(ylim = c(y_min, NA_real_))
    }

    p <- p +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank(),
        legend.position = "none"
      ) +
      ggplot2::labs(y = response_var, x = "Groups")

    p
  }

  # little wrapper for seeds
  plot_with_seed <- function(...) {
    withr::with_seed(.seed, make_plot(...))
  }

  if (!is_null(.writer)) {
    if (!is_null(splits)) {
      .results <- split_results(.results, splits)
    }
    map_plots(
      .results,
      .f = plot_with_seed,
      .writer = .writer,
      .extra = .extra,
      ...
    )
  } else {
    imap(.results, plot_with_seed)
  }
}

split_results <- function(.results, split) {
  out <- imap(.results, function(.x, .nm) {
    if (any(!split %in% names(.x))) {
      cli_warn(c(
        "Cannot split results: missing split variables.",
        "i" = "Required variables: {.var {split}}",
        "i" = "Available variables: {.var {names(.x)}}"
      ))
      return(.x)
    }

    # group locations and split rows
    locs <- vctrs::vec_group_loc(.x[split])
    chunks <- vctrs::vec_chop(.x, indices = locs$loc)

    # build readable group keys (keep raw keys for joining, use character keys for names)
    raw_keys <- locs$key
    keys_df <- raw_keys |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
    keys_chr <- purrr::pmap_chr(keys_df, ~ paste(c(...), collapse = "_"))
    names(chunks) <- keys_chr

    # preserve and propagate attributes
    orig_attrs <- attributes(.x)

    # copy non-row metadata (except names/row.names) to each chunk
    copy_attrs <- setdiff(names(orig_attrs), c("names", "row.names", "data"))
    chunks <- purrr::map(chunks, function(.chunk) {
      for (an in copy_attrs) {
        attr(.chunk, an) <- orig_attrs[[an]]
      }
      .chunk
    })

    # handle `data` attribute specially: subset it by the same split values
    if (!is.null(orig_attrs$data) && is.data.frame(orig_attrs$data)) {
      orig_data <- orig_attrs$data
      if (!all(split %in% names(orig_data))) {
        cli_warn(c(
          "Original `data` attribute does not contain required split variables.",
          "i" = "Required: {.var {split}}",
          "i" = "Available in `data`: {.var {names(orig_data)}}",
          "i" = "Copying full `data` attribute to each chunk."
        ))
        chunks <- purrr::map(chunks, function(.chunk) {
          attr(.chunk, "data") <- orig_data
          .chunk
        })
      } else {
        chunks <- purrr::map2(
          chunks,
          seq_len(vec_size(keys_df)),
          function(.chunk, .i) {
            key_row <- raw_keys[.i, , drop = FALSE]
            attr(.chunk, "data") <- dplyr::semi_join(
              orig_data,
              key_row,
              by = split
            )
            .chunk
          }
        )
      }
    }

    chunks
  })
  unlist(out, recursive = FALSE)
}
