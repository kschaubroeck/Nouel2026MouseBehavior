validate_string <- function(allow_empty = FALSE) {
  force(allow_empty)
  function(value) {
    if (length(value) != 1L) {
      "must be a single string"
    } else if (!allow_empty && !nzchar(value)) {
      "must be a single non-empty string"
    }
  }
}

validate_positive_number <- function(integer_only = FALSE) {
  force(integer_only)
  function(value) {
    if (length(value) != 1L || value <= 0) {
      sprintf(
        "must be a single positive %s",
        if (integer_only) "integer" else "number"
      )
    }
  }
}

validate_choice <- function(allowed) {
  force(allowed)
  function(value) {
    if (length(value) != 1L || !(value %in% allowed)) {
      sprintf("must be one of: %s", paste(allowed, collapse = ", "))
    }
  }
}

validate_boolean <- function(value) {
  if (length(value) != 1L || is.na(value)) "must be a single boolean"
}

# Property definitions
prop_outdir <- new_property(class_character, validator = validate_string())
prop_prefix <- new_property(
  class_character,
  validator = validate_string(allow_empty = TRUE)
)
prop_overwrite <- new_property(class_logical, validator = validate_boolean)
prop_width <- new_property(class_double, validator = validate_positive_number())
prop_height <- new_property(
  class_double,
  validator = validate_positive_number()
)
prop_units <- new_property(class_character, validator = validate_string())
prop_dpi <- new_property(
  class_integer,
  validator = validate_positive_number(integer_only = TRUE)
)
prop_ext <- new_property(class_character, validator = validate_string())
prop_data_ext <- new_property(
  class_character,
  validator = validate_choice(c("csv", "tsv", "rds"))
)
prop_plot_ext <- new_property(
  class_character,
  validator = validate_choice(c("png", "jpg", "jpeg", "tiff", "webp"))
)

new_file_output <- new_class(
  "FileOutput",
  abstract = TRUE,
  properties = list(
    outdir = prop_outdir,
    prefix = prop_prefix,
    overwrite = prop_overwrite
  )
)

new_data_output <- new_class(
  "DataOutput",
  parent = new_file_output,
  properties = list(ext = prop_data_ext)
)

new_plot_output <- new_class(
  "PlotOutput",
  parent = new_file_output,
  properties = list(
    ext = prop_plot_ext,
    width = prop_width,
    height = prop_height,
    units = prop_units,
    dpi = prop_dpi
  )
)

#' Check if object is a PlotOutput
#'
#' @param x An object to test
#' @return A logical value indicating whether the object inherits from PlotOutput
#' @export
is_plot_output <- function(x) S7_inherits(x, new_plot_output)

#' Check if object is a DataOutput
#'
#' @param x An object to test
#' @return A logical value indicating whether the object inherits from DataOutput
#' @export
is_data_output <- function(x) S7_inherits(x, new_data_output)

#' Create a data writer function
#'
#' Creates a closure that generates DataOutput objects with specified default
#' settings for writing data files to disk.
#'
#' @param .outdir Output directory path (default: ".")
#' @param .ext File extension for data files: "csv", "tsv", or "rds" (default: "csv")
#' @param .overwrite Whether to overwrite existing files (default: FALSE)
#'
#' @return A function that creates DataOutput objects. The returned function accepts
#'   optional arguments: prefix, ext, and overwrite to override defaults.
#' @export
data_writer <- function(.outdir = ".", .ext = "csv", .overwrite = FALSE) {
  .ext <- arg_match0(.ext, c("csv", "tsv", "rds"))

  assert(
    "`.outdir` must be a non-empty string.",
    is_string(.outdir),
    nzchar(.outdir)
  )

  assert("`.overwrite` must be a boolean.", is_bool(.overwrite))

  function(..., prefix = NULL, ext = NULL, overwrite = NULL) {
    dots <- list(...)

    if (length(dots) > 0) {
      if (!all(map_lgl(dots, is_string))) {
        cli_abort("{.arg ...} must be zero or more path components as strings.")
      }
      outdir <- exec(fs::path, .outdir, !!!dots)
    } else {
      outdir <- .outdir
    }

    assert(
      "`prefix` must be a string.",
      is_null(prefix) || is_string(prefix)
    )

    assert(
      "`ext` must be a string.",
      is_null(ext) || is_string(ext)
    )

    assert(
      "`overwrite` must be a boolean.",
      is_null(overwrite) || is_bool(overwrite)
    )

    if (!is_null(ext)) {
      ext <- arg_match0(ext, c("csv", "tsv", "rds"))
    }

    new_data_output(
      outdir = outdir,
      prefix = prefix %||% "",
      ext = if (!is_null(ext)) {
        arg_match0(ext, c("csv", "tsv", "rds"))
      } else {
        .ext
      },
      overwrite = overwrite %||% .overwrite
    )
  }
}

#' Create a plot writer function
#'
#' Creates a closure that generates PlotOutput objects with specified default
#' settings for saving ggplot2 plots to disk.
#'
#' @param .outdir Output directory path (default: ".")
#' @param .ext File extension for plot files: "png", "jpg", "jpeg", "tiff", or "webp" (default: "png")
#' @param .width Plot width (default: 6)
#' @param .height Plot height (default: 4)
#' @param .units Units for width and height (default: "in")
#' @param .dpi Plot resolution in dots per inch (default: 300L)
#' @param .overwrite Whether to overwrite existing files (default: FALSE)
#'
#' @return A function that creates PlotOutput objects. The returned function accepts
#'   optional arguments: prefix, ext, width, height, units, dpi, and overwrite to
#'   override defaults.
#' @export
plot_writer <- function(
  .outdir = ".",
  .ext = "png",
  .width = 6,
  .height = 4,
  .units = "in",
  .dpi = 300L,
  .overwrite = FALSE
) {
  .ext <- arg_match0(.ext, c("png", "jpg", "jpeg", "tiff", "webp"))

  assert(
    "`.outdir` must be a non-empty string.",
    is_string(.outdir),
    nzchar(.outdir)
  )

  assert("`.overwrite` must be a boolean.", is_bool(.overwrite))

  assert(
    "`.width` must be a single positive number.",
    is_scalar_double(.width) || is_scalar_integer(.width),
    .width > 0
  )

  assert(
    "`.height` must be a single positive number.",
    is_scalar_double(.height) || is_scalar_integer(.height),
    .height > 0
  )

  assert(
    "`.dpi` must be a single positive integer.",
    is_scalar_integerish(.dpi),
    .dpi > 0
  )

  assert("`.units` must be a string.", is_string(.units))

  function(
    ...,
    prefix = NULL,
    ext = NULL,
    width = NULL,
    height = NULL,
    units = NULL,
    dpi = NULL,
    overwrite = NULL
  ) {
    dots <- list(...)

    if (length(dots) > 0) {
      if (!all(map_lgl(dots, is_string))) {
        cli_abort("{.arg ...} must be zero or more path components as strings.")
      }
      outdir <- exec(fs::path, .outdir, !!!dots)
    } else {
      outdir <- .outdir
    }

    assert("`prefix` must be a string.", is_null(prefix) || is_string(prefix))
    assert("`ext` must be a string.", is_null(ext) || is_string(ext))
    assert("`units` must be a string.", is_null(units) || is_string(units))

    assert(
      "`overwrite` must be a boolean.",
      is_null(overwrite) || is_bool(overwrite)
    )

    assert(
      "`width` must be a single positive number.",
      is_null(width) || (is_scalar_double(width) || is_scalar_integer(width)),
      width > 0
    )

    assert(
      "`height` must be a single positive number.",
      is_null(height) ||
        (is_scalar_double(height) || is_scalar_integer(height)),
      height > 0
    )

    assert(
      "`dpi` must be a single positive integer.",
      is_null(width) || is_scalar_integerish(dpi),
      dpi > 0
    )

    new_plot_output(
      outdir = outdir,
      prefix = prefix %||% "",
      ext = if (!is_null(ext)) {
        arg_match0(ext, c("png", "jpg", "jpeg", "tiff", "webp"))
      } else {
        .ext
      },
      width = width %||% .width,
      height = height %||% .height,
      units = units %||% .units,
      dpi = dpi %||% .dpi,
      overwrite = overwrite %||% .overwrite
    )
  }
}

# --------------

#' Save data frames to disk
#'
#' Writes a named list of data frames to disk using the specified DataOutput writer.
#' Each data frame is saved as a separate file with a name derived from the list element name.
#'
#' @param .x A named list of data frames to save
#' @param ... Additional arguments passed to the data writing function (e.g., readr::write_csv)
#' @param .writer A DataOutput object created by data_writer() specifying output settings
#'
#' @return Invisibly returns the file paths of successfully saved files
#' @export
save_data <- function(.x, ..., .writer) {
  assert(
    "`.writer` must be a DataOutput.",
    !is_null(.writer) && is_data_output(.writer)
  )

  assert("`.x` must be a named list.", is_list(.x), is_named(.x))

  # Prepare output directory and file paths
  ensure_outdir(.writer@outdir)
  out <- build_outpaths(.x, .writer@outdir, .writer@prefix, .writer@ext)

  # Write each data frame to disk
  pwalk(
    list(.x, names2(.x), out),
    function(df, nm, path) {
      # skip_if_exists prints messasges if needed
      if (skip_if_exists(path, .writer@overwrite, "file")) {
        return(invisible(path))
      }

      if (!is.data.frame(df)) {
        cli_warn("{.field {nm}} is not a data.frame; skipping.")
        return(invisible(NULL))
      }

      try_fetch(
        write_data_file(df, path, .writer@ext, ...),
        error = function(e) {
          cli_warn("Saving failed for {.field {nm}}: {conditionMessage(e)}")
          return(NULL)
        }
      )

      cli::cli_alert_success("Saved {.field {nm}} to {.path {path}}")
      invisible(path)
    }
  )
}

#' Combine multiple data files into a single file
#'
#' Reads all data files with extensions `csv`, `tsv`, or `rds` from a directory,
#' row-binds them together, and writes the combined table to the specified
#' output file. Any files that fail to read are skipped with a warning.
#'
#' @param .dir Path to a directory containing data files to combine.
#' @param .outfile Path to the output file to write. Must have extension
#'   'csv', 'tsv', or 'rds'. The output directory will be created if needed.
#' @param ... Additional arguments passed on to the appropriate `readr`
#'   reader (`read_csv`, `read_tsv`, `read_rds`) when reading input files and
#'   to `write_data_file()` when writing the combined output.
#'
#' @return Invisibly returns the combined data frame created by row-binding
#'   the readable input files.
#' @export
combine_data <- function(
  .dir,
  .outfile,
  ...,
  .adjust = c("none", "bonferroni", "BH", "fdr")
) {
  assert("`.dir` must be a string.", is_string(.dir), nzchar(.dir))
  assert("`.outfile` must be a string.", is_string(.outfile), nzchar(.outfile))
  .adjust <- arg_match0(.adjust, c("none", "bonferroni", "BH", "fdr"))

  # Ensure output directory exists and infer extension
  ensure_outdir(fs::path_dir(.outfile))
  out_ext <- tolower(fs::path_ext(.outfile))
  if (!out_ext %in% c("csv", "tsv", "rds")) {
    cli_abort("`.outfile` must have extension 'csv', 'tsv', or 'rds'.")
  }

  # List data files in directory
  files <- fs::dir_ls(
    .dir,
    regexp = "\\.(csv|tsv|rds)$",
    type = "file",
    recurse = FALSE
  )

  if (length(files) == 0L) {
    cli_abort("No data files found in {.path { .dir }}")
  }

  read_one <- function(path) {
    ext <- tolower(fs::path_ext(path))
    txt <- try_fetch(
      switch(
        ext,
        csv = readr::read_csv(path, ..., show_col_types = FALSE),
        tsv = readr::read_tsv(path, ..., show_col_types = FALSE),
        rds = readr::read_rds(path, ...),
        cli_abort("Unsupported extension: {.val {ext}}")
      ),
      error = function(e) {
        cli_warn("Failed to read {.path {path}}: {conditionMessage(e)}")
        NULL
      }
    )
    cli::cli_alert_success("Read {.path {path}} successfully.")
    txt
  }

  tbls <- purrr::compact(map(files, read_one))

  if (length(tbls) == 0L) {
    cli_abort("No files could be read from {.path { .dir }}")
  }

  combined <- dplyr::bind_rows(tbls)

  # adjust p values if needed
  if (.adjust != "none" && "p" %in% colnames(combined)) {
    combined <- combined |>
      dplyr::mutate(p_adj_all = p.adjust(.data$p, method = .adjust))
  }

  try_fetch(
    write_data_file(combined, .outfile, out_ext, ...),
    error = function(e) {
      cli_abort("Failed to save combined file: {conditionMessage(e)}")
    }
  )

  cli::cli_alert_success("Saved combined data to {.path { .outfile }}")
  invisible(combined)
}

#' Map a plotting function and save plots
#'
#' Applies a plotting function to each element of a list and saves the resulting
#' ggplot2 plots to disk using the specified PlotOutput writer.
#'
#' @param .x A named list of variables to plot
#' @param .f A function that takes a variable and its name and returns a ggplot object
#' @param ... Additional arguments passed to the plotting function .f
#' @param .writer A PlotOutput object created by plot_writer() specifying output settings
#' @param .extra Optional list of quosures to add to each plot (e.g., additional layers)
#' @param .data Optional data frame to use within the extra quosures
#'
#' @return Invisibly returns the generated plots
#' @export
map_plots <- function(.x, .f, ..., .writer, .extra = NULL, .data = NULL) {
  assert(
    "`.writer` must be a PlotOutput.",
    !is_null(.writer) && is_plot_output(.writer)
  )

  assert("`.f` must be a callable function.", is_callable(.f))
  assert("`.x` must be a named list.", is_list(.x), is_named(.x))

  assert(
    "`.data` must be NULL or a data frame.",
    is_null(.data) || is.data.frame(.data)
  )

  assert(
    "`.extra` must be NULL or a quosure.",
    is_null(.extra) || (is_list(.extra) && all(map_lgl(.extra, is_quosure)))
  )

  .f <- as_function(.f)

  # Select appropriate graphics device for file extension
  device_map <- list(
    png = ragg::agg_png,
    jpg = ragg::agg_jpeg,
    jpeg = ragg::agg_jpeg,
    tiff = ragg::agg_tiff,
    webp = ragg::agg_webp
  )

  device <- device_map[[.writer@ext]]
  if (is.null(device)) {
    cli_abort("Unsupported extension: {.val {.writer@ext}}")
  }

  ensure_outdir(.writer@outdir)
  out <- build_outpaths(.x, .writer@outdir, .writer@prefix, .writer@ext)

  # Apply plotting function and save plots
  pwalk(
    list(.x, names2(.x), out),
    function(variable, name, path) {
      # Check if file exists and skip if not overwriting
      # skip_if_exists prints messasges if needed
      if (skip_if_exists(path, .writer@overwrite, "plot")) {
        return(invisible(path))
      }

      p <- try_fetch(
        .f(variable, name, ...),
        error = function(e) {
          cli_warn("Plotting failed for {.field {name}}: {conditionMessage(e)}")
          return(NULL)
        }
      )

      if (!ggplot2::is_ggplot(p)) {
        cli_warn(
          "{.arg .f} did not return a ggplot plot for {.field {name}}."
        )
        return(invisible(NULL))
      }

      if (!is_null(.extra)) {
        model_formula <- attr_or(variable, "model_formula", NULL)
        lhs <- if (is_formula(model_formula)) f_lhs(model_formula)
        frame <- attr_or(variable, "data", NULL)
        p <- p +
          map_quos(
            .extra,
            .input = variable,
            .name = name,
            .response = deparse1(lhs),
            .lhs = lhs,
            .tbl = .data,
            .frame = frame
          )
      }

      ggplot2::ggsave(
        filename = path,
        plot = p,
        device = device,
        width = .writer@width,
        height = .writer@height,
        units = .writer@units,
        dpi = .writer@dpi
      )

      cli::cli_alert_success("Saved plot {.field {name}} to {.path {path}}")
      invisible(p)
    }
  )
}

# Utils ------------------------------------------------------------------------

ensure_outdir <- function(outdir) {
  if (!fs::dir_exists(outdir)) {
    fs::dir_create(outdir, recurse = TRUE)
  }
  invisible(NULL)
}

build_outpaths <- function(x, outdir, prefix, ext) {
  map_chr(names2(x), function(y) {
    fs::path(
      outdir,
      fs::path_sanitize(fs::path_tidy(glue("{prefix}{y}.{ext}")))
    )
  })
}

# Helper to centralize writing data frames by extension
write_data_file <- function(df, path, ext, ...) {
  switch(
    ext,
    csv = readr::write_csv(df, path, ...),
    rds = readr::write_rds(df, path, ...),
    tsv = readr::write_tsv(df, path, ...),
    cli::cli_abort("Unsupported data extension: {.val {ext}}")
  )
}

skip_if_exists <- function(path, overwrite, type = "file") {
  if (!is_true(overwrite) && fs::file_exists(path)) {
    cli::cli_alert_info("Skipping existing {type}: {.path {path}}")
    return(TRUE)
  }
  FALSE
}
