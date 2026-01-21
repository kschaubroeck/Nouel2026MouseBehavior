class_facet <- new_class(
  "Facet",
  properties = list(
    type = new_property(
      class = class_character,
      validator = function(value) {
        if (!value %in% c("wrap", "grid")) "must be either 'wrap' or 'grid'"
      }
    ),
    formula = class_formula,
    nrow = class_integer | NULL,
    ncol = class_integer | NULL,
    scales = new_property(
      class = class_character,
      validator = function(value) {
        if (!value %in% c("fixed", "free", "free_x", "free_y")) {
          "must be one of: 'fixed', 'free', 'free_x', 'free_y'"
        }
      }
    ),
    space = new_property(
      class = class_character,
      validator = function(value) {
        if (!value %in% c("fixed", "free", "free_x", "free_y")) {
          "must be one of: 'fixed', 'free', 'free_x', 'free_y'"
        }
      }
    ),
    dir = new_property(
      class = class_character,
      validator = function(value) {
        if (!value %in% c("h", "v")) "must be either 'h' or 'v'"
      }
    ),
    strip_position = new_property(
      class = class_character,
      validator = function(value) {
        if (!value %in% c("top", "bottom", "left", "right")) {
          "must be one of: 'top', 'bottom', 'left', 'right'"
        }
      }
    )
  )
)

#' Check if Object is a Facet Specification
#' @param x Object to check
#' @return `TRUE` if `x` is a `Facet` object, `FALSE` otherwise
#' @export
is_facet_class <- function(x) {
  S7_inherits(x, class_facet)
}

#' Create a Facet Specification
#'
#' @description
#' User-friendly constructor for facet specifications. Specify variables by name
#' without needing to know formula syntax.
#'
#' @param ... These dots are for future extensions and must be empty
#' @param rows Variables to use for row facets (unquoted variable names)
#' @param cols Variables to use for column facets (unquoted variable names)
#' @param type Either "wrap" or "grid" for `facet_wrap()` or `facet_grid()`.
#'   Defaults to "wrap" if only rows or cols specified, "grid" if both.
#' @param nrow Number of rows (for facet_wrap)
#' @param ncol Number of columns (for facet_wrap)
#' @param scales Should scales be fixed or free? One of "fixed", "free", "free_x", "free_y"
#' @param space Should space be fixed or free? One of "fixed", "free", "free_x", "free_y"
#' @param dir Direction: "h" for horizontal, "v" for vertical (facet_wrap only)
#' @param strip_position Position of strip labels
#'
#' @return A `class_facet` object
#' @export
facet <- function(
  ...,
  rows = NULL,
  cols = NULL,
  type = NULL,
  nrow = NULL,
  ncol = NULL,
  scales = c("fixed", "free", "free_x", "free_y"),
  space = c("fixed", "free", "free_x", "free_y"),
  dir = c("h", "v"),
  strip_position = c("top", "bottom", "left", "right")
) {
  check_dots_empty0(...)

  # Validate arguments
  scales <- arg_match0(scales, c("fixed", "free", "free_x", "free_y"))
  space <- arg_match0(space, c("fixed", "free", "free_x", "free_y"))
  dir <- arg_match0(dir, c("h", "v"))
  strip_position <- arg_match0(
    strip_position,
    c("top", "bottom", "left", "right")
  )

  assert(
    "{.arg nrow} must be a positive integer.",
    is.null(nrow) || is_scalar_integerish(nrow),
    nrow > 0
  )

  assert(
    "{.arg ncol} must be a positive integer.",
    is.null(ncol) || is_scalar_integerish(ncol),
    ncol > 0
  )

  # Extract variable names
  rows_vars <- sym_names({{ rows }}, allow_null = TRUE)
  cols_vars <- sym_names({{ cols }}, allow_null = TRUE)

  # Check that at least one is specified
  assert(
    "At least one of {.arg rows} or {.arg cols} must be specified.",
    !is.null(rows_vars) || !is.null(cols_vars)
  )

  # Determine type if not specified
  type <- type %||%
    if (!is.null(rows_vars) && !is.null(cols_vars)) "grid" else "wrap"
  type <- arg_match0(type, c("wrap", "grid"))

  # Helper to build expression from variable names
  build_expr <- function(vars) {
    if (is.null(vars)) {
      NULL
    } else if (length(vars) == 1L) {
      sym(vars)
    } else {
      purrr::reduce(syms(vars), function(.x, .y) call2("+", .x, .y))
    }
  }

  # Build formula based on type
  formula <- switch(
    type,
    grid = new_formula(build_expr(rows_vars), build_expr(cols_vars)),
    wrap = new_formula(NULL, build_expr(c(rows_vars, cols_vars)))
  )

  class_facet(
    type = type,
    formula = formula,
    nrow = if (!is.null(nrow)) as.integer(nrow),
    ncol = if (!is.null(ncol)) as.integer(ncol),
    scales = scales,
    space = space,
    dir = dir,
    strip_position = strip_position
  )
}

#' Convert Object to ggplot2 Facet Layer
#'
#' @description
#' Generic function for converting facet specification objects into ggplot2 facet layers.
#' This allows seamless integration of custom facet objects with ggplot2 visualizations.
#'
#' @param x Object to convert, typically a `Facet` object created by `facet()`
#' @param ... Additional arguments passed to underlying ggplot2 facet functions
#' @return A ggplot2 facet layer (either `facet_wrap()` or `facet_grid()`) that can be
#'   added to a ggplot object
#' @export
as_facet_layer <- new_generic("as_facet_layer", "x")

method(as_facet_layer, class_facet) <- function(x, ...) {
  if (x@type == "wrap") {
    ggplot2::facet_wrap(
      x@formula,
      nrow = x@nrow,
      ncol = x@ncol,
      scales = x@scales,
      dir = x@dir,
      strip.position = x@strip_position,
      ...
    )
  } else {
    ggplot2::facet_grid(
      rows = x@formula,
      cols = NULL,
      scales = x@scales,
      space = x@space,
      switch = if (x@strip_position %in% c("bottom", "left")) x@strip_position,
      ...
    )
  }
}
