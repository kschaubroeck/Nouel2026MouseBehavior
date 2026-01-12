# Constants ------------------------------------------------------------------

SHAPE_MIN <- 0L
SHAPE_MAX <- 25L

VALID_LINETYPES <- c(
  "blank",
  "solid",
  "dashed",
  "dotted",
  "dotdash",
  "longdash",
  "twodash"
)

LINETYPE_NUMERIC_RANGE <- 0:6

# Aesthetic validation and normalization functions -------------------------

make_normalizer <- function(na_value, type_check, transform) {
  force(na_value)
  force(type_check)
  force(transform)
  function(x) {
    if (is.null(x) || length(x) != 1L || is.na(x) || !type_check(x)) {
      na_value
    } else {
      transform(x)
    }
  }
}

make_validator <- function(...) {
  checks <- list(...)
  force(checks)

  function(value) {
    if (is.na(value)) {
      return(NULL)
    }
    for (check in checks) {
      result <- check(value)
      if (!is.null(result)) return(result)
    }
    NULL
  }
}

normalize_color <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x)) {
    return(NA_character_)
  }
  rgb_mat <- tryCatch(
    grDevices::col2rgb(x, alpha = TRUE),
    error = function(e) NULL
  )
  if (is.null(rgb_mat)) {
    return(NA_character_)
  }
  r <- rgb_mat[1, 1]
  g <- rgb_mat[2, 1]
  b <- rgb_mat[3, 1]
  if (nrow(rgb_mat) >= 4) {
    a <- rgb_mat[4, 1]
    sprintf("#%02X%02X%02X%02X", r, g, b, a)
  } else {
    sprintf("#%02X%02X%02X", r, g, b)
  }
}

normalize_shape <- make_normalizer(
  na_value = NA_integer_,
  type_check = is_scalar_integerish,
  transform = function(x) {
    x <- as.integer(x)
    if (x < SHAPE_MIN || x > SHAPE_MAX) NA_integer_ else x
  }
)

normalize_linetype <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x)) {
    return(NA_character_)
  }
  if (is_integerish(x)) {
    xi <- as.integer(x)
    if (!xi %in% LINETYPE_NUMERIC_RANGE) {
      return(NA_character_)
    }
    return(VALID_LINETYPES[xi + 1L])
  }
  x_low <- tolower(x)
  if (!x_low %in% VALID_LINETYPES) NA_character_ else x_low
}

normalize_linewidth <- make_normalizer(
  na_value = NA_real_,
  type_check = is.numeric,
  transform = function(x) {
    x <- as.double(x)
    if (x <= 0) NA_real_ else x
  }
)

normalize_label <- make_normalizer(
  na_value = NA_character_,
  type_check = is_string,
  transform = identity
)

validate_color <- make_validator(
  function(value) if (length(value) != 1L) "must be a single color",
  function(value) if (!is_color(value)) "must be a valid color"
)

validate_shape <- make_validator(
  function(value) {
    if (!is_scalar_integerish(value)) "must be a scalar integral value"
  },
  function(value) {
    if (value < SHAPE_MIN || value > SHAPE_MAX) {
      sprintf("must be between %d and %d", SHAPE_MIN, SHAPE_MAX)
    }
  }
)

validate_linetype <- make_validator(
  function(value) if (!is_character(value, 1L)) "must be a single string",
  function(value) {
    if (!value %in% VALID_LINETYPES) {
      sprintf("must be one of: %s", paste(VALID_LINETYPES, collapse = ", "))
    }
  }
)

validate_linewidth <- make_validator(
  function(value) {
    if (!is_scalar_double(value)) "must be a scalar numeric value"
  },
  function(value) if (value <= 0) "must be a positive number"
)

validate_label <- make_validator(
  function(value) if (!is_string(value)) "must be a single string"
)

# Aesthetics -------------------------------------------------------------------

new_aesthetic_prop <- function(nm, class_type, validate, norm_fn) {
  new_property(
    class_type,
    validator = validate,
    getter = function(self) prop(self, nm),
    setter = function(self, value) {
      prop(self, nm) <- norm_fn(value)
      self
    }
  )
}

#' Aesthetic
#'
#' S7 class describing graphical aesthetics used in styles.
#'
#' @return An object of class `Aesthetic`.
#' @export
aesthetic <- new_class(
  "Aesthetic",
  properties = list(
    shape = new_aesthetic_prop(
      "shape",
      class_integer,
      validate_shape,
      normalize_shape
    ),
    color = new_aesthetic_prop(
      "color",
      class_character,
      validate_color,
      normalize_color
    ),
    fill = new_aesthetic_prop(
      "fill",
      class_character,
      validate_color,
      normalize_color
    ),
    linewidth = new_aesthetic_prop(
      "linewidth",
      class_double,
      validate_linewidth,
      normalize_linewidth
    ),
    linetype = new_aesthetic_prop(
      "linetype",
      class_character,
      validate_linetype,
      normalize_linetype
    ),
    label = new_aesthetic_prop(
      "label",
      class_character,
      validate_label,
      normalize_label
    )
  ),
  constructor = function(
    ...,
    shape = NA_integer_,
    color = NULL,
    colour = NULL,
    fill = color,
    linewidth = NA_real_,
    linetype = NA_character_,
    label = NA_character_
  ) {
    check_dots_empty0(...)
    if (!is.null(colour)) {
      color <- colour
    }
    new_object(
      S7_object(),
      shape = shape,
      color = color,
      fill = fill,
      linewidth = linewidth,
      linetype = linetype,
      label = label
    )
  }
)

is_aesthetic <- function(x) S7_inherits(x, aesthetic)

# Style ------------------------------------------------------------------------

prop_fct <- new_property(
  class_character,
  validator = function(value) {
    if (length(value) == 0L) {
      "must have at least one element."
    } else if (vec_duplicate_any(value)) {
      "must not have duplicate elements."
    }
  }
)

prop_lvls <- new_property(
  class_list,
  validator = function(value) {
    if (!every(value, is_character)) "each member must be a character vector."
  }
)

prop_aesthetics <- new_property(
  class_list,
  validator = function(value) {
    if (!every(value, is_aesthetic)) "must be a list of aesthetic objects."
  }
)

validate_style <- function(self) {
  if (any(lengths(self@levels) != length(self@factors))) {
    "each member of @levels must have the same length as @factors"
  }
}

new_style <- new_class(
  "Style",
  properties = list(
    aesthetics = prop_aesthetics,
    factors = prop_fct,
    levels = prop_lvls
  ),
  validator = validate_style,
  constructor = function(factors, aesthetics, env) {
    if (!is_call(aesthetics, c("c", "list"))) {
      cli_abort("The aesthetics argument must be a collection of aesthetics.")
    }

    aesthetics <- call_args(aesthetics)
    factors <- split_colon(factors)

    if (!every(aesthetics, function(.x) is_call(.x, "~"))) {
      cli_abort("Each element of the collection must be a formula.")
    }

    if (vec_duplicate_any(factors)) {
      cli_abort("Factors in a style must be unique.")
    }

    lvls <- map(aesthetics, function(.x) split_colon(f_lhs(.x)))
    values <- map(aesthetics, function(.x) eval_tidy(f_rhs(.x), env = env))

    if (!every(lvls, function(.x) length(.x) == length(factors))) {
      cli_abort("Each aesthetic formula must have the same number of factors.")
    }

    if (!every(values, is_aesthetic)) {
      cli_abort("Each values must be an {.cls aesthetic} object.")
    }

    new_object(
      S7_object(),
      aesthetics = values,
      factors = factors,
      levels = lvls
    )
  }
)

is_style <- function(x, factors = NULL) {
  quo_fct <- enquo(factors)
  if (!S7_inherits(x, new_style)) {
    FALSE
  } else if (!quo_is_null(quo_fct)) {
    factors <- sym_names(
      {{ factors }},
      allow_null = FALSE,
      duplicates = "error"
    )
    sets_match_exact(factors, x@factors)
  } else {
    TRUE
  }
}

method(names, new_style) <- function(x) {
  map_chr(x@levels, function(.lvls) paste(.lvls, collapse = ":"))
}

method(length, new_style) <- function(x) length(x@levels)

method(sort, new_style) <- function(x, decreasing = FALSE, ..., by) {
  if (!is_false(decreasing)) {
    cli_abort("Sorting a style with `decreasing` is not supported.")
  }
  by <- sym_names({{ by }}, allow_null = FALSE, duplicates = "error")
  if (!sets_match_exact(by, x@factors)) {
    cli_abort("The {.arg by} argument must match the factors in the style.")
  }
  i <- vec_match(by, x@factors)
  props(x) <- list(
    levels = map(x@levels, function(.levels) .levels[i]),
    factors = x@factors[i]
  )
  x
}

method(labels, new_style) <- function(object, ..., by = NULL, sep = ":") {
  by_quo <- enquo(by)
  if (!quo_is_null(by_quo)) {
    object <- sort(object, by = {{ by }})
  }
  vals <- map_chr(object@aesthetics, function(a) a@label)
  nms <- map_chr(object@levels, function(.lvls) paste(.lvls, collapse = sep))
  set_names(vals, nms)
}

# Helper to reduce duplication in convert methods
convert_style_to_vec <- function(from, prop, by = NULL, call = caller_env()) {
  by_quo <- enquo(by)
  if (!quo_is_null(by_quo)) {
    from <- sort(from, by = !!by_quo)
  }
  set_names(aesthetic_prop_vec(from@aesthetics, prop), names(from))
}

method(convert, list(new_style, class_character)) <-
  function(
    from,
    to,
    prop = c("color", "colour", "fill", "linetype"),
    by = NULL
  ) {
    prop <- arg_match0(prop, c("color", "colour", "fill", "linetype"))
    convert_style_to_vec(from, prop, {{ by }})
  }

method(convert, list(new_style, class_integer)) <-
  function(from, to, prop = "shape", by = NULL) {
    prop <- arg_match0(prop, "shape")
    convert_style_to_vec(from, prop, {{ by }})
  }

method(convert, list(new_style, class_double)) <-
  function(from, to, prop = "linewidth", by = NULL) {
    prop <- arg_match0(prop, "linewidth")
    convert_style_to_vec(from, prop, {{ by }})
  }

# Helper to extract a property from aesthetic objects
aesthetic_prop_vec <- function(aesthetics, prop) {
  switch(
    prop,
    color = map_chr(aesthetics, function(a) a@color),
    colour = map_chr(aesthetics, function(a) a@colour),
    fill = map_chr(aesthetics, function(a) a@fill),
    linetype = map_chr(aesthetics, function(a) a@linetype),
    shape = map_int(aesthetics, function(a) a@shape),
    linewidth = map_dbl(aesthetics, function(a) a@linewidth),
    cli_abort("Unknown {.arg prop}: {prop}")
  )
}

split_colon <- function(expr) {
  if (is_symbol(expr)) {
    as_string(expr)
  } else if (is_call(expr, ":")) {
    c(split_colon(expr[[2L]]), split_colon(expr[[3L]]))
  } else {
    cli_abort(
      "Expression must be a symbol or a series of colon-separated symbols."
    )
  }
}

# Theme class -----------------------------------------------------------------

#' Create a collection of styles
#'
#' @param ... Style formulas, each specified as `factors ~ aesthetics`
#'
#' @return A styles object containing the specified style definitions
#' @export
new_styles <- new_class(
  "Styles",
  properties = list(
    values = new_property(
      class_list,
      validator = function(value) {
        if (!every(value, is_style)) "must be a list of lists of style objects."
      }
    )
  ),
  constructor = function(...) {
    new_object(
      S7_object(),
      values = map(enquos(...), function(quo) {
        expr <- quo_get_expr(quo)
        env <- quo_get_env(quo)
        if (!is_call(expr, "~")) {
          cli_abort("Each argument must be a formula.")
        }
        lhs <- f_lhs(expr)
        rhs <- f_rhs(expr)
        new_style(lhs, rhs, env)
      })
    )
  }
)

#' Check if an object is a styles collection
#'
#' @param x An object to test
#'
#' @return A logical scalar indicating whether `x` is a styles object
#' @export
is_styles <- function(x) S7_inherits(x, new_styles)

find_style <- function(x, by) {
  by_quo <- enquo(by)
  match <- purrr::detect(x, function(s) is_style(s, !!by_quo))
  if (is_null(match)) {
    cli_abort("No style found matching the specified factors.")
  }
  match
}

# Theme scale functions -------------------------------------------------------

make_scale <- function(prop, target_class, scale_fn) {
  function(theme, keys, ...) {
    assert(
      "{.arg {caller_arg(theme)}} must be a {.cls class(class_styles)} object.",
      is_styles(theme)
    )
    keys_quo <- enquo(keys)
    p <- find_style(theme@values, !!keys_quo)
    if (!quo_is_null(keys_quo)) {
      p <- sort(p, by = !!keys_quo)
    }
    values <- convert(p, target_class, prop = prop)
    scale_fn(
      values = values,
      breaks = names(p),
      labels = labels(p),
      ...
    )
  }
}

make_axis_scale <- function(scale_fn) {
  function(theme, keys, ...) {
    assert(
      "{.arg {caller_arg(theme)}} must be a {.cls class(class_styles)} object.",
      is_styles(theme)
    )
    keys_quo <- enquo(keys)
    p <- find_style(theme@values, !!keys_quo)
    if (!quo_is_null(keys_quo)) {
      p <- sort(p, by = !!keys_quo)
    }
    scale_fn(labels = labels(p), ...)
  }
}

#' Apply color scale from a style theme
#'
#' @param theme A styles object
#' @param keys Factor names to match and sort by
#' @param ... Additional arguments passed to `ggplot2::scale_color_manual()`
#'
#' @return A ggplot2 scale object
#' @export
scale_color_style <-
  make_scale("color", class_character, ggplot2::scale_color_manual)

#' Apply colour scale from a style theme
#'
#' @param theme A styles object
#' @param keys Factor names to match and sort by
#' @param ... Additional arguments passed to `ggplot2::scale_color_manual()`
#'
#' @return A ggplot2 scale object
#' @export
scale_colour_style <- scale_color_style

#' Apply fill scale from a style theme
#'
#' @param theme A styles object
#' @param keys Factor names to match and sort by
#' @param ... Additional arguments passed to `ggplot2::scale_fill_manual()`
#'
#' @return A ggplot2 scale object
#' @export
scale_fill_style <-
  make_scale("fill", class_character, ggplot2::scale_fill_manual)

#' Apply shape scale from a style theme
#'
#' @param theme A styles object
#' @param keys Factor names to match and sort by
#' @param ... Additional arguments passed to `ggplot2::scale_shape_manual()`
#'
#' @return A ggplot2 scale object
#' @export
scale_shape_style <-
  make_scale("shape", class_integer, ggplot2::scale_shape_manual)

#' Apply linetype scale from a style theme
#'
#' @param theme A styles object
#' @param keys Factor names to match and sort by
#' @param ... Additional arguments passed to `ggplot2::scale_linetype_manual()`
#'
#' @return A ggplot2 scale object
#' @export
scale_linetype_style <-
  make_scale("linetype", class_character, ggplot2::scale_linetype_manual)

#' Apply discrete x-axis scale from a style theme
#'
#' @param theme A styles object
#' @param keys Factor names to match and sort by
#' @param ... Additional arguments passed to `ggplot2::scale_x_discrete()`
#'
#' @return A ggplot2 scale object
#' @export
scale_x_discrete_style <- make_axis_scale(ggplot2::scale_x_discrete)

#' Apply discrete y-axis scale from a style theme
#'
#' @param theme A styles object
#' @param keys Factor names to match and sort by
#' @param ... Additional arguments passed to `ggplot2::scale_y_discrete()`
#'
#' @return A ggplot2 scale object
#' @export
scale_y_discrete_style <- make_axis_scale(ggplot2::scale_y_discrete)
