construct_theme <- function(...) {
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
      new_palette(lhs, rhs, env)
    })
  )
}

prop_theme_values <- new_property(
  class_list,
  validator = function(value) {
    if (!every(value, is_palette)) {
      "must be a list of lists of palette objects."
    }
  }
)

new_theme <- new_class(
  "Theme",
  properties = list(values = prop_theme_values),
  constructor = construct_theme
)

is_theme <- function(x) S7_inherits(x, new_theme)

find_palette <- function(x, by) {
  by_quo <- enquo(by)
  match <- purrr::detect(x, function(pal) is_palette(pal, !!by_quo))
  if (is_null(match)) {
    cli_abort("No palette found matching the specified factors.")
  }
  match
}

scale_color_theme <- function(theme, keys, ...) {
  assert(
    "{.arg {caller_arg(theme)}} must be a {.cls Theme} object.",
    is_theme(theme)
  )
  keys_quo <- enquo(keys)
  p <- find_palette(theme@values, !!keys_quo)
  if (!quo_is_null(keys_quo)) {
    p <- sort(p, by = !!keys_quo)
  }
  colors <- convert(p, class_character, prop = "color")
  ggplot2::scale_color_manual(
    values = colors,
    breaks = names(p),
    labels = labels(p),
    ...
  )
}

scale_colour_theme <- scale_color_theme

scale_fill_theme <- function(theme, keys, ...) {
  assert(
    "{.arg {caller_arg(theme)}} must be a {.cls Theme} object.",
    is_theme(theme)
  )
  keys_quo <- enquo(keys)
  p <- find_palette(theme@values, !!keys_quo)
  if (!quo_is_null(keys_quo)) {
    p <- sort(p, by = !!keys_quo)
  }
  fills <- convert(p, class_character, prop = "fill")
  ggplot2::scale_fill_manual(
    values = fills,
    breaks = names(p),
    labels = labels(p),
    ...
  )
}

scale_shape_theme <- function(theme, keys, ...) {
  assert(
    "{.arg {caller_arg(theme)}} must be a {.cls Theme} object.",
    is_theme(theme)
  )
  keys_quo <- enquo(keys)
  p <- find_palette(theme@values, !!keys_quo)
  if (!quo_is_null(keys_quo)) {
    p <- sort(p, by = !!keys_quo)
  }
  shapes <- convert(p, class_integer, prop = "shape")
  ggplot2::scale_shape_manual(
    values = shapes,
    breaks = names(p),
    labels = labels(p),
    ...
  )
}

scale_linetype_theme <- function(theme, keys, ...) {
  assert(
    "{.arg {caller_arg(theme)}} must be a {.cls Theme} object.",
    is_theme(theme)
  )
  keys_quo <- enquo(keys)
  p <- find_palette(theme@values, !!keys_quo)
  if (!quo_is_null(keys_quo)) {
    p <- sort(p, by = !!keys_quo)
  }
  lts <- convert(p, class_character, prop = "linetype")
  ggplot2::scale_linetype_manual(
    values = lts,
    breaks = names(p),
    labels = labels(p),
    ...
  )
}

scale_x_discrete_theme <- function(theme, keys, ...) {
  assert(
    "{.arg {caller_arg(theme)}} must be a {.cls Theme} object.",
    is_theme(theme)
  )
  keys_quo <- enquo(keys)
  p <- find_palette(theme@values, !!keys_quo)
  if (!quo_is_null(keys_quo)) {
    p <- sort(p, by = !!keys_quo)
  }
  ggplot2::scale_x_discrete(labels = labels(p), ...)
}

scale_y_discrete_theme <- function(theme, keys, ...) {
  assert(
    "{.arg {caller_arg(theme)}} must be a {.cls Theme} object.",
    is_theme(theme)
  )
  keys_quo <- enquo(keys)
  p <- find_palette(theme@values, !!keys_quo)
  if (!quo_is_null(keys_quo)) {
    p <- sort(p, by = !!keys_quo)
  }
  ggplot2::scale_y_discrete(labels = labels(p), ...)
}
