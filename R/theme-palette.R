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

validate_palette <- function(self) {
  if (any(lengths(self@levels) != length(self@factors))) {
    "each member of @levels must have the same length as @factors"
  }
}

new_palette <- new_class(
  "Palette",
  properties = list(
    aesthetics = prop_aesthetics,
    factors = prop_fct,
    levels = prop_lvls
  ),
  validator = validate_palette,
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
      cli_abort("Factors in a palette must be unique.")
    }

    lvls <- map(aesthetics, function(.x) split_colon(f_lhs(.x)))
    values <- map(aesthetics, function(.x) eval_tidy(f_rhs(.x), env = env))

    if (!every(lvls, function(.x) length(.x) == length(factors))) {
      cli_abort("Each aesthetic formula must have the same number of factors.")
    }

    if (!every(values, is_aesthetic)) {
      cli_abort("Each values must be an {.cls aesthetic} object.")
    }

    S7::new_object(
      S7_object(),
      aesthetics = values,
      factors = factors,
      levels = lvls
    )
  }
)

is_palette <- function(x, factors = NULL) {
  quo_fct <- enquo(factors)
  if (!S7_inherits(x, new_palette)) {
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

method(names, new_palette) <- function(x) {
  map_chr(x@levels, function(.lvls) paste(.lvls, collapse = ":"))
}

method(length, new_palette) <- function(x) length(x@levels)

method(sort, new_palette) <- function(x, decreasing = FALSE, ..., by) {
  if (!is_false(decreasing)) {
    cli_abort("Sorting a palette with `decreasing` is not supported.")
  }
  by <- sym_names({{ by }}, allow_null = FALSE, duplicates = "error")
  if (!sets_match_exact(by, x@factors)) {
    cli_abort("The {.arg by} argument must match the factors in the palette.")
  }
  i <- vec_match(by, x@factors)
  props(x) <- list(
    levels = map(x@levels, function(.levels) .levels[i]),
    factors = x@factors[i]
  )
  x
}

method(labels, new_palette) <- function(object, ..., by = NULL, sep = ":") {
  by_quo <- enquo(by)
  if (!quo_is_null(by_quo)) {
    object <- sort(object, by = {{ by }})
  }
  vals <- map_chr(object@aesthetics, function(a) a@label)
  nms <- map_chr(object@levels, function(.lvls) paste(.lvls, collapse = sep))
  set_names(vals, nms)
}

method(convert, list(new_palette, class_character)) <-
  function(
    from,
    to,
    prop = c("color", "colour", "fill", "linetype"),
    by = NULL
  ) {
    prop <- arg_match0(prop, c("color", "colour", "fill", "linetype"))
    by_quo <- enquo(by)
    if (!quo_is_null(by_quo)) {
      from <- sort(from, by = !!by_quo)
    }
    set_names(aesthetic_prop_vec(from@aesthetics, prop), names(from))
  }

method(convert, list(new_palette, class_integer)) <-
  function(from, to, prop = "shape", by = NULL) {
    prop <- arg_match0(prop, "shape")
    by_quo <- enquo(by)
    if (!quo_is_null(by_quo)) {
      from <- sort(from, by = !!by_quo)
    }
    set_names(aesthetic_prop_vec(from@aesthetics, prop), names(from))
  }

method(convert, list(new_palette, class_double)) <-
  function(from, to, prop = "linewidth", by = NULL) {
    prop <- arg_match0(prop, "linewidth")
    by_quo <- enquo(by)
    if (!quo_is_null(by_quo)) {
      from <- sort(from, by = !!by_quo)
    }
    set_names(aesthetic_prop_vec(from@aesthetics, prop), names(from))
  }

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
