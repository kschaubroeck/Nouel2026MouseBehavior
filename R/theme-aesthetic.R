is_color <- function(y) {
  tryCatch(
    {
      grDevices::col2rgb(y)
      TRUE
    },
    error = function(e) FALSE
  )
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

normalize_shape <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x) || !is_scalar_integerish(x)) {
    return(NA_integer_)
  }
  x <- as.integer(x)
  if (x < 0L || x > 25L) {
    return(NA_integer_)
  }
  x
}

normalize_linetype <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x)) {
    return(NA_character_)
  }
  valid_types <- c(
    "blank",
    "solid",
    "dashed",
    "dotted",
    "dotdash",
    "longdash",
    "twodash"
  )
  if (is_integerish(x)) {
    xi <- as.integer(x)
    if (!xi %in% 0:6) {
      return(NA_character_)
    }
    return(valid_types[xi + 1L])
  }
  x_low <- tolower(x)
  if (!x_low %in% valid_types) {
    return(NA_character_)
  }
  x_low
}

normalize_linewidth <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x) || !is.numeric(x)) {
    return(NA_real_)
  }
  x <- as.double(x)
  if (x <= 0) {
    return(NA_real_)
  }
  x
}

normalize_label <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x) || !is_string(x)) {
    return(NA_character_)
  }
  x
}

validate_color <- function(value) {
  if (is.na(value)) {
    NULL
  } else if (length(value) != 1L) {
    "must be a single color"
  } else if (!is_color(value)) {
    "must be a valid color"
  }
}

validate_shape <- function(value) {
  if (is.na(value)) {
    NULL
  } else if (!is_scalar_integerish(value)) {
    "must be a scalar integral value"
  } else if (value < 0 || value > 25) {
    "must be between 0 and 25"
  }
}

validate_linetype <- function(value) {
  valid_types <- c(
    "blank",
    "solid",
    "dashed",
    "dotted",
    "dotdash",
    "longdash",
    "twodash"
  )
  if (is.na(value)) {
    NULL
  } else if (!is_character(value, 1L)) {
    "must be a single string"
  } else if (!value %in% valid_types) {
    sprintf("must be one of: %s", paste(valid_types, collapse = ", "))
  }
}

validate_linewidth <- function(value) {
  if (is.na(value)) {
    NULL
  } else if (!is_scalar_double(value)) {
    "must be a scalar numeric value"
  } else if (value <= 0) {
    "must be a positive number"
  }
}

validate_label <- function(value) {
  if (is.na(value)) {
    NULL
  } else if (!is_string(value)) {
    "must be a single string"
  }
}

prop_color <- new_property(
  class_character,
  validator = validate_color,
  getter = function(self) self@color,
  setter = function(self, value) {
    self@color <- normalize_color(value)
    self
  }
)

prop_shape <- new_property(
  class_integer,
  validator = validate_shape,
  getter = function(self) self@shape,
  setter = function(self, value) {
    self@shape <- normalize_shape(value)
    self
  }
)

prop_fill <- new_property(
  class_character,
  validator = validate_color,
  getter = function(self) self@fill,
  setter = function(self, value) {
    self@fill <- normalize_color(value)
    self
  }
)

prop_linewidth <- new_property(
  class_double,
  validator = validate_linewidth,
  getter = function(self) self@linewidth,
  setter = function(self, value) {
    self@linewidth <- normalize_linewidth(value)
    self
  }
)

prop_linetype <- new_property(
  class_character,
  validator = validate_linetype,
  getter = function(self) self@linetype,
  setter = function(self, value) {
    self@linetype <- normalize_linetype(value)
    self
  }
)

prop_label <- new_property(
  class_character,
  validator = validate_label,
  getter = function(self) self@label,
  setter = function(self, value) {
    self@label <- normalize_label(value)
    self
  }
)

construct_aesthetic <- function(
  ...,
  shape = NULL,
  color = NULL,
  colour = NULL,
  fill = NULL,
  linewidth = NULL,
  linetype = NULL,
  label = NULL
) {
  check_dots_empty0(...)
  final_color <- if (!is.null(color)) color else colour
  final_fill <- if (!is.null(fill)) fill else final_color
  new_object(
    S7_object(),
    shape = shape,
    color = final_color,
    fill = final_fill,
    linewidth = linewidth,
    linetype = linetype,
    label = label
  )
}

aesthetic <- new_class(
  "Aesthetic",
  properties = list(
    color = prop_color,
    shape = prop_shape,
    fill = prop_fill,
    linewidth = prop_linewidth,
    linetype = prop_linetype,
    label = prop_label
  ),
  constructor = construct_aesthetic
)

is_aesthetic <- function(x) S7_inherits(x, aesthetic)
