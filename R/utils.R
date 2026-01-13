is_lmer <- function(x) inherits_any(x, c("lmerMod", "lmerModLmerTest"))

does_error <- function(expr) {
  tryCatch(
    error = function(cnd) TRUE,
    {
      expr
      FALSE
    }
  )
}

is_color <- function(y) !does_error(grDevices::col2rgb(y))

map_quos <- function(.quos, ..., .quos_arg = caller_arg(.quos)) {
  dots <- dots_list(..., .named = TRUE)
  map(.quos, function(x) {
    if (!is_quosure(x)) {
      cli_abort("Each element of {.arg {.quos_arg}} must be a quosure.")
    }
    if (quo_is_null(x) || quo_is_missing(x)) {
      return(NULL)
    }
    eval_bare(
      quo_get_expr(x),
      new_environment(dots, parent = quo_get_env(x))
    )
  })
}

pull_call_syms <- function(call, error_arg = NULL, error_call = caller_call()) {
  if (!is_call(call, c("c", "list"))) {
    cli_abort(
      "{.arg {error_arg}} must be a call to {.code c()} or {.code list()}.",
      call = error_call
    )
  }
  map_chr(call_args(call), function(.x) as_string(sym(.x)))
}

sym_names <- function(
  expr,
  spec = NULL,
  ...,
  allow_null = FALSE,
  duplicates = c("error", "ignore", "remove"),
  error_arg = caller_arg(expr),
  error_call = caller_call()
) {
  # Type checking
  check_dots_empty0(...)
  duplicates <- arg_match0(duplicates, c("error", "ignore", "remove"))

  # Convert expression to character vector
  value <- quo_get_expr(enquo(expr))
  out <- switch(
    typeof(value),
    NULL = NULL,
    symbol = as_string(value),
    character = value,
    language = pull_call_syms(value, error_arg, error_call),
    cli_abort(
      "{.arg {error_arg}} must be a symbol, character vector, or a call to {.code c()} or {.code list()}.",
      call = error_call
    )
  )

  if (!allow_null && is_null(out)) {
    cli_abort("{.arg {error_arg}} must not be NULL.", call = error_call)
  }

  # Duplicate handling
  if (duplicates != "ignore" && vec_duplicate_any(out)) {
    if (duplicates == "error") {
      cli_abort(
        "{.arg {error_arg}} cannot contain duplicate values.",
        call = error_call
      )
    }
    out <- vec_unique(out)
  }

  # early return if no spec to validate against
  if (is_null(spec)) {
    return(out)
  }

  invalid <- vec_set_difference(out, spec, ptype = character())
  if (length(invalid) > 0L) {
    cli_abort("{.arg {error_arg}} contains invalid values.", call = error_call)
  }

  out
}

sets_match_exact <- function(set1, set2) {
  set1 <- vec_unique(set1)
  set2 <- vec_unique(set2)
  length(set1) == length(set2) && all(vec_in(set1, set2))
}


make_inverse_transform <- function(expr) {
  if (is_symbol(expr)) {
    # identity
    return(function(y) y)
  }

  if (!is_call(expr)) {
    return(NULL)
  }

  fname <- rlang::call_name(expr)
  switch(
    fname,
    log = function(y) exp(y),
    log10 = function(y) 10^y,
    log2 = function(y) 2^y,
    sqrt = function(y) y^2,
    function(y) NA_real_
  )
}

attr_or <- function(x, which, default = NULL) {
  out <- attr(x, which)
  if (is_null(out)) {
    return(default)
  }
  out
}
