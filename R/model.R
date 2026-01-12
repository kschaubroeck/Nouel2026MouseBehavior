responses <- function(...) {
  dots <- enexprs(
    ...,
    .named = TRUE,
    .ignore_empty = "all",
    .ignore_null = "all"
  )
  assert("At least one response must be provided.", length(dots) > 0L)
  dots
}

formulas <- function(y, responses, env = caller_env()) {
  rhs <- enexpr(y)

  assert(
    "{.arg {caller_arg(env)}} must be an environment",
    is_environment(env)
  )

  assert(
    "{.arg {caller_arg(y)}} must be a symbol or expression",
    is_symbol(rhs) || is_call(rhs)
  )

  assert(
    "{.arg {caller_arg(responses)}} must be NULL or a named list of calls",
    is_null(responses) || (is_list(responses) && is_named(responses))
  )

  if (is_null(responses)) {
    return(list(new_formula(lhs = NULL, rhs = rhs, env = call)))
  }

  assert(
    "Each response in {.arg {caller_arg(responses)}} must be a symbol or call",
    all(purrr::map_lgl(responses, function(.x) is_symbol(.x) || is_call(.x)))
  )

  map(responses, function(expr) new_formula(lhs = expr, rhs = rhs, env = env))
}

models <- function(
  .formulas,
  .f = stats::lm,
  ...,
  .data_arg = "data",
  .formula_arg = "formula"
) {
  func <- enexpr(.f)

  assert(
    "{.arg {caller_arg(.formulas)}} must be a named list with at least 1 formula.",
    is_named(.formulas),
    length(.formulas) > 0L
  )

  assert(
    "{.arg {caller_arg(.data_arg)}} must be a non-empty string.",
    is_string(.data_arg),
    nzchar(.data_arg)
  )

  assert(
    "{.arg {caller_arg(.formula_arg)}} must be a non-empty string.",
    is_string(.formula_arg),
    nzchar(.formula_arg)
  )

  assert(
    "{.arg {caller_arg(.f)}} must be a callable function.",
    is_callable(.f)
  )

  data_arg <- as_string(ensym(.data_arg))
  formula_arg <- as_string(ensym(.formula_arg))

  temp <- call2(
    func,
    !!data_arg := expr(.data),
    !!formula_arg := NULL,
    !!!exprs(...)
  )

  map(.formulas, function(formula) call_modify(temp, !!formula_arg := formula))
}


fits <- function(.data, .models, ..., .env = caller_env()) {
  check_dots_empty0(...)

  assert(
    "{.arg {caller_arg(.models)}} must be a named list with at least 1 model.",
    is_list(.models),
    is_named(.models),
    length(.models) > 0L
  )

  assert(
    "{.arg {caller_arg(.data)}} must be a data frame.",
    is.data.frame(.data)
  )

  assert(
    "{.arg {caller_arg(.env)}} must be an environment.",
    is_environment(.env)
  )

  mask <- new_environment(list(.data = .data), parent = .env)

  fit <- function(model, nm) {
    if (is_null(model)) {
      return(NULL)
    }
    cli::cli_alert_info("Fitting model {.field {nm}}")
    eval_bare(model, mask)
  }

  err <- function(e, mode, nm) {
    cli_warn("Model {.field {nm}} failed to fit: {conditionMessage(e)}")
    NULL
  }

  f <- function(...) {
    try_fetch(fit(...), error = function(e, ...) err(e, ...))
  }

  purrr::compact(imap(.models, f))
}
