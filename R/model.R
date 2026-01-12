#' Create Response Variable Expressions
#'
#' Creates a named list of response variable expressions for use in model formulas.
#' All arguments must be named and will be captured as expressions.
#'
#' @param ... Named expressions representing response variables. At least one
#'   response must be provided.
#'
#' @return A named list of unevaluated expressions.
#'
#' @export
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

#' Create Model Formulas from Response Variables
#'
#' Constructs a list of model formulas by combining response variable expressions
#' with a common right-hand side (predictor) expression.
#'
#' @param y An expression representing the right-hand side (predictors) of the
#'   formula. Must be a symbol or expression.
#' @param responses A named list of expressions (from `responses()`) or NULL.
#'   If NULL, creates a single formula with no left-hand side.
#' @param env Environment in which to evaluate the formulas. Defaults to the
#'   caller's environment.
#'
#' @return A named list of formula objects, one for each response variable.
#'
#' @export
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

#' Create Model Call Expressions
#'
#' Generates a list of unevaluated model call expressions from formulas and a
#' modeling function (e.g., `lm`, `glm`). These calls can be evaluated later
#' with actual data.
#'
#' @param .formulas A named list of formula objects (from `formulas()`).
#' @param .f A modeling function to use. Defaults to `stats::lm`.
#' @param ... Additional arguments passed to the modeling function.
#' @param .data_arg Name of the data argument in the modeling function.
#'   Defaults to "data".
#' @param .formula_arg Name of the formula argument in the modeling function.
#'   Defaults to "formula".
#'
#' @return A named list of unevaluated call expressions, one for each formula.
#'
#' @export
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


#' Fit Statistical Models to Data
#'
#' Evaluates model call expressions with actual data, fitting each model and
#' returning the results. Models that fail to fit will generate a warning and
#' be excluded from the results.
#'
#' @param .data A data frame containing the variables referenced in the models.
#' @param .models A named list of model call expressions (from `models()`).
#' @param ... Additional arguments (currently unused; reserved for future use).
#' @param .env Environment in which to evaluate the model calls. Defaults to
#'   the caller's environment.
#'
#' @return A named list of fitted model objects. Models that failed to fit are
#'   automatically removed from the list.
#'
#' @export
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
