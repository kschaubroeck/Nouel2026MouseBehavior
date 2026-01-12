VALID_METHODS <- c("HC0", "HC1", "HC2", "HC3", "HC4", "HC4m", "HC5", "CR2")

prop_cluster <- new_property(
  class_character | NULL,
  validator = function(value) {
    if (!is_null(value) && !is_string(value)) "must be a string or NULL"
  }
)

prop_method <- new_property(
  class_character | NULL,
  validator = function(value) {
    if (!is_null(value) && !value %in% VALID_METHODS) {
      sprintf(
        "method must be one of %s or NULL, not %s",
        paste(VALID_METHODS, collapse = ", "),
        value
      )
    }
  },
  getter = function(self) self@method %||% "HC3",
  setter = function(self, value) {
    if (is_null(value)) {
      self@method <- NULL
    } else {
      self@method <- arg_match0(value, VALID_METHODS)
    }
    self
  }
)

class_vcov_spec <- new_class(
  "VcovSpec",
  properties = list(cluster = prop_cluster, method = prop_method),
  validator = function(self) {
    if (is_null(self@cluster)) {
      if (is_null(self@method)) {
        "at least one of @cluster or @method must be provided"
      } else if (self@method == "CR2") {
        "@cluster must be provided when @method is 'CR2'"
      }
    } else {
      if (!is_null(self@method) && self@method != "CR2") {
        "@method must be 'CR2' when @cluster is provided"
      }
    }
  }
)

#' Robust Variance-Covariance Specification
#'
#' Create a specification for robust variance-covariance matrix estimation.
#' Supports heteroscedasticity-consistent (HC) and cluster-robust (CR2) methods.
#'
#' @param ... Must be empty. Exists to force named arguments.
#' @param cluster Optional cluster variable name (string or symbol) for cluster-robust
#'   standard errors. When provided, automatically uses CR2 method.
#' @param method Optional heteroscedasticity-consistent method. Must be one of:
#'   "HC0", "HC1", "HC2", "HC3" (default), "HC4", "HC4m", or "HC5".
#'   Cannot be specified when `cluster` is provided.
#' @param .call Internal use only. The calling environment for error reporting.
#'
#' @return A `VcovSpec` S7 object containing the variance-covariance specification.
#'
#' @export
robust <- function(..., cluster = NULL, method = NULL, .call = caller_env()) {
  check_dots_empty0(...)

  if (!is_null(method)) {
    method <- arg_match0(
      method,
      c("HC0", "HC1", "HC2", "HC3", "HC4", "HC4m", "HC5"),
      error_call = .call
    )
  }

  cluster <- enquo(cluster)
  if (quo_is_null(cluster)) {
    cluster <- NULL
  } else if (quo_is_symbol(cluster)) {
    cluster <- as_string(quo_get_expr(cluster))
  } else {
    expr <- quo_get_expr(cluster)
    if (is_string(expr)) {
      cluster <- expr
    } else {
      cli_abort("{.arg {arg}} must be a string, symbol, or NULL.", call = .call)
    }
  }

  if (!is_null(cluster) && !is_null(method)) {
    cli_abort(
      "`method` cannot be specified when `cluster` is provided.",
      call = .call
    )
  }

  if (!is_null(cluster)) {
    method <- "CR2"
  }

  class_vcov_spec(cluster = cluster, method = method)
}

method(convert, list(class_vcov_spec, class_double)) <-
  function(from, to, fit, data, use_lmer, call = caller_env()) {
    if (use_lmer) {
      if (!is_string(from@method, "CR2")) {
        cli_abort("Only 'CR2' is supported for lmer models.", call = call)
      }
      if (!from@cluster %in% colnames(data)) {
        cli_abort(
          "Cluster variable {.val {from@cluster}} not found in data.",
          call = call
        )
      }
      return(clubSandwich::vcovCR(
        fit,
        cluster = data[[from@cluster]],
        type = from@method
      ))
    }

    sandwich::vcovHC(fit, type = from@method)
  }

get_vcov <- function(fit, spec, data, call = caller_env()) {
  if (is_null(spec)) {
    stats::vcov(fit)
  } else if (S7_inherits(spec, class_vcov_spec)) {
    convert(spec, class_double, fit, data, is_lmer(fit), call = call)
  } else {
    cli_abort(
      "{.arg {caller_arg(spec)}} must be NULL or a {.cls {class_vcov_spec}} object.",
      call = call
    )
  }
}

# Estimate Functions -----------------------------------------------------------

#' Estimate Marginal Means
#'
#' Compute estimated marginal means (EMMs) from fitted model results, including
#' both unadjusted means and adjusted means with confidence intervals.
#'
#' @param .results A named list of fitted model objects.
#' @param .covariates A tidyselect specification of covariate columns to compute
#'   means over.
#' @param ... Must be empty. Exists to force named arguments.
#' @param .scale Scale for reporting results. Either "response" (default) for
#'   the original response scale, or "link" for the link function scale.
#' @param .weights Method for weighting cells. One of "proportional" (default),
#'   "cell", or "equal".
#' @param .level Confidence level for intervals (default: 0.95).
#' @param .seed Random seed for reproducibility (default: 1234).
#' @param .vcov Optional variance-covariance specification created by `robust()`.
#'   If NULL, uses model's default vcov.
#'
#' @return A named list with the same names as `.results`, where each element is
#'   a tibble containing unadjusted means, adjusted means, and confidence intervals,
#'   or NULL if estimation failed.
#'
#' @export
emms <- function(
  .results,
  .covariates,
  ...,
  .scale = c("response", "link"),
  .weights = c("proportional", "cell", "equal"),
  .level = 0.95,
  .seed = 1234,
  .vcov = NULL
) {
  check_dots_empty0(...)
  .scale <- arg_match0(.scale, c("response", "link"))
  .weights <- arg_match0(.weights, c("proportional", "cell", "equal"))

  assert(
    "{.arg {caller_arg(.results)}} must be a named list.",
    is_list(.results),
    is_named(.results)
  )

  assert(
    "{.arg {caller_arg(.seed)}} must be a single integerish value.",
    is_scalar_integerish(.seed)
  )

  assert(
    "{.arg {caller_arg(.level)}} must be a single numeric value between 0 and 1.",
    is_scalar_double(.level),
    .level > 0,
    .level < 1
  )

  assert(
    "{.arg {caller_arg(.vcov)}} must be NULL or a {.cls {class_vcov_spec}} object.",
    S7_inherits(.vcov, class_vcov_spec) || is_null(.vcov)
  )

  quo_covariates <- enquo(.covariates)
  assert(
    "{.arg {caller_arg(.covariates)}} must be provided.",
    !quo_is_null(quo_covariates) && !quo_is_missing(quo_covariates)
  )

  imap(.results, function(fit, nm) {
    df <- as_tibble(stats::model.frame(fit))

    vars <- select_model_vars(quo_covariates, df, ".covariates", nm)
    if (is_null(vars)) {
      return(NULL)
    }

    lhs <- sym(paste(deparse(f_lhs(stats::formula(fit))), collapse = ""))
    raw <- dplyr::summarize(
      df,
      unadjusted = mean(!!lhs, na.rm = TRUE),
      .by = dplyr::all_of(vars)
    )

    # One limitation I can deal with later: theunadjusted mean is not
    if (.scale == "response") {
      fn <- make_inverse_transform(f_lhs(stats::formula(fit)))
      if (!is_null(fn)) {
        raw <- raw |> dplyr::mutate(unadjusted = fn(unadjusted))
      } else {
        cli_warn(
          c(
            "No inverse transformation available for {.field {nm}}; ",
            "unadjusted means reported on link scale."
          )
        )
      }
    }

    if (is_null(raw)) {
      cli_warn("Mean estimation failed for {.field {nm}}: no data available.")
      return(NULL)
    }

    adjusted <- try_fetch(
      emm_estimate(
        fit = fit,
        covariates = vars,
        weights = .weights,
        vcov_mtx = get_vcov(fit, .vcov, df, call = caller_env())
      ) |>
        confint(level = .level, type = .scale) |>
        as.data.frame() |>
        as_tibble() |>
        dplyr::select(
          dplyr::all_of(vars),
          adjusted = dplyr::any_of(c("emmean", "response")),
          ci_lower = lower.CL,
          ci_upper = upper.CL
        ),
      error = function(e) {
        cli_warn(
          "Mean estimation failed for {.field {nm}}: {conditionMessage(e)}"
        )
        NULL
      }
    )

    if (is_null(adjusted)) {
      return(NULL)
    }

    dplyr::left_join(raw, adjusted, by = vars)
  })
}

#' Compare Estimated Marginal Means
#'
#' Perform pairwise comparisons of estimated marginal means across levels of
#' a specified factor, with adjustment for multiple comparisons.
#'
#' @param .results A named list of fitted model objects.
#' @param .compare A tidyselect specification of the factor to compare across.
#' @param .covariates A tidyselect specification of covariate columns.
#' @param ... Must be empty. Exists to force named arguments.
#' @param .scale Scale for comparisons. Either "response" (default) or "link".
#' @param .adjust Method for p-value adjustment. One of "mvt" (default,
#'   multivariate t), "bonferroni", "sidak", or "none".
#' @param .weights Method for weighting cells. One of "proportional" (default),
#'   "cell", or "equal".
#' @param .level Confidence level for intervals (default: 0.95).
#' @param .seed Random seed for reproducibility (default: 1234).
#' @param .vcov Optional variance-covariance specification created by `robust()`.
#'   If NULL, uses model's default vcov.
#'
#' @return A named list with the same names as `.results`, where each element is
#'   a tibble containing pairwise comparisons with estimates, confidence intervals,
#'   and p-values, or NULL if comparison failed.
#'
#' @export
compare <- function(
  .results,
  .compare,
  .covariates,
  ...,
  .scale = c("response", "link"),
  .adjust = c("mvt", "bonferroni", "sidak", "none"),
  .weights = c("proportional", "cell", "equal"),
  .level = 0.95,
  .seed = 1234,
  .vcov = NULL
) {
  check_dots_empty0(...)

  .scale <- arg_match0(.scale, c("response", "link"))
  .adjust <- arg_match0(.adjust, c("mvt", "bonferroni", "sidak", "none"))
  .weights <- arg_match0(.weights, c("proportional", "cell", "equal"))

  assert(
    "{.arg {caller_arg(.results)}} must be a named list.",
    is_list(.results),
    is_named(.results)
  )

  assert(
    "{.arg {caller_arg(.seed)}} must be a single integerish value.",
    is_scalar_integerish(.seed)
  )

  assert(
    "{.arg {caller_arg(.level)}} must be a single numeric value between 0 and 1.",
    is_scalar_double(.level),
    .level > 0,
    .level < 1
  )

  assert(
    "{.arg {caller_arg(.vcov)}} must be NULL or a {.cls {class_vcov_spec}} object.",
    S7_inherits(.vcov, class_vcov_spec) || is_null(.vcov)
  )

  quo_covariates <- enquo(.covariates)
  quo_compare <- enquo(.compare)

  assert(
    "{.arg {caller_arg(.covariates)}} must be provided.",
    !quo_is_null(quo_covariates) && !quo_is_missing(quo_covariates)
  )

  assert(
    "{.arg {caller_arg(.compare)}} must be provided.",
    !quo_is_null(quo_compare) && !quo_is_missing(quo_compare)
  )

  imap(.results, function(fit, nm) {
    df <- as_tibble(stats::model.frame(fit))

    covariates <- select_model_vars(quo_covariates, df, ".covariates", nm)
    compare <- select_model_vars(quo_compare, df, ".compare", nm)

    if (is_null(covariates) || is_null(compare)) {
      return(NULL)
    }

    emm <- try_fetch(
      emm_estimate(
        fit = fit,
        covariates = unique(c(covariates, compare)),
        weights = .weights,
        vcov_mtx = get_vcov(fit, .vcov, df, call = caller_env())
      ),
      error = function(e) {
        cli_warn(
          "Comparison failed for {.field {nm}}: {conditionMessage(e)}"
        )
        NULL
      }
    )

    if (is_null(emm)) {
      return(NULL)
    }

    prs <- pairs(emm, simple = compare, type = .scale)
    interval <- confint(prs, type = .scale, adjust = "none")
    test <- withr::with_seed(
      .seed,
      emmeans::test(prs, type = .scale, by = NULL, adjust = .adjust)
    )

    join_emm_tables(prs, interval, test) |>
      dplyr::mutate(hypothesis = factor(contrast)) |>
      dplyr::select(-contrast) |>
      dplyr::relocate(hypothesis)
  })
}

#' Compare Differences in Estimated Marginal Means
#'
#' Perform conditional pairwise comparisons (differences of differences) to test
#' whether the effect of one factor differs across levels of another factor.
#'
#' @param .results A named list of fitted model objects.
#' @param .compare A tidyselect specification of the factor to compare across.
#' @param .conditioned A tidyselect specification of the conditioning factor.
#' @param .covariates A tidyselect specification of covariate columns.
#' @param ... Must be empty. Exists to force named arguments.
#' @param .scale Scale for comparisons. Either "response" (default) or "link".
#' @param .adjust Method for p-value adjustment. One of "mvt" (default,
#'   multivariate t), "bonferroni", "sidak", or "none".
#' @param .weights Method for weighting cells. One of "proportional" (default),
#'   "cell", or "equal".
#' @param .level Confidence level for intervals (default: 0.95).
#' @param .seed Random seed for reproducibility (default: 1234).
#' @param .vcov Optional variance-covariance specification created by `robust()`.
#'   If NULL, uses model's default vcov.
#'
#' @return A named list with the same names as `.results`, where each element is
#'   a tibble containing conditional comparisons with estimates, confidence intervals,
#'   and p-values, or NULL if comparison failed.
#'
#' @export
compare_by <- function(
  .results,
  .compare,
  .conditioned,
  .covariates,
  ...,
  .scale = c("response", "link"),
  .adjust = c("mvt", "bonferroni", "sidak", "none"),
  .weights = c("proportional", "cell", "equal"),
  .level = 0.95,
  .seed = 1234,
  .vcov = NULL
) {
  check_dots_empty0(...)

  .scale <- arg_match0(.scale, c("response", "link"))
  .adjust <- arg_match0(.adjust, c("mvt", "bonferroni", "sidak", "none"))
  .weights <- arg_match0(.weights, c("proportional", "cell", "equal"))

  assert(
    "{.arg {caller_arg(.results)}} must be a named list.",
    is_list(.results),
    is_named(.results)
  )

  assert(
    "{.arg {caller_arg(.seed)}} must be a single integerish value.",
    is_scalar_integerish(.seed)
  )

  assert(
    "{.arg {caller_arg(.level)}} must be a single numeric value between 0 and 1.",
    is_scalar_double(.level),
    .level > 0,
    .level < 1
  )

  assert(
    "{.arg {caller_arg(.vcov)}} must be NULL or a {.cls {class_vcov_spec}} object.",
    S7_inherits(.vcov, class_vcov_spec) || is_null(.vcov)
  )

  quo_covariates <- enquo(.covariates)
  quo_compare <- enquo(.compare)
  quo_conditioned <- enquo(.conditioned)

  assert(
    "{.arg {caller_arg(.covariates)}} must be provided.",
    !quo_is_null(quo_covariates) && !quo_is_missing(quo_covariates)
  )

  assert(
    "{.arg {caller_arg(.compare)}} must be provided.",
    !quo_is_null(quo_compare) && !quo_is_missing(quo_compare)
  )

  assert(
    "{.arg {caller_arg(.conditioned)}} must be provided.",
    !quo_is_null(quo_conditioned) && !quo_is_missing(quo_conditioned)
  )

  imap(.results, function(fit, nm) {
    df <- as_tibble(stats::model.frame(fit))

    covariates <- select_model_vars(quo_covariates, df, ".covariates", nm)
    compare <- select_model_vars(quo_compare, df, ".compare", nm)
    conditioned <- select_model_vars(quo_conditioned, df, ".conditioned", nm)

    if (is_null(covariates) || is_null(conditioned) || is_null(compare)) {
      return(NULL)
    }

    emm <- try_fetch(
      emm_estimate(
        fit = fit,
        covariates = unique(c(covariates, conditioned, compare)),
        weights = .weights,
        vcov_mtx = get_vcov(fit, .vcov, df, call = caller_env())
      ),
      error = function(e) {
        cli_warn(
          "Difference estimation failed for {.field {nm}}: {conditionMessage(e)}"
        )
        NULL
      }
    )

    if (is_null(emm)) {
      return(NULL)
    }

    prs0 <- pairs(emm, simple = compare, type = .scale)
    prs <- pairs(prs0, simple = conditioned, type = .scale)
    interval <- confint(prs, type = .scale, adjust = "none")
    test <- withr::with_seed(
      .seed,
      emmeans::test(prs, by = NULL, adjust = .adjust)
    )

    join_emm_tables(prs, interval, test) |>
      tidyr::separate_wider_regex(
        contrast1,
        patterns = c(a = "^[^ ]+", symbol = " [^ ]+ ", b = "[^ ]+$")
      ) |>
      dplyr::mutate(
        hypothesis = as.character(glue(
          "({contrast} | {a}) {trimws(symbol)} ({contrast} | {b})"
        ))
      ) |>
      dplyr::mutate(hypothesis = factor(hypothesis)) |>
      dplyr::select(-a, -symbol, -b, -contrast) |>
      dplyr::relocate(hypothesis)
  })
}

# Helper Functions --------------------------------------------------------

# Select and validate variables from model
select_model_vars <- function(quo, df, arg_name, model_name) {
  vars <- names(tidyselect::eval_select(quo, df))
  if (length(vars) == 0L) {
    cli_warn(
      "No variables selected in {.arg {arg_name}} for {.field {model_name}}."
    )
    return(NULL)
  }
  vars
}

emm_estimate <- function(fit, covariates, weights, vcov_mtx) {
  terms <- purrr::map(covariates, ~ sym(.x))
  if (length(terms) == 0L) {
    cli_abort("At least one covariate must be specified for EMM estimation.")
  }
  f <- new_formula(lhs = NULL, rhs = purrr::reduce(terms, ~ call2("*", .x, .y)))
  args <- list(
    object = fit,
    specs = f,
    weights = weights,
    vcov. = vcov_mtx,
    lmer.df = if (is_lmer(fit)) "satterthwaite"
  )
  exec(emmeans::emmeans, !!!purrr::compact(args))
}

join_emm_tables <- function(prs, interval, test) {
  lvls <- names(levels(prs))

  interval <- as.data.frame(interval)
  test <- as.data.frame(test)
  prs <- as.data.frame(prs)

  diff <- setdiff(lvls, c(names(interval), names(test), names(prs)))
  if (length(diff) > 0L) {
    msg <- "Column names of emmeans results do not match expected levels."
    cli_abort(
      c(
        msg,
        i = "Unexpected columns: {.field {paste(diff, collapse = ', ')}}"
      ),
      .internal = TRUE
    )
  }

  out <- dplyr::left_join(
    prs |> dplyr::rename(p = p.value),
    dplyr::left_join(interval, test, by = lvls) |>
      dplyr::rename(p_adj = p.value),
    by = lvls
  ) |>
    dplyr::select(
      dplyr::all_of(lvls),
      dplyr::any_of(c("estimate", "ratio")),
      ci_lower = lower.CL,
      ci_upper = upper.CL,
      p,
      p_adj,
      dplyr::any_of("df")
    ) |>
    as_tibble()

  attr(out, "emm") <- prs
  out
}
