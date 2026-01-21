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
  .weights = c("cell", "proportional", "flat", "equal"),
  .level = 0.95,
  .seed = 1234
) {
  check_dots_empty0(...)
  .scale <- arg_match0(.scale, c("response", "link"))
  .weights <- arg_match0(.weights, c("cell", "proportional", "flat", "equal"))

  assert(
    "`.results` must be a named list.",
    is_list(.results),
    is_named(.results)
  )

  assert(
    "`.seed` must be a single integerish value.",
    is_scalar_integerish(.seed)
  )

  assert(
    "`.level` must be a single numeric value between 0 and 1.",
    is_scalar_double(.level),
    .level > 0,
    .level < 1
  )

  quo_covariates <- enquo(.covariates)
  assert(
    "`.covariates` must be provided.",
    !quo_is_null(quo_covariates) && !quo_is_missing(quo_covariates)
  )

  imap(.results, function(fit, nm) {
    df <- as_tibble(stats::model.frame(fit))
    frmla <- stats::formula(fit)

    vars <- select_model_vars(quo_covariates, df, ".covariates", nm)
    if (is_null(vars)) {
      return(NULL)
    }

    # model.frame puts the entire expression as the column name, transforms
    # and all.
    lhs <- sym(paste(deparse(f_lhs(frmla)), collapse = ""))
    raw <- dplyr::summarize(
      df,
      unadjusted = mean(!!lhs, na.rm = TRUE),
      .by = dplyr::all_of(vars)
    )

    # One limitation I can deal with later: theunadjusted mean is not
    if (.scale == "response") {
      fn <- make_inverse_transform(f_lhs(frmla))
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
        vcov_mtx = stats::vcov(fit)
      ) |>
        confint(level = .level, type = .scale) |>
        as.data.frame() |>
        as_tibble() |>
        dplyr::mutate(measure = as_string(lhs)) |>
        dplyr::select(
          measure,
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

    out <- dplyr::left_join(raw, adjusted, by = vars) |>
      dplyr::relocate(measure)

    attr(out, "model_formula") <- frmla
    attr(out, "data") <- df
    out
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
  .weights = c("cell", "proportional", "flat", "equal"),
  .level = 0.95,
  .seed = 1234
) {
  check_dots_empty0(...)

  .scale <- arg_match0(.scale, c("response", "link"))
  .adjust <- arg_match0(.adjust, c("mvt", "bonferroni", "sidak", "none"))
  .weights <- arg_match0(.weights, c("cell", "proportional", "flat", "equal"))

  assert(
    "`.results` must be a named list.",
    is_list(.results),
    is_named(.results)
  )

  assert(
    "`.seed` must be a single integerish value.",
    is_scalar_integerish(.seed)
  )

  assert(
    "`.level` must be a single numeric value between 0 and 1.",
    is_scalar_double(.level),
    .level > 0,
    .level < 1
  )

  quo_covariates <- enquo(.covariates)
  quo_compare <- enquo(.compare)

  assert(
    "`.covariates` must be provided.",
    !quo_is_null(quo_covariates) && !quo_is_missing(quo_covariates)
  )

  assert(
    "`.compare` must be provided.",
    !quo_is_null(quo_compare) && !quo_is_missing(quo_compare)
  )

  imap(.results, function(fit, nm) {
    df <- as_tibble(stats::model.frame(fit))
    frmla <- stats::formula(fit)
    lhs <- sym(paste(deparse(f_lhs(frmla)), collapse = ""))

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
        vcov_mtx = stats::vcov(fit)
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

    out <- join_emm_tables(prs, interval, test) |>
      dplyr::mutate(hypothesis = factor(contrast)) |>
      dplyr::select(-contrast) |>
      mutate(measure = as_string(lhs)) |>
      dplyr::relocate(measure, hypothesis)

    attr(out, "model_formula") <- frmla
    attr(out, "data") <- df
    out
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
  .weights = c("cell", "proportional", "flat", "equal"),
  .level = 0.95,
  .seed = 1234
) {
  check_dots_empty0(...)

  .scale <- arg_match0(.scale, c("response", "link"))
  .adjust <- arg_match0(.adjust, c("mvt", "bonferroni", "sidak", "none"))
  .weights <- arg_match0(.weights, c("cell", "proportional", "flat", "equal"))

  assert(
    "`.results` must be a named list.",
    is_list(.results),
    is_named(.results)
  )

  assert(
    "`.seed` must be a single integerish value.",
    is_scalar_integerish(.seed)
  )

  assert(
    "`.level` must be a single numeric value between 0 and 1.",
    is_scalar_double(.level),
    .level > 0,
    .level < 1
  )

  quo_covariates <- enquo(.covariates)
  quo_compare <- enquo(.compare)
  quo_conditioned <- enquo(.conditioned)

  assert(
    "`.covariates` must be provided.",
    !quo_is_null(quo_covariates) && !quo_is_missing(quo_covariates)
  )

  assert(
    "`.compare` must be provided.",
    !quo_is_null(quo_compare) && !quo_is_missing(quo_compare)
  )

  assert(
    "`.conditioned` must be provided.",
    !quo_is_null(quo_conditioned) && !quo_is_missing(quo_conditioned)
  )

  imap(.results, function(fit, nm) {
    df <- as_tibble(stats::model.frame(fit))
    frmla <- stats::formula(fit)
    lhs <- sym(paste(deparse(f_lhs(frmla)), collapse = ""))

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
        vcov_mtx = stats::vcov(fit)
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

    out <- join_emm_tables(prs, interval, test) |>
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
      dplyr::mutate(measure = as_string(lhs)) |>
      dplyr::relocate(measure, hypothesis)

    attr(out, "model_formula") <- frmla
    attr(out, "data") <- df
    out
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
  attr(out, "interval") <- interval
  attr(out, "test") <- test
  out
}
