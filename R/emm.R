# Estimate Functions -------------------------------------------------------
robust <- function(..., cluster = NULL, method = NULL, .call = caller_env()) {
  check_dots_empty0(...)

  cluster_quo <- parse_cluster_quo(
    enquo(cluster),
    arg = "cluster",
    call = .call
  )

  if (!is_null(cluster) && !is_null(method)) {
    msg <- "`method` cannot be specified when `cluster` is provided."
    cli_abort(msg, call = .call)
  }

  if (!is_null(method)) {
    method <- arg_match0(
      method,
      c("HC0", "HC1", "HC2", "HC3", "HC4", "HC4m", "HC5"),
      error_call = .call
    )
  }

  if (!is_null(cluster)) {
    method <- "CR2"
  }

  structure(
    list(cluster = cluster, method = method, type = "robust"),
    class = "vcov_spec"
  )
}

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

  quo_covariates <- enquo(.covariates)
  assert(
    "{.arg {caller_arg(.covariates)}} must be provided.",
    !quo_is_null(quo_covariates),
    !quo_is_missing(quo_covariates)
  )

  imap(.results, function(fit, nm) {
    .data <- as_tibble(stats::model.frame(fit))

    vars <- names(tidyselect::eval_select(quo_covariates, .data))
    if (!check_selected_vars(vars, ".covariates", nm)) {
      return(NULL)
    }

    lhs <- sym(deparse(f_lhs(stats::formula(fit))))
    raw <- dplyr::summarize(
      .data,
      unadjusted = mean(!!lhs, na.rm = TRUE),
      .by = dplyr::all_of(vars)
    )

    if (is_null(raw)) {
      cli_warn("Mean estimation failed for {.field {nm}}: no data available.")
      return(NULL)
    }

    vcov_mtx <- vcov_from_spec(fit, .vcov, .data, is_lmer(fit))

    # marginal means
    adjusted <- try_fetch(
      emm_estimate(
        fit = fit,
        covariates = vars,
        weights = .weights,
        vcov_mtx = vcov_mtx
      ) |>
        confint(level = .level, type = .scale) |>
        as.data.frame() |>
        dplyr::select(
          dplyr::all_of(vars),
          adjusted = dplyr::any_of(c("emmean", "response")),
          ci_lower = lower.CL,
          ci_upper = upper.CL
        ) |>
        as_tibble(),
      error = function(e) {
        msg <- "Mean estimation failed for {.field {nm}}: {conditionMessage(e)}"
        cli_warn(msg)
        return(NULL)
      }
    )

    if (is_null(adjusted)) {
      cli_warn("Mean estimation failed for {.field {nm}}.")
      return(NULL)
    }

    # combine results
    dplyr::left_join(raw, adjusted, by = vars)
  })
}

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

  quo_covariates <- enquo(.covariates)
  quo_compare <- enquo(.compare)

  assert(
    "{.arg {caller_arg(.covariates)}} must be provided.",
    !quo_is_null(quo_covariates),
    !quo_is_missing(quo_covariates)
  )

  assert(
    "{.arg {caller_arg(.compare)}} must be provided.",
    !quo_is_null(quo_compare),
    !quo_is_missing(quo_compare)
  )

  imap(.results, function(fit, nm) {
    df <- stats::model.frame(fit)

    covariates <- names(tidyselect::eval_select(quo_covariates, df))
    compare <- names(tidyselect::eval_select(quo_compare, df))

    if (!check_selected_vars(covariates, ".covariates", nm)) {
      return(NULL)
    }

    if (!check_selected_vars(compare, ".compare", nm)) {
      return(NULL)
    }

    vcov_mtx <- vcov_from_spec(
      fit,
      .vcov,
      df,
      is_lmer(fit),
      call = caller_env()
    )
    combined <- unique(c(covariates, compare))

    try_fetch(
      emm_compare(
        emm_estimate(
          fit = fit,
          covariates = combined,
          weights = .weights,
          vcov_mtx = vcov_mtx
        ),
        compare = compare,
        type = .scale,
        adjust = .adjust,
        seed = .seed
      ),
      error = function(e) {
        msg <- "Comparison failed for {.field {nm}}: {conditionMessage(e)}"
        cli_warn(msg)
        NULL
      }
    )
  })
}

differences <- function(
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

  quo_covariates <- enquo(.covariates)
  quo_compare <- enquo(.compare)
  quo_conditioned <- enquo(.conditioned)

  assert(
    "{.arg {caller_arg(.covariates)}} must be provided.",
    !quo_is_null(quo_covariates),
    !quo_is_missing(quo_covariates)
  )

  assert(
    "{.arg {caller_arg(.compare)}} must be provided.",
    !quo_is_null(quo_compare),
    !quo_is_missing(quo_compare)
  )

  assert(
    "{.arg {caller_arg(.conditioned)}} must be provided.",
    !quo_is_null(quo_conditioned),
    !quo_is_missing(quo_conditioned)
  )

  imap(.results, function(fit, nm) {
    df <- stats::model.frame(fit)

    covariates <- names(tidyselect::eval_select(quo_covariates, df))
    compare <- names(tidyselect::eval_select(quo_compare, df))
    conditioned <- names(tidyselect::eval_select(quo_conditioned, df))

    if (!check_selected_vars(covariates, ".covariates", nm)) {
      return(NULL)
    }

    if (!check_selected_vars(compare, ".compare", nm)) {
      return(NULL)
    }

    if (!check_selected_vars(conditioned, ".conditioned", nm)) {
      return(NULL)
    }

    vcov_mtx <- vcov_from_spec(
      fit,
      .vcov,
      df,
      is_lmer(fit),
      call = caller_env()
    )

    try_fetch(
      emm_compare_by(
        emm_estimate(
          fit = fit,
          covariates = unique(c(covariates, conditioned, compare)),
          weights = .weights,
          vcov_mtx = vcov_mtx
        ),
        compare = compare,
        conditioned = conditioned,
        type = .scale,
        adjust = .adjust,
        seed = .seed
      ),
      error = function(e) {
        msg <- "Difference estimation failed for {.field {nm}}: {conditionMessage(e)}"
        cli_warn(msg)
        NULL
      }
    )
  })
}

# Emmeans Wrappers for Mean Estimation -----------------------------------------

emm_estimate <- function(fit, covariates, weights, vcov_mtx) {
  args <- list(
    object = fit,
    specs = make_interaction_formula(covariates),
    weights = weights,
    vcov. = if (is.null(vcov_mtx)) stats::vcov(fit) else vcov_mtx,
    lmer.df = if (is_lmer(fit)) "satterthwaite"
  )
  args <- purrr::compact(args)
  exec(emmeans::emmeans, !!!args)
}

emm_compare <- function(emm, compare, type, adjust, seed) {
  prs <- pairs(emm, simple = compare, type = type)
  interval <- confint(prs, type = type, adjust = "none")
  test <- withr::with_seed(
    seed,
    emmeans::test(prs, type = type, by = NULL, adjust = adjust)
  )
  join_emm_tables(prs, interval, test) |>
    dplyr::mutate(hypothesis = factor(hypothesis)) |>
    dplyr::select(-contrast) |>
    dplyr::relocate(hypothesis)
}

emm_compare_by <- function(
  emm,
  compare,
  conditioned,
  type,
  adjust,
  seed
) {
  prs0 <- pairs(emm, simple = compare, type = type)
  prs <- pairs(prs0, simple = conditioned, type = type)
  interval <- confint(prs, type = type, adjust = "none")
  test <- withr::with_seed(seed, emmeans::test(prs, by = NULL, adjust = adjust))
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
}

# Utils -----------------------------------------------------------------------

parse_cluster_quo <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (quo_is_null(x)) {
    return(NULL)
  }
  if (quo_is_symbol(x)) {
    return(as_string(quo_get_expr(x)))
  }
  expr <- quo_get_expr(x)
  if (is_string(expr)) {
    return(expr)
  }
  cli_abort("{.arg {arg}} must be a string, symbol, or NULL.", call = call)
}

validate_vcov_spec <- function(spec, df, call = caller_env()) {
  if (identical(spec$method, "CR2")) {
    if (is_null(spec$cluster)) {
      msg <- "Cluster variable must be provided for CR2 covariance matrix."
      cli_abort(msg, call = call)
    }
    if (!spec$cluster %in% names(df)) {
      msg <- "Cluster variable {.field {spec$cluster}} not found in data frame."
      cli_abort(msg, call = call)
    }
  }
  spec
}

vcov_from_spec <- function(fit, spec, df, .is_lmer, call = caller_env()) {
  if (is_null(spec)) {
    return(NULL)
  }
  validate_vcov_spec(spec, df, call)
  method <- spec$method
  if (is_null(method) && !.is_lmer) {
    method <- "HC3"
  }
  hc_methods <- c("HC0", "HC1", "HC2", "HC3", "HC4", "HC4m", "HC5")
  if (method %in% hc_methods) {
    sandwich::vcovHC(fit, type = method)
  } else if (identical(method, "CR2")) {
    clubSandwich::vcovCR(fit, cluster = df[[spec$cluster]], type = "CR2")
  } else {
    cli_abort("Unknown vcov_spec method: {.val {method}}.", call = call)
  }
}

join_emm_tables <- function(prs, interval, test) {
  interval <- as.data.frame(interval)
  test <- as.data.frame(test)
  prs <- as.data.frame(prs)

  lvls <- names(levels(prs))
  diff <- setdiff(c(names(interval), names(test), names(prs)), lvls)
  if (length(diff) > 0L) {
    msg <- "Column names of emmeans results do not match expected levels."
    cli_abort(msg, .internal = TRUE)
  }

  dplyr::left_join(
    prs |> dplyr::rename(p = p.value),
    dplyr::left_join(interval, test, by = lvls) |>
      dplyr::rename(p.value = p_adj),
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
}

make_interaction_formula <- function(covariates) {
  terms <- purrr::map(covariates, ~ sym(.x))
  if (length(terms) == 0L) {
    return(new_formula(lhs = NULL, rhs = expr(1)))
  }
  new_formula(lhs = NULL, rhs = purrr::reduce(terms, ~ call2("*", .x, .y)))
}

check_selected_vars <- function(selected, arg_name, nm) {
  if (length(selected) == 0L) {
    cli_warn("No variables selected in {.arg {arg_name}} for {.field {nm}}.")
    return(FALSE)
  }
  TRUE
}
