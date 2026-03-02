get_model_type <- function(model) {
  if (inherits(model, "glmmTMB")) {
    "glmmTMB"
  } else if (inherits(model, "lmerMod")) {
    "lmer"
  } else if (inherits(model, "lm")) {
    "lm"
  } else if (inherits(model, "glmerMod")) {
    "glmer"
  } else if (inherits(model, "glm")) {
    "glm"
  } else {
    class(model)[1]
  }
}

get_expectation <- function(
  model,
  newdata = NULL,
  ...,
  re_mode = c("population", "conditional"),
  target = c("mean", "mu", "eta", "zprob", "zeta"),
  allow_new_levels = FALSE,
  call = caller_env()
) {
  newdata <- newdata %||% stats::model.frame(model)
  re_mode <- rlang::arg_match(re_mode)
  target <- rlang::arg_match(target)
  mtype <- get_model_type(model)
  if (target %in% c("zprob", "zeta") && mtype != "glmmTMB") {
    cli_abort(
      c(
        "{.val {target}} is not a valid target for {.cls {class(model)}}.",
        "i" = "Only {.cls glmmTMB} models support this target."
      ),
      call = call
    )
  }
  re_form <- if (re_mode == "population") NA else NULL
  allow_new_levels <- rlang::is_true(allow_new_levels)
  type <- switch(
    target,
    mean = "response",
    mu = if (mtype == "glmmTMB") "conditional" else "response",
    eta = "link",
    zprob = "zprob",
    zeta = "zlink"
  )
  out <- switch(
    mtype,
    lm = stats::predict(model, newdata = newdata, ...),
    lmer = stats::predict(
      model,
      newdata = newdata,
      re.form = re_form,
      allow.new.levels = allow_new_levels,
      ...
    ),
    glm = stats::predict(model, newdata = newdata, type = type, ...),
    glmer = stats::predict(
      model,
      newdata = newdata,
      type = type,
      re.form = re_form,
      allow.new.levels = allow_new_levels,
      ...
    ),
    glmmTMB = stats::predict(
      model,
      newdata = newdata,
      type = type,
      re.form = re_form,
      allow.new.levels = allow_new_levels,
      ...
    ),
    cli_abort(
      "Prediction for {.cls {class(model)}} is not supported.",
      call = call
    )
  )
  as.double(out)
}

get_fixef <- function(model, target = c("cond", "zi"), call = caller_env()) {
  target <- rlang::arg_match(target)
  mtype <- get_model_type(model)
  if (target == "zi" && mtype != "glmmTMB") {
    cli_abort(
      c(
        "{.val {target}} is not a valid target for {.cls {class(model)}}.",
        "i" = "Only {.cls glmmTMB} models support this target."
      ),
      call = call
    )
  }
  switch(
    mtype,
    lmer = lme4::fixef(model),
    glmer = lme4::fixef(model),
    glmmTMB = glmmTMB::fixef(model)[[target]],
    lm = stats::coef(model),
    glm = stats::coef(model),
    cli_abort(
      "Extracting fixed effects is not supported for {.cls {class(model)}}.",
      call = call
    )
  )
}

get_group_expectation <- function(model, ..., data = NULL, by) {
  data <- data %||% stats::model.frame(model)
  fam <- stats::family(model)
  grps <- vctrs::vec_group_loc(data[by])
  idx <- grps$loc
  keys <- tibble::as_tibble(grps$key)
  response <- purrr::map_dbl(idx, function(i) {
    dat0 <- vctrs::vec_slice(data, i)
    mdl <- stats::update(model, formula. = . ~ 1, data = dat0)
    unname(get_fixef(mdl)[1L] %||% NA_real_)
  })
  out <- tibble::tibble(n = lengths(idx), response = fam$linkinv(response))
  vctrs::vec_cbind(keys, out)
}

get_family <- function(model) {
  stats::family(model)$family %||% NA_character_
}

get_response_name <- function(model, call = caller_env()) {
  left <- rlang::f_lhs(stats::formula(model))
  if (is.null(left)) {
    cli_abort(
      "Failed to extract response variable from model formula.",
      call = call
    )
  }
  all.vars(left)
}

get_groupings <- function(model) {
  reformulas::randint(stats::formula(model)) |>
    reformulas::findbars() |>
    purrr::map(all.vars) |>
    purrr::compact() |>
    unlist(use.names = FALSE, recursive = FALSE) |>
    unique()
}

link_to_response <- function(model, eta, call = caller_env()) {
  fam <- stats::family(model)
  if (!is.function(fam$linkinv)) {
    cli_abort("Family does not provide a `linkinv` function.", call = call)
  }
  fam$linkinv(eta)
}

response_to_link <- function(model, mu, call = caller_env()) {
  fam <- stats::family(model)
  if (!is.function(fam$linkfun)) {
    cli_abort("Family does not provide a `linkfun` function.", call = call)
  }
  fam$linkfun(mu)
}

get_dispersion <- function(
  model,
  newdata = NULL,
  ...,
  allow_new_levels = FALSE,
  call = caller_env()
) {
  newdata <- newdata %||% stats::model.frame(model)
  out <- switch(
    get_model_type(model),
    # stats::predict matches stats::sigma, but stats::sigma returns NA if there
    # are multiple possible sigmas. Best to use predict
    glmmTMB = stats::predict(
      model,
      newdata = newdata,
      type = "disp",
      ...,
      allow.new.levels = allow_new_levels
    ),
    lmer = stats::sigma(model),
    glm = stats::sigma(model),
    glmer = stats::sigma(model),
    lm = stats::sigma(model),
    cli_abort(
      "Dispersion prediction is not supported for {.cls {class(model)}}.",
      call = call
    )
  )
  n_out <- vctrs::vec_size(out)
  n_new <- vctrs::vec_size(newdata)
  if (n_out == 1L) {
    return(as.double(rep_len(out, n_new)))
  }
  if (n_out == n_new) {
    return(as.double(out))
  }
  cli_abort(
    paste0(
      "Expected length of dispersion predictions to be either 1 or {n_new}, ",
      "but got {n_out}."
    ),
    call = call
  )
}

family_density <- function(y, mu, disp, fam, log = FALSE, call = caller_env()) {
  switch(
    tolower(fam),
    poisson = stats::dpois(y, lambda = mu, log = log),
    nbinom2 = stats::dnbinom(y, size = disp, mu = mu, log = log),
    nbinom1 = stats::dnbinom(y, size = mu / disp, mu = mu, log = log),
    gaussian = stats::dnorm(y, mean = mu, sd = disp, log = log),
    lognormal = {
      sdlog <- sqrt(log1p((disp / mu)^2))
      meanlog <- log(mu) - 0.5 * sdlog^2
      stats::dlnorm(y, meanlog = meanlog, sdlog = sdlog, log = log)
    },
    cli_abort("Unsupported distribution: {.val {fam}}.", call = call)
  )
}

family_probability <- function(y, mu, disp, fam, call = caller_env()) {
  switch(
    tolower(fam),
    poisson = stats::ppois(y, lambda = mu),
    nbinom2 = stats::pnbinom(y, size = disp, mu = mu),
    nbinom1 = stats::pnbinom(y, size = mu / disp, mu = mu),
    gaussian = stats::pnorm(y, mean = mu, sd = disp),
    lognormal = {
      sdlog <- sqrt(log1p((disp / mu)^2))
      meanlog <- log(mu) - 0.5 * sdlog^2
      stats::plnorm(y, meanlog = meanlog, sdlog = sdlog)
    },
    cli_abort("Unsupported distribution: {.val {fam}}.", call = call)
  )
}

family_quantile <- function(p, mu, disp, fam, call = caller_env()) {
  switch(
    tolower(fam),
    poisson = stats::qpois(p, lambda = mu),
    nbinom2 = stats::qnbinom(p, size = disp, mu = mu),
    nbinom1 = stats::qnbinom(p, size = mu / disp, mu = mu),
    gaussian = stats::qnorm(p, mean = mu, sd = disp),
    lognormal = {
      sdlog <- sqrt(log1p((disp / mu)^2))
      meanlog <- log(mu) - 0.5 * sdlog^2
      stats::qlnorm(p, meanlog = meanlog, sdlog = sdlog)
    },
    cli_abort("Unsupported distribution: {.val {fam}}.", call = call)
  )
}

get_density <- function(
  model,
  newdata = NULL,
  log = FALSE,
  ...,
  allow_new_levels = FALSE,
  call = caller_env()
) {
  if (is.null(newdata)) {
    newdata <- model.frame(model)
    left <- deparse1(rlang::f_lhs(stats::formula(model)))
    y <- newdata[[left]]
  } else {
    y <- newdata[[get_response_name(model)]]
  }
  mu <- get_expectation(
    model,
    newdata = newdata,
    target = "mu",
    allow_new_levels = allow_new_levels
  )
  disp <- get_dispersion(
    model,
    newdata = newdata,
    allow_new_levels = allow_new_levels
  )
  family_density(y, mu, disp, get_family(model), log = log)
}

model_has_fit_problem <- function(model, call = caller_env()) {
  switch(
    get_model_type(model),
    lmer = {
      optinfo <- model@optinfo
      msgs <- c(
        optinfo$conv$lme4$messages %||% character(),
        optinfo$conv$opt %||% character(),
        optinfo$warnings %||% character()
      )
      has_conv_msg <- length(msgs) > 0
      singular <- lme4::isSingular(model, tol = 1e-4)
      has_conv_msg || singular
    },
    glmer = {
      optinfo <- model@optinfo
      msgs <- c(
        optinfo$conv$lme4$messages %||% character(),
        optinfo$conv$opt %||% character(),
        optinfo$warnings %||% character()
      )
      has_conv_msg <- length(msgs) > 0
      singular <- lme4::isSingular(model, tol = 1e-4)
      has_conv_msg || singular
    },
    glmmTMB = {
      fit <- model$fit
      sdr <- model$sdr
      bad_conv <- !is.null(fit$convergence) && fit$convergence != 0
      bad_hess <- !is.null(sdr$pdHess) && !isTRUE(sdr$pdHess)
      bad_conv || bad_hess
    },
    glm = {
      bad_conv <- isFALSE(model$converged)
      rankdef <- anyNA(stats::coef(model))
      bad_conv || rankdef
    },
    lm = {
      anyNA(stats::coef(model))
    },
    cli_abort(
      "Model type {.cls {class(model)}} is not supported for fit diagnostics.",
      call = call
    )
  )
}

#' Leave-one-group-out cross-validation
#'
#' Performs leave-one-group-out cross-validation (LOGOCV) for a fitted model.
#' For each unique value of `group`, the model is re-fitted on all remaining
#' observations and used to predict the held-out group. Fit diagnostics,
#' predicted values, dispersions, log-densities, and cumulative probabilities
#' are returned alongside the original data.
#'
#' @param model A fitted model object (e.g. from `lme4::lmer()`,
#'   `lme4::glmer()`, `glmmTMB::glmmTMB()`, `stats::lm()`, or `stats::glm()`).
#' @param data A data frame containing all variables required by `model`.
#' @param group Character string naming the column in `data` whose unique
#'   values define the cross-validation folds.
#'
#' @return A list with three elements:
#'   \describe{
#'     \item{`data`}{The input data frame augmented with columns `.obs`
#'       (observed response), `.pred` (out-of-fold expected value), `.disp`
#'       (dispersion), `.problem` (logical fit-problem flag), `.ld`
#'       (log-density), and `.prob` (cumulative probability).}
#'     \item{`coefs`}{A data frame of per-fold fixed-effect coefficient
#'       estimates with columns `group`, `term`, `estimate`, and `probs`
#'       (logical fit-problem flag).}
#'     \item{`family`}{Character string naming the response distribution.}
#'   }
#' @export
loocv <- function(model, data, group) {
  # keep <- all.vars(rlang::f_rhs(stats::formula(model)))
  observed <- model.response(model.frame(model))

  groups <- vctrs::vec_group_loc(data[[group]])
  locs <- groups$loc
  keys <- groups$key

  predictions <- rlang::new_double(vctrs::vec_size(observed))
  dispersions <- rlang::new_double(vctrs::vec_size(observed))
  problems <- logical(vctrs::vec_size(observed))

  # Modified Coefs
  coefs <- vctrs::vec_expand_grid(
    group = keys,
    term = names(get_fixef(model))
  )

  # Set all to NA
  coefs$estimate <- rlang::new_double(vctrs::vec_size(coefs))
  coefs$probs <- logical(vctrs::vec_size(coefs))

  for (i in seq_along(keys)) {
    test_indices <- locs[[i]]
    train_indices <- setdiff(seq_len(nrow(data)), test_indices)

    train_data <- data[train_indices, ]
    test_data <- data[test_indices, ]

    model0 <- tryCatch(
      update(model, data = train_data),
      error = function(e) NULL
    )

    if (is.null(model0)) {
      predictions[test_indices] <- NA_real_
      dispersions[test_indices] <- NA_real_
      problems[test_indices] <- TRUE
      coefs$probs[coefs$group == keys[i]] <- TRUE
      next
    }

    has_problem <- model_has_fit_problem(model0)
    problems[test_indices] <- has_problem
    coefs$probs[coefs$group == keys[i]] <- has_problem

    # Update the terms
    mapping <- get_fixef(model0)
    sel_nms <- coefs$group == keys[i]
    sel_idx <- which(sel_nms)
    if (length(sel_idx) > 0L) {
      # vals sorted to match order found in terms (sorted to match coefs)
      vals <- mapping[coefs$term[sel_idx]]
      ok <- !is.na(vals)
      if (any(ok)) {
        coefs$estimate[sel_idx[ok]] <- as.numeric(vals[ok])
      }
      coefs$probs[sel_idx[!ok]] <- TRUE
    }

    predictions[test_indices] <- tryCatch(
      get_expectation(
        model0,
        newdata = test_data,
        target = "mu",
        allow_new_levels = TRUE
      ),
      error = function(e) NA_real_
    )

    dispersions[test_indices] <- tryCatch(
      get_dispersion(
        model0,
        newdata = test_data,
        allow_new_levels = TRUE
      ),
      error = function(e) NA_real_
    )

    problems[test_indices] <- problems[test_indices] |
      is.na(predictions[test_indices]) |
      is.na(dispersions[test_indices])
  }

  data[[".obs"]] <- observed
  data[[".pred"]] <- predictions
  data[[".disp"]] <- dispersions
  data[[".problem"]] <- problems

  data[[".ld"]] <- family_density(
    y = observed,
    mu = predictions,
    disp = dispersions,
    fam = get_family(model),
    log = TRUE
  )

  data[[".prob"]] <- family_probability(
    y = observed,
    mu = predictions,
    disp = dispersions,
    fam = get_family(model)
  )

  list(
    data = data,
    coefs = coefs,
    family = get_family(model)
  )
}

#' Plot leave-one-group-out prediction intervals
#'
#' Visualises the out-of-fold prediction intervals produced by `loocv()`. Each
#' row of the data is shown as a point (observed value) overlaid on a
#' horizontal error bar spanning the `(alpha/2, 1-alpha/2)` quantiles of the
#' fitted distribution. Points are coloured according to whether they fall
#' inside the interval, outside it, or are associated with a model fit
#' problem.
#'
#' @param data The list returned by `loocv()`.
#' @param by Optional character vector of column names to use as faceting
#'   variables. When `NULL` (default) no faceting is applied.
#' @param alpha Significance level used to construct the prediction interval.
#'   Defaults to `0.05` (giving a 95\% interval).
#' @param show_expected Logical; if `TRUE`, the predicted (expected) value for
#'   each observation is also drawn as a small cross marker. Defaults to
#'   `FALSE`.
#'
#' @return A `ggplot2` plot object.
#' @export
plot_loocv_interval <- function(
  data,
  by = NULL,
  alpha = 0.05,
  show_expected = FALSE
) {
  family <- data$family
  data <- data$data

  # Compute per-observation prediction intervals using existing helpers
  lower <- family_quantile(
    alpha / 2,
    data$.pred,
    data$.disp,
    family
  )

  upper <- family_quantile(
    1 - alpha / 2,
    data$.pred,
    data$.disp,
    family
  )

  pal_okabe <- c(
    blue = "#0072B2",
    light_blue = "#56B4E9",
    green = "#009E73",
    yellow = "#F0E442",
    orange = "#E69F00",
    red = "#D55E00",
    pink = "#CC79A7",
    black = "#000000"
  )

  dat <- data |>
    dplyr::mutate(
      status = dplyr::case_when(.problem ~ "Problem", TRUE ~ "OK") |>
        factor(levels = c("OK", "Problem")),
      .lower = lower,
      .upper = upper,
      .outside = (.obs < .lower | .obs > .upper) &
        !is.na(.lower) &
        !is.na(.upper),
      .cover_status = dplyr::case_when(
        .problem & .outside ~ "Problem/Outside",
        .problem & !.outside ~ "Problem",
        .outside ~ "Outside Interval",
        TRUE ~ "Inside Interval"
      ) |>
        factor(
          levels = c(
            "Inside Interval",
            "Outside Interval",
            "Problem",
            "Problem/Outside"
          )
        )
    )

  # Compute row index (within facet if faceting) for y axis placement
  if (!is.null(by) && length(by) > 0) {
    dat <- dat |>
      dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
      dplyr::mutate(.row = dplyr::row_number()) |>
      dplyr::ungroup()
  } else {
    dat <- dat |>
      dplyr::mutate(.row = dplyr::row_number())
  }

  # Base plot: horizontal error bars (prediction interval) at x, row on y
  p <- ggplot2::ggplot(dat, ggplot2::aes(y = .row)) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = .lower, xmax = .upper),
      color = "#9E9E9E",
      width = 0.0,
      linewidth = 0.35,
      alpha = 0.28,
      na.rm = TRUE
    ) +
    # observed values: color encodes combined coverage/status (no shapes)
    ggplot2::geom_point(
      ggplot2::aes(x = .obs, color = .cover_status),
      size = 1.4,
      alpha = 0.50,
      na.rm = TRUE
    )

  # Optionally draw the expected/predicted point as a smaller marker
  if (rlang::is_true(show_expected)) {
    p <- p +
      ggplot2::geom_point(
        ggplot2::aes(x = .pred),
        shape = 4,
        color = "#000000",
        size = 1.2,
        alpha = 0.9,
        na.rm = TRUE
      )
  }

  p <- p +
    ggplot2::scale_color_manual(
      values = c(
        "Inside Interval" = pal_okabe[["black"]],
        "Outside Interval" = pal_okabe[["blue"]],
        "Problem" = pal_okabe[["pink"]],
        "Problem/Outside" = pal_okabe[["red"]]
      ),
      name = "Coverage / Status",
      labels = c("Inside", "Outside", "Fit Problem", "Problem + Outside"),
      drop = FALSE
    ) +
    ggplot2::labs(x = "Value") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "vertical",
      legend.title = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 9),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank()
    )

  if (!is.null(by) && length(by) > 0) {
    facet_formula <- as.formula(paste("~", paste(by, collapse = " + ")))
    p <- p + ggplot2::facet_wrap(facet_formula, scales = "free_y")
  }

  p
}

#' Plot leave-one-group-out coefficient estimates
#'
#' Displays the per-fold fixed-effect coefficient estimates stored in the
#' `coefs` element of the list returned by `loocv()`. Each point represents
#' one fold-by-term combination; the shape encodes whether a fit problem was
#' detected for that fold.
#'
#' @param data The list returned by `loocv()`.
#' @param use_color Logical; if `TRUE`, points are coloured by fold/group.
#'   Defaults to `FALSE`.
#'
#' @return A `ggplot2` plot object.
#' @export
plot_loocv_coef <- function(data, use_color = FALSE) {
  p <- ggplot2::ggplot(data$coefs, ggplot2::aes(y = term, x = estimate))

  if (rlang::is_true(use_color)) {
    p <- p +
      ggplot2::geom_point(
        ggplot2::aes(color = group, shape = probs),
        size = 2.4,
        alpha = 0.8,
        na.rm = TRUE
      ) +
      ggplot2::guides(
        shape = ggplot2::guide_legend(title = "Fit Problem"),
        color = ggplot2::guide_legend(title = "Variable")
      )
  } else {
    p <- p +
      ggplot2::geom_point(
        ggplot2::aes(shape = probs),
        size = 2.4,
        alpha = 0.8,
        color = "#000000",
        na.rm = TRUE
      ) +
      ggplot2::guides(
        shape = ggplot2::guide_legend(title = "Fit Problem")
      )
  }

  p +
    ggplot2::theme_minimal() +
    ggplot2::labs(y = "Term", x = "Coefficient Estimate") +
    ggplot2::scale_shape_manual(
      values = c("FALSE" = 16, "TRUE" = 17),
      name = "Fit Problem",
      drop = FALSE,
      labels = c("No Problem", "Problem Detected")
    )
}

plot_compare_loocv <- function(m1, m2) {
  by <- c(intersect(names(m1$data), names(m2$data))) |>
    unique()
  by <- setdiff(by, c(".obs", ".pred", ".disp", ".problem", ".ld", ".prob"))
  m <- dplyr::left_join(
    m1$data,
    m2$data,
    by = by,
    suffix = c("_m1", "_m2")
  ) |>
    dplyr::mutate(
      .model_problem = dplyr::case_when(
        .problem_m1 & .problem_m2 ~ "Both Models",
        .problem_m1 & !.problem_m2 ~ "Model 1",
        .problem_m2 & !.problem_m1 ~ "Model 2",
        TRUE ~ "No Fit Issues"
      ) |>
        factor(levels = c("No Fit Issues", "Model 1", "Model 2", "Both Models"))
    )
  limits <- floor(min(m$.ld_m1, m$.ld_m2, na.rm = TRUE))
  m <- m |>
    dplyr::mutate(
      .ld_m1 = dplyr::if_else(is.na(.ld_m1), -Inf, .ld_m1),
      .ld_m2 = dplyr::if_else(is.na(.ld_m2), -Inf, .ld_m2)
    )
  ggplot2::ggplot(
    m,
    ggplot2::aes(
      x = .ld_m1,
      y = .ld_m2,
      color = .model_problem
    )
  ) +
    ggplot2::geom_point(size = 2.2, alpha = 0.5) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::labs(
      x = "Model 1",
      y = "Model 2"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::coord_cartesian(
      xlim = c(limits, 0),
      ylim = c(limits, 0)
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "No Fit Issues" = "#000000",
        "Model 1" = "#56B4E9",
        "Model 2" = "#D55E00",
        "Both Models" = "#CC79A7"
      ),
      labels = c(
        "No Issues",
        "Model 1",
        "Model 2",
        "Both Models"
      ),
      drop = FALSE,
      name = "Model Fit Alert"
    ) +
    ggplot2::annotate(
      "text",
      label = "Model 1 Better",
      x = Inf,
      y = -Inf,
      hjust = 1.2,
      vjust = -0.7
    ) +
    ggplot2::annotate(
      "text",
      label = "Model 2 Better",
      x = -Inf,
      y = Inf,
      hjust = -0.2,
      vjust = 1.7
    )
}
