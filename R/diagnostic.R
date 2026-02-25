simulate_residuals0 <- function(
  name,
  model,
  n = 1000,
  seed = 12345,
  mode = c("unconditional", "conditional", "zero"),
  tmb_terms = "ALL",
  ...
) {
  sim_mode <- arg_match0(mode, c("unconditional", "conditional", "zero"))
  type <- model_type(model)

  if (is.na(type)) {
    cli_warn(
      c(
        "Don't know how to simulate residuals for {.val {name}}.",
        "x" = "Model class {.cls {class(model)}} is not supported."
      )
    )
    return(NULL)
  }

  sim_args <- list2(...)
  if (type == "merMod") {
    # lme4 simulate.merMod:
    # re.form = NULL => unconditional (simulate RE)
    # re.form = NA   => conditional on fitted RE (don't simulate RE)
    #
    # There isn't a direct analogue of "set RE to zero" via re.form.
    # Best approximation is conditional on fitted RE (re.form = NA).
    if ("re.form" %in% names(sim_args)) {
      cli::cli_warn(
        c(
          "{.arg re.form} was manually set for {.val {name}} - overriding.",
          "i" = "Use {.arg mode} to control random-effect simulation."
        )
      )
    }
    if (sim_mode == "unconditional") {
      sim_args$re.form <- NULL
    } else if (sim_mode %in% c("conditional", "zero")) {
      if (sim_mode == "zero") {
        cli::cli_warn(
          c(
            "Setting RE to zero isn't directly supported for {.cls merMod}",
            "i" = "Falling back to conditional simulation for {.val {name}}."
          )
        )
      }
      sim_args$re.form <- NA
    }
  }

  if (type == "glmmTMB") {
    # glmmTMB: set_simcodes modifies the TMB object in place.
    # Cache current simCode values and restore on exit.
    if (
      is.null(model$obj) ||
        is.null(model$obj$env) ||
        is.null(model$obj$env$data) ||
        is.null(model$obj$env$data$terms)
    ) {
      cli_abort(
        c(
          "Failed to access the TMB internals of {.val {name}}.",
          "x" = "{.field model$obj$env$data$terms} is {.code NULL} or missing.",
          "i" = "Was the model fitted successfully?"
        ),
        call = caller_env()
      )
    }

    # Preserve original simCode values (may be numeric or character)
    # https://github.com/glmmTMB/glmmTMB/issues/888
    old_term <- duplicate(model$obj$env$data$terms, shallow = FALSE)
    on.exit(
      model$obj$env$data$terms <- old_term,
      add = TRUE
    )

    val <- switch(
      sim_mode,
      conditional = "fix",
      unconditional = "random",
      zero = "zero"
    )

    glmmTMB::set_simcodes(model$obj, val = val, terms = tmb_terms)
  }

  out <- rlang::try_fetch(
    withr::with_seed(
      seed,
      exec(
        DHARMa::simulateResiduals,
        fittedModel = model,
        plot = FALSE,
        seed = seed,
        n = n,
        !!!sim_args
      )
    ),
    error = function(cnd) {
      cli::cli_alert_danger(
        "Simulation failed for {.val {name}}: {cnd$message}"
      )
      NULL
    }
  )

  if (!is.null(out)) {
    cli::cli_alert_success("Simulated residuals for {.val {name}}.")
  }

  out
}

#' Simulate DHARMa residuals for multiple models
#'
#' Apply `simulate_residuals0()` over a list of fitted model objects and
#' return a list of DHARMa simulated residual objects. This is a convenience
#' wrapper that iterates over `x`, passing each element's name (or integer
#' position when `x` is unnamed) as the `name` identifier used in diagnostics
#' and error messages.
#'
#' Errors thrown for individual models are caught with `rlang::try_fetch`,
#' re-emitted as warnings that identify the failing model by name/position,
#' and replaced with `NULL` in the output list so that the remaining models
#' are still processed.
#'
#' @param x A named or unnamed list of fitted model objects.
#' @param n Integer; number of simulations passed to each single-model call.
#' @param seed Integer; random seed for reproducibility.
#' @param mode Character; one of "unconditional", "conditional", or "zero".
#' @param tmb_terms Character; terms argument passed to `glmmTMB::set_simcodes`.
#' @param ... Additional arguments passed to `simulate_residuals0()` /
#'   `DHARMa::simulateResiduals()`.
#'
#' @return A list of DHARMa simulated residual objects (one per element of `x`,
#'   `NULL` for any model whose simulation failed).
#' @export
simulate_residuals <- function(
  x,
  n = 1000,
  seed = 12345,
  mode = c("unconditional", "conditional", "zero"),
  tmb_terms = "ALL",
  ...
) {
  purrr::imap(
    x,
    ~ simulate_residuals0(
      name = .y,
      model = .x,
      n = n,
      seed = seed,
      mode = mode,
      tmb_terms = tmb_terms
    )
  )
}
