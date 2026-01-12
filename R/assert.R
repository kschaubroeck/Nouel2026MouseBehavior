assert <- function(message, ..., call = parent.frame()) {
  n <- ...length()
  if (n == 0L) {
    return(invisible())
  }

  assert_call <- match.call()
  i <- 1L

  tryCatch(
    while (i <= n) {
      res <- ...elt(i)
      if (!is.logical(res)) {
        cli_abort(
          "Assertions must return logicals, not {.obj_type_friendly {res}}",
          call = assert_call,
          class = "assertion_invalid"
        )
      }
      if (anyNA(res)) {
        cli_abort(
          "Assertions must not return NA values.",
          call = assert_call,
          class = "assertion_invalid"
        )
      }
      if (!all(res)) {
        err <- list(message = message, call = call)
        class(err) <- c("assertion_failed", "error", "condition")
        stop(err)
      }
      i <- i + 1L
    },
    error = function(cnd) {
      switch(
        class(cnd)[1L],
        assertion_failed = cli_abort(message, call = call, .envir = call),
        assertion_invalid = abort(
          sprintf("Invalid assertion at index %s.", i),
          call = call,
          parent = cnd,
          .internal = TRUE
        ),
        {
          # Get the specific assertion that caused the error, adding 2 for
          # the `assert()` call and `message` formal.
          cnd$call <- assert_call[[i + 2L]]

          assert_cnd <- error_cnd(
            "assertion_error",
            message = "Unexpected error evaluating assertion.",
            call = assert_call,
            parent = cnd
          )

          abort(
            sprintf("Error evaluating assertion at index %s.", i),
            call = call,
            parent = assert_cnd,
            .internal = TRUE
          )
        }
      )
    }
  )

  invisible()
}
