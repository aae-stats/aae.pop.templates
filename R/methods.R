#' @name methods
#' @title Methods for population dynamics templates
#' @description Methods to simulate and plot population dynamics
#'   defined in \code{template} objects.
NULL

# S3 plot method
#' @export
plot.template <- function(x, y, ...) {
  plot(x$dynamics)
}

#' @rdname methods
#'
#' @export
#'
# nolint start
simulate.template <- function(
    object,
    nsim = 1,
    seed = NULL,
    ...,
    init = NULL,
    options = list(),
    args = list()
) {
  # nolint end

  # pull out dynamics object
  dyn <- object$dynamics

  # combine arguments
  combined <- combine_args(object$arguments, args)

  # simulate
  simulate(
    dyn,
    nsim = nsim,
    seed = seed,
    ...,
    init = init,
    options = options,
    args = combined
  )

}

# internal function: combine arguments from template (pre-specified)
#   with any additional arguments provided to `simulate`
combine_args <- function(x, y) {

  conflict <- names(y) %in% names(x)
  if (any(conflict)) {

    # add non-conflicting arguments
    combined <- c(x, y[!conflict])

    # print warning about conflicting arguments
    warning(
      "the following arguments are provided in both args ",
      "and in the template object: ",
      args[conflict],
      ". This is currently not supported, update template$args ",
      "directly prior to calling simulate",
      call. = FALSE
    )

  } else {
    # can just concatenate the two lists
    combined <- c(x, y)
  }

  # return
  combined

}
