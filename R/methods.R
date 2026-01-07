#' @name methods
#' @title Methods for population dynamics templates
#' @description Methods to simulate and plot population dynamics
#'   defined in \code{template} objects.
#'
#' @param object a \code{template} object returned by
#'   \code{\link{get_template}}
#' @param nsim the number of replicate simulations (default = 1)
#' @param seed optional seed used prior to initialisation and simulation to
#'   give reproducible results
#' @param \dots additional arguments passed to the \code{simulate.dynamics}
#'   function
#' @param init an array of initial conditions with one row per replicate and one
#'   column per population stage. If \code{obj} has been created with
#'   \code{\link{multispecies}}, initial conditions can be provided as a list or
#'   array with one element or slice per species, or as a matrix, in which case
#'   all species are assumed to share the same initial conditions. Defaults
#'   to \code{NULL}, in which case initial conditions are generated randomly
#'   according to \code{options()$aae.pop_initialisation}
#' @param options a named \code{list} of simulation options. Currently accepted
#'   values are:
#'
#'   - \code{ntime} the number of time steps to simulate, ignored if \code{obj}
#'       includes a \code{\link{covariates}} (default = 50)
#'
#'   - \code{keep_slices} \code{logical} defining whether to keep intermediate
#'       population abundances or (if \code{FALSE}) to return only the final
#'       time slice
#'
#'   - \code{tidy_abundances} a function to handle predicted abundance data
#'       that may be non-integer. Defaults to \code{identity}; suggested
#'       alternatives are \code{floor}, \code{round}, or \code{ceiling}
#'
#'   - \code{initialise_args} a list of arguments passed to the function
#'       used to initialise abundance trajectories. Only used if
#'       \code{init = NULL}. Defaults to \code{options()$aae.pop_lambda},
#'       which specifies lambda for Poisson random draws. The default
#'       initialisation function is defined by
#'       \code{options()$aae.pop_initialisation}.
#'
#'    - \code{update} a function to update abundances from one time
#'       step to the next. Defaults to \code{options()$aae.pop_update}.
#'
#' @param args named list of lists passing arguments to processes defined
#'   in \code{object}, including \code{interaction} for
#'   \code{\link{multispecies}} objects. Lists (up to one per process)
#'   can contain a mix of static, dynamic, and function arguments.
#'   Dynamic arguments must be lists with one element per time step.
#'   Function arguments must be functions that calculate arguments
#'   dynamically in each generation based on from the population dynamics
#'   object, population abundances, and time step in each generation.
#'   All other classes (e.g., single values, matrices, data frames)
#'   are treated as static arguments. Covariates contained in numeric
#'   vectors, matrices, or data frames can be formatted as dynamic
#'   arguments with the \code{format_covariates} function.
#'
#'   \code{args} for \code{\link{multispecies}} objects must have one
#'   element per species (defaults will expand automatically if not provided)
#' @param .future flag to determine whether the future package should be
#'   used to manage updates for multispecies models (an embarrassingly
#'   parallel problem)
#'
#' @details Details of how to use \code{simulate} are covered in the
#'   documentation and vignettes for the `aae.pop` R package. These functions
#'   provide convenient argument handling and wrappers for templated population
#'   dynamics models but are otherwise passing arguments directly to
#'   \code{simulate}
NULL

# S3 plot method
#' @importFrom graphics plot
#'
#' @export
plot.template <- function(x, y, ...) {
  plot(x$dynamics)
}

#' @rdname methods
#'
#' @importFrom stats simulate
#'
#' @export
# nolint start
simulate.template <- function(
    object,
    nsim = 1,
    seed = NULL,
    ...,
    init = NULL,
    options = list(),
    args = list(),
    .future = FALSE
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
    args = combined,
    .future = .future
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
      names(x)[conflict],
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
