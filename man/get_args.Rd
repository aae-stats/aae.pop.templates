% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_args.R
\name{get_args}
\alias{get_args}
\title{Define additional arguments relevant to specific population
  model templates}
\usage{
get_args(sp, ...)
}
\arguments{
\item{sp}{character naming the species to be collected. Will be
partially matched to included species}

\item{\dots}{additional arguments passed to the species template}
}
\description{
Define additional arguments used to control the
  simulation of population dynamics in \code{\link[aae.pop]{simulate}}.
}
\details{
The \code{get_args} function can be used to access
  previously defined arguments to be passed to
  \code{\link[aae.pop]{simulate}} for any given species.
  In general, arguments for a particular species can be
  accessed with \code{get_args("species_name")}, where
  \code{species_name} is replaced with the function name used
  to define the \code{dynamics} object for that species. For example,
  arguments for Macquarie perch (function \code{macquarie_perch})
  can be accessed with \code{get_args("macquarie_perch")}.

  When defining templates, arguments are best contained in a
  separate function named \code{args_species_name}, where
  \code{species_name} is described above. This function must
  return a named list with elements corresponding to any of
  the processes required by \code{dynamics} or
  \code{multispecies}.

  The \code{get_args} function has been deprecated, and is now
  called directly within a call to \code{get_template}.
}
\examples{
# define a basic model for Macquarie perch with
#   carrying capacity = 1000
mp <- macquarie_perch(k = 1000)

# define required arguments
mp_args <- get_args("macquarie_perch")

# simulate from this model
sims <- simulate(mp, nsim = 100, args = mp_args)

# plot the simulated values
plot(sims)
}
