% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/river_blackfish.R
\name{river_blackfish}
\alias{river_blackfish}
\alias{args_river_blackfish}
\title{Parameterised river blackfish population model}
\usage{
river_blackfish(k = 1000, ntime = 50)

args_river_blackfish(ntime = 50)
}
\arguments{
\item{k}{carrying capacity. Defaults to \code{1000}}

\item{ntime}{number of time steps used in population
simulation. Defaults to \code{50}}
}
\description{
Use a pre-defined population dynamics object to
  simulate population dynamics of river blackfish
  (*Gadopsis marmoratus*). Model parameters are based
  on existing data sets and published studies on river blackfish.
}
\details{
The \code{river_blackfish} population model is a
  stage-structured model with 4 classes and includes negative
  density dependence, environmental and demographic
  stochasticity, and optional associations with hydrological
  conditions and instream and riparian habitat.

  The overarching model structure was based the life history of river
  blackfish. The primary model parameters (survival, fecundity,
  density dependence) were estimated from observed counts of river
  blackfish in 4 length classes (0-80 mm, 80-140 mm, 140-200 mm, > 200 mm).
  These length classes roughly correspond with 0+, 1+, 2+, and older
  individuals based on preliminary assessments of survey data in
  Victorian rivers. The effects of hydrology and habitat were based on
  (minimal) published literature on the species and are subject to
  moderate levels of uncertainty.

  The river_blackfish population template does not
  currently require additional arguments.
}
\examples{
# define a basic model for river blackfish
p <- river_blackfish()

# simulate from this model
sims <- simulate(p, nsim = 100)

# plot the simulated values
plot(sims)
}
