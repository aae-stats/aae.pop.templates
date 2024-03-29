context("templates")

test_that("murray cod template returns correct dynamics object", {

  # simulate from a Murray cod object
  dyn <- murray_cod()
  sim <- simulate(dyn)
  expect_equal(dim(sim), c(1L, 50L, 51L))

  # expect most processes to be defined
  expect_equal(
    class(dyn$dynamics$density_dependence_n),
    c("density_dependence_n", "function")
  )
  expect_equal(
    class(dyn$dynamics$covariates),
    c("covariates", "function")
  )
  expect_equal(
    class(dyn$dynamics$density_dependence),
    c("density_dependence", "function")
  )
  expect_null(dyn$dynamics$demographic_stochasticity)
  expect_equal(
    class(dyn$dynamics$environmental_stochasticity),
    c("environmental_stochasticity", "function")
  )

})

test_that("macquarie perch template returns correct dynamics object", {

  # simulate from a Macquarie perch object
  dyn <- macquarie_perch()
  sim <- simulate(dyn)
  expect_equal(dim(sim), c(1L, 30L, 51L))

  # expect most processes to be defined
  expect_equal(
    class(dyn$dynamics$density_dependence_n),
    c("density_dependence_n", "function")
  )
  expect_equal(
    class(dyn$dynamics$covariates),
    c("covariates", "function")
  )
  expect_equal(
    class(dyn$dynamics$density_dependence),
    c("density_dependence", "function")
  )
  expect_null(dyn$dynamics$demographic_stochasticity)
  expect_equal(
    class(dyn$dynamics$environmental_stochasticity),
    c("environmental_stochasticity", "function")
  )

})

test_that("platypus template returns correct dynamics object", {

  # simulate from a Macquarie perch object
  dyn <- platypus()
  sim <- simulate(dyn)
  expect_equal(dim(sim), c(1L, 2L, 51L))

  # expect most processes to be defined
  expect_equal(
    class(dyn$dynamics$density_dependence_n),
    c("density_dependence_n", "function")
  )
  expect_equal(
    class(dyn$dynamics$covariates),
    c("covariates", "function")
  )
  expect_equal(
    class(dyn$dynamics$density_dependence),
    c("density_dependence", "function")
  )
  expect_equal(
    class(dyn$dynamics$demographic_stochasticity),
    c("demographic_stochasticity", "function")
  )
  expect_equal(
    class(dyn$dynamics$environmental_stochasticity),
    c("environmental_stochasticity", "function")
  )

})

test_that("estuary perch template returns correct dynamics object", {

  # simulate from a Macquarie perch object
  dyn <- estuary_perch()
  sim <- simulate(dyn)
  expect_equal(dim(sim), c(1L, 40L, 51L))

  # expect most processes to be defined
  expect_equal(
    class(dyn$dynamics$density_dependence_n),
    c("density_dependence_n", "function")
  )
  expect_equal(
    class(dyn$dynamics$covariates),
    c("covariates", "function")
  )
  expect_equal(
    class(dyn$dynamics$density_dependence),
    c("density_dependence", "function")
  )
  expect_null(dyn$dynamics$demographic_stochasticity)
  expect_equal(
    class(dyn$dynamics$environmental_stochasticity),
    c("environmental_stochasticity", "function")
  )

})

test_that("pygmy perch template returns correct dynamics object", {

  # simulate from a Macquarie perch object
  dyn <- pygmy_perch()
  sim <- simulate(dyn)
  expect_equal(dim(sim), c(1L, 4L, 51L))

  # expect most processes to be defined
  expect_equal(
    class(dyn$dynamics$density_dependence_n),
    c("density_dependence_n", "function")
  )
  expect_equal(
    class(dyn$dynamics$covariates),
    c("covariates", "function")
  )
  expect_equal(
    class(dyn$dynamics$density_dependence),
    c("density_dependence", "function")
  )
  expect_null(dyn$dynamics$demographic_stochasticity)
  expect_equal(
    class(dyn$dynamics$environmental_stochasticity),
    c("environmental_stochasticity", "function")
  )

})

test_that("barred galaxias template returns correct dynamics object", {

  # simulate from a Macquarie perch object
  dyn <- barred_galaxias()
  sim <- simulate(dyn)
  expect_equal(dim(sim), c(1L, 4L, 51L))

  # expect most processes to be defined
  expect_equal(
    class(dyn$dynamics$density_dependence_n),
    c("density_dependence_n", "function")
  )
  expect_equal(
    class(dyn$dynamics$covariates),
    c("covariates", "function")
  )
  expect_equal(
    class(dyn$dynamics$density_dependence),
    c("density_dependence", "function")
  )
  expect_equal(
    class(dyn$dynamics$demographic_stochasticity),
    c("demographic_stochasticity", "function")
  )
  expect_equal(
    class(dyn$dynamics$environmental_stochasticity),
    c("environmental_stochasticity", "function")
  )

})

test_that("australian bass template returns correct dynamics object", {

  # simulate from a Murray cod object
  dyn <- australian_bass()
  sim <- simulate(dyn)
  expect_equal(dim(sim), c(1L, 50L, 51L))

  # expect most processes to be defined
  expect_equal(
    class(dyn$dynamics$density_dependence_n),
    c("density_dependence_n", "function")
  )
  expect_equal(
    class(dyn$dynamics$covariates),
    c("covariates", "function")
  )
  expect_equal(
    class(dyn$dynamics$density_dependence),
    c("density_dependence", "function")
  )
  expect_null(dyn$dynamics$demographic_stochasticity)
  expect_equal(
    class(dyn$dynamics$environmental_stochasticity),
    c("environmental_stochasticity", "function")
  )

})

test_that("common carp template returns correct dynamics object", {

  # simulate from a Murray cod object
  dyn <- common_carp()
  sim <- simulate(dyn)
  expect_equal(dim(sim), c(1L, 28L, 51L))

  # expect most processes to be defined
  expect_equal(
    class(dyn$dynamics$density_dependence_n),
    c("density_dependence_n", "function")
  )
  expect_equal(
    class(dyn$dynamics$covariates),
    c("covariates", "function")
  )
  expect_equal(
    class(dyn$dynamics$density_dependence),
    c("density_dependence", "function")
  )
  expect_null(dyn$dynamics$demographic_stochasticity)
  expect_equal(
    class(dyn$dynamics$environmental_stochasticity),
    c("environmental_stochasticity", "function")
  )

})

test_that("rainbowfish template returns correct dynamics object", {

  # simulate from a Murray cod object
  dyn <- murray_rainbowfish()
  sim <- simulate(dyn)
  expect_equal(dim(sim), c(1L, 7L, 51L))

  # expect most processes to be defined
  expect_null(dyn$dynamics$density_dependence_n)
  expect_equal(
    class(dyn$dynamics$covariates),
    c("covariates", "function")
  )
  expect_equal(
    class(dyn$dynamics$density_dependence),
    c("density_dependence", "function")
  )
  expect_equal(
    class(dyn$dynamics$demographic_stochasticity),
    c("demographic_stochasticity", "function")
  )
  expect_equal(
    class(dyn$dynamics$environmental_stochasticity),
    c("environmental_stochasticity", "function")
  )

})

test_that("river blackfish template returns correct dynamics object", {

  # simulate from a Murray cod object
  dyn <- river_blackfish()
  sim <- simulate(dyn)
  expect_equal(dim(sim), c(1L, 11L, 51L))

  # expect most processes to be defined
  expect_null(dyn$dynamics$density_dependence_n)
  expect_equal(
    class(dyn$dynamics$covariates),
    c("covariates", "function")
  )
  expect_equal(
    class(dyn$dynamics$density_dependence),
    c("density_dependence", "function")
  )
  expect_equal(
    class(dyn$dynamics$demographic_stochasticity),
    c("demographic_stochasticity", "function")
  )
  expect_equal(
    class(dyn$dynamics$environmental_stochasticity),
    c("environmental_stochasticity", "function")
  )

})

test_that("get_template returns correct template without species wrappers", {

  # check Murray cod template
  value <- get_template("murray_cod")
  target <- murray_cod()
  target$dynamics$hex <- value$dynamics$hex
  expect_equal(value, target)

  # check Macquarie perch template
  value <- get_template("macquarie_perch")
  target <- macquarie_perch()
  target$dynamics$hex <- value$dynamics$hex
  expect_equal(value, target)

  # check Murray cod template
  value <- get_template("platypus")
  target <- platypus()
  target$dynamics$hex <- value$dynamics$hex
  expect_equal(value, target)

})

test_that("get_template errors informatively", {

  # check random species name
  expect_error(
    get_template("labradoodle"),
    "labradoodle is not a defined population model"
  )

})
