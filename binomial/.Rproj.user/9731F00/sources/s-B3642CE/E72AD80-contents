context("Test the binomial functions")

test_that("Check if the probability is ok and it fails for invalid imput", {
  expect_true(check_prob(0.5))
  expect_error(check_prob(2))
  expect_error(check_prob(-1))
})


test_that("Check if the trials is ok and it fails for invalid imput", {
  expect_true(check_trials(5))
  expect_error(check_trials(0.5))
  expect_error(check_trials(-1))
})

test_that("Check if the success is ok and it fails for invalid imput", {
  expect_true(check_success(5, 10))
  expect_error(check_success(20, 10))
  expect_error(check_success(30, 10))
})

test_that("Check if the bin_choose is ok and it fails for invalid imput", {
  expect_output(bin_choose(10, 5), regexp = NA)
  expect_error(bin_choose(10, 20))
  expect_error(bin_choose(10, 30))
})

test_that("Check if the bin_probability is ok and it fails for invalid imput", {
  expect_output(bin_probability(5, 10, 0.5), regexp = NA)
  expect_error(bin_probability(20, 10, 0.5))
  expect_error(bin_choose(-20, 10, 0.5))
})

test_that("Check if the bin_cumulative is ok and it fails for invalid imput", {
  expect_output(bin_cumulative(10, 0.5), regexp = NA)
  expect_error(bin_cumulative(10, 20))
  expect_error(bin_cumulative(10, -20))
})

test_that("Check if the bin_distribution is ok and it fails for invalid imput", {
  expect_output(bin_distribution(10, 0.5), regexp = NA)
  expect_error(bin_distribution(10, 20))
  expect_error(bin_distribution(10, -20))
})

test_that("Check if the aux_mean is ok", {
  expect_output(aux_mean(10, 0.5), regexp = NA)
  expect_output(aux_mean(20, 0.3), regexp = NA)
  expect_output(aux_mean(30, 1/10), regexp = NA)
})

test_that("Check if the aux_variance is ok", {
  expect_output(aux_variance(10, 0.5), regexp = NA)
  expect_output(aux_variance(20, 0.3), regexp = NA)
  expect_output(aux_variance(30, 1/10), regexp = NA)
})

test_that("Check if the aux_mode is ok", {
  expect_output(aux_mode(10, 0.5), regexp = NA)
  expect_output(aux_mode(20, 0.3), regexp = NA)
  expect_output(aux_mode(30, 1/10), regexp = NA)
})

test_that("Check if the aux_kurtosis is ok", {
  expect_output(aux_kurtosis(10, 0.5), regexp = NA)
  expect_output(aux_kurtosis(20, 0.3), regexp = NA)
  expect_output(aux_kurtosis(30, 1/10), regexp = NA)
})

test_that("Check if the skewness is ok", {
  expect_output(aux_skewness(10, 0.5), regexp = NA)
  expect_output(aux_skewness(20, 0.3), regexp = NA)
  expect_output(aux_skewness(30, 1/10), regexp = NA)
})
