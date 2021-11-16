library(biostatslib)
library(PropCIs)

test_that("power_calc with \"medium\" risk reports correct values", {
  expect_equal(
    power_calc(sample_size = 100,
               true_prob = .90,
               requirement = .95,
               requirement_type = "gt",
               alpha = .05,
               interval_type = "ws",
               AC_type = "medium")$power,
    sum(subset(power_calc(sample_size = 100,
               true_prob = .90,
               requirement = .95,
               requirement_type = "gt",
               alpha = .05,
               interval_type = "ws",
               AC_type = "medium")$df, pass == 0)$prob),
  )
  expect_equal(
    nrow(subset(power_calc(sample_size = 100,
               true_prob = .90,
               requirement = .95,
               requirement_type = "gt",
               alpha = .05,
               interval_type = "ws",
               AC_type = "medium")$df, pass == 1)),
    length(95:100)
  )
})
#> Test passed

test_that("power_calc with \"low\" risk reports correct values", {
  expect_equal(
    power_calc(sample_size = 100,
               true_prob = .90,
               requirement = .95,
               requirement_type = "gt",
               alpha = .05,
               interval_type = "ws",
               AC_type = "low")$power,
    sum(subset(power_calc(sample_size = 100,
               true_prob = .90,
               requirement = .95,
               requirement_type = "gt",
               alpha = .05,
               interval_type = "ws",
               AC_type = "low")$df, pass == 0)$prob),
  )
  expect_equal(
    nrow(subset(power_calc(sample_size = 100,
               true_prob = .90,
               requirement = .95,
               requirement_type = "gt",
               alpha = .05,
               interval_type = "ws",
               AC_type = "low")$df, pass == 1)),
    length(91:100)
  )
})
#> Test passed

power_calc(sample_size = 100,
           true_prob = .90,
           requirement = .92,
           requirement_type = "gt",
           alpha = .05,
           interval_type = "ws",
           AC_type = "high_delta",
           prq_delta = .90)$df

test_that("power_calc with \"high\" risk reports correct values", {
  expect_equal(
    power_calc(sample_size = 100,
               true_prob = .90,
               requirement = .95,
               requirement_type = "gt",
               alpha = .05,
               interval_type = "ws",
               AC_type = "high")$power,
    sum(subset(power_calc(sample_size = 100,
               true_prob = .90,
               requirement = .95,
               requirement_type = "gt",
               alpha = .05,
               interval_type = "ws",
               AC_type = "high")$df, pass == 0)$prob),
  )
  expect_equal(
    nrow(subset(power_calc(sample_size = 100,
               true_prob = .90,
               requirement = .95,
               requirement_type = "gt",
               alpha = .05,
               interval_type = "ws",
               AC_type = "high")$df, pass == 1)),
    length(c(100))
  )
})
#> Test passed

test_that("power_calc with \"high_delta\" risk reports correct values", {
  expect_error(
    power_calc(sample_size = 100,
               true_prob = .90,
               requirement = .95,
               requirement_type = "gt",
               alpha = .05,
               interval_type = "ws",
               AC_type = "high_delta")$power,
  )
  expect_equal(
    power_calc(sample_size = 100,
               true_prob = .90,
               requirement = .92,
               requirement_type = "gt",
               alpha = .05,
               interval_type = "ws",
               AC_type = "high_delta",
               prq_delta = .90)$power,
    sum(subset(power_calc(sample_size = 100,
               true_prob = .90,
               requirement = .92,
               requirement_type = "gt",
               alpha = .05,
               interval_type = "ws",
               AC_type = "high_delta",
               prq_delta = .90)$df, pass == 0)$prob),
  )
  expect_equal(
    nrow(subset(power_calc(sample_size = 100,
               true_prob = .90,
               requirement = .92,
               requirement_type = "gt",
               alpha = .05,
               interval_type = "ws",
               AC_type = "high_delta",
               prq_delta = .90)$df, pass == 1)),
    length(c(96:100))
  )
})
#> Test passed

test_that("power_calc with \"medium\" risk (lt) reports correct values", {
  expect_equal(
    power_calc(sample_size = 100,
               true_prob = .05,
               requirement = .039,
               requirement_type = "lt",
               alpha = .05,
               interval_type = "ws",
               AC_type = "medium")$power,
    sum(subset(power_calc(sample_size = 100,
               true_prob = .05,
               requirement = .039,
               requirement_type = "lt",
               alpha = .05,
               interval_type = "ws",
               AC_type = "medium")$df, pass == 0)$prob),
  )
  expect_equal(
    nrow(subset(power_calc(sample_size = 100,
               true_prob = .05,
               requirement = .039,
               requirement_type = "lt",
               alpha = .05,
               interval_type = "ws",
               AC_type = "medium")$df, pass == 1)),
    length(0:3)
  )
})
#> Test passed

test_that("power_calc with \"low\" risk (lt) reports correct values", {
  expect_equal(
    power_calc(sample_size = 100,
               true_prob = .05,
               requirement = .039,
               requirement_type = "lt",
               alpha = .05,
               interval_type = "ws",
               AC_type = "low")$power,
    sum(subset(power_calc(sample_size = 100,
               true_prob = .05,
               requirement = .039,
               requirement_type = "lt",
               alpha = .05,
               interval_type = "ws",
               AC_type = "low")$df, pass == 0)$prob),
  )
  expect_equal(
    nrow(subset(power_calc(sample_size = 100,
               true_prob = .05,
               requirement = .039,
               requirement_type = "lt",
               alpha = .05,
               interval_type = "ws",
               AC_type = "low")$df, pass == 1)),
    length(0:7)
  )
})
#> Test passed

test_that("power_calc with \"high\" risk (lt) reports correct values", {
  expect_equal(
    power_calc(sample_size = 100,
               true_prob = .05,
               requirement = .039,
               requirement_type = "lt",
               alpha = .05,
               interval_type = "ws",
               AC_type = "high")$power,
    sum(subset(power_calc(sample_size = 100,
               true_prob = .05,
               requirement = .039,
               requirement_type = "lt",
               alpha = .05,
               interval_type = "ws",
               AC_type = "high")$df, pass == 0)$prob),
  )
  expect_equal(
    nrow(subset(power_calc(sample_size = 100,
               true_prob = .05,
               requirement = .039,
               requirement_type = "lt",
               alpha = .05,
               interval_type = "ws",
               AC_type = "high")$df, pass == 1)),
    length(c(0))
  )
})
#> Test passed

test_that("power_calc with \"high_delta\" risk (lt) reports correct values", {
  expect_error(
    power_calc(sample_size = 100,
               true_prob = .05,
               requirement = .039,
               requirement_type = "lt",
               alpha = .05,
               interval_type = "ws",
               AC_type = "high_delta")$power
  )
  expect_equal(
    power_calc(sample_size = 100,
               true_prob = .05,
               requirement = .039,
               requirement_type = "lt",
               alpha = .05,
               interval_type = "ws",
               AC_type = "high_delta",
               prq_delta = .06)$power,
    sum(subset(power_calc(sample_size = 100,
               true_prob = .05,
               requirement = .039,
               requirement_type = "lt",
               alpha = .05,
               interval_type = "ws",
               AC_type = "high_delta",
               prq_delta = .06)$df, pass == 0)$prob),
  )
  expect_equal(
    nrow(subset(power_calc(sample_size = 100,
               true_prob = .05,
               requirement = .039,
               requirement_type = "lt",
               alpha = .05,
               interval_type = "ws",
               AC_type = "high_delta",
               prq_delta = .06)$df, pass == 1)),
    length(0:1)
  )
})
#> Test passed

