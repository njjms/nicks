library(biostatslib)
library(PropCIs)

test_that("Clopper-Pearson intevals give correct values", {
  expect_equal(clopper_pearson(x = 0,
                               n = 10,
                               conf.level = .95)$lower,
               0)
  expect_equal(clopper_pearson(x = 0,
                               n = 10,
                               conf.level = .95)$upper,
               qbeta(.975, 1, 10))
  expect_equal(clopper_pearson(x = 4,
                               n = 10,
                               conf.level = .95)$point_estimate,
               .4)
})
#> Test passed

test_that("Clopper-Pearson report errors", {
  expect_error(clopper_pearson(x = 11,
                               n = 10,
                               conf.level = .95))
  expect_error(clopper_pearson(x = 9,
                               n = 10,
                               conf.level = 99))
})
#> Test passed

test_that("Wilson-Score give correct values", {
  expect_equal(wilson_score(x = 0,
                            n = 10,
                            conf.level = .95)$lower,
               0)
  expect_equal(round(wilson_score(x = 0,
                                  n = 10,
                                  conf.level = .95)$upper, 4),
               PropCIs::scoreci(x = 0,
                                n = 10,
                                conf.level = .95)$conf.int[2])
  expect_equal(wilson_score(x = 4,
                            n = 10,
                            conf.level = .95)$point_estimate,
               .4)
})
#> Test passed

test_that("Wilson-Score report errors", {
  expect_error(wilson_score(x = 11,
                            n = 10,
                            conf.level = .95))
  expect_error(wilson_score(x = 9,
                            n = 10,
                            conf.level = 99))
})
#> Test passed
