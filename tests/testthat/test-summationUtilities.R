context("Summation Utilities")

test_that("getParentVariable removes number suffix", {
  expect_equal(getParentVariable("Emissions|N2O"), "Emissions|N2O")
  expect_equal(getParentVariable("Emissions|N2O 2"), "Emissions|N2O")
  expect_equal(getParentVariable("Emissions|N2O 123"), "Emissions|N2O")
})

test_that("getParentVariable handles equations without suffix", {
  expect_equal(getParentVariable("Emissions|N2O|AFOLU"), "Emissions|N2O|AFOLU")
  expect_equal(getParentVariable("Simple"), "Simple")
})

test_that("getPiamChildrenForEquation returns empty for unknown equation", {
  test_summations <- data.frame(
    parent = c("Known", "Known"),
    child = c("Child1", "Child2"),
    factor = c(1, 1),
    stringsAsFactors = FALSE
  )

  children <- getPiamChildrenForEquation("Unknown|Equation", test_summations)

  expect_equal(length(children), 0)
})

test_that("getPiamChildrenForEquation skips NA and empty children", {
  test_summations <- data.frame(
    parent = c("Test", "Test", "Test"),
    child = c("Child1", NA, ""),
    factor = c(1, 1, 1),
    stringsAsFactors = FALSE
  )

  children <- getPiamChildrenForEquation("Test", test_summations)

  expect_equal(length(children), 1)
  expect_equal(children[1], "Child1")
})

# NOTE: User will provide assertion data for real summation file testing
# These placeholders await:
# - listEquationsInSummationFile with actual summation_n2o.csv
# - getPiamChildrenForEquation("Emissions|N2O", summation_n2o.csv)
# - getPiamChildrenForEquation("Emissions|N2O 2", summation_n2o.csv)
