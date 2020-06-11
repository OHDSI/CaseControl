library("testthat")

checkPower <- function(zAlpha, ccRatio, expPrevalence, nCases, or, power) {
  alpha <- (1 - pnorm(zAlpha))*2
  nControls <- nCases*ccRatio
  exposedControls <- round(expPrevalence*nControls)
  caseControlData <- data.frame(isCase = c(rep(1, nCases), rep(0, nControls)),
                                exposed = c(rep(0, nCases), rep(1, exposedControls), rep(0, nControls - exposedControls)))
  mdrr <- computeMdrr(caseControlData, alpha = alpha, power = power)

  expect_equal(mdrr$mdrr, or, tolerance = 0.1)
}

test_that("power calculation", {
  # Examples from EpiSheet
  zAlpha <- 1.645
  ccRatio <- 4
  expPrevalence <-  0.25
  nCases <- 220
  or <- 1.5

  power <- 0.7921790

  checkPower(zAlpha, ccRatio, expPrevalence, nCases, or, power)


  zAlpha <- 1.645
  ccRatio <- 4
  expPrevalence <-  0.25
  nCases <- 22
  or <- 3

  power <- 0.7232217

  checkPower(zAlpha, ccRatio, expPrevalence, nCases, or, power)


  zAlpha <- 1.96
  ccRatio <- 1
  expPrevalence <-  0.01
  nCases <- 220
  or <- 2

  power <- 0.1190723

  checkPower(zAlpha, ccRatio, expPrevalence, nCases, or, power)
})

