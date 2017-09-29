library("testthat")

test_that("Washout period for cases", {
  caseData <- list(cases = data.frame(nestingCohortId = c(1),
                                      outcomeId = c(1),
                                      indexDate = as.Date(c("2000-07-01"))),
                   nestingCohorts = ff::as.ffdf(data.frame(nestingCohortId = c(1),
                                                           personId = c(1),
                                                           observationPeriodStartDate = as.Date(c("2000-01-01")),
                                                           startDate = as.Date(c("2000-01-01")),
                                                           endDate = as.Date(c("2010-01-01")),
                                                           dateOfBirth = as.Date(c("2000-01-01")),
                                                           genderConceptId = c(8532),
                                                           careSiteId = c(1),
                                                           providerId = c(1))))

  # Case after washout period:
  cc <- selectControls(caseData = caseData,
                       outcomeId = 1,
                       washoutPeriod = 180,
                       removedUnmatchedCases = FALSE)
  expect_equal(cc$personId, c(1))

  # Case before washout period:
  cc <- selectControls(caseData = caseData,
                       outcomeId = 1,
                       washoutPeriod = 365,
                       removedUnmatchedCases = FALSE)
  expect_equal(nrow(cc), 0)
})

test_that("Washout period for controls", {
  caseData <- list(cases = data.frame(nestingCohortId = c(1),
                                      outcomeId = c(1),
                                      indexDate = as.Date(c("2001-01-01"))),
                   nestingCohorts = ff::as.ffdf(data.frame(nestingCohortId = c(1, 2, 3),
                                                           personId = c(1, 2, 3),
                                                           observationPeriodStartDate = as.Date(c("2000-01-01", "2000-01-01", "2000-11-01")),
                                                           startDate = as.Date(c("2000-01-01", "2000-01-01", "2000-11-01")),
                                                           endDate = as.Date(c("2010-01-01", "2010-01-01", "2010-01-01")),
                                                           dateOfBirth = as.Date(c("2000-01-01", "2000-01-01", "2000-01-01")),
                                                           genderConceptId = c(8532, 8532, 8532),
                                                           careSiteId = c(1, 1, 1),
                                                           providerId = c(1, 1, 1))))

  # One control after washout period:
  cc <- selectControls(caseData = caseData, outcomeId = 1, washoutPeriod = 180)
  expect_equal(cc$personId, c(1, 2))

  # Both controls afterwashout period:
  cc <- selectControls(caseData = caseData, outcomeId = 1, washoutPeriod = 0, matchOnTimeInCohort = FALSE)
  expect_equal(cc$personId[order(cc$personId)], c(1, 2, 3))
})

test_that("Match on index date", {
  caseData <- list(cases = data.frame(nestingCohortId = c(1),
                                      outcomeId = c(1),
                                      indexDate = as.Date(c("2001-01-01"))),
                   nestingCohorts = ff::as.ffdf(data.frame(nestingCohortId = c(1, 2, 3),
                                                           personId = c(1, 2, 3),
                                                           observationPeriodStartDate = as.Date(c("2000-01-01", "2000-01-01", "2000-01-01")),
                                                           startDate = as.Date(c("2000-01-01", "2002-01-01", "2000-01-01")),
                                                           endDate = as.Date(c("2010-01-01", "2010-01-01", "2010-01-01")),
                                                           dateOfBirth = as.Date(c("2000-01-01", "2000-01-01", "2000-01-01")),
                                                           genderConceptId = c(8532, 8532, 8532),
                                                           careSiteId = c(1, 1, 1),
                                                           providerId = c(1, 1, 1))))

  # One control with overlapping cohort time:
  cc <- selectControls(caseData = caseData, outcomeId = 1, washoutPeriod = 180)
  expect_equal(cc$personId, c(1, 3))
  expect_equal(cc$indexDate, as.Date(c("2001-01-01", "2001-01-01")))
})

test_that("Match on gender", {
  caseData <- list(cases = data.frame(nestingCohortId = c(1),
                                      outcomeId = c(1),
                                      indexDate = as.Date(c("2001-01-01"))),
                   nestingCohorts = ff::as.ffdf(data.frame(nestingCohortId = c(1, 2, 3),
                                                           personId = c(1, 2, 3),
                                                           observationPeriodStartDate = as.Date(c("2000-01-01", "2000-01-01", "2000-01-01")),
                                                           startDate = as.Date(c("2000-01-01", "2000-01-01", "2000-01-01")),
                                                           endDate = as.Date(c("2010-01-01", "2010-01-01", "2010-01-01")),
                                                           dateOfBirth = as.Date(c("2000-01-01", "2000-01-01", "2000-01-01")),
                                                           genderConceptId = c(8532, 8532, 8507),
                                                           careSiteId = c(1, 1, 1),
                                                           providerId = c(1, 1, 1))))

  # Two control without matching on gender:
  cc <- selectControls(caseData = caseData,
                       outcomeId = 1,
                       washoutPeriod = 180,
                       matchOnGender = FALSE)
  expect_equal(cc$personId[order(cc$personId)], c(1, 2, 3))

  # One control with same gender:
  cc <- selectControls(caseData = caseData,
                       outcomeId = 1,
                       washoutPeriod = 180,
                       matchOnGender = TRUE)
  expect_equal(cc$personId, c(1, 2))
})

test_that("Match on age", {
  caseData <- list(cases = data.frame(nestingCohortId = c(1),
                                      outcomeId = c(1),
                                      indexDate = as.Date(c("2001-01-01"))),
                   nestingCohorts = ff::as.ffdf(data.frame(nestingCohortId = c(1, 2, 3),
                                                           personId = c(1, 2, 3),
                                                           observationPeriodStartDate = as.Date(c("2000-01-01", "2000-01-01", "2000-01-01")),
                                                           startDate = as.Date(c("2000-01-01", "2000-01-01", "2000-01-01")),
                                                           endDate = as.Date(c("2010-01-01", "2010-01-01", "2010-01-01")),
                                                           dateOfBirth = as.Date(c("2000-01-01", "2000-01-01", "1990-01-01")),
                                                           genderConceptId = c(8532, 8532, 8532),
                                                           careSiteId = c(1, 1, 1),
                                                           providerId = c(1, 1, 1))))

  # Two control without matching on age:
  cc <- selectControls(caseData = caseData, outcomeId = 1, washoutPeriod = 180, matchOnAge = FALSE)
  expect_equal(cc$personId[order(cc$personId)], c(1, 2, 3))

  # One control with simlar age:
  cc <- selectControls(caseData = caseData,
                       outcomeId = 1,
                       washoutPeriod = 180,
                       matchOnAge = TRUE,
                       ageCaliper = 2)
  expect_equal(cc$personId, c(1, 2))
})

test_that("Match on provider", {
  caseData <- list(cases = data.frame(nestingCohortId = c(1),
                                      outcomeId = c(1),
                                      indexDate = as.Date(c("2001-01-01"))),
                   nestingCohorts = ff::as.ffdf(data.frame(nestingCohortId = c(1, 2, 3),
                                                           personId = c(1, 2, 3),
                                                           observationPeriodStartDate = as.Date(c("2000-01-01", "2000-01-01", "2000-01-01")),
                                                           startDate = as.Date(c("2000-01-01", "2000-01-01", "2000-01-01")),
                                                           endDate = as.Date(c("2010-01-01", "2010-01-01", "2010-01-01")),
                                                           dateOfBirth = as.Date(c("2000-01-01", "2000-01-01", "2000-01-01")),
                                                           genderConceptId = c(8532, 8532, 8532),
                                                           careSiteId = c(1, 1, 1),
                                                           providerId = c(1, 2, 1))))

  # Two control without matching on provider:
  cc <- selectControls(caseData = caseData,
                       outcomeId = 1,
                       washoutPeriod = 180,
                       matchOnProvider = FALSE)
  expect_equal(cc$personId[order(cc$personId)], c(1, 2, 3))

  # One control with simlar provider:
  cc <- selectControls(caseData = caseData,
                       outcomeId = 1,
                       washoutPeriod = 180,
                       matchOnProvider = TRUE)
  expect_equal(cc$personId, c(1, 3))
})

test_that("Match on care site", {
  caseData <- list(cases = data.frame(nestingCohortId = c(1),
                                      outcomeId = c(1),
                                      indexDate = as.Date(c("2001-01-01"))),
                   nestingCohorts = ff::as.ffdf(data.frame(nestingCohortId = c(1, 2, 3),
                                                           personId = c(1, 2, 3),
                                                           observationPeriodStartDate = as.Date(c("2000-01-01", "2000-01-01", "2000-01-01")),
                                                           startDate = as.Date(c("2000-01-01", "2000-01-01", "2000-01-01")),
                                                           endDate = as.Date(c("2010-01-01", "2010-01-01", "2010-01-01")),
                                                           dateOfBirth = as.Date(c("2000-01-01", "2000-01-01", "2000-01-01")),
                                                           genderConceptId = c(8532, 8532, 8532),
                                                           careSiteId = c(1, 2, 1),
                                                           providerId = c(1, 1, 1))))

  # Two control without matching on provider:
  cc <- selectControls(caseData = caseData,
                       outcomeId = 1,
                       washoutPeriod = 180,
                       matchOnCareSite = FALSE)
  expect_equal(cc$personId[order(cc$personId)], c(1, 2, 3))

  # One control with simlar provider:
  cc <- selectControls(caseData = caseData,
                       outcomeId = 1,
                       washoutPeriod = 180,
                       matchOnCareSite = TRUE)
  expect_equal(cc$personId, c(1, 3))
})

test_that("Match on visit", {
  caseData <- list(cases = data.frame(nestingCohortId = c(1),
                                      outcomeId = c(1),
                                      indexDate = as.Date(c("2001-01-01"))),
                   nestingCohorts = ff::as.ffdf(data.frame(nestingCohortId = c(1, 2, 3),
                                                           personId = c(1, 2, 3),
                                                           observationPeriodStartDate = as.Date(c("2000-01-01", "2000-01-01", "2000-01-01")),
                                                           startDate = as.Date(c("2000-01-01", "2000-01-01", "2000-01-01")),
                                                           endDate = as.Date(c("2010-01-01", "2010-01-01", "2010-01-01")),
                                                           dateOfBirth = as.Date(c("2000-01-01", "2000-01-01", "2000-01-01")),
                                                           genderConceptId = c(8532, 8532, 8532),
                                                           careSiteId = c(1, 1, 1),
                                                           providerId = c(1, 2, 1))),
                   visits = ff::as.ffdf(data.frame(nestingCohortId = c(1, 2, 3),
                                                   visitStartDate = as.Date(c("2001-01-01", "2001-01-02", "2001-03-01")))),
                   metaData = list(hasVisits = TRUE))

  # Two control without matching on visit:
  cc <- selectControls(caseData = caseData,
                       outcomeId = 1,
                       washoutPeriod = 180,
                       matchOnVisitDate = FALSE)
  expect_equal(cc$personId[order(cc$personId)], c(1, 2, 3))

  # One control with matching visit date:
  cc <- selectControls(caseData = caseData,
                       outcomeId = 1,
                       washoutPeriod = 180,
                       matchOnVisitDate = TRUE,
                       visitDateCaliper = 30)
  expect_equal(cc$personId, c(1, 2))
  expect_equal(cc$indexDate, as.Date(c("2001-01-01", "2001-01-02")))
})

test_that("Match on time in cohort", {
  caseData <- list(cases = data.frame(nestingCohortId = c(1),
                                      outcomeId = c(1),
                                      indexDate = as.Date(c("2001-01-01"))),
                   nestingCohorts = ff::as.ffdf(data.frame(nestingCohortId = c(1, 2, 3),
                                                           personId = c(1, 2, 3),
                                                           observationPeriodStartDate = as.Date(c("1999-01-01", "1999-01-01", "2000-01-01")),
                                                           startDate = as.Date(c("2000-01-01", "1999-01-01", "2000-01-01")),
                                                           endDate = as.Date(c("2010-01-01", "2010-01-01", "2010-01-01")),
                                                           dateOfBirth = as.Date(c("2000-01-01", "1999-01-01", "2000-01-01")),
                                                           genderConceptId = c(8532, 8532, 8532),
                                                           careSiteId = c(1, 1, 1),
                                                           providerId = c(1, 2, 1))),
                   metaData = list(hasVisits = FALSE))

  # Two control without matching on time in cohort:
  cc <- selectControls(caseData = caseData,
                       outcomeId = 1,
                       washoutPeriod = 180,
                       matchOnGender = FALSE,
                       matchOnAge = FALSE,
                       matchOnTimeInCohort = FALSE)
  expect_equal(cc$personId[order(cc$personId)], c(1, 2, 3))

  # One control with simlar time in cohort:
  cc <- selectControls(caseData = caseData,
                       outcomeId = 1,
                       washoutPeriod = 180,
                       matchOnGender = FALSE,
                       matchOnAge = FALSE,
                       matchOnTimeInCohort = TRUE,
                       daysInCohortCaliper = 30)
  expect_equal(cc$personId, c(1, 3))
})

test_that("Restrict on age", {
  caseData <- list(cases = data.frame(nestingCohortId = c(1),
                                      outcomeId = c(1),
                                      indexDate = as.Date(c("2001-01-01"))),
                   nestingCohorts = ff::as.ffdf(data.frame(nestingCohortId = c(1, 2, 3),
                                                           personId = c(1, 2, 3),
                                                           observationPeriodStartDate = as.Date(c("2000-01-01", "2000-01-01", "2000-01-01")),
                                                           startDate = as.Date(c("2000-01-01", "2000-01-01", "2000-01-01")),
                                                           endDate = as.Date(c("2010-01-01", "2010-01-01", "2010-01-01")),
                                                           dateOfBirth = as.Date(c("1995-01-01", "1995-01-01", "1990-01-01")),
                                                           genderConceptId = c(8532, 8532, 8532),
                                                           careSiteId = c(1, 1, 1),
                                                           providerId = c(1, 1, 1))))

  # One case, one control without retricting on age:
  cc <- selectControls(caseData = caseData, outcomeId = 1, washoutPeriod = 180, matchOnAge = TRUE)
  expect_equal(cc$personId[order(cc$personId)], c(1, 2))

  # No cases when restricting on age:
  cc <- selectControls(caseData = caseData, outcomeId = 1, washoutPeriod = 180, matchOnAge = TRUE, minAge = 8, maxAge = 12)

  expect_equal(length(cc$personId), 0)
})


test_that("Sampling with replacement", {
  caseData <- list(cases = data.frame(nestingCohortId = c(1, 2),
                                      outcomeId = c(1, 1),
                                      indexDate = as.Date(c("2001-01-01", "2001-01-01"))),
                   nestingCohorts = ff::as.ffdf(data.frame(nestingCohortId = c(1, 2, 3),
                                                           personId = c(1, 2, 3),
                                                           observationPeriodStartDate = as.Date(c("2000-01-01", "2000-01-01", "2000-01-01")),
                                                           startDate = as.Date(c("2000-01-01", "2000-01-01", "2000-01-01")),
                                                           endDate = as.Date(c("2010-01-01", "2010-01-01", "2010-01-01")),
                                                           dateOfBirth = as.Date(c("2000-01-01", "2000-01-01", "2000-01-01")),
                                                           genderConceptId = c(8532, 8532, 8532),
                                                           careSiteId = c(1, 1, 1),
                                                           providerId = c(1, 1, 1))))

  # Control should be used twice:
  cc <- selectControls(caseData = caseData, outcomeId = 1, washoutPeriod = 180)
  expect_equal(cc$personId, c(1, 3, 2, 3))
})

