# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

selectControlsInternal <- function(nestingCohorts, cases, visits, firstOutcomeOnly, washoutPeriod, controlsPerCase, matchOnAge, ageCaliper, matchOnGender, matchOnProvider, matchOnCareSite, matchOnVisitDate, visitDateCaliper, matchOnTimeInCohort, daysInCohortCaliper, minAgeDays, maxAgeDays, seed) {
    .Call('_CaseControl_selectControlsInternal', PACKAGE = 'CaseControl', nestingCohorts, cases, visits, firstOutcomeOnly, washoutPeriod, controlsPerCase, matchOnAge, ageCaliper, matchOnGender, matchOnProvider, matchOnCareSite, matchOnVisitDate, visitDateCaliper, matchOnTimeInCohort, daysInCohortCaliper, minAgeDays, maxAgeDays, seed)
}

