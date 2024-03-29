# This file has been autogenerated. Do not change by hand. 

#' Create a parameter object for the function getDbCaseData
#'
#' @details
#' Create an object defining the parameter values.
#'
#' @param useNestingCohort                    Should the study be nested in a cohort (e.g. people with
#'                                            a specific indication)? If not, the study will be nested
#'                                            in the general population.
#' @param useObservationEndAsNestingEndDate   When using a nesting cohort, should the observation
#'                                            period end date be used instead of the cohort end date?
#' @param getVisits                           Get data on visits? This is needed when matching on visit
#'                                            date is requested later on.
#' @param studyStartDate                      A calendar date specifying the minimum date where data is
#'                                            used. Date format is 'yyyymmdd'.
#' @param studyEndDate                        A calendar date specifying the maximum date where data is
#'                                            used. Date format is 'yyyymmdd'.
#' @param maxNestingCohortSize                If the nesting cohort is larger than this number it will
#'                                            be sampled to this size. maxCohortSize = 0 indicates no
#'                                            maximum size.
#' @param maxCasesPerOutcome                  If there are more than this number of cases for a single
#'                                            outcome cases will be sampled to this size.
#'                                            maxCasesPerOutcome = 0 indicates no maximum size.
#'
#' @export
createGetDbCaseDataArgs <- function(useNestingCohort = FALSE,
                                    useObservationEndAsNestingEndDate = TRUE,
                                    getVisits = FALSE,
                                    studyStartDate = "",
                                    studyEndDate = "",
                                    maxNestingCohortSize = 1e+07,
                                    maxCasesPerOutcome = 5e+05) {
  analysis <- list()
  for (name in names(formals(createGetDbCaseDataArgs))) {
    analysis[[name]] <- get(name)
  }
  class(analysis) <- "args"
  return(analysis)
}

#' Create a parameter object for the function selectControls
#'
#' @details
#' Create an object defining the parameter values.
#'
#' @param firstOutcomeOnly           Use the first outcome per person?
#' @param washoutPeriod              Minimum required numbers of days of observation for inclusion as
#'                                   either case or control.
#' @param controlSelectionCriteria   Either a matchingCriteria object as generated using the
#'                                   createMatchingCriteria function, or a samplingCriteria object as
#'                                   generated using the createSamplingCriteria function.
#' @param minAge                     Minimum age at which patient time will be included in the
#'                                   analysis. Note that information prior to the min age is still used
#'                                   to determine exposure status after the minimum age (e.g. when a
#'                                   prescription was started just prior to reaching the minimum age).
#'                                   Also, outcomes occurring before the minimum age is reached will be
#'                                   considered as prior outcomes when using first outcomes only. Age
#'                                   should be specified in years, but non-integer values are allowed.
#'                                   If not specified, no age restriction will be applied.
#' @param maxAge                     Maximum age at which patient time will be included in the
#'                                   analysis. Age should be specified in years, but non-integer values
#'                                   are allowed. If not specified, no age restriction will be applied.
#'
#' @export
createSelectControlsArgs <- function(firstOutcomeOnly = TRUE,
                                     washoutPeriod = 180,
                                     controlSelectionCriteria = createMatchingCriteria(),
                                     minAge = NULL,
                                     maxAge = NULL) {
  analysis <- list()
  for (name in names(formals(createSelectControlsArgs))) {
    analysis[[name]] <- get(name)
  }
  class(analysis) <- "args"
  return(analysis)
}

#' Create a parameter object for the function getDbExposureData
#'
#' @details
#' Create an object defining the parameter values.
#'
#' @param covariateSettings   Either an object of type covariateSettings as created using the
#'                            createCovariateSettings function in the FeatureExtraction package, or an
#'                            object of type SimpleCovariateSettings as created using the
#'                            createSimpleCovariateSettings function. If NULL then no covariate data is
#'                            retrieved.
#'
#' @export
createGetDbExposureDataArgs <- function(covariateSettings = NULL) {
  analysis <- list()
  for (name in names(formals(createGetDbExposureDataArgs))) {
    analysis[[name]] <- get(name)
  }
  class(analysis) <- "args"
  return(analysis)
}

#' Create a parameter object for the function createCaseControlData
#'
#' @details
#' Create an object defining the parameter values.
#'
#' @param firstExposureOnly       Should only the first exposure per subject be included?
#' @param riskWindowStart         The start of the risk window (in days) relative to the index date.
#'                                This number should be non-positive.
#' @param riskWindowEnd           The end of the risk window (in days) relative to the index date. This
#'                                number should be non-positive.
#' @param exposureWashoutPeriod   Minimum required numbers of days of observation for inclusion of an
#'                                exposure.
#'
#' @export
createCreateCaseControlDataArgs <- function(firstExposureOnly = FALSE,
                                            riskWindowStart = 0,
                                            riskWindowEnd = 0,
                                            exposureWashoutPeriod = 0) {
  analysis <- list()
  for (name in names(formals(createCreateCaseControlDataArgs))) {
    analysis[[name]] <- get(name)
  }
  class(analysis) <- "args"
  return(analysis)
}

#' Create a parameter object for the function fitCaseControlModel
#'
#' @details
#' Create an object defining the parameter values.
#'
#' @param useCovariates         Whether to use the covariates in the caseControlsExposure.
#' @param excludeCovariateIds   Exclude these covariates from the model.
#' @param includeCovariateIds   Include only these covariates in the model.
#' @param profileGrid           A one-dimensional grid of points on the log(relative risk) scale where
#'                              the likelihood for coefficient of variables is sampled. See details.
#' @param profileBounds         The bounds (on the log relative risk scale) for the adaptive sampling
#'                              of the likelihood function.
#' @param prior                 The prior used to fit the model. See createPrior for details.
#' @param control               The control object used to control the cross-validation used to
#'                              determine the hyperparameters of the prior (if applicable). See
#'                              createControl for details.
#'
#' @export
createFitCaseControlModelArgs <- function(useCovariates = FALSE,
                                          excludeCovariateIds = c(),
                                          includeCovariateIds = c(),
                                          profileGrid = NULL,
                                          profileBounds = c(log(0.1), log(10)),
                                          prior = createPrior("laplace", useCrossValidation = TRUE),
                                          control = createControl(cvType = "auto",
                                                                  startingVariance = 0.01,
                                                                  tolerance = 2e-07,
                                                                  cvRepetitions = 10,
                                                                  selectorType = "byPid",
                                                                  noiseLevel = "quiet")) {
  analysis <- list()
  for (name in names(formals(createFitCaseControlModelArgs))) {
    analysis[[name]] <- get(name)
  }
  class(analysis) <- "args"
  return(analysis)
}
