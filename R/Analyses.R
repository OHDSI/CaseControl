# @file Analyses.R
#
# Copyright 2016 Observational Health Data Sciences and Informatics
#
# This file is part of CaseControl
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Create a case-control analysis specification
#'
#' @details
#' Create a set of analysis choices, to be used with the \code{\link{runCcAnalyses}} function.
#'
#' @param analysisId                      An integer that will be used later to refer to this specific
#'                                        set of analysis choices.
#' @param description                     A short description of the analysis.
#' @param exposureType            If more than one exposure is provided for each exposureOutcomeNestingCohort, this
#'                                field should be used to select the specific exposure to use in this
#'                                analysis.
#' @param outcomeType             If more than one outcome is provided for each exposureOutcomeNestingCohort, this
#'                                field should be used to select the specific outcome to use in this
#'                                analysis.
#' @param nestingCohortType       If more than one nesting cohort is provided for each exposureOutcomeNestingCohort, this
#'                                field should be used to select the specific nesting cohort to use in this
#'                                analysis.
#' @param getDbCaseDataArgs       An object representing the arguments to be used when calling
#'                                        the \code{\link{createGetDbCaseDataArgs}} function.
#' @param selectControlsArgs              An object representing the arguments to be used when calling
#'                                        the \code{\link{createSelectControlsArgs}} function.
#' @param createCaseControlDataArgs       An object representing the arguments to be used when calling
#'                                        the \code{\link{createCreateCaseControlDataArgs}} function.
#'
#' @export
createCcAnalysis <- function(analysisId = 1,
                             description = "",
                             exposureType = NULL,
                             outcomeType = NULL,
                             getDbCaseDataArgs,
                             selectControlsArgs,
                             createCaseControlDataArgs) {
  if (selectControlsArgs$matchOnVisitDate && getDbCaseDataArgs$getVisits)
    stop("Requested matching on visit date, but getVisits argument for getDbCaseData is FALSE")

  # First: get the default values:
  analysis <- list()
  for (name in names(formals(createccAnalysis))) {
    analysis[[name]] <- get(name)
  }

  # Next: overwrite defaults with actual values if specified:
  values <- lapply(as.list(match.call())[-1], function(x) eval(x, envir = sys.frame(-3)))
  for (name in names(values)) {
    if (name %in% names(analysis)) {
      analysis[[name]] <- values[[name]]
    }
  }

  class(analysis) <- "ccAnalysis"
  return(analysis)
}

#' Save a list of ccAnalysis to file
#'
#' @description
#' Write a list of objects of type \code{ccAnalysis} to file. The file is in JSON format.
#'
#' @param ccAnalysisList   The ccAnalysis list to be written to file
#' @param file             The name of the file where the results will be written
#'
#' @export
saveCcAnalysisList <- function(ccAnalysisList, file) {
  stopifnot(is.list(ccAnalysisList))
  stopifnot(length(ccAnalysisList) > 0)
  for (i in 1:length(ccAnalysisList)) {
    stopifnot(class(ccAnalysisList[[i]]) == "ccAnalysis")
  }
  OhdsiRTools::saveSettingsToJson(ccAnalysisList, file)
}

#' Load a list of ccAnalysis from file
#'
#' @description
#' Load a list of objects of type \code{ccAnalysis} from file. The file is in JSON format.
#'
#' @param file   The name of the file
#'
#' @return
#' A list of objects of type \code{ccAnalysis}.
#'
#' @export
loadCcAnalysisList <- function(file) {
  return(OhdsiRTools::loadSettingsFromJson(file))
}

#' Create exposure-outcome-nesting-cohort combinations.
#'
#' @details
#' Create a set of hypotheses of interest, to be used with the \code{\link{runCcAnalyses}} function.
#'
#' @param exposureId   A concept ID indentifying the target drug in the exposure table. If multiple
#'                     strategies for picking the exposure will be tested in the analysis, a named list
#'                     of numbers can be provided instead. In the analysis, the name of the number to
#'                     be used can be specified using the #' \code{exposureType} parameter in the
#'                     \code{\link{createCcAnalysis}} function.
#' @param outcomeId    A concept ID indentifying the outcome in the outcome table. If multiple
#'                     strategies for picking the outcome will be tested in the analysis, a named list
#'                     of numbers can be provided instead. In the analysis, the name of the number to
#'                     be used can be specified using the \code{outcomeType} parameter in the
#'                     \code{\link{createCcAnalysis}} function.
#' @param nestingCohortId    A concept ID indentifying the nesting cohort in the nesting cohort table. If multiple
#'                     strategies for picking the nesting cohort will be tested in the analysis, a named list
#'                     of numbers can be provided instead. In the analysis, the name of the number to
#'                     be used can be specified using the \code{nestingCohortType} parameter in the
#'                     \code{\link{createCcAnalysis}} function.
#'
#'
#' @export
createExposureOutcomeNestingCohort <- function(exposureId,
                                               outcomeId,
                                               nestingCohortId = NULL) {
  # First: get the default values:
  exposureOutcomeNestingCohort <- list()
  for (name in names(formals(createExposureOutcomeNestingCohort))) {
    exposureOutcomeNestingCohort[[name]] <- get(name)
  }

  # Next: overwrite defaults with actual values if specified:
  values <- lapply(as.list(match.call())[-1], function(x) eval(x, envir = sys.frame(-3)))
  for (name in names(values)) {
    if (name %in% names(exposureOutcomeNestingCohort)) {
      exposureOutcomeNestingCohort[[name]] <- values[[name]]
    }
  }
  class(exposureOutcomeNestingCohort) <- "exposureOutcomeNestingCohort"
  return(exposureOutcomeNestingCohort)
}

#' Save a list of drugComparatorOutcome to file
#'
#' @description
#' Write a list of objects of type \code{exposureOutcomeNestingCohort} to file. The file is in JSON format.
#'
#' @param exposureOutcomeNestingCohortList   The exposureOutcomeNestingCohort list to be written to file
#' @param file                         The name of the file where the results will be written
#'
#' @export
saveExposureOutcomeNestingCohortList <- function(exposureOutcomeNestingCohortList, file) {
  stopifnot(is.list(exposureOutcomeNestingCohortList))
  stopifnot(length(exposureOutcomeNestingCohortList) > 0)
  for (i in 1:length(exposureOutcomeNestingCohortList)) {
    stopifnot(class(exposureOutcomeNestingCohortList[[i]]) == "exposureOutcomeNestingCohort")
  }
  OhdsiRTools::saveSettingsToJson(exposureOutcomeNestingCohortList, file)
}

#' Load a list of exposureOutcomeNestingCohort from file
#'
#' @description
#' Load a list of objects of type \code{exposureOutcomeNestingCohort} from file. The file is in JSON format.
#'
#' @param file   The name of the file
#'
#' @return
#' A list of objects of type \code{drugComparatorOutcome}.
#'
#' @export
loadExposureOutcomeNestingCohortList <- function(file) {
  return(OhdsiRTools::loadSettingsFromJson(file))
}
