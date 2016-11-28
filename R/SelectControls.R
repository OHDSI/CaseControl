# @file SelectControls.R
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

#' Select matched controls per case
#'
#' @details
#' Select controls per case. Controls are matched on calendar time and the criteria defined in the
#' arguments. Controls are randomly sampled to the required number.
#'
#' @param caseData                An object of type \code{caseData} as generated using the
#'                                \code{\link{getDbCaseData}} function.
#' @param outcomeId               The outcome ID of the cases for which we need to pick controls.
#' @param firstOutcomeOnly        Use the first outcome per person?
#' @param washoutPeriod           Minimum required numbers of days of observation for inclusion as
#'                                either case or control.
#' @param controlsPerCase         Maximum number of controls to select per case.
#' @param matchOnAge              Match on age?
#' @param ageCaliper              Maximum difference (in years) in age when matching on age.
#' @param matchOnGender           Match on gender?
#' @param matchOnProvider         Match on provider (as specified in the person table)?
#' @param matchOnCareSite         Match on care site (as specified in the person table)?
#' @param matchOnVisitDate        Should the index date of the control be changed to the nearest visit
#'                                date?
#' @param visitDateCaliper        Maximum difference (in days) between the index date and the visit
#'                                date when matching on visit date.
#' @param matchOnTimeInCohort     Match on time in nesting cohort? When not using nesting, this is
#'                                interpreted as time observed prior to index.
#' @param daysInCohortCaliper     Maximum difference (in days) in time in cohort.
#' @param removedUnmatchedCases   Should cases with no matched controls be removed?
#'
#' @return
#' A data frame with these columns: \describe{ \item{personId}{The person ID} \item{indexDate}{The
#' index date} \item{isCase}{Is the person a case or a control?} \item{stratumId}{The ID linking cases
#' and controls in a matched set} }
#'
#' @export
selectControls <- function(caseData,
                           outcomeId,
                           firstOutcomeOnly = TRUE,
                           washoutPeriod = 180,
                           controlsPerCase = 2,
                           matchOnAge = TRUE,
                           ageCaliper = 2,
                           matchOnGender = TRUE,
                           matchOnProvider = FALSE,
                           matchOnCareSite = FALSE,
                           matchOnVisitDate = FALSE,
                           visitDateCaliper = 30,
                           matchOnTimeInCohort = FALSE,
                           daysInCohortCaliper = 30,
                           removedUnmatchedCases = TRUE) {
  if (matchOnVisitDate && !caseData$metaData$hasVisits)
    stop("Cannot match on visits because no visit data was loaded. Please rerun getDbCaseData with getVisits = TRUE")
  if (matchOnProvider && matchOnCareSite)
    stop("Cannot match on both provider and care site")
    if (matchOnProvider && ffbase::all.ff(ffbase::is.na.ff(caseData$nestingCohorts$providerId)))
    stop("Cannot match on provider because no providers specified in the person table")
  if (matchOnCareSite && ffbase::all.ff(ffbase::is.na.ff(caseData$nestingCohorts$careSiteId)))
    stop("Cannot match on care site because no care sites specified in the person table")

  start <- Sys.time()
  writeLines(paste("Selecting up to", controlsPerCase, "controls per case"))

  cases <- caseData$cases[caseData$cases$outcomeId == outcomeId, c("nestingCohortId", "indexDate")]
  cases <- cases[order(cases$nestingCohortId), ]
  cases <- ff::as.ffdf(cases)

  nestingCohorts <- caseData$nestingCohorts
  rownames(nestingCohorts) <- NULL  #Needs to be null or the ordering of ffdf will fail
  nestingCohorts <- nestingCohorts[ff::ffdforder(nestingCohorts[c("nestingCohortId")]), ]

  if (matchOnVisitDate && caseData$metaData$hasVisits) {
    visits <- caseData$visits
    if (!Cyclops::isSorted(visits, c("nestingCohortId", "visitStartDate"))) {
      writeLines("- Sorting visits")
      rownames(visits) <- NULL  #Needs to be null or the ordering of ffdf will fail
      visits <- visits[ff::ffdforder(visits[c("nestingCohortId", "visitStartDate")]), ]
    }
  } else {
    # Use one dummy visit so code won't break:
    visits <- ff::as.ffdf(data.frame(nestingCohortId = -1, visitStartDate = "1900-01-01"))
  }

  caseControls <- .selectControls(nestingCohorts,
                                  cases,
                                  visits,
                                  firstOutcomeOnly,
                                  washoutPeriod,
                                  controlsPerCase,
                                  matchOnAge,
                                  ageCaliper,
                                  matchOnGender,
                                  matchOnProvider,
                                  matchOnCareSite,
                                  matchOnVisitDate,
                                  visitDateCaliper,
                                  matchOnTimeInCohort,
                                  daysInCohortCaliper)
  caseControls$indexDate <- as.Date(caseControls$indexDate, origin = "1970-01-01")
  delta <- Sys.time() - start
  writeLines(paste("Selection took", signif(delta, 3), attr(delta, "units")))

  # Create counts
  counts <- data.frame()

  cases <- caseData$cases[caseData$cases$outcomeId == outcomeId, "nestingCohortId"]
  eventCount <- length(cases)
  if (eventCount == 0) {
    caseCount <- 0
  } else {
    t <- is.na(ffbase::ffmatch(caseData$nestingCohorts$nestingCohortId, ff::as.ff(cases)))
    caseCount <- length(ffbase::unique.ff(caseData$nestingCohorts[ffbase::ffwhich(t, t == FALSE),
                                          "personId"]))
  }
  counts <- rbind(counts, data.frame(description = "Original counts",
                                     eventCount = eventCount,
                                     caseCount = caseCount))

  if (firstOutcomeOnly) {
    counts <- rbind(counts, data.frame(description = "First event only",
                                       eventCount = caseCount,
                                       caseCount = caseCount))
  }

  if (washoutPeriod != 0) {
    eventCount <- sum(caseControls$isCase)
    caseCount <- length(unique(caseControls$personId[caseControls$isCase]))
    counts <- rbind(counts,
                    data.frame(description = paste("Require", washoutPeriod, "days of prior obs."),
                                       eventCount = eventCount,
                                       caseCount = caseCount))
  }
  if (removedUnmatchedCases) {
    strataWithControls <- caseControls$stratumId[caseControls$isCase == FALSE]
    caseControls <- caseControls[caseControls$stratumId %in% strataWithControls, ]
    eventCount <- sum(caseControls$isCase)
    caseCount <- length(unique(caseControls$personId[caseControls$isCase]))
    counts <- rbind(counts, data.frame(description = paste("Remove unmatched controls"),
                                       eventCount = eventCount,
                                       caseCount = caseCount))
  }

  metaData <- list(nestingCohortId = caseData$metaData$nestingCohortId,
                   outcomeId = outcomeId,
                   firstOutcomeOnly = firstOutcomeOnly,
                   washoutPeriod = washoutPeriod,
                   controlsPerCase = controlsPerCase,
                   matchOnAge = matchOnAge,
                   ageCaliper = ageCaliper,
                   matchOnGender = matchOnGender,
                   matchOnProvider = matchOnProvider,
                   matchOnCareSite = matchOnCareSite,
                   matchOnVisitDate = matchOnVisitDate,
                   visitDateCaliper = visitDateCaliper,
                   counts = counts)
  attr(caseControls, "metaData") <- metaData
  return(caseControls)
}

#' Get the attrition table for a population
#'
#' @param caseControls   A data frame of cases and controls as generated by the function
#'                       \code{\link{selectControls}}.
#'
#' @return
#' A data frame specifying the number of cases and events after various steps of filtering.
#'
#'
#' @export
getAttritionTable <- function(caseControls) {
  if (is.null(attr(caseControls, "metaData")))
    stop("Metadata not found. Has this object been generated using the selectControls function?")

  return(attr(caseControls, "metaData")$counts)
}

