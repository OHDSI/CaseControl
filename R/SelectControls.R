# @file SelectControls.R
#
# Copyright 2020 Observational Health Data Sciences and Informatics
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

#' Create matching criteria
#'
#' @description
#' Criteria to use to select individual matches for cases.
#'
#' @param controlsPerCase       Maximum number of controls to select per case.
#' @param matchOnAge            Match on age?
#' @param ageCaliper            Maximum difference (in years) in age when matching on age.
#' @param matchOnGender         Match on gender?
#' @param matchOnProvider       Match on provider (as specified in the person table)?
#' @param matchOnCareSite       Match on care site (as specified in the person table)?
#' @param matchOnVisitDate      Should the index date of the control be changed to the nearest visit
#'                              date?
#' @param visitDateCaliper      Maximum difference (in days) between the index date and the visit date
#'                              when matching on visit date.
#' @param matchOnTimeInCohort   Match on time in nesting cohort? When not using nesting, this is
#'                              interpreted as time observed prior to index.
#' @param daysInCohortCaliper   Maximum difference (in days) in time in cohort.
#' @param removedUnmatchedCases Should cases with no matched controls be removed?
#' @param seed                  The number generator seed. A null value sets seed via \code{\link{Sys.time}}.
#'
#' @return
#' A settings object to be used in the \code{\link{selectControls}} function.
#'
#' @export
createMatchingCriteria <- function(controlsPerCase = 1,
                                   matchOnAge = TRUE,
                                   ageCaliper = 2,
                                   matchOnGender = TRUE,
                                   matchOnProvider = FALSE,
                                   matchOnCareSite = FALSE,
                                   matchOnVisitDate = FALSE,
                                   visitDateCaliper = 30,
                                   matchOnTimeInCohort = FALSE,
                                   daysInCohortCaliper = 30,
                                   removedUnmatchedCases = TRUE,
                                   seed = 1) {
  # First: get default values:
  analysis <- list()
  for (name in names(formals(createMatchingCriteria))) {
    analysis[[name]] <- get(name)
  }
  # Second: overwrite defaults with actual values:
  values <- lapply(as.list(match.call())[-1], function(x) eval(x, envir = sys.frame(-3)))
  for (name in names(values)) {
    if (name %in% names(analysis))
      analysis[[name]] <- values[[name]]
  }
  class(analysis) <- "matchingCriteria"
  return(analysis)
}

#' Create sampling criteria
#'
#' @description
#' Criteria to use when controls are simlpy sampled from the (nesting) population.
#'
#' @param controlsPerCase       Maximum number of controls to select per case.
#' @param seed                    The number generator seed. A null value sets seed via \code{\link{Sys.time}}.
#'
#' @return
#' A settings object to be used in the \code{\link{selectControls}} function.
#'
#' @export
createSamplingCriteria <- function(controlsPerCase = 1,
                                   seed = 1) {
  # First: get default values:
  analysis <- list()
  for (name in names(formals(createSamplingCriteria))) {
    analysis[[name]] <- get(name)
  }
  # Second: overwrite defaults with actual values:
  values <- lapply(as.list(match.call())[-1], function(x) eval(x, envir = sys.frame(-3)))
  for (name in names(values)) {
    if (name %in% names(analysis))
      analysis[[name]] <- values[[name]]
  }
  class(analysis) <- "samplingCriteria"
  return(analysis)
}

#' Select controls
#'
#' @details
#' Select controls either by individually matching controls to each case, or by random sampling controls from the
#' (nested) population.
#'
#' @param caseData                 An object of type \code{caseData} as generated using the
#'                                 \code{\link{getDbCaseData}} function.
#' @param outcomeId                The outcome ID of the cases for which we need to pick controls.
#' @param firstOutcomeOnly         Use the first outcome per person?
#' @param washoutPeriod            Minimum required numbers of days of observation for inclusion as
#'                                 either case or control.
#' @param controlSelectionCriteria Either a \code{matchingCriteria} object as generated using the \code{
#'                                 \link{createMatchingCriteria}} function, or a \code{samplingCriteria}
#'                                 object as generated using the \code{\link{createSamplingCriteria}}
#'                                 function.
#' @param minAge                   Minimum age at which patient time will be included in the analysis.
#'                                 Note that information prior to the min age is still used to determine
#'                                 exposure status after the minimum age (e.g. when a prescription was
#'                                 started just prior to reaching the minimum age). Also, outcomes
#'                                 occurring before the minimum age is reached will be considered as
#'                                 prior outcomes when using first outcomes only. Age should be specified
#'                                 in years, but non-integer values are allowed. If not specified, no age
#'                                 restriction will be applied.
#' @param maxAge                   Maximum age at which patient time will be included in the analysis. Age
#'                                 should be specified in years, but non-integer values are allowed. If not
#'                                 specified, no age restriction will be applied.
#'
#' @return
#' A data frame with these columns: \describe{ \item{personId}{The person ID} \item{indexDate}{The
#' index date} \item{isCase}{Is the person a case or a control?} \item{stratumId}{The ID linking cases
#' and controls in a matched set (only available when matching)} }
#'
#' @export
selectControls <- function(caseData,
                           outcomeId,
                           firstOutcomeOnly = TRUE,
                           washoutPeriod = 180,
                           controlSelectionCriteria = createMatchingCriteria(),
                           minAge = NULL,
                           maxAge = NULL) {
  if (!(class(controlSelectionCriteria) %in% c("matchingCriteria", "samplingCriteria")))
    stop("The controlSelectionCriteria argument must be either a matchingCriteria or a samplingCriteria object.")

  matching <- (class(controlSelectionCriteria) == "matchingCriteria")
  if (matching) {
    if (controlSelectionCriteria$matchOnVisitDate && !caseData$metaData$hasVisits)
      stop("Cannot match on visits because no visit data was loaded. Please rerun getDbCaseData with getVisits = TRUE")
    if (controlSelectionCriteria$matchOnProvider && controlSelectionCriteria$matchOnCareSite)
      stop("Cannot match on both provider and care site")
    if (controlSelectionCriteria$matchOnProvider && ffbase::all.ff(ffbase::is.na.ff(caseData$nestingCohorts$providerId)))
      stop("Cannot match on provider because no providers specified in the person table")
    if (controlSelectionCriteria$matchOnCareSite && ffbase::all.ff(ffbase::is.na.ff(caseData$nestingCohorts$careSiteId)))
      stop("Cannot match on care site because no care sites specified in the person table")
  }

  start <- Sys.time()
  ParallelLogger::logInfo("Selecting up to ", controlSelectionCriteria$controlsPerCase, " controls per case for outcome ", outcomeId)
  ParallelLogger::logDebug("Case data object has ", ffbase::sum.ff(caseData$cases$outcomeId == outcomeId), " cases with outcomeId ", outcomeId)
  idx <- caseData$cases$outcomeId == outcomeId
  if (ffbase::any.ff(idx, na.rm = TRUE)) {
    cases <- ff::as.ram(caseData$cases[idx, c("nestingCohortId", "indexDate")])
    cases <- cases[order(cases$nestingCohortId), ]
    cases <- ff::as.ffdf(cases)

    nestingCohorts <- caseData$nestingCohorts
    rownames(nestingCohorts) <- NULL  #Needs to be null or the ordering of ffdf will fail
    nestingCohorts <- nestingCohorts[ff::ffdforder(nestingCohorts[c("nestingCohortId")]), ]

    if (matching) {
      if (controlSelectionCriteria$matchOnVisitDate && caseData$metaData$hasVisits) {
        visits <- caseData$visits
        if (!Cyclops::isSorted(visits, c("nestingCohortId", "visitStartDate"))) {
          ParallelLogger::logInfo("- Sorting visits")
          rownames(visits) <- NULL  #Needs to be null or the ordering of ffdf will fail
          visits <- visits[ff::ffdforder(visits[c("nestingCohortId", "visitStartDate")]), ]
        }
      } else {
        # Use one dummy visit so code won't break:
        visits <- ff::as.ffdf(data.frame(nestingCohortId = -1, visitStartDate = "1900-01-01"))
      }
    }
    if (missing(minAge) || is.null(minAge)) {
      minAgeDays <- 0
    } else {
      minAgeDays <- as.integer(minAge * 365.25)
    }
    if (missing(maxAge) || is.null(maxAge)) {
      maxAgeDays <- 9999999
    } else {
      maxAgeDays <- as.integer(maxAge * 365.25)
    }

    if (is.null(controlSelectionCriteria$seed))
      controlSelectionCriteria$seed = as.integer(Sys.time())

    if (matching) {
      caseControls <- selectControlsInternal(nestingCohorts,
                                             cases,
                                             visits,
                                             firstOutcomeOnly,
                                             washoutPeriod,
                                             controlSelectionCriteria$controlsPerCase,
                                             controlSelectionCriteria$matchOnAge,
                                             controlSelectionCriteria$ageCaliper,
                                             controlSelectionCriteria$matchOnGender,
                                             controlSelectionCriteria$matchOnProvider,
                                             controlSelectionCriteria$matchOnCareSite,
                                             controlSelectionCriteria$matchOnVisitDate,
                                             controlSelectionCriteria$visitDateCaliper,
                                             controlSelectionCriteria$matchOnTimeInCohort,
                                             controlSelectionCriteria$daysInCohortCaliper,
                                             minAgeDays,
                                             maxAgeDays,
                                             controlSelectionCriteria$seed)
      caseControls$indexDate <- as.Date(caseControls$indexDate, origin = "1970-01-01")
    } else {
      # Sampling
      caseControls <- data.frame()
      controls <- nestingCohorts
      controls$indexDate <- ff::as.ff(sample(cases$indexDate, size = nrow(nestingCohorts), replace = TRUE))
      idx <- controls$indexDate >= controls$startDate + washoutPeriod &
        controls$indexDate <= controls$endDate
      if (ffbase::any.ff(idx)) {
        controls <- controls[idx, ]
        cases <- ff::as.ram(merge(cases, nestingCohorts))
        controls <- merge(controls, ff::as.ffdf(data.frame(personId = cases$personId,
                                                           caseIndexDate = cases$indexDate)), all.x = TRUE)
        idx <- ffbase::is.na.ff(controls$caseIndexDate) | (controls$caseIndexDate > controls$indexDate)
        if (ffbase::any.ff(idx)) {
          controls <- controls[idx, ]
          maxSampleSize <- controlSelectionCriteria$controlsPerCase * nrow(cases)
          if (maxSampleSize < nrow(controls)) {
            controls <- controls[sample.int(nrow(controls), maxSampleSize, replace = FALSE), ]
          }
          controls <- ff::as.ram(controls)
          controls$isCase <- FALSE
          cases$isCase <- TRUE
          caseControls <- rbind(controls[, c("personId", "indexDate", "isCase")],
                                cases[, c("personId", "indexDate", "isCase")])

        } else {
          warning("No controls left after removing cases.")
        }
      } else {
        warning("No controls have (random) index date in their observation time.")
      }
    }
    delta <- Sys.time() - start
    ParallelLogger::logInfo(paste("Selection took", signif(delta, 3), attr(delta, "units")))

    # Create counts
    cases <- caseData$cases[caseData$cases$outcomeId == outcomeId, "nestingCohortId"]
    eventCount <- length(cases)
    if (eventCount == 0) {
      caseCount <- 0
    } else {
      t <- is.na(ffbase::ffmatch(caseData$nestingCohorts$nestingCohortId, ff::as.ff(cases)))
      caseCount <- length(ffbase::unique.ff(caseData$nestingCohorts[ffbase::ffwhich(t, t == FALSE),
                                                                    "personId"]))
    }
  } else {
    caseCount <- 0
    eventCount <- 0
    caseControls <- data.frame()
  }
  counts <- data.frame()
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
    if (missing(minAge) | is.null(minAge) | missing(maxAge) | is.null(maxAge)) {
      description = paste("Require", washoutPeriod, "days of prior obs.")
    } else {
      description = paste("Require", washoutPeriod, "days of prior obs + age restrict.")
    }
    counts <- rbind(counts,
                    data.frame(description = description,
                               eventCount = eventCount,
                               caseCount = caseCount))
  }
  if (matching && controlSelectionCriteria$removedUnmatchedCases) {
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
                   controlsPerCase = controlSelectionCriteria$controlsPerCase,
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

