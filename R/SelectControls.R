# @file SelectControls.R
#
# Copyright 2021 Observational Health Data Sciences and Informatics
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
#' Criteria to use when controls are simply sampled from the (nesting) population.
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
  metaData <- attr(caseData, "metaData")
  if (matching) {
    if (controlSelectionCriteria$matchOnVisitDate && !metaData$hasVisits)
      stop("Cannot match on visits because no visit data was loaded. Please rerun getDbCaseData with getVisits = TRUE")
    if (controlSelectionCriteria$matchOnProvider && controlSelectionCriteria$matchOnCareSite)
      stop("Cannot match on both provider and care site")

    if (controlSelectionCriteria$matchOnProvider) {
      validProviderIdCount <- caseData$nestingCohorts %>% summarise(sum(!is.na(.data$providerId), na.rm = TRUE)) %>% pull()
      if (validProviderIdCount == 0)
        stop("Cannot match on provider because no providers specified in the person table")
    }
    if (controlSelectionCriteria$matchOnCareSite) {
      validCareSiteIdCount <- caseData$nestingCohorts %>% summarise(sum(!is.na(.data$careSiteId), na.rm = TRUE)) %>% pull()
      if (validCareSiteIdCount == 0)
        stop("Cannot match on care site because no care sites specified in the person table")
    }
  }

  start <- Sys.time()
  ParallelLogger::logInfo("Selecting up to ", controlSelectionCriteria$controlsPerCase, " controls per case for outcome ", outcomeId)
  cases <- caseData$cases %>%
    filter(.data$outcomeId == !!outcomeId)

  eventCount <- cases %>%
    count() %>%
    pull()
  caseCount <- cases %>%
    summarise(n_distinct(.data$nestingCohortId)) %>%
    pull()
  ParallelLogger::logDebug("Case data object has ", eventCount, " events with outcomeId ", outcomeId)
  if (eventCount > 0) {
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
      # Cannot iterate over multiple Andromeda tables simultaneously, so move smaller tables to
      # their own temp Andromeda.
      casesAndromeda <- Andromeda::andromeda()
      casesAndromeda$cases <- cases %>%
        select(.data$nestingCohortId, .data$indexDate)
      cases <- casesAndromeda$cases %>%
        arrange(.data$nestingCohortId)
      on.exit(close(casesAndromeda))

      nestingCohortAndromeda <- Andromeda::andromeda()
      nestingCohortAndromeda$nestingCohorts <- caseData$nestingCohorts %>%
        arrange(.data$nestingCohortId)
      nestingCohorts <- nestingCohortAndromeda$nestingCohorts

      on.exit(close(nestingCohortAndromeda), add = TRUE)

      if (controlSelectionCriteria$matchOnVisitDate && metaData$hasVisits) {
        visits <- caseData$visits %>%
          arrange(.data$nestingCohortId, .data$visitStartDate)


      } else {
        # Create a visits table with 1 dummy row:
        visitAndromeda <- Andromeda::andromeda()
        visitAndromeda$visits <- tibble(nestingCohortId = -1, visitStartDate = as.Date("1900-01-01"))
        visits <- visitAndromeda$visits
        on.exit(suppressWarnings(close(visitAndromeda)), add = TRUE)
      }

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
      caseControls <- as_tibble(caseControls)
      caseControls <- caseData$nestingCohorts %>%
        select(.data$personSeqId, .data$personId) %>%
        inner_join(caseControls, copy = TRUE, by = "personSeqId") %>%
        collect()

    } else {
      # Sampling
      filteredCases <- cases %>%
        inner_join(caseData$nestingCohorts, by = "nestingCohortId") %>%
        filter(.data$indexDate >= .data$startDate &
                 .data$indexDate >= .data$observationPeriodStartDate + washoutPeriod &
                 .data$indexDate <= .data$endDate) %>%
        select(.data$indexDate, .data$nestingCohortId, .data$personSeqId, .data$personId) %>%
        collect()

      if (nrow(filteredCases) == 0) {
        caseCount <- 0
        eventCount <- 0
        caseControls <- data.frame()
      } else {
        controls <- caseData$nestingCohorts %>%
          select(.data$nestingCohortId, .data$personSeqId, .data$personId, .data$observationPeriodStartDate, .data$startDate, .data$endDate) %>%
          collect()
        controls$indexDate <- sample(filteredCases$indexDate, size = nrow(controls), replace = TRUE)
        controls <- controls %>%
          filter(.data$indexDate >= .data$startDate &
                   .data$indexDate >= .data$observationPeriodStartDate + washoutPeriod &
                   .data$indexDate <= .data$endDate)
        if (firstOutcomeOnly) {
          allCases <- cases %>%
            inner_join(caseData$nestingCohorts, by = "nestingCohortId") %>%
            select(caseIndexDate = .data$indexDate, .data$personSeqId) %>%
            collect()

          controls <- controls %>%
            left_join(allCases, by = "personSeqId") %>%
            filter(is.na(.data$caseIndexDate) | .data$caseIndexDate > .data$indexDate)
        }
        maxSampleSize <- controlSelectionCriteria$controlsPerCase * nrow(filteredCases)
        if (maxSampleSize < nrow(controls)) {
          controls <- controls[sample.int(nrow(controls), maxSampleSize, replace = FALSE), ]
        }
        controls$isCase <- FALSE
        filteredCases$isCase <- TRUE
        caseControls <- bind_rows(controls[, c("personSeqId", "personId", "indexDate", "isCase")],
                                  filteredCases[, c("personSeqId", "personId",  "indexDate", "isCase")])
        caseControls$indexDate <- Andromeda::restoreDate(caseControls$indexDate)
      }
    }
    delta <- Sys.time() - start
    ParallelLogger::logInfo(paste("Selection took", signif(delta, 3), attr(delta, "units")))
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
    caseCount <- length(unique(caseControls$personSeqId[caseControls$isCase]))
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
    caseCount <- length(unique(caseControls$personSeqId[caseControls$isCase]))
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

