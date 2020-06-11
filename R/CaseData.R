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

#' Case Data
#'
#' @description
#' \code{CaseData} is an S4 class that inherits from \code{\link[Andromeda]{Andromeda}}. It contains
#' information on cases.
#'
#' A \code{CaseData} object is typically created using \code{\link{getDbCaseData}}, can only be saved using
#' \code{\link{saveCaseData}}, and loaded using \code{\link{loadCaseData}}.
#'
#' @seealso \code{\link{isCaseData}}
#' @name CaseData-class
#' @aliases CaseData
#' @export
#' @import Andromeda
setClass("CaseData", contains = "Andromeda")


#' Save the case data to file
#'
#' @description
#' \code{saveCaseData} saves an object of type caseData to file
#'
#' @param caseData   An object of type \code{CaseData} as generated using
#'                        \code{\link{getDbCaseData}}.
#' @param file            The name of the file where the data will be written. If the file
#'                        exists it will be overwritten.
#'
#' @details
#' The data will be written to the file specified by the user.
#'
#' @export
saveCaseData <- function(caseData, file) {
  if (missing(caseData))
    stop("Must specify caseData")
  if (missing(file))
    stop("Must specify file")
  if (!inherits(caseData, "CaseData"))
    stop("Data not of class CaseData")

  Andromeda::saveAndromeda(caseData, file)
}

#' Load the covariate data from a folder
#'
#' @description
#' \code{loadCaseData} loads an object of type caseData from a file in the file system.
#'
#' @param file       The name of the file containing the data.
#'
#' @details
#' The data will be read from the file specified by the user.
#'
#' @return
#' An object of class \code{CaseData}.
#'
#' @export
loadCaseData <- function(file) {
  if (!file.exists(file))
    stop("Cannot find file ", file)
  if (file.info(file)$isdir)
    stop(file , " is a folder, but should be a file")
  caseData <- Andromeda::loadAndromeda(file)
  class(caseData) <- "CaseData"
  attr(class(caseData), "package") <- "CaseControl"
  return(caseData)
}

# show()
#' @param object  An object of class `CaseData`.
#'
#' @export
#' @rdname CaseData-class
setMethod("show", "CaseData", function(object) {
  cli::cat_line(pillar::style_subtle("# CaseData object"))
  cli::cat_line("")
  outcomeIds <- attr(object, "metaData")$outcomeIds
  cli::cat_line(paste("Outcome of interest ID(s):", paste(outcomeIds, collapse = ", ")))
  cli::cat_line("")
  nestingCohortId <- attr(object, "metaData")$nestingCohortId
  if (!is.null(nestingCohortId) && nestingCohortId != -1) {
    cli::cat_line(paste("Nesting cohort ID:", nestingCohortId))
    cli::cat_line("")
  }
  cli::cat_line(pillar::style_subtle("Inherits from Andromeda:"))
  class(object) <- "Andromeda"
  attr(class(object), "package") <- "CaseControl"
  show(object)
})


# summary()
#' @param object  An object of class `CaseData`.
#'
#' @export
#' @rdname CaseData-class
setMethod("summary", "CaseData", function(object) {

  populationCount <- object$nestingCohorts %>%
    summarise(nestingCohorts = n_distinct(.data$nestingCohortId), persons = n_distinct(.data$personId)) %>%
    collect()

  outcomeCounts <- object$cases %>%
    inner_join(object$nestingCohorts, by = "nestingCohortId") %>%
    group_by(.data$outcomeId) %>%
    summarise(events = n(), nestingCohorts = n_distinct(.data$nestingCohortId), persons = n_distinct(.data$personId)) %>%
    collect()

  result <- list(metaData = attr(object, "metaData"),
                 populationCount = populationCount,
                 outcomeCounts = outcomeCounts)
  class(result) <- "summary.CaseData"
  return(result)
})

#' @export
print.summary.CaseData <- function(x, ...) {
  writeLines("CaseData object summary")
  writeLines("")
  writeLines(paste("Outcome concept ID(s):", paste(x$metaData$outcomeIds, collapse = ",")))
  if (x$metaData$nestingCohortId != -1) {
    writeLines(paste("Nesting cohort ID:", x$metaData$nestingCohortId))
  }
  writeLines("")
  writeLines(paste("Population count:", paste(x$populationCount$persons)))
  writeLines(paste("Population window count:", paste(x$populationCount$nestingCohorts)))
  writeLines("")
  writeLines("Outcome counts:")
  outcomeCounts <- as.data.frame(x$outcomeCounts)
  rownames(outcomeCounts) <- outcomeCounts$outcomeId
  outcomeCounts$outcomeId <- NULL
  colnames(outcomeCounts) <- c("Event count", "Nesting cohort count", "Person count")
  if (x$metaData$nestingCohortId == -1) {
    colnames(outcomeCounts)[2] <- "Observation period count"
  }
  printCoefmat(outcomeCounts)
}

#' Check whether an object is a CaseData object
#'
#' @param x  The object to check.
#'
#' @return
#' A logical value.
#'
#' @export
isCaseData <- function(x) {
  return(inherits(x, "CaseData"))
}
