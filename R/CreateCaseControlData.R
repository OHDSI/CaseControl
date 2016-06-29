# @file CreateCaseControlData.R
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

#' Create case-control data
#'
#' @details
#' For each case and control, assesses whether exposure takes place within the risk window. The output
#' can be directly used in a conditional logistic regression.
#'
#' @param caseControlsExposure   An object of type \code{caseControlsExposure} as created using the
#'                               \code{\link{getDbExposureData}} function.
#' @param exposureId             The identifier of the exposure.
#' @param firstExposureOnly      Should only the first exposure per subject be included?
#' @param riskWindowStart        The start of the risk window (in days) relative to the index date.
#'                               This number should be non-positive.
#' @param riskWindowEnd          The end of the risk window (in days) relative to the index date. This
#'                               number should be non-positive.
#'
#' @return
#' A data frame with these columns: \describe{ \item{personId}{The person ID} \item{indexDate}{The
#' index date} \item{isCase}{Is the person a case or a control?} \item{stratumId}{The ID linking cases
#' and controls in a matched set} \item{exposed}{Was the subject exposed during the risk window?} }
#'
#' @export
createCaseControlData <- function(caseControlsExposure,
                                  exposureId,
                                  firstExposureOnly = FALSE,
                                  riskWindowStart = 0,
                                  riskWindowEnd = 0) {
  if (riskWindowStart > riskWindowEnd)
    stop("riskWindowStart cannot be after riskWindowEnd")
  if (riskWindowStart > 0)
    stop("Risk window cannot start after index date")
  if (riskWindowEnd > 0)
    stop("Risk window cannot end after index date")

  exposure <- caseControlsExposure$exposure[caseControlsExposure$exposure$exposureId == exposureId, ]
  if (firstExposureOnly) {
    exposure <- exposure[order(exposure$rowId, exposure$daysSinceExposureStart), ]
    idx <- duplicated(exposure$rowId)
    exposure <- exposure[!idx, ]
  }
  idx <- exposure$daysSinceExposureStart > (-riskWindowEnd) & exposure$daysSinceExposureEnd < (-riskWindowStart)
  exposedRowIds <- exposure$rowId[idx]
  caseControlData <- caseControlsExposure$caseControls
  caseControlData$exposed <- 0
  caseControlData$exposed[caseControlData$rowId %in% exposedRowIds] <- 1
  return(caseControlData)
}
