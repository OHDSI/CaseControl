# @file RunAnalyses.R
#
# Copyright 2018 Observational Health Data Sciences and Informatics
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

#' Run a list of analyses
#'
#' @details
#' Run a list of analyses for the exposure-outcome-nesting cohorts of interest. This function will run
#' all specified analyses against all hypotheses of interest, meaning that the total number of outcome
#' models is `length(ccAnalysisList) * length(exposureOutcomeNestingCohortList)` (if all analyses
#' specify an outcome model should be fitted). When you provide several analyses it will determine
#' whether any of the analyses have anything in common, and will take advantage of this fact. For
#' example, if we specify several analyses that only differ in the way the outcome model is fitted,
#' then this function will extract the data and fit the propensity model only once, and re-use this in
#' all the analysis.
#'
#' @param connectionDetails                  An R object of type \code{ConnectionDetails} created using
#'                                           the function \code{createConnectionDetails} in the
#'                                           \code{DatabaseConnector} package.
#' @param cdmDatabaseSchema                  The name of the database schema that contains the OMOP CDM
#'                                           instance.  Requires read permissions to this database. On
#'                                           SQL Server, this should specifiy both the database and the
#'                                           schema, so for example 'cdm_instance.dbo'.
#' @param oracleTempSchema                   A schema where temp tables can be created in Oracle.
#' @param outcomeDatabaseSchema              The name of the database schema that is the location where
#'                                           the data used to define the outcome cohorts is available.
#'                                           If outcomeTable = CONDITION_ERA, outcomeDatabaseSchema is
#'                                           not used.  Requires read permissions to this database.
#' @param outcomeTable                       The tablename that contains the outcome cohorts.  If
#'                                           outcomeTable is not CONDITION_OCCURRENCE or CONDITION_ERA,
#'                                           then expectation is outcomeTable has format of COHORT
#'                                           table: COHORT_DEFINITION_ID, SUBJECT_ID,
#'                                           COHORT_START_DATE, COHORT_END_DATE.
#' @param exposureDatabaseSchema             The name of the database schema that is the location where
#'                                           the exposure data used to define the exposure cohorts is
#'                                           available. If exposureTable = DRUG_ERA,
#'                                           exposureDatabaseSchema is not used but assumed to be
#'                                           cdmSchema.  Requires read permissions to this database.
#' @param exposureTable                      The tablename that contains the exposure cohorts.  If
#'                                           exposureTable <> drug_era, then expectation is
#'                                           exposureTable has format of COHORT table:
#'                                           cohort_definition_id, subject_id, cohort_start_date,
#'                                           cohort_end_date.
#' @param nestingCohortDatabaseSchema        The name of the database schema that is the location where
#'                                           the nesting cohort is defined.
#' @param nestingCohortTable                 Name of the table holding the nesting cohort. This table
#'                                           should have the same structure as the cohort table.
#' @param ccAnalysisList                     A list of objects of type \code{ccAnalysis} as created
#'                                           using the \code{\link{createCcAnalysis}} function.
#' @param exposureOutcomeNestingCohortList   A list of objects of type
#'                                           \code{exposureOutcomeNestingCohort} as created using the
#'                                           \code{\link{createExposureOutcomeNestingCohort}} function.
#' @param outputFolder                       Name of the folder where all the outputs will written to.
#' @param prefetchExposureData               Should exposure data for the entire nesting cohort be fetched at
#'                                           the beginning, or should exposure data be fetch later specifically
#'                                           for a set of cases and controls. Prefetching can be faster
#'                                           when there are many outcomes but only few exposures. Prefetching
#'                                           does not speed up performance when covariates also need to be
#'                                           constructed.
#' @param compressCaseDataFiles              Should compression be used when saving?
#' @param getDbCaseDataThreads               The number of parallel threads to use for building the
#'                                           caseData objects.
#' @param selectControlsThreads              The number of parallel threads to use for selecting
#'                                           controls.
#' @param getDbExposureDataThreads           The number of parallel threads to use for fetchign data on
#'                                           exposures for cases and controls.
#' @param createCaseControlDataThreads       The number of parallel threads to use for creating case
#'                                           and control data including exposure status indicators
#' @param fitCaseControlModelThreads         The number of parallel threads to use for fitting the
#'                                           models.
#' @param cvThreads                          The number of parallel threads used for the
#'                                           cross-validation to determine the hyper-parameter when
#'                                           fitting the model.
#'
#' @export
runCcAnalyses <- function(connectionDetails,
                          cdmDatabaseSchema,
                          oracleTempSchema = cdmDatabaseSchema,
                          exposureDatabaseSchema = cdmDatabaseSchema,
                          exposureTable = "drug_era",
                          outcomeDatabaseSchema = cdmDatabaseSchema,
                          outcomeTable = "condition_era",
                          nestingCohortDatabaseSchema = cdmDatabaseSchema,
                          nestingCohortTable = "condition_era",
                          outputFolder = "./CcOutput",
                          ccAnalysisList,
                          exposureOutcomeNestingCohortList,
                          prefetchExposureData = FALSE,
                          compressCaseDataFiles = FALSE,
                          getDbCaseDataThreads = 1,
                          selectControlsThreads = 1,
                          getDbExposureDataThreads = 1,
                          createCaseControlDataThreads = 1,
                          fitCaseControlModelThreads = 1,
                          cvThreads = 1) {
  for (exposureOutcomeNestingCohort in exposureOutcomeNestingCohortList) stopifnot(class(exposureOutcomeNestingCohort) ==
                                                                                     "exposureOutcomeNestingCohort")
  for (ccAnalysis in ccAnalysisList) stopifnot(class(ccAnalysis) == "ccAnalysis")
  uniqueExposureOutcomeNcList <- unique(ParallelLogger::selectFromList(exposureOutcomeNestingCohortList,
                                                                    c("exposureId",
                                                                      "outcomeId",
                                                                      "nestingCohortId")))
  if (length(uniqueExposureOutcomeNcList) != length(exposureOutcomeNestingCohortList))
    stop("Duplicate exposure-outcome-nesting cohort combinations are not allowed")
  uniqueAnalysisIds <- unlist(unique(ParallelLogger::selectFromList(ccAnalysisList, "analysisId")))
  if (length(uniqueAnalysisIds) != length(ccAnalysisList))
    stop("Duplicate analysis IDs are not allowed")

  if (!file.exists(outputFolder))
    dir.create(outputFolder)

  outcomeReference <- data.frame()
  for (ccAnalysis in ccAnalysisList) {
    analysisId <- ccAnalysis$analysisId
    for (exposureOutcomeNc in exposureOutcomeNestingCohortList) {
      exposureId <- .selectByType(ccAnalysis$exposureType, exposureOutcomeNc$exposureId, "exposure")
      outcomeId <- .selectByType(ccAnalysis$outcomeType, exposureOutcomeNc$outcomeId, "outcome")
      nestingCohortId <- .selectByType(ccAnalysis$nestingCohortType,
                                       exposureOutcomeNc$nestingCohortId,
                                       "nestingCohort")
      if (is.null(nestingCohortId)) {
        nestingCohortId <- NA
      }
      row <- data.frame(exposureId = exposureId,
                        outcomeId = outcomeId,
                        nestingCohortId = nestingCohortId,
                        analysisId = analysisId)
      outcomeReference <- rbind(outcomeReference, row)
    }
  }

  cdObjectsToCreate <- list()
  getDbCaseDataArgsList <- unique(ParallelLogger::selectFromList(ccAnalysisList,
                                                              c("getDbCaseDataArgs")))
  for (d in 1:length(getDbCaseDataArgsList)) {
    getDbCaseDataArgs <- getDbCaseDataArgsList[[d]]
    analyses <- ParallelLogger::matchInList(ccAnalysisList, getDbCaseDataArgs)
    analysesIds <- unlist(ParallelLogger::selectFromList(analyses, "analysisId"))
    if (getDbCaseDataArgs$getDbCaseDataArgs$useNestingCohort) {
      nestingCohortIds <- unique(outcomeReference$nestingCohortId[outcomeReference$analysisId %in%
                                                                    analysesIds])
      for (nestingCohortId in nestingCohortIds) {
        if (is.na(nestingCohortId)) {
          idx <- outcomeReference$analysisId %in% analysesIds & is.na(outcomeReference$nestingCohortId)
        } else {
          idx <- outcomeReference$analysisId %in% analysesIds & outcomeReference$nestingCohortId ==
            nestingCohortId
        }
        outcomeIds <- unique(outcomeReference$outcomeId[idx])

        cdFileName <- .createCaseDataFileName(d, nestingCohortId)
        outcomeReference$caseDataFolder[idx] <- cdFileName
        if (!file.exists(file.path(outputFolder, cdFileName))) {
          args <- list(connectionDetails = connectionDetails,
                       cdmDatabaseSchema = cdmDatabaseSchema,
                       oracleTempSchema = oracleTempSchema,
                       outcomeDatabaseSchema = outcomeDatabaseSchema,
                       outcomeTable = outcomeTable,
                       nestingCohortDatabaseSchema = nestingCohortDatabaseSchema,
                       nestingCohortTable = nestingCohortTable,
                       outcomeIds = outcomeIds,
                       nestingCohortId = nestingCohortId,
                       getExposures = prefetchExposureData)
          if (prefetchExposureData) {
            args$exposureDatabaseSchema <- exposureDatabaseSchema
            args$exposureTable <- exposureTable
            args$exposureIds <- unique(outcomeReference$exposureId[idx])
          }
          args <- append(args, getDbCaseDataArgs$getDbCaseDataArgs)
          if (is.na(nestingCohortId)) {
            args$nestingCohortId <- NULL
            args$useObservationEndAsNestingEndDate <- FALSE
          }
          cdObjectsToCreate[[length(cdObjectsToCreate) + 1]] <- list(args = args,
                                                                     compressCaseDataFiles = compressCaseDataFiles,
                                                                     cdFileName = file.path(outputFolder, cdFileName))
        }
      }
    } else {
      idx <- outcomeReference$analysisId %in% analysesIds
      outcomeIds <- unique(outcomeReference$outcomeId[idx])
      cdFileName <- .createCaseDataFileName(d)
      idx <- outcomeReference$analysisId %in% analysesIds
      outcomeReference$caseDataFolder[idx] <- cdFileName
      if (!file.exists(file.path(outputFolder, cdFileName))) {
        args <- list(connectionDetails = connectionDetails,
                     cdmDatabaseSchema = cdmDatabaseSchema,
                     oracleTempSchema = oracleTempSchema,
                     outcomeDatabaseSchema = outcomeDatabaseSchema,
                     outcomeTable = outcomeTable,
                     nestingCohortDatabaseSchema = nestingCohortDatabaseSchema,
                     nestingCohortTable = nestingCohortTable,
                     outcomeIds = outcomeIds,
                     nestingCohortId = nestingCohortId,
                     getExposures = prefetchExposureData)
        if (prefetchExposureData) {
          args$exposureDatabaseSchema <- exposureDatabaseSchema
          args$exposureTable <- exposureTable
          args$exposureIds <- unique(outcomeReference$exposureId[idx])
        }
        args <- append(args, getDbCaseDataArgs$getDbCaseDataArgs)
        cdObjectsToCreate[[length(cdObjectsToCreate) + 1]] <- list(args = args,
                                                                   compressCaseDataFiles = compressCaseDataFiles,
                                                                   cdFileName = file.path(outputFolder, cdFileName))
      }
    }
  }

  ccObjectsToCreate <- list()
  selectControlsArgsList <- unique(ParallelLogger::selectFromList(ccAnalysisList,
                                                               c("selectControlsArgs")))
  for (i in 1:length(selectControlsArgsList)) {
    selectControlsArgs <- selectControlsArgsList[[i]]
    analyses <- ParallelLogger::matchInList(ccAnalysisList, selectControlsArgs)
    analysesIds <- unlist(ParallelLogger::selectFromList(analyses, "analysisId"))
    cdFileNames <- unique(outcomeReference$caseDataFolder[outcomeReference$analysisId %in% analysesIds])
    for (cdFileName in cdFileNames) {
      cdId <- gsub("^.*caseData_", "", cdFileName)
      idx <- outcomeReference$analysisId %in% analysesIds & outcomeReference$caseDataFolder ==
        cdFileName
      outcomeIds <- unique(outcomeReference$outcomeId[idx])
      for (outcomeId in outcomeIds) {
        ccFilename <- .createCaseControlsFileName(cdId, i, outcomeId)
        outcomeReference$caseControlsFile[idx & outcomeReference$outcomeId == outcomeId] <- ccFilename
        if (!file.exists(file.path(outputFolder, ccFilename))) {
          args <- list(outcomeId = outcomeId)
          args <- append(args, selectControlsArgs$selectControlsArgs)
          ccObjectsToCreate[[length(ccObjectsToCreate) + 1]] <- list(args = args,
                                                                     cdFileName = file.path(outputFolder, cdFileName),
                                                                     ccFilename = file.path(outputFolder, ccFilename))
        }
      }
    }
  }

  edObjectsToCreate <- list()
  for (ccFilename in unique(outcomeReference$caseControlsFile)) {
    analysisIds <- unique(outcomeReference$analysisId[outcomeReference$caseControlsFile == ccFilename])
    edArgsList <- unique(sapply(ccAnalysisList, function(x) if (x$analysisId %in% analysisIds)
      return(x$getDbExposureDataArgs), simplify = FALSE))
    edArgsList <- edArgsList[!sapply(edArgsList, is.null)]
    for (ed in 1:length(edArgsList)) {
      edArgs <- edArgsList[[ed]]
      analysisIds <- unlist(unique(ParallelLogger::selectFromList(ParallelLogger::matchInList(ccAnalysisList,
                                                                                        list(getDbExposureDataArgs = edArgs)),
                                                               "analysisId")))
      idx <- outcomeReference$caseControlsFile == ccFilename & outcomeReference$analysisId %in%
        analysisIds
      exposureIds <- unique(outcomeReference$exposureId[idx])
      edFilename <- .createExposureDataFileName(ccFilename, ed)
      outcomeReference$exposureDataFile[idx] <- edFilename
      if (!file.exists(file.path(outputFolder, edFilename))) {
        args <- list(connectionDetails = connectionDetails,
                     oracleTempSchema = oracleTempSchema,
                     exposureDatabaseSchema = exposureDatabaseSchema,
                     exposureTable = exposureTable,
                     exposureIds = exposureIds,
                     cdmDatabaseSchema = cdmDatabaseSchema)
        if (prefetchExposureData) {
          cdFilename <- outcomeReference$caseDataFolder[outcomeReference$caseControlsFile == ccFilename][1]
        } else {
          cdFilename <- NULL
        }
        args <- append(args, edArgs)
        edObjectsToCreate[[length(edObjectsToCreate) + 1]] <- list(args = args,
                                                                   ccFilename = file.path(outputFolder, ccFilename),
                                                                   cdFilename = file.path(outputFolder, cdFilename),
                                                                   edFilename = file.path(outputFolder, edFilename))
      }
    }
  }

  ccdObjectsToCreate <- list()
  for (edFilename in unique(outcomeReference$exposureDataFile)) {
    analysisIds <- unique(outcomeReference$analysisId[outcomeReference$exposureDataFile == edFilename])
    ccdArgsList <- unique(sapply(ccAnalysisList, function(x) if (x$analysisId %in% analysisIds)
      return(x$createCaseControlDataArgs), simplify = FALSE))
    ccdArgsList <- ccdArgsList[!sapply(ccdArgsList, is.null)]
    for (ccd in 1:length(ccdArgsList)) {
      ccdArgs <- ccdArgsList[[ccd]]
      analysisIds <- unlist(unique(ParallelLogger::selectFromList(ParallelLogger::matchInList(ccAnalysisList,
                                                                                        list(createCaseControlDataArgs = ccdArgs)),
                                                               "analysisId")))
      idx <- outcomeReference$exposureDataFile == edFilename & outcomeReference$analysisId %in%
        analysisIds
      exposureIds <- unique(outcomeReference$exposureId[idx])
      for (exposureId in exposureIds) {
        ccdFilename <- .createCaseControlDataFileName(edFilename, exposureId, ccd)
        outcomeReference$caseControlDataFile[idx & outcomeReference$exposureId == exposureId] <- ccdFilename
        if (!file.exists(file.path(outputFolder, ccdFilename))) {
          args <- ccdArgs
          args$exposureId <- exposureId
          ccdObjectsToCreate[[length(ccdObjectsToCreate) + 1]] <- list(args = args,
                                                                       ccdFilename = file.path(outputFolder, ccdFilename),
                                                                       edFilename = file.path(outputFolder, edFilename))
        }
      }
    }
  }

  modelObjectsToCreate <- list()
  for (ccAnalysis in ccAnalysisList) {
    # ccAnalysis = ccAnalysisList[[1]]
    analysisFolder <- paste("Analysis_", ccAnalysis$analysisId, sep = "")
    if (!file.exists(file.path(outputFolder, analysisFolder)))
      dir.create(file.path(outputFolder, analysisFolder))
    for (i in which(outcomeReference$analysisId == ccAnalysis$analysisId)) {
      # i = 1
      exposureId <- outcomeReference$exposureId[i]
      outcomeId <- outcomeReference$outcomeId[i]
      edFilename <- outcomeReference$exposureDataFile[i]
      ccdFilename <- outcomeReference$caseControlDataFile[i]
      modelFilename <- .createModelFileName(analysisFolder, exposureId, outcomeId)
      outcomeReference$modelFile[i] <- modelFilename
      if (!file.exists(file.path(outputFolder, modelFilename))) {
        args <- ccAnalysis$fitCaseControlModelArgs
        args$control$threads <- cvThreads
        modelObjectsToCreate[[length(modelObjectsToCreate) + 1]] <- list(args = args,
                                                                         ccdFilename = file.path(outputFolder, ccdFilename),
                                                                         edFilename = file.path(outputFolder, edFilename),
                                                                         modelFilename = file.path(outputFolder, modelFilename))
      }
    }
  }

  saveRDS(outcomeReference, file.path(outputFolder, "outcomeModelReference.rds"))

  ### Actual construction of objects ###

  ParallelLogger::logInfo("*** Creating caseData objects ***")
  if (length(cdObjectsToCreate) != 0) {
    cluster <- ParallelLogger::makeCluster(getDbCaseDataThreads)
    ParallelLogger::clusterRequire(cluster, "CaseControl")
    dummy <- ParallelLogger::clusterApply(cluster, cdObjectsToCreate, createCaseDataObject)
    ParallelLogger::stopCluster(cluster)
  }

  ParallelLogger::logInfo("*** Creating caseControls objects ***")
  if (length(ccObjectsToCreate) != 0) {
    cluster <- ParallelLogger::makeCluster(selectControlsThreads)
    ParallelLogger::clusterRequire(cluster, "CaseControl")
    ParallelLogger::clusterRequire(cluster, "ffbase")
    dummy <- ParallelLogger::clusterApply(cluster, ccObjectsToCreate, createCaseControlsObject)
    ParallelLogger::stopCluster(cluster)
  }

  ParallelLogger::logInfo("*** Creating caseControlsExposure objects ***")
  if (length(edObjectsToCreate) != 0) {
    cluster <- ParallelLogger::makeCluster(getDbExposureDataThreads)
    ParallelLogger::clusterRequire(cluster, "CaseControl")
    ParallelLogger::clusterRequire(cluster, "ffbase")
    dummy <- ParallelLogger::clusterApply(cluster, edObjectsToCreate, createExposureDataObject)
    ParallelLogger::stopCluster(cluster)
  }

  ParallelLogger::logInfo("*** Creating caseControlData objects ***")
  if (length(ccdObjectsToCreate) != 0) {
    cluster <- ParallelLogger::makeCluster(createCaseControlDataThreads)
    ParallelLogger::clusterRequire(cluster, "CaseControl")
    ParallelLogger::clusterRequire(cluster, "ffbase")
    dummy <- ParallelLogger::clusterApply(cluster, ccdObjectsToCreate, createCaseControlDataObject)
    ParallelLogger::stopCluster(cluster)
  }

  ParallelLogger::logInfo("*** Creating case-control model objects ***")
  if (length(modelObjectsToCreate) != 0) {
    cluster <- ParallelLogger::makeCluster(fitCaseControlModelThreads)
    ParallelLogger::clusterRequire(cluster, "CaseControl")
    ParallelLogger::clusterRequire(cluster, "ffbase")
    dummy <- ParallelLogger::clusterApply(cluster, modelObjectsToCreate, createCaseControlModelObject)
    ParallelLogger::stopCluster(cluster)
  }

  invisible(outcomeReference)
}


getCaseData <- function(caseDataFileName) {
  if (mget("caseDataFileName", envir = globalenv(), ifnotfound = "") == caseDataFileName) {
    caseData <- get("caseData", envir = globalenv())
  } else {
    caseData <- loadCaseData(caseDataFileName, readOnly = TRUE)
    assign("caseData", caseData, envir = globalenv())
    assign("caseDataFileName", caseDataFileName, envir = globalenv())
  }
  return(caseData)
}

createCaseDataObject <- function(params) {
  caseData <- do.call("getDbCaseData", params$args)
  saveCaseData(caseData, params$cdFileName, compress = params$compressCaseDataFiles)
  return(NULL)
}

createCaseControlsObject <- function(params) {
  caseData <- getCaseData(params$cdFileName)
  params$args$caseData <- caseData
  caseControls <- do.call("selectControls", params$args)
  saveRDS(caseControls, params$ccFilename)
  return(NULL)
}

createExposureDataObject <- function(params) {
  caseControls <- readRDS(params$ccFilename)
  params$args$caseControls <- caseControls
  if (length(params$cdFilename) != 0) {
    caseData <- getCaseData(params$cdFilename)
    params$args$caseData <- caseData
  }
  exposureData <- do.call("getDbExposureData", params$args)
  saveCaseControlsExposure(exposureData, params$edFilename)
  return(NULL)
}

createCaseControlDataObject <- function(params) {
  exposureData <- loadCaseControlsExposure(params$edFilename)
  params$args$caseControlsExposure <- exposureData
  caseControlData <- do.call("createCaseControlData", params$args)
  saveRDS(caseControlData, params$ccdFilename)
  return(NULL)
}

createCaseControlModelObject <- function(params) {
  caseControlData <- readRDS(params$ccdFilename)
  exposureData <- loadCaseControlsExposure(params$edFilename)
  params$args$caseControlData <- caseControlData
  params$args$caseControlsExposure <- exposureData
  model <- do.call("fitCaseControlModel", params$args)
  saveRDS(model, params$modelFilename)
  return(NULL)
}

.createCaseDataFileName <- function(loadId, nestingCohortId = NULL) {
  name <- paste0("caseData_cd", loadId)
  if (!is.null(nestingCohortId) && !is.na(nestingCohortId))
    name <- paste0(name, "_n", nestingCohortId)
  return(name)
}

.createCaseControlsFileName <- function(cdId, i, outcomeId) {
  name <- paste0("caseControls_", cdId, "_cc", i, "_o", outcomeId, ".rds")
  return(name)
}

.createExposureDataFileName <- function(ccFilename, ed) {
  name <- gsub("caseControls_", "exposureData_", ccFilename)
  name <- gsub(".rds", "", name)
  name <- paste0(name, "_ed", ed)
  return(name)
}

.createCaseControlDataFileName <- function(edFilename, exposureId, ccd) {
  name <- gsub("exposureData_", "ccd_", edFilename)
  name <- paste0(name, "_e", exposureId, "_ccd", ccd, ".rds")
  return(name)
}

.createModelFileName <- function(folder, exposureId, outcomeId) {
  name <- paste("model_e", exposureId, "_o", outcomeId, ".rds", sep = "")
  return(file.path(folder, name))
}

.selectByType <- function(type, value, label) {
  if (is.null(type)) {
    if (is.list(value)) {
      stop(paste("Multiple ",
                 label,
                 "s specified, but none selected in analyses (comparatorType).",
                 sep = ""))
    }
    return(value)
  } else {
    if (!is.list(value) || is.null(value[type])) {
      stop(paste(label, "type not found:", type))
    }
    return(value[type])
  }
}

#' Create a summary report of the analyses
#'
#' @param outcomeReference   A data.frame as created by the \code{\link{runCcAnalyses}} function.
#' @param outputFolder       Name of the folder where all the outputs have been written to.
#'
#' @export
summarizeCcAnalyses <- function(outcomeReference, outputFolder) {
  columns <- c("analysisId", "exposureId", "nestingCohortId", "outcomeId")
  result <- outcomeReference[, columns]
  result$rr <- 0
  result$ci95lb <- 0
  result$ci95ub <- 0
  result$p <- 1
  result$cases <- 0
  result$controls <- 0
  result$exposedCases <- 0
  result$exposedControls <- 0
  result$logRr <- 0
  result$seLogRr <- 0
  for (i in 1:nrow(outcomeReference)) {
    if (outcomeReference$modelFile[i] != "") {
      model <- readRDS(file.path(outputFolder, outcomeReference$modelFile[i]))
      result$rr[i] <- if (is.null(coef(model)))
        NA else exp(coef(model))
      result$ci95lb[i] <- if (is.null(coef(model)))
        NA else exp(confint(model)[1])
      result$ci95ub[i] <- if (is.null(coef(model)))
        NA else exp(confint(model)[2])
      if (is.null(coef(model))) {
        result$p[i] <- NA
      } else {
        z <- coef(model)/model$outcomeModelTreatmentEstimate$seLogRr
        result$p[i] <- 2 * pmin(pnorm(z), 1 - pnorm(z))
      }
      result$cases[i] <- model$outcomeCounts$cases
      result$controls[i] <- model$outcomeCounts$controls
      result$exposedCases[i] <- model$outcomeCounts$exposedCases
      result$exposedControls[i] <- model$outcomeCounts$exposedControls
      result$logRr[i] <- if (is.null(coef(model)))
        NA else coef(model)
      result$seLogRr[i] <- if (is.null(coef(model)))
        NA else model$outcomeModelTreatmentEstimate$seLogRr
    }
  }
  return(result)
}
