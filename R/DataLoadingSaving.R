# @file DataLoadingSaving.R
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

#' Load case data from the database
#'
#' @description
#' Load all data about the cases and nesting cohort from the database.
#'
#' @return
#' Returns an object of type \code{caseData}, containing information on the cases, the nesting cohort,
#' and optionally visits. Information about multiple outcomes can be captured at once for efficiency
#' reasons. The generic \code{summary()} function has been implemented for this object.
#'
#' @param connectionDetails                   An R object of type \code{ConnectionDetails} created
#'                                            using the function \code{createConnectionDetails} in the
#'                                            \code{DatabaseConnector} package.
#' @param cdmDatabaseSchema                   The name of the database schema that contains the OMOP
#'                                            CDM instance.  Requires read permissions to this
#'                                            database. On SQL Server, this should specifiy both the
#'                                            database and the schema, so for example
#'                                            'cdm_instance.dbo'.
#' @param oracleTempSchema                    A schema where temp tables can be created in Oracle.
#' @param outcomeDatabaseSchema               The name of the database schema that is the location
#'                                            where the data used to define the outcome cohorts is
#'                                            available. If outcomeTable = CONDITION_ERA,
#'                                            outcomeDatabaseSchema is not used.  Requires read
#'                                            permissions to this database.
#' @param outcomeTable                        The tablename that contains the outcome cohorts.  If
#'                                            outcomeTable is not CONDITION_OCCURRENCE or
#'                                            CONDITION_ERA, then expectation is outcomeTable has
#'                                            format of COHORT table: COHORT_DEFINITION_ID, SUBJECT_ID,
#'                                            COHORT_START_DATE, COHORT_END_DATE.
#' @param outcomeIds                          A list of ids used to define outcomes.  If outcomeTable =
#'                                            CONDITION_OCCURRENCE, the list is a set of ancestor
#'                                            CONCEPT_IDs, and all occurrences of all descendant
#'                                            concepts will be selected.  If outcomeTable <>
#'                                            CONDITION_OCCURRENCE, the list contains records found in
#'                                            COHORT_DEFINITION_ID field.
#' @param useNestingCohort                    Should the study be nested in a cohort (e.g. people with
#'                                            a specific indication)? If not, the study will be nested
#'                                            in the general population.
#' @param nestingCohortDatabaseSchema         The name of the database schema that is the location
#'                                            where the nesting cohort is defined.
#' @param nestingCohortTable                  Name of the table holding the nesting cohort. This table
#'                                            should have the same structure as the cohort table.
#' @param nestingCohortId                     A cohort definition ID identifying the records in the
#'                                            nestingCohortTable to use as nesting cohort.
#' @param useObservationEndAsNestingEndDate   When using a nesting cohort, should the observation
#'                                            period end date be used instead of the cohort end date?
#' @param getVisits                           Get data on visits? This is needed when matching on visit
#'                                            date is requested later on.
#' @param getExposures                        Should data on exposures be fetched? All exposure information
#'                                            for the nesting cohort will be retrieved, which may be time-consuming.
#'                                            Usually it is more efficient to fetch exposure data only for the cases
#'                                            and controls, as can be done using the \code{\link{getDbExposureData}} function.
#' @param exposureDatabaseSchema          The name of the database schema that is the location where
#'                                        the exposure data used to define the exposure cohorts is
#'                                        available. If exposureTable = DRUG_ERA,
#'                                        exposureDatabaseSchema is not used but assumed to be
#'                                        cdmSchema.  Requires read permissions to this database.
#' @param exposureTable                   The tablename that contains the exposure cohorts.  If
#'                                        exposureTable <> DRUG_ERA, then expectation is exposureTable
#'                                        has format of COHORT table: cohort_concept_id, SUBJECT_ID,
#'                                        COHORT_START_DATE, COHORT_END_DATE.
#' @param exposureIds                     A list of identifiers to define the exposures of interest. If
#'                                        exposureTable = DRUG_ERA, exposureIds should be CONCEPT_ID.
#'                                        If exposureTable <> DRUG_ERA, exposureIds is used to select
#'                                        the cohort_concept_id in the cohort-like table. If no
#'                                        exposureIds are provided, all drugs or cohorts in the
#'                                        exposureTable are included as exposures.
#' @param studyStartDate                      A calendar date specifying the minimum date where data is
#'                                            used. Date format is 'yyyymmdd'.
#' @param studyEndDate                        A calendar date specifying the maximum date where data is
#'                                            used. Date format is 'yyyymmdd'.
#'
#' @export
getDbCaseData <- function(connectionDetails,
                          cdmDatabaseSchema,
                          oracleTempSchema = cdmDatabaseSchema,
                          outcomeDatabaseSchema = cdmDatabaseSchema,
                          outcomeTable = "condition_era",
                          outcomeIds = c(),
                          useNestingCohort = FALSE,
                          nestingCohortDatabaseSchema = cdmDatabaseSchema,
                          nestingCohortTable = "cohort",
                          nestingCohortId = NULL,
                          useObservationEndAsNestingEndDate = TRUE,
                          getVisits = TRUE,
                          getExposures = FALSE,
                          exposureDatabaseSchema = cdmDatabaseSchema,
                          exposureTable = "drug_era",
                          exposureIds = c(),
                          studyStartDate = "",
                          studyEndDate = "") {
  if (useNestingCohort == TRUE && missing(nestingCohortId)) {
    stop("Must provide nesting cohort ID if useNestingCohort is TRUE")
  }
  if (studyStartDate != "" && regexpr("^[12][0-9]{3}[01][0-9][0-3][0-9]$", studyStartDate) == -1) {
    stop("Study start date must have format YYYYMMDD")
  }
  if (studyEndDate != "" && regexpr("^[12][0-9]{3}[01][0-9][0-3][0-9]$", studyEndDate) == -1) {
    stop("Study end date must have format YYYYMMDD")
  }
  if (getExposures && length(exposureIds) == 0) {
    stop("Must provide exposure IDs when getExposures = TRUE")
  }
  if (is.null(nestingCohortId) || !useNestingCohort) {
    nestingCohortId <- -1
    useObservationEndAsNestingEndDate <- FALSE
  }
  # Usign attr to implement hidden function argument:
  caseCrossover <- !is.null(attr(useNestingCohort, "caseCrossover"))

  conn <- connect(connectionDetails)
  renderedSql <- SqlRender::loadRenderTranslateSql("createCases.sql",
                                                   packageName = "CaseControl",
                                                   dbms = connectionDetails$dbms,
                                                   oracleTempSchema = oracleTempSchema,
                                                   cdm_database_schema = cdmDatabaseSchema,
                                                   outcome_database_schema = outcomeDatabaseSchema,
                                                   outcome_table = outcomeTable,
                                                   outcome_ids = outcomeIds,
                                                   use_nesting_cohort = (nestingCohortId != -1),
                                                   nesting_cohort_database_schema = nestingCohortDatabaseSchema,
                                                   nesting_cohort_table = nestingCohortTable,
                                                   nesting_cohort_id = nestingCohortId,
                                                   use_observation_end_as_nesting_end_date = useObservationEndAsNestingEndDate,
                                                   study_start_date = studyStartDate,
                                                   study_end_date = studyEndDate,
                                                   case_crossover = caseCrossover)

  ParallelLogger::logInfo("Executing multiple queries. This could take a while")
  DatabaseConnector::executeSql(conn, renderedSql)

  ParallelLogger::logInfo("Fetching data from server")
  start <- Sys.time()
  ParallelLogger::logInfo("- Fetching nesting cohorts")
  renderedSql <- SqlRender::loadRenderTranslateSql("queryNestingCohort.sql",
                                                   packageName = "CaseControl",
                                                   dbms = connectionDetails$dbms,
                                                   oracleTempSchema = oracleTempSchema)
  nestingCohorts <- DatabaseConnector::querySql.ffdf(conn, renderedSql)
  colnames(nestingCohorts) <- SqlRender::snakeCaseToCamelCase(colnames(nestingCohorts))

  ParallelLogger::logInfo("- Fetching cases")
  renderedSql <- SqlRender::loadRenderTranslateSql("queryCases.sql",
                                                   packageName = "CaseControl",
                                                   dbms = connectionDetails$dbms,
                                                   oracleTempSchema = oracleTempSchema)
  cases <- DatabaseConnector::querySql.ffdf(conn, renderedSql)
  colnames(cases) <- SqlRender::snakeCaseToCamelCase(colnames(cases))

  if (getVisits) {
    ParallelLogger::logInfo("- Fetching visits")
    renderedSql <- SqlRender::loadRenderTranslateSql("queryVisits.sql",
                                                     packageName = "CaseControl",
                                                     dbms = connectionDetails$dbms,
                                                     oracleTempSchema = oracleTempSchema,
                                                     cdm_database_schema = cdmDatabaseSchema)
    visits <- DatabaseConnector::querySql.ffdf(conn, renderedSql)
    colnames(visits) <- SqlRender::snakeCaseToCamelCase(colnames(visits))

    # Quicker to sort in ff than in the database (at least for PDW)
    rownames(visits) <- NULL  #Needs to be null or the ordering of ffdf will fail
    visits <- visits[ff::ffdforder(visits[c("nestingCohortId", "visitStartDate")]), ]
  }

  if (getExposures) {
    ParallelLogger::logInfo("- Fetching exposures")
    renderedSql <- SqlRender::loadRenderTranslateSql("queryAllExposures.sql",
                                                     packageName = "CaseControl",
                                                     dbms = connectionDetails$dbms,
                                                     oracleTempSchema = oracleTempSchema,
                                                     exposure_database_schema = exposureDatabaseSchema,
                                                     exposure_table = exposureTable,
                                                     exposure_ids = exposureIds)
    exposures <- querySql.ffdf(conn, renderedSql)
    colnames(exposures) <- SqlRender::snakeCaseToCamelCase(colnames(exposures))
  }

  delta <- Sys.time() - start
  ParallelLogger::logInfo(paste("Loading took", signif(delta, 3), attr(delta, "units")))

  renderedSql <- SqlRender::loadRenderTranslateSql("removeTempTables.sql",
                                                   packageName = "CaseControl",
                                                   dbms = connectionDetails$dbms,
                                                   oracleTempSchema = oracleTempSchema)
  DatabaseConnector::executeSql(conn, renderedSql, progressBar = FALSE, reportOverallTime = FALSE)
  DatabaseConnector::disconnect(conn)

  metaData <- list(outcomeIds = outcomeIds,
                   call = match.call(),
                   hasVisits = getVisits,
                   hasExposures = getExposures,
                   nestingCohortId = nestingCohortId)
  result <- list(nestingCohorts = nestingCohorts, cases = cases, metaData = metaData)
  open(result$nestingCohorts)
  open(result$cases)
  if (getVisits) {
    result$visits <- visits
    open(result$visits)
  }
  if (getExposures) {
    result$exposures <- exposures
    open(result$exposures)
  }
  class(result) <- "caseData"
  return(result)
}

#' Save the case data to folder
#'
#' @description
#' \code{saveCaseData} saves an object of type caseData to folder.
#'
#' @param caseData   An object of type \code{caseData} as generated using \code{\link{getDbCaseData}}.
#' @param folder     The name of the folder where the data will be written. The folder should not yet
#'                   exist.
#' @param compress   Should compression be used when saving?
#'
#' @details
#' The data will be written to a set of files in the specified folder.
#'
#' @export
saveCaseData <- function(caseData, folder, compress = FALSE) {
  if (missing(caseData))
    stop("Must specify caseData")
  if (missing(folder))
    stop("Must specify folder")
  if (class(caseData) != "caseData")
    stop("Data not of class caseData")

  nestingCohorts <- caseData$nestingCohorts
  cases <- caseData$cases
  if (caseData$metaData$hasVisits) {
    visits <- caseData$visits
    if (caseData$metaData$hasExposures) {
      exposures <- caseData$exposures
      if (compress) {
        saveCompressedFfdf(nestingCohorts, file.path(folder, "nestingCohorts"))
        saveCompressedFfdf(cases, file.path(folder, "cases"))
        saveCompressedFfdf(visits, file.path(folder, "visits"))
        saveCompressedFfdf(exposures, file.path(folder, "exposures"))
      } else {
        ffbase::save.ffdf(nestingCohorts, cases, visits, exposures, dir = folder)
      }
      open(caseData$exposures)
    } else {
      if (compress) {
        saveCompressedFfdf(nestingCohorts, file.path(folder, "nestingCohorts"))
        saveCompressedFfdf(cases, file.path(folder, "cases"))
        saveCompressedFfdf(visits, file.path(folder, "visits"))
      } else {
        ffbase::save.ffdf(nestingCohorts, cases, visits, dir = folder)
      }
    }
    open(caseData$visits)
  } else {
    if (caseData$metaData$hasExposures) {
      exposures <- caseData$exposures
      if (compress) {
        saveCompressedFfdf(nestingCohorts, file.path(folder, "nestingCohorts"))
        saveCompressedFfdf(cases, file.path(folder, "cases"))
        saveCompressedFfdf(exposures, file.path(folder, "exposures"))
      } else {
        ffbase::save.ffdf(nestingCohorts, cases, exposures, dir = folder)
      }
      open(caseData$exposures)
    } else {
      if (compress) {
        saveCompressedFfdf(nestingCohorts, file.path(folder, "nestingCohorts"))
        saveCompressedFfdf(cases, file.path(folder, "cases"))
      } else {
        ffbase::save.ffdf(nestingCohorts, cases, dir = folder)
      }
    }
  }
  open(caseData$nestingCohorts)
  open(caseData$cases)
  saveRDS(caseData$metaData, file = file.path(folder, "metaData.rds"))
  invisible(TRUE)
}

#' Load the case data from a folder
#'
#' @description
#' \code{loadCaseData} loads an object of type caseData from a folder in the file system.
#'
#' @param folder     The name of the folder containing the data.
#' @param readOnly   If true, the data is opened read only.
#'
#' @details
#' The data will be written to a set of files in the folder specified by the user.
#'
#' @return
#' An object of class \code{caseData}.
#'
#' @export
loadCaseData <- function(folder, readOnly = TRUE) {
  if (!file.exists(folder))
    stop(paste("Cannot find folder", folder))
  if (!file.info(folder)$isdir)
    stop(paste("Not a folder:", folder))

  metaData <- readRDS(file.path(folder, "metaData.rds"))
  caseData <- list(metaData = metaData)

  temp <- setwd(folder)
  absolutePath <- setwd(temp)
  if (file.exists(file.path(absolutePath, "nestingCohorts.zip"))) {
    caseData$nestingCohorts <- loadCompressedFfdf(file.path(absolutePath, "nestingCohorts"))
    caseData$cases <- loadCompressedFfdf(file.path(absolutePath, "cases"))
    if (caseData$metaData$hasVisits) {
      caseData$visits <- loadCompressedFfdf(file.path(absolutePath, "visits"))
    }
    if (caseData$metaData$hasExposures) {
      caseData$exposures <- loadCompressedFfdf(file.path(absolutePath, "exposures"))
    }
  } else {
    e <- new.env()
    ffbase::load.ffdf(absolutePath, e)
    caseData$nestingCohorts <- get("nestingCohorts", envir = e)
    caseData$cases <- get("cases", envir = e)
    open(caseData$nestingCohorts, readonly = readOnly)
    open(caseData$cases, readonly = readOnly)
    if (caseData$metaData$hasVisits) {
      caseData$visits <- get("visits", envir = e)
      open(caseData$visits, readonly = readOnly)
    }
    if (caseData$metaData$hasExposures) {
      caseData$exposures <- get("exposures", envir = e)
      open(caseData$exposures, readonly = readOnly)
    }
    rm(e)
  }
  class(caseData) <- "caseData"
  return(caseData)
}

#' @export
print.caseData <- function(x, ...) {
  writeLines("Case data object")
  writeLines("")
  writeLines(paste("Outcome concept ID(s):", paste(x$metaData$outcomeIds, collapse = ",")))
  if (x$metaData$nestingCohortId != -1) {
    writeLines(paste("Nesting cohort ID:", x$metaData$nestingCohortId))
  }
}

#' @export
summary.caseData <- function(object, ...) {
  populationCount <- length(ffbase::unique.ff(object$nestingCohorts$personId))
  populationWindowCount <- nrow(object$nestingCohorts)
  outcomeCounts <- data.frame(outcomeConceptId = object$metaData$outcomeIds,
                              eventCount = 0,
                              caseCount = 0)
  for (i in 1:nrow(outcomeCounts)) {
    cases <- object$cases[object$cases$outcomeId == object$metaData$outcomeIds[i], "nestingCohortId"]
    outcomeCounts$eventCount[i] <- length(cases)
    if (outcomeCounts$eventCount[i] == 0) {
      outcomeCounts$caseCount[i] <- 0
    } else {
      idx <- ffbase::`%in%`(object$nestingCohorts$nestingCohortId, cases)
      outcomeCounts$caseCount[i] <- length(ffbase::unique.ff(object$nestingCohorts$personId[idx]))
    }
  }
  result <- list(metaData = object$metaData,
                 populationCount = populationCount,
                 populationWindowCount = populationWindowCount,
                 outcomeCounts = outcomeCounts)
  class(result) <- "summary.caseData"
  return(result)
}

#' @export
print.summary.caseData <- function(x, ...) {
  writeLines("caseData object summary")
  writeLines("")
  writeLines(paste("Outcome concept ID(s):", paste(x$metaData$outcomeIds, collapse = ",")))
  if (x$metaData$nestingCohortId != -1) {
    writeLines(paste("Nesting cohort ID:", x$metaData$nestingCohortId))
  }
  writeLines("")
  writeLines(paste("Population count:", paste(x$populationCount)))
  writeLines(paste("Population window count:", paste(x$populationWindowCount)))
  writeLines("")
  writeLines("Outcome counts:")
  outcomeCounts <- x$outcomeCounts
  rownames(outcomeCounts) <- outcomeCounts$outcomeConceptId
  outcomeCounts$outcomeConceptId <- NULL
  colnames(outcomeCounts) <- c("Event count", "Case count")
  printCoefmat(outcomeCounts)
}

#' Insert cases and controls into a database
#'
#' @details
#' Inserts cases and controls into a database. The table in the database will have the same structure
#' as the 'cohort' table in the Common Data Model.
#'
#' @param caseControls           A data frame as generated by the \code{\link{selectControls}}
#'                               function.
#' @param cohortIds              The IDs to be used for the cohorts of cases and controls,
#'                               respectively.
#' @param connectionDetails      An R object of type\cr\code{connectionDetails} created using the
#'                               function \code{createConnectionDetails} in the
#'                               \code{DatabaseConnector} package.
#' @param cohortDatabaseSchema   The name of the database schema where the data will be written.
#'                               Requires write permissions to this database. On SQL Server, this
#'                               should specifiy both the database and the schema, so for example
#'                               'cdm_instance.dbo'.
#' @param cohortTable            The name of the table in the database schema where the data will be
#'                               written.
#' @param createTable            Should a new table be created? If not, the data will be inserted into
#'                               an existing table.
#' @param dropTableIfExists      If \code{createTable = TRUE} and the table already exists it will be
#'                               overwritten.
#'
#' @export
insertDbPopulation <- function(caseControls,
                               cohortIds = c(1, 0),
                               connectionDetails,
                               cohortDatabaseSchema,
                               cohortTable = "cohort",
                               createTable = FALSE,
                               dropTableIfExists = TRUE) {
  cohortDefinitionId <- plyr::mapvalues(caseControls$isCase, c(TRUE, FALSE), cohortIds)
  caseControls <- caseControls[, c("personId", "indexDate")]
  colnames(caseControls) <- c("subjectId", "cohortStartDate")
  caseControls$cohortDefinitionId <- cohortDefinitionId
  caseControls$cohortEndDate <- NA
  colnames(caseControls) <- SqlRender::camelCaseToSnakeCase(colnames(caseControls))

  connection <- DatabaseConnector::connect(connectionDetails)
  ParallelLogger::logInfo(paste("Writing",
                   nrow(caseControls),
                   "rows to",
                   paste(cohortDatabaseSchema, cohortTable, sep = ".")))
  start <- Sys.time()
  if (!createTable) {
    sql <- "DELETE FROM @table WHERE cohort_definition_id IN (@cohort_ids);"
    sql <- SqlRender::renderSql(sql,
                                table = paste(cohortDatabaseSchema, cohortTable, sep = "."),
                                cohort_ids = cohortIds)$sql
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails$dbms)$sql
    DatabaseConnector::executeSql(connection = connection,
                                  sql = sql,
                                  progressBar = FALSE,
                                  reportOverallTime = FALSE)
  }
  DatabaseConnector::insertTable(connection = connection,
                                 tableName = paste(cohortDatabaseSchema, cohortTable, sep = "."),
                                 data = caseControls,
                                 dropTableIfExists = dropTableIfExists,
                                 createTable = createTable,
                                 tempTable = FALSE,
                                 oracleTempSchema = NULL)
  DatabaseConnector::disconnect(connection)
  delta <- Sys.time() - start
  ParallelLogger::logInfo(paste("Inserting rows took", signif(delta, 3), attr(delta, "units")))
  invisible(TRUE)
}

saveCompressedFfdf <- function(ffdf, fileName) {
  dir.create(dirname(fileName), showWarnings = FALSE, recursive = TRUE)
  saveRDS(ffdf, paste0(fileName, ".rds"))
  fileNames <- sapply(bit::physical(ffdf), function(x) bit::physical(x)$filename)
  sourceDir <- dirname(fileNames[1])
  oldWd <- setwd(sourceDir)
  on.exit(setwd(oldWd))
  sourceNames <- basename(fileNames)
  ff::close.ffdf(ffdf)
  DatabaseConnector::createZipFile(zipFile = paste0(fileName, ".zip"), files = sourceNames)
  ff::open.ffdf(ffdf)
}

loadCompressedFfdf <- function(fileName) {
  ffdf <- readRDS(paste0(fileName, ".rds"))
  tempRoot <- ff::fftempfile("temp")
  utils::unzip(zipfile = paste0(fileName, ".zip"), exdir = tempRoot)
  for (ff in bit::physical(ffdf)) {
    newFileName <- ff::fftempfile("")
    file.rename(file.path(tempRoot, basename(bit::physical(ff)$filename)), newFileName)
    bit::physical(ff)$filename <- newFileName
    bit::physical(ff)$finalizer <- "delete"
    ff::open.ff(ff)
    reg.finalizer(attr(ff,"physical"), ff::finalize.ff_pointer, onexit = bit::physical(ff)$finonexit)
  }
  unlink(tempRoot, recursive = TRUE)
  return(ffdf)
}
