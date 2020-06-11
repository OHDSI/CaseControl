# @file DataLoadingSaving.R
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
#'                                            database. On SQL Server, this should specify both the
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
#' @param maxNestingCohortSize                If the nesting cohort is larger than
#'                                     this number it will be sampled to this size. \code{maxCohortSize = 0}
#'                                     indicates no maximum size.
#' @param maxCasesPerOutcome              If there are more than this number of cases for a single
#'                                        outcome cases will be sampled to this size. \code{maxCasesPerOutcome = 0}
#'                                        indicates no maximum size.
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
                          getVisits = FALSE,
                          getExposures = FALSE,
                          exposureDatabaseSchema = cdmDatabaseSchema,
                          exposureTable = "drug_era",
                          exposureIds = c(),
                          studyStartDate = "",
                          studyEndDate = "",
                          maxNestingCohortSize = 1e7,
                          maxCasesPerOutcome = 5e5) {
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
  # Using attr to implement hidden function argument:
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
  if (maxNestingCohortSize == 0) {
    sampleNestingCohorts <- FALSE
  } else {
    sql <- "SELECT COUNT(*) FROM #nesting_cohort;"
    sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms, oracleTempSchema = oracleTempSchema)
    nestingCohortCount <- DatabaseConnector::querySql(conn, sql)[1, 1]
    if (nestingCohortCount > maxNestingCohortSize) {
      ParallelLogger::logInfo("Downsampling nesting cohort from ", nestingCohortCount, " to ", maxNestingCohortSize)
      sampleNestingCohorts <- TRUE
      renderedSql <- SqlRender::loadRenderTranslateSql("sampleNestingCohort.sql",
                                                       packageName = "CaseControl",
                                                       dbms = connectionDetails$dbms,
                                                       oracleTempSchema = oracleTempSchema,
                                                       max_nesting_cohort_size = maxNestingCohortSize)
      DatabaseConnector::executeSql(conn, renderedSql)

    } else {
      sampleNestingCohorts <- FALSE
    }
  }
  caseData <- Andromeda::andromeda()
  ParallelLogger::logInfo("- Fetching nesting cohorts")
  renderedSql <- SqlRender::loadRenderTranslateSql("queryNestingCohort.sql",
                                                   packageName = "CaseControl",
                                                   dbms = connectionDetails$dbms,
                                                   oracleTempSchema = oracleTempSchema,
                                                   sample_nesting_cohorts = sampleNestingCohorts)
  DatabaseConnector::querySqlToAndromeda(connection = conn,
                                         sql = renderedSql,
                                         andromeda = caseData,
                                         andromedaTableName = "nestingCohorts",
                                         snakeCaseToCamelCase = TRUE)

  if (maxCasesPerOutcome == 0) {
    sampleCases <- FALSE
  } else {
    sql <- SqlRender::loadRenderTranslateSql("queryCaseCounts.sql",
                                             packageName = "CaseControl",
                                             dbms = connectionDetails$dbms,
                                             oracleTempSchema = oracleTempSchema,
                                             sample_nesting_cohorts = sampleNestingCohorts)
    caseCounts <- DatabaseConnector::querySql(conn, sql, snakeCaseToCamelCase = TRUE)
    sampleCases <- FALSE
    if (nrow(caseCounts) > 0) {
      for (i in 1:nrow(caseCounts)) {
        if (caseCounts$caseCount[i] > maxCasesPerOutcome) {
          ParallelLogger::logInfo("Downsampling cases for outcome ", caseCounts$outcomeId[i], " from ", caseCounts$caseCount[i], " to ", maxCasesPerOutcome)
          sampleCases <- TRUE
        }
      }
    }
  }
  ParallelLogger::logInfo("- Fetching cases")
  renderedSql <- SqlRender::loadRenderTranslateSql("queryCases.sql",
                                                   packageName = "CaseControl",
                                                   dbms = connectionDetails$dbms,
                                                   oracleTempSchema = oracleTempSchema,
                                                   sample_nesting_cohorts = sampleNestingCohorts,
                                                   sample_cases = sampleCases,
                                                   max_cases_per_outcome = maxCasesPerOutcome)
  DatabaseConnector::querySqlToAndromeda(connection = conn,
                                         sql = renderedSql,
                                         andromeda = caseData,
                                         andromedaTableName = "cases",
                                         snakeCaseToCamelCase = TRUE)

  if (getVisits) {
    ParallelLogger::logInfo("- Fetching visits")
    renderedSql <- SqlRender::loadRenderTranslateSql("queryVisits.sql",
                                                     packageName = "CaseControl",
                                                     dbms = connectionDetails$dbms,
                                                     oracleTempSchema = oracleTempSchema,
                                                     cdm_database_schema = cdmDatabaseSchema,
                                                     sample_nesting_cohorts = sampleNestingCohorts)
    DatabaseConnector::querySqlToAndromeda(connection = conn,
                                           sql = renderedSql,
                                           andromeda = caseData,
                                           andromedaTableName = "visits",
                                           snakeCaseToCamelCase = TRUE)
  }

  if (getExposures) {
    ParallelLogger::logInfo("- Fetching exposures")
    renderedSql <- SqlRender::loadRenderTranslateSql("queryAllExposures.sql",
                                                     packageName = "CaseControl",
                                                     dbms = connectionDetails$dbms,
                                                     oracleTempSchema = oracleTempSchema,
                                                     exposure_database_schema = exposureDatabaseSchema,
                                                     exposure_table = exposureTable,
                                                     exposure_ids = exposureIds,
                                                     sample_nesting_cohorts = sampleNestingCohorts)
    DatabaseConnector::querySqlToAndromeda(connection = conn,
                                           sql = renderedSql,
                                           andromeda = caseData,
                                           andromedaTableName = "exposures",
                                           snakeCaseToCamelCase = TRUE)
  }

  delta <- Sys.time() - start
  ParallelLogger::logInfo(paste("Loading took", signif(delta, 3), attr(delta, "units")))

  renderedSql <- SqlRender::loadRenderTranslateSql("removeTempTables.sql",
                                                   packageName = "CaseControl",
                                                   dbms = connectionDetails$dbms,
                                                   oracleTempSchema = oracleTempSchema,
                                                   sample_nesting_cohorts = sampleNestingCohorts)
  DatabaseConnector::executeSql(conn, renderedSql, progressBar = FALSE, reportOverallTime = FALSE)
  DatabaseConnector::disconnect(conn)

  metaData <- list(outcomeIds = outcomeIds,
                   hasVisits = getVisits,
                   hasExposures = getExposures,
                   nestingCohortId = nestingCohortId)
  attr(caseData, "metaData") <- metaData

  class(caseData) <- "CaseData"
  attr(class(caseData), "package") <- "CaseControl"
  return(caseData)
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
#'                               should specify both the database and the schema, so for example
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
    sql <- SqlRender::render(sql,
                             table = paste(cohortDatabaseSchema, cohortTable, sep = "."),
                             cohort_ids = cohortIds)
    sql <- SqlRender::translate(sql = sql, targetDialect = connectionDetails$dbms)
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
