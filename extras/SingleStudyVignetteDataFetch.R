# @file SingleStudyVignetteDataFetch.R
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

# This code should be used to fetch the data that is used in the vignettes.
library(SqlRender)
library(DatabaseConnector)
library(CaseControl)
options(andromedaTempFolder = "s:/andromedaTemp")

cdmDatabaseSchema <- "cdm"
cohortDatabaseSchema <- "scratch_mschuemi2"
cohortTable <- "mschuemi_cc_vignette"

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "redshift",
                                                                connectionString = keyring::key_get("redShiftConnectionStringMdcd"),
                                                                user = keyring::key_get("redShiftUserName"),
                                                                password = keyring::key_get("redShiftPassword"))

Sys.setenv("AWS_OBJECT_KEY" = "bulk")
Sys.setenv("AWS_ACCESS_KEY_ID" = Sys.getenv("bulkUploadS3Key"))
Sys.setenv("AWS_SECRET_ACCESS_KEY" = Sys.getenv("bulkUploadS3Secret"))
Sys.setenv("AWS_BUCKET_NAME" = Sys.getenv("bulkUploadS3Bucket"))
Sys.setenv("AWS_DEFAULT_REGION" = "us-east-1")
Sys.setenv("AWS_SSE_TYPE" = "AES256")
Sys.setenv("DATABASE_CONNECTOR_BULK_UPLOAD" = TRUE)


connection <- DatabaseConnector::connect(connectionDetails)

sql <- SqlRender::loadRenderTranslateSql("vignette.sql",
                                         packageName = "CaseControl",
                                         dbms = connectionDetails$dbms,
                                         cdmDatabaseSchema = cdmDatabaseSchema,
                                         cohortDatabaseSchema = cohortDatabaseSchema,
                                         cohortTable = cohortTable)

DatabaseConnector::executeSql(connection, sql)

# Check number of subjects per cohort:
sql <- "SELECT cohort_definition_id, COUNT(*) AS count FROM @cohortDatabaseSchema.@cohortTable GROUP BY cohort_definition_id"
sql <- SqlRender::render(sql,
                         cohortDatabaseSchema = cohortDatabaseSchema,
                         cohortTable = cohortTable)
sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
DatabaseConnector::querySql(connection, sql)

DatabaseConnector::disconnect(connection)

caseData <- getDbCaseData(connectionDetails = connectionDetails,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          outcomeDatabaseSchema = cohortDatabaseSchema,
                          outcomeTable = cohortTable,
                          outcomeIds = 1,
                          useNestingCohort = TRUE,
                          nestingCohortDatabaseSchema = cohortDatabaseSchema,
                          nestingCohortTable = cohortTable,
                          nestingCohortId = 2,
                          useObservationEndAsNestingEndDate = TRUE,
                          getVisits = TRUE)

# caseData <- getDbCaseData(connectionDetails = connectionDetails,
#                           cdmDatabaseSchema = cdmDatabaseSchema,
#                           outcomeDatabaseSchema = cohortDatabaseSchema,
#                           outcomeTable = cohortTable,
#                           outcomeIds = 1,
#                           useNestingCohort = TRUE,
#                           nestingCohortDatabaseSchema = cohortDatabaseSchema,
#                           nestingCohortTable = cohortTable,
#                           nestingCohortId = 2,
#                           useObservationEndAsNestingEndDate = TRUE,
#                           getVisits = FALSE,
#                           maxNestingCohortSize = 1e5,
#                           maxCasesPerOutcome = 1000,
#                           getExposures = TRUE,
#                           exposureDatabaseSchema = cdmDatabaseSchema,
#                           exposureTable = "drug_era",
#                           exposureIds = 1124300)
# saveCaseData(caseData, "s:/temp/vignetteCaseControl/caseExposureData.zip")
# caseData <- loadCaseData("s:/temp/vignetteCaseControl/caseExposureData.zip")

saveCaseData(caseData, "s:/temp/vignetteCaseControl/caseData.zip")

caseData <- loadCaseData("s:/temp/vignetteCaseControl/caseData.zip")

caseData

summary(caseData)

matchingCriteria <- createMatchingCriteria(controlsPerCase = 2,
                                           matchOnAge = TRUE,
                                           ageCaliper = 2,
                                           matchOnGender = TRUE,
                                           matchOnProvider = FALSE,
                                           matchOnVisitDate = TRUE,
                                           visitDateCaliper = 30)

caseControls <- selectControls(caseData = caseData,
                               outcomeId = 1,
                               firstOutcomeOnly = TRUE,
                               washoutPeriod = 180,
                               controlSelectionCriteria = matchingCriteria)

# samplingCriteria <- createSamplingCriteria(controlsPerCase = 1)
#
# caseControls2 <- selectControls(caseData = caseData,
#                                 outcomeId = 1,
#                                 firstOutcomeOnly = TRUE,
#                                 washoutPeriod = 180,
#                                 controlSelectionCriteria = samplingCriteria)

saveRDS(caseControls, "s:/temp/vignetteCaseControl/caseControls.rds")

caseControls <- readRDS("s:/temp/vignetteCaseControl/caseControls.rds")


covariateSettings <- createCovariateSettings(useCharlsonIndex = TRUE,
                                             useChads2 = TRUE,
                                             useDcsi = TRUE)

caseControlsExposure <- getDbExposureData(connectionDetails = connectionDetails,
                                          caseControls = caseControls,
                                          exposureDatabaseSchema = cdmDatabaseSchema,
                                          exposureTable = "drug_era",
                                          exposureIds = 1124300,
                                          covariateSettings = covariateSettings)

# caseControlsExposure <- getDbExposureData(connectionDetails = connectionDetails,
#                                           caseControls = caseControls,
#                                           exposureIds = 1124300,
#                                           covariateSettings = NULL,
#                                           caseData = caseData)

saveCaseControlsExposure(caseControlsExposure, "s:/temp/vignetteCaseControl/caseControlsExposure")

caseControlsExposure <- loadCaseControlsExposure("s:/temp/vignetteCaseControl/caseControlsExposure")

caseControlData <- createCaseControlData(caseControlsExposure = caseControlsExposure,
                                         exposureId = 1124300,
                                         firstExposureOnly = FALSE,
                                         riskWindowStart = 0,
                                         riskWindowEnd = 0,
                                         exposureWashoutPeriod = 1)



caseControlData

fit <- fitCaseControlModel(caseControlData,
                           useCovariates = TRUE,
                           caseControlsExposure = caseControlsExposure,
                           prior = createPrior("none"))

saveRDS(fit, "s:/temp/vignetteCaseControl/fit.rds")

fit <- readRDS("s:/temp/vignetteCaseControl/fit.rds")

confint(fit)

coef(fit)

fit
