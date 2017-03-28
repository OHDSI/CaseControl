# @file SingleStudyVignetteDataFetch.R
#
# Copyright 2017 Observational Health Data Sciences and Informatics
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
setwd("s:/temp")
options(fftempdir = "s:/fftemp")

pw <- NULL
dbms <- "pdw"
user <- NULL
server <- "JRDUSAPSCTL01"
cdmDatabaseSchema <- "CDM_Truven_MDCD_V464.dbo"
cohortDatabaseSchema <- "scratch.dbo"
oracleTempSchema <- NULL
cohortTable <- "mschuemi_cc_vignette"
port <- 17001

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)

connection <- DatabaseConnector::connect(connectionDetails)

sql <- SqlRender::loadRenderTranslateSql("vignette.sql",
                                         packageName = "CaseControl",
                                         dbms = dbms,
                                         cdmDatabaseSchema = cdmDatabaseSchema,
                                         cohortDatabaseSchema = cohortDatabaseSchema,
                                         cohortTable = cohortTable)

DatabaseConnector::executeSql(connection, sql)

# Check number of subjects per cohort:
sql <- "SELECT cohort_definition_id, COUNT(*) AS count FROM @cohortDatabaseSchema.@cohortTable GROUP BY cohort_definition_id"
sql <- SqlRender::renderSql(sql,
                            cohortDatabaseSchema = cohortDatabaseSchema,
                            cohortTable = cohortTable)$sql
sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
DatabaseConnector::querySql(connection, sql)

RJDBC::dbDisconnect(connection)

caseData <- getDbCaseData(connectionDetails = connectionDetails,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          oracleTempSchema = oracleTempSchema,
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
#                           oracleTempSchema = oracleTempSchema,
#                           outcomeDatabaseSchema = cohortDatabaseSchema,
#                           outcomeTable = cohortTable,
#                           outcomeIds = 1,
#                           useNestingCohort = TRUE,
#                           nestingCohortDatabaseSchema = cohortDatabaseSchema,
#                           nestingCohortTable = cohortTable,
#                           nestingCohortId = 2,
#                           useObservationEndAsNestingEndDate = TRUE,
#                           getVisits = TRUE,
#                           getExposures = FALSE,
#                           exposureDatabaseSchema = cdmDatabaseSchema,
#                           exposureTable = "drug_era",
#                           exposureIds = 1124300)

saveCaseData(caseData, "s:/temp/vignetteCaseControl/caseData")

caseData <- loadCaseData("s:/temp/vignetteCaseControl/caseData")

caseData

summary(caseData)

caseControls <- selectControls(caseData = caseData,
                               outcomeId = 1,
                               firstOutcomeOnly = TRUE,
                               washoutPeriod = 180,
                               controlsPerCase = 2,
                               matchOnAge = TRUE,
                               ageCaliper = 2,
                               matchOnGender = TRUE,
                               matchOnProvider = FALSE,
                               matchOnVisitDate = TRUE,
                               visitDateCaliper = 30)

saveRDS(caseControls, "s:/temp/vignetteCaseControl/caseControls.rds")

caseControls <- readRDS("s:/temp/vignetteCaseControl/caseControls.rds")


covariateSettings <- createCovariateSettings(useCovariateRiskScores = TRUE,
                                             useCovariateRiskScoresCharlson = TRUE,
                                             useCovariateRiskScoresDCSI = TRUE,
                                             useCovariateRiskScoresCHADS2 = TRUE)

caseControlsExposure <- getDbExposureData(connectionDetails = connectionDetails,
                                          caseControls = caseControls,
                                          oracleTempSchema = oracleTempSchema,
                                          exposureDatabaseSchema = cdmDatabaseSchema,
                                          exposureTable = "drug_era",
                                          exposureIds = 1124300,
                                          covariateSettings = covariateSettings)

# caseControlsExposure <- getDbExposureData(connectionDetails = connectionDetails,
#                                           caseControls = caseControls,
#                                           oracleTempSchema = oracleTempSchema,
#                                           exposureIds = 1124300,
#                                           covariateSettings = NULL,
#                                           caseData = caseData)

saveCaseControlsExposure(caseControlsExposure, "s:/temp/vignetteCaseControl/caseControlsExposure")

caseControlsExposure <- loadCaseControlsExposure("s:/temp/vignetteCaseControl/caseControlsExposure")

caseControlData <- createCaseControlData(caseControlsExposure = caseControlsExposure,
                                         exposureId = 1124300,
                                         firstExposureOnly = FALSE,
                                         riskWindowStart = 0,
                                         riskWindowEnd = 0)



head(caseControlData)

fit <- fitCaseControlModel(caseControlData,
                           useCovariates = TRUE,
                           caseControlsExposure = caseControlsExposure,
                           prior = createPrior("none"))

saveRDS(fit, "s:/temp/vignetteCaseControl/fit.rds")

fit <- readRDS("s:/temp/vignetteCaseControl/fit.rds")

confint(fit)

coef(fit)

summary(fit)
