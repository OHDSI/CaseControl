# @file MultiAnalysesVignetteDataFetch.R
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

outputFolder <- "s:/temp/vignetteCaseControl2"

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
sql <- SqlRender::render(sql,
                         cohortDatabaseSchema = cohortDatabaseSchema,
                         cohortTable = cohortTable)
sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
DatabaseConnector::querySql(connection, sql)

DatabaseConnector::disconnect(connection)

negativeControls <- c(705178,
                      705944,
                      710650,
                      714785,
                      719174,
                      719311,
                      735340,
                      742185,
                      780369,
                      781182,
                      924724,
                      990760,
                      1110942,
                      1111706,
                      1136601,
                      1317967,
                      1501309,
                      1505346,
                      1551673,
                      1560278,
                      1584910,
                      19010309,
                      40163731)
diclofenac <- 1124300
giBleed <- 1
rheumatoidArthritis <- 2

exposureOutcomeNcList <- list()
for (exposureId in c(diclofenac, negativeControls)) {
  exposureOutcomeNc <- createExposureOutcomeNestingCohort(exposureId = exposureId,
                                                          outcomeId = giBleed,
                                                          nestingCohortId = rheumatoidArthritis)
  exposureOutcomeNcList[[length(exposureOutcomeNcList) + 1]] <- exposureOutcomeNc
}

getDbCaseDataArgs1 <- createGetDbCaseDataArgs(useNestingCohort = FALSE, getVisits = FALSE)

matchingCriteria1 <- createMatchingCriteria(controlsPerCase = 2,
                                            matchOnAge = TRUE,
                                            ageCaliper = 2,
                                            matchOnGender = TRUE,
                                            matchOnProvider = FALSE,
                                            matchOnVisitDate = FALSE)

selectControlsArgs1 <- createSelectControlsArgs(firstOutcomeOnly = FALSE,
                                                washoutPeriod = 180,
                                                controlSelectionCriteria = matchingCriteria1)

getDbExposureDataArgs1 <- createGetDbExposureDataArgs()

createCaseControlDataArgs1 <- createCreateCaseControlDataArgs(firstExposureOnly = FALSE,
                                                              riskWindowStart = 0,
                                                              riskWindowEnd = 0)

fitCaseControlModelArgs1 <- createFitCaseControlModelArgs()

ccAnalysis1 <- createCcAnalysis(analysisId = 1,
                                description = "Matching on age and gender",
                                getDbCaseDataArgs = getDbCaseDataArgs1,
                                selectControlsArgs = selectControlsArgs1,
                                getDbExposureDataArgs = getDbExposureDataArgs1,
                                createCaseControlDataArgs = createCaseControlDataArgs1,
                                fitCaseControlModelArgs = fitCaseControlModelArgs1)

getDbCaseDataArgs2 <- createGetDbCaseDataArgs(useNestingCohort = TRUE, getVisits = TRUE)

ccAnalysis2 <- createCcAnalysis(analysisId = 2,
                                description = "Matching on age and gender, nesting in indication",
                                getDbCaseDataArgs = getDbCaseDataArgs2,
                                selectControlsArgs = selectControlsArgs1,
                                getDbExposureDataArgs = getDbExposureDataArgs1,
                                createCaseControlDataArgs = createCaseControlDataArgs1,
                                fitCaseControlModelArgs = fitCaseControlModelArgs1)

covariateSettings <- createCovariateSettings(useCharlsonIndex = TRUE,
                                             useChads2 = TRUE,
                                             useDcsi = TRUE)
getDbExposureDataArgs2 <- createGetDbExposureDataArgs(covariateSettings = covariateSettings)

fitCaseControlModelArgs2 <- createFitCaseControlModelArgs(useCovariates = TRUE,
                                                          prior = createPrior("none"))

ccAnalysis3 <- createCcAnalysis(analysisId = 3,
                                description = "Matching on age and gender, nesting in indication, using covars",
                                getDbCaseDataArgs = getDbCaseDataArgs2,
                                selectControlsArgs = selectControlsArgs1,
                                getDbExposureDataArgs = getDbExposureDataArgs2,
                                createCaseControlDataArgs = createCaseControlDataArgs1,
                                fitCaseControlModelArgs = fitCaseControlModelArgs2)

matchingCriteria2 <- createMatchingCriteria(controlsPerCase = 2,
                                            matchOnAge = TRUE,
                                            ageCaliper = 2,
                                            matchOnGender = TRUE,
                                            matchOnProvider = FALSE,
                                            matchOnVisitDate = TRUE,
                                            visitDateCaliper = 30)

selectControlsArgs2 <- createSelectControlsArgs(firstOutcomeOnly = FALSE,
                                                washoutPeriod = 180,
                                                controlSelectionCriteria = matchingCriteria2)

ccAnalysis4 <- createCcAnalysis(analysisId = 4,
                                description = "Matching on age, gender and visit, nesting in indication, using covars",
                                getDbCaseDataArgs = getDbCaseDataArgs2,
                                selectControlsArgs = selectControlsArgs2,
                                getDbExposureDataArgs = getDbExposureDataArgs2,
                                createCaseControlDataArgs = createCaseControlDataArgs1,
                                fitCaseControlModelArgs = fitCaseControlModelArgs2)

samplingCriteria <- createSamplingCriteria(controlsPerCase = 2)

selectControlsArgs3 <- createSelectControlsArgs(firstOutcomeOnly = FALSE,
                                                washoutPeriod = 180,
                                                controlSelectionCriteria = samplingCriteria)

covariateSettings <- createCovariateSettings(useDemographicsAgeGroup = TRUE,
                                             useDemographicsGender = TRUE)

getDbExposureDataArgs3 <- createGetDbExposureDataArgs(covariateSettings = covariateSettings)

ccAnalysis5 <- createCcAnalysis(analysisId = 5,
                                description = "Sampling controls, adjusting for age and gender",
                                getDbCaseDataArgs = getDbCaseDataArgs1,
                                selectControlsArgs = selectControlsArgs3,
                                getDbExposureDataArgs = getDbExposureDataArgs3,
                                createCaseControlDataArgs = createCaseControlDataArgs1,
                                fitCaseControlModelArgs = fitCaseControlModelArgs2)

ccAnalysis6 <- createCcAnalysis(analysisId = 6,
                                description = "Sampling controls, adjusting for age and gender, nesting in indication",
                                getDbCaseDataArgs = getDbCaseDataArgs2,
                                selectControlsArgs = selectControlsArgs3,
                                getDbExposureDataArgs = getDbExposureDataArgs3,
                                createCaseControlDataArgs = createCaseControlDataArgs1,
                                fitCaseControlModelArgs = fitCaseControlModelArgs2)

ccAnalysisList <- list(ccAnalysis1, ccAnalysis2, ccAnalysis3, ccAnalysis4, ccAnalysis5, ccAnalysis6)

saveExposureOutcomeNestingCohortList(exposureOutcomeNcList,
                                     "s:/temp/vignetteCaseControl2/exposureOutcomeNestingCohortList.txt")
saveCcAnalysisList(ccAnalysisList, "s:/temp/vignetteCaseControl2/ccAnalysisList.txt")

# exposureOutcomeNcList <- loadExposureOutcomeNestingCohortList('s:/temp/vignetteCaseControl2/exposureOutcomeNestingCohortList.txt')
# ccAnalysisList <- loadCcAnalysisList('s:/temp/vignetteCaseControl2/ccAnalysisList.txt')

ParallelLogger::addDefaultFileLogger(file.path(outputFolder, "log.txt"))

result <- runCcAnalyses(connectionDetails = connectionDetails,
                        cdmDatabaseSchema = cdmDatabaseSchema,
                        tempEmulationSchema = cohortDatabaseSchema,
                        exposureDatabaseSchema = cdmDatabaseSchema,
                        exposureTable = "drug_era",
                        outcomeDatabaseSchema = cohortDatabaseSchema,
                        outcomeTable = cohortTable,
                        nestingCohortDatabaseSchema = cohortDatabaseSchema,
                        nestingCohortTable = cohortTable,
                        outputFolder = outputFolder,
                        exposureOutcomeNestingCohortList = exposureOutcomeNcList,
                        ccAnalysisList = ccAnalysisList,
                        getDbCaseDataThreads = 1,
                        selectControlsThreads = 4,
                        getDbExposureDataThreads = 1,
                        createCaseControlDataThreads = 4,
                        fitCaseControlModelThreads = 4,
                        cvThreads = 10)

# result <- readRDS('s:/temp/sccsVignette2/outcomeModelReference.rds')

analysisSum <- summarizeCcAnalyses(result, outputFolder)
saveRDS(analysisSum, "s:/temp/vignetteCaseControl2/analysisSummary.rds")

x <- readRDS(result$modelFile[1])
summary(x)
max(x$exposed)
