# @file MultiAnalysesVignetteDataFetch.R
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
options(fftempdir = "s:/fftemp")

pw <- NULL
dbms <- "pdw"
user <- NULL
server <- "JRDUSAPSCTL01"
cdmDatabaseSchema <- "CDM_Truven_MDCD_V417.dbo"
cohortDatabaseSchema <- "scratch.dbo"
cohortTable <- "mschuemi_sccs_vignette"
oracleTempSchema <- NULL
outputFolder <- "s:/temp/vignetteCaseControl2"
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
                      19044727,
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

selectControlsArgs1 <- createSelectControlsArgs(firstOutcomeOnly = FALSE,
                                                washoutPeriod = 180,
                                                controlsPerCase = 2,
                                                matchOnAge = TRUE,
                                                ageCaliper = 2,
                                                matchOnGender = TRUE,
                                                matchOnProvider = FALSE,
                                                matchOnVisitDate = FALSE)

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

covariateSettings <- createCovariateSettings(useCovariateRiskScores = TRUE,
                                             useCovariateRiskScoresCharlson = TRUE,
                                             useCovariateRiskScoresDCSI = TRUE,
                                             useCovariateRiskScoresCHADS2 = TRUE)

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

selectControlsArgs2 <- createSelectControlsArgs(firstOutcomeOnly = FALSE,
                                                washoutPeriod = 180,
                                                controlsPerCase = 2,
                                                matchOnAge = TRUE,
                                                ageCaliper = 2,
                                                matchOnGender = TRUE,
                                                matchOnProvider = FALSE,
                                                matchOnVisitDate = TRUE,
                                                visitDateCaliper = 30)

ccAnalysis4 <- createCcAnalysis(analysisId = 4,
                                description = "Matching on age, gender and visit, nesting in indication, using covars",
                                getDbCaseDataArgs = getDbCaseDataArgs2,
                                selectControlsArgs = selectControlsArgs2,
                                getDbExposureDataArgs = getDbExposureDataArgs2,
                                createCaseControlDataArgs = createCaseControlDataArgs1,
                                fitCaseControlModelArgs = fitCaseControlModelArgs2)

ccAnalysisList <- list(ccAnalysis1, ccAnalysis2, ccAnalysis3, ccAnalysis4)

saveExposureOutcomeNestingCohortList(exposureOutcomeNcList,
                                     "s:/temp/vignetteCaseControl2/exposureOutcomeNestingCohortList.txt")
saveCcAnalysisList(ccAnalysisList, "s:/temp/vignetteCaseControl2/ccAnalysisList.txt")

# exposureOutcomeNcList <-
# loadExposureOutcomeNestingCohortList('s:/temp/vignetteCaseControl2/exposureOutcomeNestingCohortList.txt')
# ccAnalysisList <- loadCcAnalysisList('s:/temp/vignetteCaseControl2/ccAnalysisList.txt')

outcomeDatabaseSchema <- cohortDatabaseSchema
outcomeTable <- cohortTable
nestingCohortDatabaseSchema <- cohortDatabaseSchema
nestingCohortTable <- cohortTable
exposureDatabaseSchema <- cdmDatabaseSchema
exposureTable <- "drug_era"
getDbCaseDataThreads <- 1
selectControlsThreads <- 1
getDbExposureDataThreads <- 1
createCaseControlDataThreads <- 1
fitCaseControlModelThreads <- 1
exposureOutcomeNestingCohortList <- exposureOutcomeNcList

result <- runCcAnalyses(connectionDetails = connectionDetails,
                        cdmDatabaseSchema = cdmDatabaseSchema,
                        oracleTempSchema = cdmDatabaseSchema,
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
                        getDbExposureDataThreads = 3,
                        createCaseControlDataThreads = 4,
                        fitCaseControlModelThreads = 4,
                        cvThreads = 10)

# result <- readRDS('s:/temp/sccsVignette2/outcomeModelReference.rds')

analysisSum <- summarizeCcAnalyses(result)
saveRDS(analysisSum, "s:/temp/sccsVignette2/analysisSummary.rds")

x <- readRDS(result$modelFile[1])
summary(x)
max(x$exposed)
