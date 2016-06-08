# @file MultiAnalysesVignetteDataFetch.R
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
cdmDatabaseSchema <- "cdm_truven_mdcd_v5.dbo"
cohortDatabaseSchema <- "scratch.dbo"
oracleTempSchema <- NULL
outcomeTable <- "mschuemi_sccs_vignette"
outputFolder <- "s:/temp/ccVignette2"
port <- 17001

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)

connection <- DatabaseConnector::connect(connectionDetails)

sql <- loadRenderTranslateSql("vignette.sql",
                              packageName = "CaseControl",
                              dbms = dbms,
                              cdmDatabaseSchema = cdmDatabaseSchema,
                              cohortDatabaseSchema = cohortDatabaseSchema,
                              outcomeTable = outcomeTable)

DatabaseConnector::executeSql(connection, sql)

# Check number of subjects per cohort:
sql <- "SELECT cohort_definition_id, COUNT(*) AS count FROM @cohortDatabaseSchema.@outcomeTable GROUP BY cohort_definition_id"
sql <- SqlRender::renderSql(sql,
                            cohortDatabaseSchema = cohortDatabaseSchema,
                            outcomeTable = outcomeTable)$sql
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
rheumatoidArthritis <- 2
giBleed <- 1

exposureOutcomeNestingCohortList <- list()
for (exposureId in c(diclofenac, negativeControls)) {
  exposureOutcomeNc <- createExposureOutcomeNestingCohort(exposureId = exposureId,
                                                          outcomeId = giBleed,
                                                          nestingCohortId = rheumatoidArthritis)
  exposureOutcomeNestingCohortList[[length(exposureOutcomeNestingCohortList) + 1]] <- exposureOutcomeNc
}

getDbCaseDataArgs1 <- createGetDbCaseDataArgs(useNestingCohort = FALSE,
                                             getVisits = FALSE)

getDbCaseDataArgs2 <- createGetDbCaseDataArgs(useNestingCohort = TRUE,
                                              getVisits = TRUE)

selectControlsArgs1 <- createSelectControlsArgs(firstOutcomeOnly = FALSE,
                                                washoutPeriod = 180,
                                                controlsPerCase = 2,
                                                matchOnAge = TRUE,
                                                ageCaliper = 2,
                                                matchOnGender = TRUE,
                                                matchOnProvider = FALSE,
                                                matchOnVisitDate = FALSE)

selectControlsArgs2 <- createSelectControlsArgs(firstOutcomeOnly = FALSE,
                                                washoutPeriod = 180,
                                                controlsPerCase = 2,
                                                matchOnAge = TRUE,
                                                ageCaliper = 2,
                                                matchOnGender = TRUE,
                                                matchOnProvider = FALSE,
                                                matchOnVisitDate = TRUE,
                                                visitDateCaliper = 30)

createCaseControlDataArgs1 <- createCreateCaseControlDataArgs(firstExposureOnly = FALSE,
                                                              riskWindowStart = -30,
                                                              riskWindowEnd = 0)

ccAnalysis1 <- createCcAnalysis(analysisId = 1,
                                description = "Matching on age and gender",
                                getDbCaseDataArgs = getDbCaseDataArgs1,
                                selectControlsArgs = selectControlsArgs1,
                                createCaseControlDataArgs = createCaseControlDataArgs1)

ccAnalysis2 <- createCcAnalysis(analysisId = 2,
                                description = "Matching on age and gender, nesting in indication",
                                getDbCaseDataArgs = getDbCaseDataArgs2,
                                selectControlsArgs = selectControlsArgs1,
                                createCaseControlDataArgs = createCaseControlDataArgs1)

ccAnalysis3 <- createCcAnalysis(analysisId = 3,
                                description = "Matching on age and gender, nesting in indication, match on visit",
                                getDbCaseDataArgs = getDbCaseDataArgs2,
                                selectControlsArgs = selectControlsArgs2,
                                createCaseControlDataArgs = createCaseControlDataArgs1)


ccAnalysisList <- list(ccAnalysis1, ccAnalysis2, ccAnalysis3)

saveExposureOutcomeNestingCohortList(exposureOutcomeNestingCohortList, "s:/temp/ccVignette2/exposureOutcomeNestingCohortList.txt")
saveCcAnalysisList(ccAnalysisList, "s:/temp/ccVignette2/ccAnalysisList.txt")

# exposureOutcomeNestingCohortList <- loadExposureOutcomeNestingCohortList("s:/temp/ccVignette2/exposureOutcomeNestingCohortList.txt")
# ccAnalysisList <- loadCcAnalysisList("s:/temp/ccVignette2/ccAnalysisList.txt")

outcomeDatabaseSchema = cohortDatabaseSchema
outcomeTable = outcomeTable
nestingCohortDatabaseSchema = cohortDatabaseSchema
nestingCohortTable = outcomeTable
exposureDatabaseSchema = cdmDatabaseSchema
exposureTable = "drug_era"
getDbCaseDataThreads = 1
selectControlsThreads = 1
getDbExposureDataThreads = 1
createCaseControlDataThreads = 1
fitCaseControlModelThreads = 1

result <- runSccsAnalyses(connectionDetails = connectionDetails,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          oracleTempSchema = cdmDatabaseSchema,
                          exposureDatabaseSchema = cdmDatabaseSchema,
                          exposureTable = "drug_era",
                          outcomeDatabaseSchema = cohortDatabaseSchema,
                          outcomeTable = outcomeTable,
                          cdmVersion = cdmVersion,
                          outputFolder = "s:/temp/sccsVignette2",
                          combineDataFetchAcrossOutcomes = TRUE,
                          exposureOutcomeList = exposureOutcomeList,
                          sccsAnalysisList = sccsAnalysisList,
                          getDbSccsDataThreads = 1,
                          createSccsEraDataThreads = 5,
                          fitSccsModelThreads = 8,
                          cvThreads = 4)

# result <- readRDS('s:/temp/sccsVignette2/outcomeModelReference.rds')

analysisSum <- summarizeSccsAnalyses(result)
saveRDS(analysisSum, "s:/temp/sccsVignette2/analysisSummary.rds")

