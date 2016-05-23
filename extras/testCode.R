setwd("s:/temp")
options(fftempdir = "s:/fftemp")
outcomeId = 1
firstOutcomeOnly = TRUE
washoutPeriod = 180
controlsPerCase = 2
matchOnAge = TRUE
ageCaliper = 2
matchOnGender = TRUE
matchOnProvider = FALSE
matchOnVisitDate = TRUE
visitDateCaliper = 30
caseData <- loadCaseData("s:/temp/vignetteCaseControl/caseData2")

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
                                         outcomeTable = outcomeTable)

DatabaseConnector::executeSql(connection, sql)

# Check number of subjects per cohort:
sql <- "SELECT cohort_definition_id, COUNT(*) AS count FROM @cohortDatabaseSchema.@outcomeTable GROUP BY cohort_definition_id"
sql <- SqlRender::renderSql(sql,
                            cohortDatabaseSchema = cohortDatabaseSchema,
                            outcomeTable = outcomeTable)$sql
sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
DatabaseConnector::querySql(connection, sql)

RJDBC::dbDisconnect(connection)

caseData <- getDbCaseData(connectionDetails = connectionDetails,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          oracleTempSchema = oracleTempSchema,
                          outcomeDatabaseSchema = cohortDatabaseSchema,
                          outcomeTable = outcomeTable,
                          outcomeId = 1,
                          useNestingCohort = F,
                          nestingCohortDatabaseSchema = cohortDatabaseSchema,
                          nestingCohortTable = outcomeTable,
                          #nestingCohortId = 2,
                          useObservationEndAsNestingEndDate = TRUE,
                          getVisits = TRUE)

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

caseControlsExposure <- getDbExposureData(connectionDetails = connectionDetails,
                                          caseControls = caseControls,
                                          oracleTempSchema = oracleTempSchema,
                                          exposureDatabaseSchema = cdmDatabaseSchema,
                                          exposureTable = "drug_era",
                                          exposureIds = 1124300)

saveRDS(caseControlsExposure, "s:/temp/vignetteCaseControl/caseControlsExposure.rds")

caseControlsExposure <- readRDS("s:/temp/vignetteCaseControl/caseControlsExposure.rds")

caseControlData <- createCaseControlData(caseControlsExposure = caseControlsExposure,
                                         exposureId = 1124300,
                                         firstExposureOnly = FALSE,
                                         riskWindowStart = 0,
                                         riskWindowEnd = 0)

fit <- fitCaseControlModel(caseControlData)

coef(fit)

summary(fit)

