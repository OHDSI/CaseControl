library(CaseControl)
setwd("s:/temp")
options(fftempdir = "s:/fftemp")

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


caseControls <- readRDS("s:/temp/vignetteCaseControl/caseControlsNoVisit.rds")

caseControlsExposure <- getDbExposureData(connectionDetails = connectionDetails,
                                          caseControls = caseControls,
                                          oracleTempSchema = oracleTempSchema,
                                          exposureDatabaseSchema = cdmDatabaseSchema,
                                          exposureTable = "drug_era",
                                          exposureIds = c(diclofenac, negativeControls))

caseControlsExposure2 <- loadCaseControlsExposure("s:/temp/vignetteCaseControl/caseControlsExposureNoVisit")

caseControlsExposure$covariates <- ff::clone.ffdf(caseControlsExposure2$covariates)
caseControlsExposure$covariateRef <- ff::clone.ffdf(caseControlsExposure2$covariateRef)
caseControlsExposure$metaData$hasCovariates <- TRUE

saveCaseControlsExposure(caseControlsExposure, "s:/temp/ccQnD/caseControlsExposureNoVisit")


caseControlsExposure <- loadCaseControlsExposure("s:/temp/ccQnD/caseControlsExposureNoVisit")
control <- createControl(cvType = "auto",
                         startingVariance = 0.01,
                         tolerance = 2e-07,
                         cvRepetitions = 10,
                         selectorType = "byPid",
                         noiseLevel = "quiet",
                         threads = 20)
for (exposureId in c(diclofenac, negativeControls)) {
  writeLines(paste("Exposure:", exposureId))
  fileName <- paste0("s:/temp/ccQnD/NoVisit/model_e", exposureId, ".rds")
  if (!file.exists(fileName) && exposureId != 735340 && exposureId != 1584910) {
    caseControlData <- createCaseControlData(caseControlsExposure = caseControlsExposure,
                                             exposureId = exposureId,
                                             firstExposureOnly = FALSE,
                                             riskWindowStart = -30,
                                             riskWindowEnd = 0)
    model <- fitCaseControlModel(caseControlData,
                                 useCovariates = TRUE,
                                 caseControlsExposure = caseControlsExposure,
                                 prior = createPrior("none"))
    saveRDS(model, fileName)
  }
}

for (exposureId in c(diclofenac, negativeControls)) {
  writeLines(paste("Exposure:", exposureId))
  fileName <- paste0("s:/temp/ccQnD/noCovars/model_e", exposureId, ".rds")
  if (!file.exists(fileName)) {
    caseControlData <- createCaseControlData(caseControlsExposure = caseControlsExposure,
                                             exposureId = exposureId,
                                             firstExposureOnly = FALSE,
                                             riskWindowStart = -30,
                                             riskWindowEnd = 0)
    model <- fitCaseControlModel(caseControlData, useCovariates = FALSE)
    saveRDS(model, fileName)
  }
}

results <- data.frame(exposureId = c(diclofenac, negativeControls), logRr = NA, seLogRr = NA)
for (i in 1:nrow(results)) {
  model <- readRDS(paste0("s:/temp/ccQnD/noCovars/model_e", results$exposureId[i], ".rds"))
  if (!is.null(model$outcomeModelTreatmentEstimat)) {
    results$logRr[i] <- model$outcomeModelTreatmentEstimate$logRr
    results$seLogRr[i] <- model$outcomeModelTreatmentEstimate$seLogRr
  }
}




results <- data.frame(exposureId = c(diclofenac, negativeControls), logRr = NA, seLogRr = NA)
for (i in 1:nrow(results)) {
  if (file.exists(paste0("s:/temp/ccQnD/NoVisit/model_e", results$exposureId[i], ".rds"))) {
    model <- readRDS(paste0("s:/temp/ccQnD/NoVisit/model_e", results$exposureId[i], ".rds"))
    if (!is.null(model$outcomeModelTreatmentEstimat)) {
      results$logRr[i] <- model$outcomeModelTreatmentEstimate$logRr
      results$seLogRr[i] <- model$outcomeModelTreatmentEstimate$seLogRr
    }
  }
}

library(EmpiricalCalibration)


negCons <- results[results$exposureId != 1124300, ]
ei <- results[results$exposureId == 1124300, ]
null <- fitNull(negCons$logRr, negCons$seLogRr)
plotCalibrationEffect(logRrNegatives = negCons$logRr,
                      seLogRrNegatives = negCons$seLogRr,
                      logRrPositives = ei$logRr,
                      seLogRrPositives = ei$seLogRr,
                      null,
                      file = "s:/temp/cal5.png")




setwd("s:/temp")
options(fftempdir = "s:/fftemp")
outcomeId <- 1
firstOutcomeOnly <- TRUE
washoutPeriod <- 180
controlsPerCase <- 2
matchOnAge <- TRUE
ageCaliper <- 2
matchOnGender <- TRUE
matchOnProvider <- FALSE
matchOnVisitDate <- TRUE
visitDateCaliper <- 30
removedUnmatchedCases <- TRUE
caseData <- loadCaseData("s:/temp/vignetteCaseControl/caseData")

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
                          outcomeId = 1,
                          useNestingCohort = F,
                          nestingCohortDatabaseSchema = cohortDatabaseSchema,
                          nestingCohortTable = cohortTable,
                          nestingCohortId = 2,
                          useObservationEndAsNestingEndDate = TRUE,
                          getVisits = TRUE)

saveCaseData(caseData, "s:/temp/vignetteCaseControl2/caseData")

caseData <- loadCaseData("s:/temp/vignetteCaseControl2/caseData")

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

saveRDS(caseControls, "s:/temp/vignetteCaseControl2/caseControls.rds")

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

