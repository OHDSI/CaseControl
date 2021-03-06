# @file PackageMaintenance
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

# Format and check code
OhdsiRTools::formatRFolder()
OhdsiRTools::checkUsagePackage("CaseControl")
OhdsiRTools::updateCopyrightYearFolder()
devtools::spell_check()

# Create manual and vignette
unlink("extras/CaseControl.pdf")
shell("R CMD Rd2pdf ./ --output=extras/CaseControl.pdf")

rmarkdown::render("vignettes/SingleStudies.Rmd",
                  output_file = "../inst/doc/SingleStudies.pdf",
                  rmarkdown::pdf_document(latex_engine = "pdflatex",
                                          toc = TRUE,
                                          number_sections = TRUE))
unlink("inst/doc/SingleStudies.tex")

rmarkdown::render("vignettes/MultipleAnalyses.Rmd",
                  output_file = "../inst/doc/MultipleAnalyses.pdf",
                  rmarkdown::pdf_document(latex_engine = "pdflatex",
                                          toc = TRUE,
                                          number_sections = TRUE))
unlink("inst/doc/MultipleAnalyses.tex")

pkgdown::build_site()

# Create arg functions:
library(CaseControl)
rCode <- c("# This file has been autogenerated. Do not change by hand. ")
rCode <- ParallelLogger::createArgFunction("getDbCaseData",
                                        excludeArgs = c("connectionDetails",
                                                        "cdmDatabaseSchema",
                                                        "tempEmulationSchema",
                                                        "outcomeDatabaseSchema",
                                                        "outcomeTable",
                                                        "outcomeIds",
                                                        "nestingCohortDatabaseSchema",
                                                        "nestingCohortTable",
                                                        "nestingCohortId",
                                                        "getExposures",
                                                        "exposureDatabaseSchema",
                                                        "exposureTable",
                                                        "exposureIds"),
                                        rCode = rCode)
rCode <- ParallelLogger::createArgFunction("selectControls",
                                        excludeArgs = c("caseData", "outcomeId"),
                                        rCode = rCode)
rCode <- ParallelLogger::createArgFunction("getDbExposureData",
                                        excludeArgs = c("caseControls",
                                                        "connectionDetails",
                                                        "tempEmulationSchema",
                                                        "exposureDatabaseSchema",
                                                        "exposureTable",
                                                        "exposureIds",
                                                        "cdmDatabaseSchema",
                                                        "caseData"),
                                        rCode = rCode)
rCode <- ParallelLogger::createArgFunction("createCaseControlData",
                                        excludeArgs = c("caseControlsExposure", "exposureId"),
                                        rCode = rCode)
rCode <- ParallelLogger::createArgFunction("fitCaseControlModel",
                                        excludeArgs = c("caseControlData", "caseControlsExposure"),
                                        rCode = rCode)
writeLines(rCode, "r/CreateArgFunctions.R")
OhdsiRTools::formatRFile("r/CreateArgFunctions.R")
