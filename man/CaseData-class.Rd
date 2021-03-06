% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CaseData.R
\docType{class}
\name{CaseData-class}
\alias{CaseData-class}
\alias{CaseData}
\alias{show,CaseData-method}
\alias{summary,CaseData-method}
\title{Case Data}
\usage{
\S4method{show}{CaseData}(object)

\S4method{summary}{CaseData}(object)
}
\arguments{
\item{object}{An object of class `CaseData`.}
}
\description{
\code{CaseData} is an S4 class that inherits from \code{\link[Andromeda]{Andromeda}}. It contains
information on cases.

A \code{CaseData} object is typically created using \code{\link{getDbCaseData}}, can only be saved using
\code{\link{saveCaseData}}, and loaded using \code{\link{loadCaseData}}.
}
\seealso{
\code{\link{isCaseData}}
}
