# @file SingleStudyVignetteDataFetch.R
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


#' Compute the minimum detectable relative risk
#'
#' @details
#' Compute the minimum detectable relative risk (MDRR) for a given study population, using the actual
#' observed sample size and number of exposed controls. Computations by  Miettinnen (1969) and
#' Rothman and Boice (1979) are used. Based on and verified using Ken Rothman's EpiSheet.
#'
#' @param caseControlData   A data frame describing the cases and controls as created using the
#'                     \code{\link{createCaseControlData}} function. This should at least have these
#'                     columns: isCase, exposed.
#' @param alpha        Type I error.
#' @param power        1 - beta, where beta is the type II error.
#' @param twoSided     Consider a two-sided test?
#'
#' @references
#' Miettinen OS (1969) Individual matching in the case of all or none responses. Biometrics, 25, 339-354.
#'
#' Rothman KJ, Boice JD (1979) Epidemiologic Analysis with a Programmable Calculator. NIH Publication No.79-1649.
#'
#' @return
#' A data frame with the MDRR and some counts.
#'
#' @export
computeMdrr <- function(caseControlData, alpha = 0.05, power = 0.8, twoSided = TRUE) {
  if (twoSided) {
    alpha <- alpha / 2
  }
  z <- qnorm(1-alpha) # Z alpha
  r <- sum(caseControlData$isCase == 0) / sum(caseControlData$isCase == 1) # Ratio controls / cases
  p0 <-  mean(caseControlData$exposed[caseControlData$isCase == 0]) # Prevalence of exposure in population
  n <- sum(caseControlData$isCase == 1) # Number of cases

  computePower <- function(z, r, p0, or, n, alpha) {
    p1 <- or*p0 /(1-p0 + or*p0)
    a <- p1 * (1-p0) + p0*(1-p1)
    b <- (r-1)*p0*(1-p0)
    zbnum <- -z * (a + b) + abs(p1 - p0) * sqrt(r * n * (a + b))
    zbden <- sqrt((a + b) * (r * a - b) - r * (p1 - p0) ^ 2)
    zb <- zbnum / zbden
    power <- pnorm(zb)
    if (power < alpha | n < 1)
      power <- alpha
    return(power)
  }

  binarySearch <- function(z, r, p0, n, power, alpha, precision = 1e-6) {
    L <- 0
    H <- 10
    while (H >= L) {
      M <- L + (H - L)/2
      powerM <- computePower(z, r, p0, exp(M), n, alpha)
      d <- powerM - power
      #writeLines(paste('M =', M, 'power = ',powerM))
      if (d > precision) {
        H <- M
      } else if (-d > precision) {
        L <- M
      } else {
        return(M)
      }
      if (M == 0 || M == 10)
        return(M)
    }
  }
  mdLogRr <- binarySearch(z, r, p0, n, power, alpha)
  mdrr <- exp(mdLogRr)

  result <- data.frame(cases = sum(caseControlData$isCase == 1),
                       controls = sum(caseControlData$isCase == 0),
                       exposedControls = mean(caseControlData$exposed[caseControlData$isCase == 0]),
                       mdrr = mdrr)
  return(result)
}
