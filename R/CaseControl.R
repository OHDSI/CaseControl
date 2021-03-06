# @file CaseControl.R
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

#' @keywords internal
"_PACKAGE"

#' @importFrom Rcpp evalCpp
#' @importFrom SqlRender loadRenderTranslateSql translate
#' @importFrom survival strata
#' @importFrom stats coef confint pnorm printCoefmat qnorm aggregate
#' @importFrom rlang .data
#' @import dplyr
#' @import Cyclops
#' @import DatabaseConnector
#' @import FeatureExtraction
#' @useDynLib CaseControl
NULL
