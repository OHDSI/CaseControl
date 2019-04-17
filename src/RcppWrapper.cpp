/*
 * @file RcppWrapper.cpp
 *
 * This file is part of SelfControlledCaseSeries
 *
 * Copyright 2019 Observational Health Data Sciences and Informatics
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#ifndef __RcppWrapper_cpp__
#define __RcppWrapper_cpp__

#include <Rcpp.h>
#include "ControlSelector.h"

using namespace Rcpp;

// [[Rcpp::export]]
DataFrame selectControlsInternal(const List& nestingCohorts, const List& cases, const List& visits, const bool firstOutcomeOnly, const int washoutPeriod,
                    const int controlsPerCase, const bool matchOnAge, const double ageCaliper, const bool matchOnGender, const bool matchOnProvider,
                    const bool matchOnCareSite, const bool matchOnVisitDate, const int visitDateCaliper, const bool matchOnTimeInCohort,
                    const int daysInCohortCaliper, const int minAgeDays, const int maxAgeDays, const int seed) {

	using namespace ohdsi::caseControl;

	try {
	  ControlSelector controlSelector(nestingCohorts, cases, visits, firstOutcomeOnly, washoutPeriod, controlsPerCase, matchOnAge, ageCaliper,
                                   matchOnGender, matchOnProvider, matchOnCareSite, matchOnVisitDate, visitDateCaliper, matchOnTimeInCohort, daysInCohortCaliper,
                                   minAgeDays, maxAgeDays, seed);
		return (controlSelector.selectControls());
	} catch (std::exception &e) {
		forward_exception_to_r(e);
	} catch (...) {
		::Rf_error("c++ exception (unknown reason)");
	}
	return DataFrame::create();
}

#endif // __RcppWrapper_cpp__
