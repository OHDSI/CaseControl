/*
 * @file NestingCohortDataIterator.cpp
 *
 * This file is part of CaseControl
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

#ifndef NESTINGCOHORTDATAITERATOR_CPP_
#define NESTINGCOHORTDATAITERATOR_CPP_

#include <Rcpp.h>
#include "NestingCohortDataIterator.h"
#include "FfdfIterator.h"

using namespace Rcpp;

namespace ohdsi {
namespace caseControl {

NestingCohortDataIterator::NestingCohortDataIterator(const List& _nestingCohorts, const List& _cases, const List& _visits, const bool& showVisitProgress) :
nestingCohortsIterator(_nestingCohorts, false), casesIterator(_cases, false), visitsIterator(_visits, showVisitProgress), nestingCohortsCursor(0), casesCursor(0), visitsCursor(0) {
  loadNextNestingCohorts();
  loadNextCases();
  loadNextVisits();
}


void NestingCohortDataIterator::loadNextNestingCohorts() {
  List nestingCohorts = nestingCohortsIterator.next();
  nestingCohortsNestingCohortId = nestingCohorts["nestingCohortId"];
  nestingCohortsPersonId = nestingCohorts["personId"];
  nestingCohortsStartDate = nestingCohorts["startDate"];
  nestingCohortsObservationPeriodStartDate = nestingCohorts["observationPeriodStartDate"];
  nestingCohortsEndDate = nestingCohorts["endDate"];
  nestingCohortsDateOfBirth = nestingCohorts["dateOfBirth"];
  nestingCohortsGenderConceptId = nestingCohorts["genderConceptId"];
  nestingCohortsProviderId = nestingCohorts["providerId"];
  nestingCohortsCareSiteId = nestingCohorts["careSiteId"];
}

void NestingCohortDataIterator::loadNextCases() {
  List cases = casesIterator.next();
  casesNestingCohortId = cases["nestingCohortId"];
  casesIndexDate = cases["indexDate"];
}

void NestingCohortDataIterator::loadNextVisits() {
  List visits = visitsIterator.next();
  visitsNestingCohortId = visits["nestingCohortId"];
  visitsVisitStartDate = visits["visitStartDate"];
}

bool NestingCohortDataIterator::hasNext() {
  return (nestingCohortsCursor < nestingCohortsNestingCohortId.length());
}

NestingCohortData NestingCohortDataIterator::next() {
  int64_t nestingCohortId = nestingCohortsNestingCohortId[nestingCohortsCursor];
  NestingCohortData nextNestingCohortData(nestingCohortsNestingCohortId[nestingCohortsCursor], nestingCohortsPersonId[nestingCohortsCursor],
                                          nestingCohortsObservationPeriodStartDate[nestingCohortsCursor], nestingCohortsStartDate[nestingCohortsCursor], nestingCohortsEndDate[nestingCohortsCursor],
                                          nestingCohortsDateOfBirth[nestingCohortsCursor], nestingCohortsGenderConceptId[nestingCohortsCursor], nestingCohortsProviderId[nestingCohortsCursor],
                                          nestingCohortsCareSiteId[nestingCohortsCursor]);
  nestingCohortsCursor++;
  if (nestingCohortsCursor == nestingCohortsNestingCohortId.length() && nestingCohortsIterator.hasNext()){
    loadNextNestingCohorts();
    nestingCohortsCursor = 0;
  }
  while (casesCursor < casesNestingCohortId.length() && casesNestingCohortId[casesCursor] == nestingCohortId) {
    nextNestingCohortData.indexDates.push_back(casesIndexDate[casesCursor]);
    casesCursor++;
    if (casesCursor == casesNestingCohortId.length()){
      if (casesIterator.hasNext()){
        loadNextCases();
        casesNestingCohortId = 0;
      } else {
        break;
      }
    }
  }
  nextNestingCohortData.indexDates.shrink_to_fit();
  while (visitsCursor < visitsNestingCohortId.length() && visitsNestingCohortId[visitsCursor] == nestingCohortId) {
    nextNestingCohortData.visitDates.push_back(visitsVisitStartDate[visitsCursor]);
    visitsCursor++;
    if (visitsCursor == visitsNestingCohortId.length()){
      if (visitsIterator.hasNext()){
        loadNextVisits();
        visitsCursor = 0;
      } else {
        break;
      }
    }
  }
  nextNestingCohortData.visitDates.shrink_to_fit();
  return nextNestingCohortData;
}
}
}
#endif /* NESTINGCOHORTDATAITERATOR_CPP_ */
