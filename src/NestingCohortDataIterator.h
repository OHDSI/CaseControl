/*
 * @file NestingCohortDataIterator.h
 *
 * This file is part of CaseControl
 *
 * Copyright 2020 Observational Health Data Sciences and Informatics
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

#ifndef NESTINGCOHORTDATAITERATOR_H_
#define NESTINGCOHORTDATAITERATOR_H_

#include <Rcpp.h>
#include "FfdfIterator.h"

using namespace Rcpp;

namespace ohdsi {
namespace caseControl {

struct NestingCohortData {
  NestingCohortData(int64_t _nestingCohortId, int64_t _personId, int _observationPeriodStartDate, int _startDate, int _endDate, int _dateOfBirth, int _genderConceptId, int64_t _providerId, int64_t _careSiteId) :
  nestingCohortId(_nestingCohortId), personId(_personId), observationPeriodStartDate(_observationPeriodStartDate), startDate(_startDate), endDate(_endDate), dateOfBirth(_dateOfBirth), genderConceptId(_genderConceptId),
  providerId(_providerId), careSiteId(_careSiteId), indexDates(), visitDates() {
  }

  bool operator < (const NestingCohortData& other) const {
    return (dateOfBirth < other.dateOfBirth);
  }

  int64_t nestingCohortId;
  int64_t personId;
  int observationPeriodStartDate;
  int startDate;
  int endDate;
  int dateOfBirth;
  int genderConceptId;
  int64_t providerId;
  int64_t careSiteId;
  std::vector<int> indexDates;
  std::vector<int> visitDates;
};

class NestingCohortDataIterator {
public:
  NestingCohortDataIterator(const List& _nestingCohorts, const List& _cases, const List& _visits, const bool& showVisitProgress);
  bool hasNext();
  NestingCohortData next();
private:
  void check(const List& list, const char* name);
  FfdfIterator nestingCohortsIterator;
  FfdfIterator casesIterator;
  FfdfIterator visitsIterator;
  NumericVector nestingCohortsNestingCohortId;
  NumericVector nestingCohortsPersonId;
  NumericVector nestingCohortsStartDate;
  NumericVector nestingCohortsObservationPeriodStartDate;
  NumericVector nestingCohortsEndDate;
  NumericVector nestingCohortsDateOfBirth;
  NumericVector nestingCohortsGenderConceptId;
  NumericVector nestingCohortsProviderId;
  NumericVector nestingCohortsCareSiteId;
  NumericVector casesNestingCohortId;
  NumericVector casesIndexDate;
  NumericVector visitsNestingCohortId;
  NumericVector visitsVisitStartDate;
  int nestingCohortsCursor;
  int casesCursor;
  int visitsCursor;
  void loadNextNestingCohorts();
  void loadNextCases();
  void loadNextVisits();
};
}
}

#endif /* NESTINGCOHORTDATAITERATOR_H_ */
