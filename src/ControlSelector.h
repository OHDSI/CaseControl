/*
 * @file ControlSelector.h
 *
 * This file is part of CaseControl
 *
 * Copyright 2016 Observational Health Data Sciences and Informatics
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

#ifndef CONTROLSELECTOR_H_
#define CONTROLSELECTOR_H_

#include <Rcpp.h>
#include <random>
#include "NestingCohortDataIterator.h"
using namespace Rcpp;

namespace ohdsi {
namespace caseControl {

struct IndexDate {
  IndexDate(const int& _date, const bool& _washedOut) : date(_date), washedOut(_washedOut) {}

  bool operator<(const IndexDate& a) const {
    return date < a.date;
  }

  int date;
  bool washedOut;
};

struct CaseData {
  CaseData() = default;
  CaseData(const int& _genderConceptId, const int& _dateOfBirth, const int64_t& _providerId) : genderConceptId(_genderConceptId),
  dateOfBirth(_dateOfBirth), providerId(_providerId), indexDates() {}
  int genderConceptId;
  int dateOfBirth;
  int64_t providerId;
  std::vector<IndexDate> indexDates;
};

struct Result {
  Result() : personId(), indexDate(), isCase(), stratumId() {}
  DataFrame toDataFrame() {
    return DataFrame::create(_["personId"] = personId, _["indexDate"] = indexDate, _["isCase"] = isCase, _["stratumId"] = stratumId);
  }
  void add(const int64_t& _personId, const int& _indexDate, const bool& _isCase, const int& _stratumId) {
    personId.push_back(_personId);
    indexDate.push_back(_indexDate);
    isCase.push_back(_isCase);
    stratumId.push_back(_stratumId);
  }
  std::vector<int64_t> personId;
  std::vector<int> indexDate;
  std::vector<bool> isCase;
  std::vector<int> stratumId;
};

class ControlSelector {
public:
  ControlSelector(const List& _nestingCohorts, const List& _cases, const List& _visits, const bool _firstOutcomeOnly, const int _washoutPeriod,
                  const int _controlsPerCase, const bool _matchOnAge, const double _ageCaliper, const bool _matchOnGender, const bool _matchOnProvider,
                  const bool _matchOnVisitDate, const int _visitDateCaliper);
  DataFrame selectControls();

private:
  void processCase(const int64_t& personId, CaseData& caseData);
  void findControls(const int64_t& personId, const CaseData& caseData, const int& indexDate, const int& stratumId);
  int isMatch(const NestingCohortData& controlData, const CaseData& caseData, const int& indexDate);
  int binarySearch(const std::vector<int>& vector, const int& key);
  std::vector<NestingCohortData> nestingCohortDatas;
  std::map<int64_t, CaseData> personId2CaseData;
  bool firstOutcomeOnly;
  int washoutPeriod;
  int controlsPerCase;
  bool matchOnAge;
  double ageCaliper;
  bool matchOnGender;
  bool matchOnProvider;
  bool matchOnVisitDate;
  int visitDateCaliper;
  std::mt19937 generator;
  std::uniform_int_distribution<int> *distribution;
  Result result;
  int stratumId;
  static const int NO_MATCH = -99999999;
  static const int MAX_ITER = 10000;
};
}
}

#endif /* CONTROLSELECTOR_H_ */
