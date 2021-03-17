/*
 * @file ControlSelector.cpp
 *
 * This file is part of CaseControl
 *
 * Copyright 2021 Observational Health Data Sciences and Informatics
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

#ifndef CONTROLSELECTOR_CPP_
#define CONTROLSELECTOR_CPP_

#include <ctime>
#include <Rcpp.h>
#include <random>
#include "ControlSelector.h"
#include "NestingCohortDataIterator.h"

using namespace Rcpp;

namespace ohdsi {
namespace caseControl {

ControlSelector::ControlSelector(const List& _nestingCohorts, const List& _cases, const List& _visits, const bool _firstOutcomeOnly, const int _washoutPeriod,
                                 const int _controlsPerCase, const bool _matchOnAge, const double _ageCaliper, const bool _matchOnGender, const bool _matchOnProvider,
                                 const bool _matchOnCareSite, const bool _matchOnVisitDate, const int _visitDateCaliper, const bool _matchOnTimeInCohort, const int _daysInCohortCaliper,
                                 const int _minAgeDays, const int _maxAgeDays, const int _seed) :
firstOutcomeOnly(_firstOutcomeOnly), washoutPeriod(_washoutPeriod), controlsPerCase(_controlsPerCase), matchOnAge(_matchOnAge),
ageCaliper(_ageCaliper), matchOnGender(_matchOnGender), matchOnProvider(_matchOnProvider), matchOnCareSite(_matchOnCareSite), matchOnVisitDate(_matchOnVisitDate),
visitDateCaliper(_visitDateCaliper), matchOnTimeInCohort(_matchOnTimeInCohort), daysInCohortCaliper(_daysInCohortCaliper), minAgeDays(_minAgeDays), maxAgeDays(_maxAgeDays),
nestingCohortDatas(), personSeqId2CaseData(), generator(), stratumId(0) {
  //Load all data in memory structures:
  if (_matchOnVisitDate) {
    Environment base = Environment::namespace_env("base");
    Function writeLines = base["writeLines"];
    writeLines("Loading visit data into memory");
  }
  NestingCohortDataIterator nestingCohortDataIterator(_nestingCohorts, _cases, _visits, _matchOnVisitDate);
  while (nestingCohortDataIterator.hasNext()){
    NestingCohortData nestingCohortData = nestingCohortDataIterator.next();
    if ((!matchOnProvider || nestingCohortData.providerSeqId != NA_INTEGER)
          && (!matchOnCareSite || nestingCohortData.careSiteSeqId != NA_INTEGER)) {
      nestingCohortDatas.push_back(nestingCohortData);
      if (personSeqId2CaseData.find(nestingCohortData.personSeqId) == personSeqId2CaseData.end()) {
        CaseData caseData(nestingCohortData.genderConceptId, nestingCohortData.dateOfBirth, nestingCohortData.providerSeqId, nestingCohortData.careSiteSeqId, nestingCohortData.startDate);
        personSeqId2CaseData.insert(std::pair<int64_t,CaseData>(nestingCohortData.personSeqId, caseData));
      }
      for (int date : nestingCohortData.indexDates) {
        if (date <= std::min(nestingCohortData.dateOfBirth + maxAgeDays, nestingCohortData.endDate)) {
          bool washedOut = date < std::max(std::max(nestingCohortData.observationPeriodStartDate + washoutPeriod, nestingCohortData.dateOfBirth + minAgeDays), nestingCohortData.startDate);
          IndexDate indexDate(date, washedOut);
          personSeqId2CaseData[nestingCohortData.personSeqId].indexDates.push_back(indexDate);
        }
      }
    }
  }
  generator.seed(_seed);
  distribution = new std::uniform_int_distribution<int>(0,nestingCohortDatas.size() - 1);
  if (matchOnAge)
    std::sort (nestingCohortDatas.begin(), nestingCohortDatas.end());
}

int ControlSelector::isMatch(const NestingCohortData& controlData, const CaseData& caseData, const int& indexDate) {
  if (indexDate < controlData.startDate || indexDate > controlData.endDate || indexDate < controlData.observationPeriodStartDate + washoutPeriod)
    return NO_MATCH;

  if (matchOnGender) {
    if (caseData.genderConceptId != controlData.genderConceptId)
      return NO_MATCH;
  }
  if (matchOnProvider) {
    if (caseData.providerSeqId != controlData.providerSeqId)
      return NO_MATCH;
  }
  if (matchOnCareSite) {
    if (caseData.careSiteSeqId != controlData.careSiteSeqId)
      return NO_MATCH;
  }
  if (matchOnTimeInCohort) {
    int timeDelta = abs(controlData.startDate - caseData.startDate);
    if (timeDelta > daysInCohortCaliper)
      return NO_MATCH;
  }
  if (firstOutcomeOnly) {
    for (int controlIndexDate : controlData.indexDates)
      if (controlIndexDate <= indexDate)
        return NO_MATCH;
  } else {
    for (int controlIndexDate : controlData.indexDates)
      if (controlIndexDate == indexDate)
        return NO_MATCH;
  }

  if (matchOnVisitDate) {
    if (controlData.visitDates.size() == 0)
      return NO_MATCH;
    int visitDate = *std::lower_bound(controlData.visitDates.begin(), controlData.visitDates.end(), indexDate);
    if (abs(visitDate - indexDate) <= visitDateCaliper)
      return visitDate;
    else
      return NO_MATCH;
  }

  return 0;
}

int ControlSelector::binarySearchDateOfBirthLowerBound(const int& key) {
  int lowerbound = 0;
  int upperbound = nestingCohortDatas.size() - 1;
  int index;
  while(lowerbound < upperbound) {
    index = (lowerbound + upperbound) / 2;
    //std::cout << "f1: index = " << index << ", lb = " << lowerbound << ", ub = " << upperbound << "\n";
    if (nestingCohortDatas[index].dateOfBirth < key)
      lowerbound = index + 1;
    else
      upperbound = index;
  }
  return lowerbound;
}

int ControlSelector::binarySearchDateOfBirthUpperBound(const int& _lowerBound, const int& key) {
  int lowerbound = _lowerBound;
  int upperbound = nestingCohortDatas.size() - 1;
  int index;
  while(lowerbound < upperbound) {
    index = ((lowerbound + upperbound)%2) ? 1 + ((lowerbound + upperbound) / 2) : (lowerbound + upperbound) / 2; // Need ceiling value
    //std::cout << "f2: index = " << index << ", lb = " << lowerbound << ", ub = " << upperbound << "\n";
    if (nestingCohortDatas[index].dateOfBirth > key)
      upperbound = index - 1;
    else
      lowerbound = index;
  }
  return lowerbound;
}


void ControlSelector::findControls(const int64_t& personSeqId, const CaseData& caseData, const int& indexDate, const int& stratumId) {
  std::set<int64_t> controlPersonSeqIds;
  int lb;
  int ub;
  std::uniform_int_distribution<int> *dist;
  if (matchOnAge) {
    lb = binarySearchDateOfBirthLowerBound(caseData.dateOfBirth - floor(ageCaliper * 365.25));
    ub = binarySearchDateOfBirthUpperBound(lb, caseData.dateOfBirth + floor(ageCaliper * 365.25));
    if (ub < lb)
      return;
    dist = new std::uniform_int_distribution<int>(lb, ub);
  } else {
    lb = 0;
    ub = nestingCohortDatas.size() - 1;
    dist = distribution;
  }

  // strategy 1: randonly pick people and see if they're a match:

  int iter = 0;
  while (controlPersonSeqIds.size() < controlsPerCase && iter < MAX_ITER) {
    iter++;
    int idx;
    idx = (*dist)(generator);
    NestingCohortData controlData = nestingCohortDatas[idx];
    int value = isMatch(controlData, caseData, indexDate);
    if (value != NO_MATCH && controlData.personSeqId != personSeqId && controlPersonSeqIds.insert(controlData.personSeqId).second) {
      if (matchOnVisitDate)
        result.add(controlData.personSeqId, value, false, stratumId);
      else
        result.add(controlData.personSeqId, indexDate, false, stratumId);
    }
  }
  // If max iterations hit, fallback to strategy 2: iterate over all people and see which match. Then randomly sample from matches:
  if (controlPersonSeqIds.size() < controlsPerCase) {
    //std::cout << "Strategy 1 failed\n";
    std::vector<int64_t> personSeqIds;
    std::vector<int> indexDates;
    for (int i = lb; i <= ub; i++){
      NestingCohortData controlData = nestingCohortDatas[i];
      int value = isMatch(controlData, caseData, indexDate);
      if (value != NO_MATCH && controlData.personSeqId != personSeqId && controlPersonSeqIds.find(controlData.personSeqId) == controlPersonSeqIds.end()) {
        personSeqIds.push_back(controlData.personSeqId);
        if (matchOnVisitDate)
          indexDates.push_back(value);
        else
          indexDates.push_back(indexDate);
      }
    }
    while (controlPersonSeqIds.size() < controlsPerCase && personSeqIds.size() > 0) {
      std::uniform_int_distribution<int> dist(0, personSeqIds.size()-1);
      int idx = dist(generator);
      result.add(personSeqIds[idx], indexDates[idx], false, stratumId);
      controlPersonSeqIds.insert(personSeqIds[idx]);
      personSeqIds.erase(personSeqIds.begin() + idx);
      indexDates.erase(indexDates.begin() + idx);
    }
  }
}

void ControlSelector::processCase(const int64_t& personSeqId, CaseData& caseData) {
  std::sort(caseData.indexDates.begin(), caseData.indexDates.end());
  for (IndexDate indexDate : caseData.indexDates) {
    if (!indexDate.washedOut) {
      stratumId++;
      result.add(personSeqId, indexDate.date, true, stratumId);
      findControls(personSeqId, caseData, indexDate.date, stratumId);
    }
    if (firstOutcomeOnly)
      break;
  }
}

DataFrame ControlSelector::selectControls() {
  if (personSeqId2CaseData.size() == 0)
    return result.toDataFrame();
  Environment utils = Environment::namespace_env("utils");
  Environment base = Environment::namespace_env("base");
  Function txtProgressBar = utils["txtProgressBar"];
  Function setTxtProgressBar = utils["setTxtProgressBar"];
  Function close = base["close"];
  Function writeLines = base["writeLines"];

  writeLines("Finding controls per case");

  List progressBar = txtProgressBar(0, personSeqId2CaseData.size(), 0, "=", NA_REAL, "" , "", 3, "");

  int i = 0;
  for(std::map<int64_t, CaseData>::iterator iterator = personSeqId2CaseData.begin(); iterator != personSeqId2CaseData.end(); iterator++) {
    processCase(iterator->first, iterator->second);
    if (i++ % 100 == 0)
      setTxtProgressBar(progressBar, i);
  }
  close(progressBar);
  return result.toDataFrame();
}
}
}

#endif /* CONTROLSELECTOR_CPP_ */
