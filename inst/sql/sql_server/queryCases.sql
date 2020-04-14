/**********************************************************************
@file queryCases.sql

Copyright 2020 Observational Health Data Sciences and Informatics

This file is part of CaseControl
 
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
 
   http://www.apache.org/licenses/LICENSE-2.0
 
Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
***********************************************************************/
{DEFAULT @sample_nesting_cohorts = FALSE}
{DEFAULT @sample_cases = FALSE}
{DEFAULT @max_cases_per_outcome = 500000}

{@sample_cases} ? {
SELECT nesting_cohort_id,
    outcome_id,
	index_date
FROM (
}
SELECT cases.nesting_cohort_id,
    outcome_id,
	index_date
{@sample_cases} ? {
	, ROW_NUMBER() OVER (PARTITION BY outcome_id ORDER BY NEWID()) AS rn 
}
FROM #cases cases
{@sample_nesting_cohorts} ? {
INNER JOIN #sample_nesting sampled_nesting_cohorts
ON cases.nesting_cohort_id = sampled_nesting_cohorts.nesting_cohort_id
}
{@sample_cases} ? {
) temp
WHERE rn <= @max_cases_per_outcome
}
;
