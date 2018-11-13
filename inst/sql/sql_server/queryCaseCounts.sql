/**********************************************************************
@file queryCases.sql

Copyright 2018 Observational Health Data Sciences and Informatics

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

SELECT outcome_id,
	COUNT(*) AS case_count
FROM #cases cases
{@sample_nesting_cohorts} ? {
INNER JOIN #sample_nesting sampled_nesting_cohorts
ON cases.nesting_cohort_id = sampled_nesting_cohorts.nesting_cohort_id
}
GROUP BY outcome_id;
