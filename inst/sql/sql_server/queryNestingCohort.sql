/**********************************************************************
@file queryNestingCohort.sql

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

SELECT nesting_cohort.nesting_cohort_id,
	person_id,
	observation_period_start_date,
	start_date,
	end_date,
	date_of_birth,
	gender_concept_id,
	provider_id,
	care_site_id
FROM #nesting_cohort nesting_cohort
{@sample_nesting_cohorts} ? {
INNER JOIN #sample_nesting sampled_nesting_cohorts
ON nesting_cohort.nesting_cohort_id = sampled_nesting_cohorts.nesting_cohort_id
}
;
