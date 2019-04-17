/**********************************************************************
@file queryExposure.sql

Copyright 2019 Observational Health Data Sciences and Informatics

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
{DEFAULT @exposure_database_schema = 'cdm4_sim.dbo'} 
{DEFAULT @exposure_table = 'drug_era'} 
{DEFAULT @exposure_ids = 1} 
{DEFAULT @sample_nesting_cohorts = FALSE}

SELECT exposure.person_id,
  exposure_id,
  exposure_start_date,
  exposure_end_date
FROM #nesting_cohort nesting_cohort
{@sample_nesting_cohorts} ? {
INNER JOIN #sample_nesting sampled_nesting_cohorts
ON nesting_cohort.nesting_cohort_id = sampled_nesting_cohorts.nesting_cohort_id
}
INNER JOIN (
{@exposure_table == 'drug_era'} ? {
	SELECT person_id,
	  drug_concept_id AS exposure_id,
	  drug_era_start_date AS exposure_start_date,
	  drug_era_end_date AS exposure_end_date
	FROM @exposure_database_schema.@exposure_table
{@exposure_ids != ''} ? {
	WHERE drug_concept_id IN (@exposure_ids)
}
} : {
SELECT subject_id AS person_id,
	  cohort_definition_id AS exposure_id,
	  cohort_start_date AS exposure_start_date,
	  cohort_end_date AS exposure_end_date
	FROM @exposure_database_schema.@exposure_table
{@exposure_ids != ''} ? {
	WHERE cohort_definition_id IN (@exposure_ids)
}
}
	) exposure
ON nesting_cohort.person_id = exposure.person_id
AND exposure.exposure_start_date <= nesting_cohort.end_date
