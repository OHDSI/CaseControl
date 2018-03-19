/**********************************************************************
@file createCases.sql

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

{DEFAULT @cdm_database_schema = 'cdm4_sim.dbo'}
{DEFAULT @outcome_database_schema = 'cdm4_sim'} 
{DEFAULT @outcome_table = 'condition_occurrence'} 
{DEFAULT @outcome_ids = ''}
{DEFAULT @use_nesting_cohort = FALSE}
{DEFAULT @nesting_cohort_database_schema = 'cdm4_sim.dbo'} 
{DEFAULT @nesting_cohort_table = 'cohort'} 
{DEFAULT @nesting_cohort_id = 1} 
{DEFAULT @use_observation_end_as_nesting_end_date = TRUE} 
{DEFAULT @study_start_date = '' }
{DEFAULT @study_end_date = '' }
{DEFAULT @case_crossover = FALSE}


IF OBJECT_ID('tempdb..#nesting_cohort', 'U') IS NOT NULL
	DROP TABLE #nesting_cohort;
	
IF OBJECT_ID('tempdb..#cases', 'U') IS NOT NULL
	DROP TABLE #cases;
		
/**********************************************************************
						Create nesting cohort
***********************************************************************/
SELECT DISTINCT nesting_cohort_id,
	person.person_id,
	observation_period_start_date,
	start_date,
	end_date,
	DATEFROMPARTS(year_of_birth, ISNULL(month_of_birth, 1), ISNULL(day_of_birth, 1)) AS date_of_birth,
	gender_concept_id,
	person.provider_id,
	person.care_site_id
INTO #nesting_cohort
FROM (
	SELECT person_id,
		observation_period_start_date,
		nesting_cohort_id,
{@study_start_date == '' } ? {
		start_date,
} : {
		CASE 
			WHEN start_date < CAST('@study_start_date' AS DATE)
				THEN CAST('@study_start_date' AS DATE)
			ELSE start_date
			END AS start_date,
} 
{@study_end_date == '' } ? {
		end_date
} : {
		CASE 
			WHEN end_date > CAST('@study_end_date' AS DATE)
				THEN CAST('@study_end_date' AS DATE)
			ELSE end_date
			END AS end_date
} 	
	FROM (
{@use_nesting_cohort} ? {
		SELECT ROW_NUMBER() OVER (ORDER BY person_id, cohort_table.cohort_start_date) AS nesting_cohort_id,
			person_id,
			observation_period_start_date,
			cohort_table.cohort_start_date AS start_date,
{@use_observation_end_as_nesting_end_date} ? {
			observation_period_end_date AS end_date
} : {
			cohort_table.cohort_end_date AS end_date
}
		FROM @nesting_cohort_database_schema.@nesting_cohort_table cohort_table
		INNER JOIN @cdm_database_schema.observation_period
		ON cohort_table.subject_id = observation_period.person_id
		WHERE cohort_table.cohort_definition_id = @nesting_cohort_id
			AND cohort_table.cohort_start_date >= observation_period_start_date
			AND cohort_table.cohort_start_date <= observation_period_end_date
} : {
		SELECT observation_period_id AS nesting_cohort_id,
			person_id,
			observation_period_start_date,
			observation_period_start_date AS start_date,
			observation_period_end_date AS end_date
		FROM @cdm_database_schema.observation_period
}
) temp_nesting_cohort
{@study_start_date != '' | @study_end_date != ''} ? {	WHERE}
{@study_start_date != '' } ? {		end_date >= CAST('@study_start_date' AS DATE) } 
{@study_start_date != '' | @study_end_date != ''} ? {		AND}
{@study_end_date != '' } ? {		start_date < CAST('@study_end_date' AS DATE) }
) nesting_cohort
INNER JOIN @cdm_database_schema.person
ON nesting_cohort.person_id = person.person_id
{@case_crossover} ? {
INNER JOIN (
{@outcome_table == 'condition_occurrence'} ? {	
	SELECT person_id,
	  condition_start_date AS index_date,
	  condition_concept_id AS outcome_id
	FROM @outcome_database_schema.condition_occurrence 
	WHERE condition_concept_id IN (@outcome_ids)
} : { 
	{@outcome_table == 'condition_era'} ? {
	SELECT person_id,
	  condition_era_start_date AS index_date,
	  condition_concept_id AS outcome_id
	FROM @outcome_database_schema.condition_era 
	WHERE condition_concept_id IN (@outcome_ids)
	} : { /* outcome table has same structure as cohort table */
	SELECT subject_id AS person_id,
	  cohort_start_date AS index_date,
	  cohort_definition_id AS outcome_id
	FROM @outcome_database_schema.@outcome_table
	WHERE cohort_definition_id IN (@outcome_ids)
	}
}
	) outcome
ON outcome.person_id = nesting_cohort.person_id
AND outcome.index_date <= nesting_cohort.end_date
AND	outcome.index_date >= nesting_cohort.observation_period_start_date
}
;
	
/**********************************************************************
							Select cases
***********************************************************************/
SELECT DISTINCT nesting_cohort_id,
	outcome_id,
	index_date
INTO #cases
FROM #nesting_cohort nesting_cohort
INNER JOIN (
{@outcome_table == 'condition_occurrence'} ? {	
	SELECT person_id,
	  condition_start_date AS index_date,
	  condition_concept_id AS outcome_id
	FROM @outcome_database_schema.condition_occurrence 
	WHERE condition_concept_id IN (@outcome_ids)
} : { 
	{@outcome_table == 'condition_era'} ? {
	SELECT person_id,
	  condition_era_start_date AS index_date,
	  condition_concept_id AS outcome_id
	FROM @outcome_database_schema.condition_era 
	WHERE condition_concept_id IN (@outcome_ids)
	} : { /* outcome table has same structure as cohort table */
	SELECT subject_id AS person_id,
	  cohort_start_date AS index_date,
	  cohort_definition_id AS outcome_id
	FROM @outcome_database_schema.@outcome_table
	WHERE cohort_definition_id IN (@outcome_ids)
	}
}
	) outcome
ON outcome.person_id = nesting_cohort.person_id
AND outcome.index_date <= nesting_cohort.end_date
AND	outcome.index_date >= nesting_cohort.observation_period_start_date;
