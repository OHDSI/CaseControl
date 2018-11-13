/**********************************************************************
@file queryNestingCohort.sql

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
{DEFAULT @max_nesting_cohort_size = 10000000} 

SELECT nesting_cohort_id
INTO #sample_nesting
FROM (
	SELECT nesting_cohort_id,
		ROW_NUMBER() OVER (ORDER BY NEWID()) AS rn
	FROM #nesting_cohort
	) temp
WHERE rn <= @max_nesting_cohort_size;
