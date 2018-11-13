{DEFAULT @sample_nesting_cohorts = FALSE}

TRUNCATE TABLE #nesting_cohort;
DROP TABLE #nesting_cohort;

TRUNCATE TABLE #cases;
DROP TABLE #cases;

{@sample_nesting_cohorts} ? {
TRUNCATE TABLE #sample_nesting;
DROP TABLE #sample_nesting;
}
