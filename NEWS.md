CaseControl 3.2.0
==================

Changes:

1. Adding likelihood profile to outcome model object.


CaseControl 3.1.0
==================

Changes:

1. Adding log likelihood ratio to outcome model object.

2. Internally replacing person_id, provider_id, and care_site_id with sequential IDs to avoid loss of precision when converting BIGINT to numeric.

3. Using `SqlRender`'s new `tempEmulationSchema` argument. Avoids deprecation warning.

4. Adding `createSimpleCovariateSettings()` function to create covariates using only the data already available in the `CaseData` object (currently age and gender), so not requiring querying the database again.


CaseControl 3.0.0
==================

Changes:

1. Using Andromeda instead of ff to store large data objects.


CaseControl 2.0.0
==================

Changes:

1. Added option to select controls using sampling instead of matching.

2. The selectControls function now expects a controlSelectionCriteria argument, which can be created using either the createMatchingCriteria or the createSamplingCriteria function.

Bugfixes:

1. Fixed error when not pre-fetching exposure data, and the cohort database schema is not the CDM database schema.

CaseControl 1.6.0
==================

Changes:

1. The riskWindowStart and riskWindowEnd arguments of the createCaseControlData function are now inclusive (meaning exposures on the start and end date are included). Before this was not the case.

2. Added exposureWashoutPeriod argument to createCaseControlData, primarily for use with positive control synthesis.
