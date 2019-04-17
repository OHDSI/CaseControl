CaseControl 1.6.0
==================

Changes:

1. The riskWindowStart and riskWindowEnd arguments of the createCaseControlData function are now inclusive (meaning exposures on the start and end date are included). Before this was not the case.

2. Added exposureWashoutPeriod argument to createCaseControlData, primarily for use with positive control synthesis.
