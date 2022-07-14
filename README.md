CaseControl
===========

[![Build Status](https://github.com/OHDSI/CaseControl/workflows/R-CMD-check/badge.svg)](https://github.com/OHDSI/CaseControl/actions?query=workflow%3AR-CMD-check)
[![codecov.io](https://codecov.io/github/OHDSI/CaseControl/coverage.svg?branch=main)](https://codecov.io/github/OHDSI/CaseControl?branch=main)

**This package is no longer being supported** It is provided as is.

Introduction
============
CaseControl is an R package for performing (nested) matched case-control analyses in an observational database in the OMOP Common Data Model.

Features
========
- Extracts the necessary data from a database in OMOP Common Data Model format
- Nesting in a cohort of interest (e.g. people with a particular prior condition)
- Matching on age, gender, provider, time in cohort, and/or visit
- Sampling of controls per case
- Fitting outcome models using conditional logistic regression

Technology
==========
CaseControl is an R package, with some functions implemented in C++.

System Requirements
===================
Requires R. Installation on Windows requires [RTools](http://cran.r-project.org/bin/windows/Rtools/). Libraries used in CaseControl require Java.

Installation
============
1. See the instructions [here](https://ohdsi.github.io/Hades/rSetup.html) for configuring your R environment, including RTools and Java.

2. In R, use the following commands to download and install CaseControl:

  ```r
  install.packages("remotes")
  remotes::install_github("ohdsi/CaseControl")
  ```

User Documentation
==================
Documentation can be found on the [package website](https://ohdsi.github.io/CaseControl).

PDF versions of the documentation are also available:
* Vignette: [Single studies using the CaseControl package](https://raw.githubusercontent.com/OHDSI/CaseControl/main/inst/doc/SingleStudies.pdf)
* Vignette: [Running multiple analyses at once using the CaseControl package](https://raw.githubusercontent.com/OHDSI/CaseControl/main/inst/doc/MultipleAnalyses.pdf)
* Package manual: [CaseControl.pdf](https://raw.githubusercontent.com/OHDSI/CaseControl/main/extras/CaseControl.pdf)

Support
=======
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="https://github.com/OHDSI/CaseControl/issues">GitHub issue tracker</a> for all bugs/issues/enhancements

Contributing
============
Read [here](https://ohdsi.github.io/Hades/contribute.html) how you can contribute to this package.

License
=======
CaseControl is licensed under Apache License 2.0

Development
===========
CaseControl is being developed in R Studio.

### Development status

No longer supported.
