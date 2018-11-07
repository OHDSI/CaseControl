CaseControl
===========

Introduction
============
CaseControl is an R package for performing (nested) matched case-control analyses in an observational database in the OMOP Common Data Model.

Features
========
- Extracts the necessary data from a database in OMOP Common Data Model format
- Nesting in a cohort of interest (e.g. people with a particular prior condition)
- Matching on age, gender, provider, time in cohort, and/or visit
- Sampling of controls per case
- Fitting outcome models using conditional logisitc regression

Technology
==========
CaseControl is an R package, with some functions implemented in C++.

System Requirements
===================
Requires R (version 3.2.2 or higher). Installation on Windows requires [RTools](http://cran.r-project.org/bin/windows/Rtools/). Libraries used in CaseControl require Java.

Dependencies
============
 * Cyclops
 * DatabaseConnector
 * SqlRender
 * ParallelLogger

Getting Started
===============
1. On Windows, make sure [RTools](http://cran.r-project.org/bin/windows/Rtools/) is installed.
2. The DatabaseConnector and SqlRender packages require Java. Java can be downloaded from
<a href="http://www.java.com" target="_blank">http://www.java.com</a>.
3. In R, use the following commands to download and install CaseControl:

  ```r
  install.packages("drat")
  drat::addRepo("OHDSI")
  install.packages("CaseControl")
  ```

Getting Involved
================
* Vignette: [Single studies using the CaseControl package](https://raw.githubusercontent.com/OHDSI/CaseControl/master/inst/doc/SingleStudies.pdf)
* Vignette: [Running multiple analyses at once using the CaseControl package](https://raw.githubusercontent.com/OHDSI/CaseControl/master/inst/doc/MultipleAnalyses.pdf)
* Package manual: [CaseControl.pdf](https://raw.githubusercontent.com/OHDSI/CaseControl/master/extras/CaseControl.pdf)
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="../../issues">GitHub issue tracker</a> for all bugs/issues/enhancements

License
=======
CaseControl is licensed under Apache License 2.0

Development
===========
CaseControl is being developed in R Studio.

### Development status
[![Build Status](https://travis-ci.org/OHDSI/CaseControl.svg?branch=master)](https://travis-ci.org/OHDSI/CaseControl)
[![codecov.io](https://codecov.io/github/OHDSI/CaseControl/coverage.svg?branch=master)](https://codecov.io/github/OHDSI/CaseControl?branch=master)

Beta
