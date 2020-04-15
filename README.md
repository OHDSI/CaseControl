CaseControl
===========

[![Build Status](https://travis-ci.org/OHDSI/CaseControl.svg?branch=master)](https://travis-ci.org/OHDSI/CaseControl)
[![codecov.io](https://codecov.io/github/OHDSI/CaseControl/coverage.svg?branch=master)](https://codecov.io/github/OHDSI/CaseControl?branch=master)

CaseControl is part of the [OHDSI Methods Library](https://ohdsi.github.io/MethodsLibrary).

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
Requires R. Installation on Windows requires [RTools](http://cran.r-project.org/bin/windows/Rtools/). Libraries used in CaseControl require Java.

Installation
============
1. Make sure your R environment is properly configured. This means that Java must be installed, and on Windows RTools must be installed. See [these instructions](https://ohdsi.github.io/MethodsLibrary/rSetup.html) for how to configure your R environment.
2. In R, use the following commands to download and install CaseControl:

  ```r
  install.packages("drat")
  drat::addRepo("OHDSI")
  install.packages("CaseControl")
  ```

User Documentation
==================
Documentation can be found on the [package website](https://ohdsi.github.io/CaseControl).

PDF versions of the documentation are also available:
* Vignette: [Single studies using the CaseControl package](https://raw.githubusercontent.com/OHDSI/CaseControl/master/inst/doc/SingleStudies.pdf)
* Vignette: [Running multiple analyses at once using the CaseControl package](https://raw.githubusercontent.com/OHDSI/CaseControl/master/inst/doc/MultipleAnalyses.pdf)
* Package manual: [CaseControl.pdf](https://raw.githubusercontent.com/OHDSI/CaseControl/master/extras/CaseControl.pdf)

Support
=======
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="https://github.com/OHDSI/CaseControl/issues">GitHub issue tracker</a> for all bugs/issues/enhancements

Contributing
============
Read [here](https://ohdsi.github.io/MethodsLibrary/contribute.html) how you can contribute to this package.

License
=======
CaseControl is licensed under Apache License 2.0

Development
===========
CaseControl is being developed in R Studio.

### Development status

Beta
