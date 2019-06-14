# LASI_2019_equity
Code for the LASI 2019 Probing Equity with Data Workshop
## Introduction 
These are R scripts and synthetic data sets that are the technical elements of the LASI 2019 Equity Workshop. 
These make use of the following libraries:
- `tidyverse`: a popular library that simplifies common data science calculations
- `optmatch`: a propensity score matching library
- `lars`:    a library that uses LASSO and related hierarachical linear modeling techniques for data reduction.
- `censusapi`: a library that provides an interface to ACS and Census tables. Requires API key.

The scripts that use these libraries will be used and described in more detail in the course of the workshop. 
We can also help with installation, etc.

## Elements
### Development
These tools are developed within the R language, which is part of the the RStudio development environment. 
Ideally RStudio will be installed on the user's machine.

### Code
- `grade_performance.R`: The top level function is a wrapper that takes the two provided data tables as inputs.
The user specifies a single course, and this function returns statistics of diversity, equity, and inclusion;
here, these are course demographic diversity, grade penalties for different groups in the course, and persistence/retention
in the next course (when a sequence is specified).

- `lasso_rank.R`: A wrapper for the `lars` package that uses LASSO and related HLM techniques for dimensional reduction, 
a key to the set-up the pseudo-experiment that will be conducted via propensity-score matching and the `optmatch` library.

- `census_api_example.R`: A wrapper for the `censusapi` library.

### Data
- `student_course.tab`: A synthetic data set of student courses and course grades, as they might exist in a real SIS,
that are central to this project. Diversity, equity, and inclusion will be considered in these contexts.
- `student_record.tab`: A synthetic data set of student background/demography data that serves as basic controls for
academic preparation gender/URM/first-gen status.
