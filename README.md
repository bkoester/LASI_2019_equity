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
- `larc.matched.outcomes.R`: A wrapper for the `optmatch` set of propensity score matching functions.

### Data
Transcript level and student information system data are generally not publically-available for many good reasons. But it
is also a limitation when it comes to developing shared methods and drawing in many people to explore the possibilities of ressearch with SIS data. Using the statistical relationships among students, courses, and their characteristics we can construct synthetic student data sets that look very much like the real thing. Many - but not ALL - relationships observed in the real data are recovered in the synthetic data. Previously unknown relationships may also be discovered in the synthetic data, and may prompt a researcher to turn around and ask if that discovery is simply an artifact of the synthetic data, or if it exists in their own real data as well.

- `student_course.tab`: A synthetic data set of student courses and course grades, as they might exist in a real SIS,
that are central to this project. Diversity, equity, and inclusion will be considered in these contexts.
- `student_record.tab`: A synthetic data set of student background/demography data that serves as basic controls for
academic preparation gender/URM/first-gen status.

### Cut and paste examples
#### Example 1
`> source(simple.grade.penalty.R)`

`> sc <- load_sc_data()`

`> sr <- load_sr_data()`

`> acs <- all_course_statistics(sr,sc)`

`> course_grade_penalty_plot(sr,sc,'PHYSICS',140)`

`> course_grade_penalty_plot_by_gender(sr,sc,'PHYSICS',140)`

`>`

`> source('grade_performance.R')`

`> aa <- grade_performance(EQUITY=TRUE,ISREAL=FALSE,SBJCT='PHYSICS',CATNUM=140) `

