# `dqLib`

`dqLib` is an R package for data quality assessment and reporting. 
`dqLib` provides methods for calculating data quality metrics and generating reports on detected data quality issues, especially in [`CORD-MI`](https://www.medizininformatik-initiative.de/de/CORD).

Acknowledgement: This work was done within the “Collaboration on Rare Diseases” of the Medical Informatics Initiative (CORD-MI) funded by the German Federal Ministry of Education and Research (BMBF), under grant number: 01ZZ1911R, FKZ-01ZZ1911R

## Data Quality Metrics and Reports
-  `dqLib` provides functions for creating specific reporting scripts that enable user to select desired data quality dimensions and indicators. The data quality reports provide adequate information to find the data quality violations and the causes of these violations.
- The following data quality dimensions, indicatos and parameters are implemented:

  | Dimension  | Data Quality Indicator | 
  | ------------- | ------------- |
  | completeness  | item completeness rate, value completeness rate, subject completeness rate, case completeness rate, orphaCoding completeness rate  | 
  | plausibility  | orphaCoding plausibility rate, range plausibility rate | 
  | uniqueness |RD case unambiguity rate, RD case dissimilarity rate|
  | concordance |concordance with reference values| 
  
    
  | No. | Data Quality Parameter | Description |
  |-----|--------------------------- | ------------|
  |  P1 | mandatory data items | number of mandatory items per year |
  |  P2 | missing data items |  number of missing data items per year |
  |  P3 | mandatory data values | number of available mandatory data values per year |
  |  P4 | missing mandatory data values| number of missing data values per year |
  |  P5 | missing orphacodes |  number of missing Orphacodes per year |
  |  P6 | tracer diagnoses |  number of tracer RD diagnoses per year |
  |  P7 | implausible links | number of implausible code links per year |
  |  P8 | checked for outliers | number of checked data values for outliers per year |
  |  P9 | outliers | number of detected outliers per year |
  |  P10 | ambigous RD cases | number of ambigous RD cases per year |
  |  P11 | RD cases | number of RD cases per year |
  |  P13 | duplicated RD cases |  number of duplicated RD cases per year |
  |  P14 | tracer cases |  number of tracer RD cases per year |
  |  P15 | inpatient cases |  number of inpatient cases per year |
  |  P16 | RD cases rel. frequency| relative frequency of inpatient RD cases per year |
  |  P17 | Orpha cases | number of available orpha-coded cases per year|
  |  P18 | tracer cases rel. frequency| relative frequency of inpatient tracer RD cases per year |

- The following references are required to assess the quality of RD documentation:

  - Current Version of Alpha-ID-SE Terminology [1]
  - A reference for tracer diagnoses such as the list provided in [2].
  
    [1]   BfArM - Alpha-ID-SE [Internet]. [cited 2022 May 23]. Available from: https://www.bfarm.de/EN/Code-systems/Terminologies/Alpha-ID-SE/_node.html 
    
    [2]   List of Tracer Diagnoses Extracted from Alpha-ID-SE Terminology [Internet]. 2022 [cited 2022 May 24]. Available from:  https://doi.org/21.11101/0000-0007-F6DF-9
------------------------------------------------------------------------

## Installation

You can install `dqLib` from local folder with:

``` r
devtools::install_local("./dqLib")
```
You can also install it directly from github with:

``` r
devtools::install_github("https://github.com/KaisTahar/dqLib")
```
## Example

Here are [examples](https://github.com/KaisTahar/cordDqChecker-MIM) for data quality analysis and reporting using this package
- [cordDQCheck.R](https://github.com/KaisTahar/cordDqChecker-MIM/blob/master/Local/cordDqChecker.R) for generating data quality reports in CORD-MI.
- Here you can see [the resulting files](https://github.com/KaisTahar/cordDqChecker-MIM/tree/master/Local/Data/Export)

## Note
The default data quality dimensions are completeness, plausibility, uniqueness and concordance. Howerver, this framework allows the user to select desired quality dimensions and indicators as well as to generate user defined DQ reports. 

To cite `dqLib`, please use the following **BibTeX** entry: 

```
@software{Tahar_dqLib,
author = {Tahar, Kais},
title = {{dqLib}},
url = {https://github.com/KaisTahar/dqLib}
year = {2021}
}
```
See also: [`CORD-MI`](https://www.medizininformatik-initiative.de/de/CORD)


