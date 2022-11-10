# `dqLib`

The data quality library (`dqLib`) is an R package for data quality (DQ) assessment and reporting. 
`dqLib` provides methods for calculating DQ metrics and generating reports on detected DQ issues, especially in [`CORD-MI`](https://www.medizininformatik-initiative.de/de/CORD).

Acknowledgment: This work was done within the “Collaboration on Rare Diseases” of the Medical Informatics Initiative (CORD-MI) funded by the German Federal Ministry of Education and Research (BMBF), under grant number: 01ZZ1911R, FKZ-01ZZ1911R

## DQ Metrics and Reports
`dqLib` provides functions for creating specific reporting scripts that enable user to select desired DQ dimensions, indicators and parameters. The DQ reports provide adequate information to find the data quality violations and the causes of these violations. 
`dqLib` also enables annual assessments of selected DQ metrics. The following DQ metrics are already implemented:

  | Dimension  | DQ Indicator | 
  | ------------- | ------------- |
  | completeness  | item completeness rate, value completeness rate, subject completeness rate, case completeness rate, orphaCoding completeness rate  | 
  | plausibility  | orphaCoding plausibility rate, range plausibility rate | 
  | uniqueness |RD case unambiguity rate, RD case dissimilarity rate|
  | concordance |concordance with reference values| 
  
    
  | No. | DQ Parameter | Description |
  |-----|--------------------------- | ------------|
  |  P1 | mandatory data items | number of mandatory items |
  |  P2 | missing mandatory data items |  number of missing data mandatory data items|
  |  P3 | mandatory data values | number of available mandatory data values |
  |  P4 | missing mandatory data values| number of missing mandatory data values|
  |  P5 | inpatient cases |  number of inpatient cases |
  |  P6 | inpatients |  number of inpatients  |
  |  P7 | incomplete subjects |  number of incomplete subject records|
  |  P8 | mandatory data values in case module  |  number of data values required for recording the case module |
  |  P9 | missing data values in case module |  number of missing mandatory data values in the case module |
  |  P10 | data values selected for outlier detection |  number of data values checked for outlier |
  |  P11 | outliers | number of detected outliers  |
  |  P12 | tracer diagnoses |  number of diagnoses that exclusively code rare diseases (RD)|
  |  P13 | missing Orphacodes |  number of missing Orphacodes by tracer diagnoses |
  |  P14 | checked links | number of ICD-10-GM/OC links|
  |  P15 | implausible links | number of implausible ICD-10-GM/OC links |
  |  P16 | RD cases | number of RD cases |
  |  P17 | ambiguous RD cases | number of ambiguous RD cases |
  |  P18 | duplicated RD cases |  number of duplicated RD cases |
  |  P19 | tracer cases |  number of tracer cases per year |
  |  P20 | Orpha cases | number of available orpha-coded cases|
  |  P21 | RD cases rel. frequency| relative frequency of RD cases |
  |  P22 | tracer cases rel. frequency| relative frequency of tracer cases normalized to 100.000 inpatient cases  |
  |  P23 | Orpha cases rel. frequency| relative frequency of Orpha cases normalized to 100.000 inpatient cases |
  |  P24 | minimal tracer cases in reference values| min. rel. frequency of tracer cases normalized to 100.000 inpatient cases found in the literature |
  |  P25 | maximale tracer cases in reference values| max. rel. frequency of tracer cases normalized to 100.000 inpatient cases found  found in the literature   |

The following references are required to assess the quality of RD documentation: (1) Current Version of Alpha-ID-SE Terminology [1] and (2) A reference for tracer diagnoses such as the list provided in [2].

  [1] BfArM - Alpha-ID-SE [Internet]. [cited 2022 May 23]. Available from: [BfArM](https://www.bfarm.de/EN/Code-systems/Terminologies/Alpha-ID-SE/_node.html)
 
  [2] Tahar K, Martin T, Mou Y, et al. Distributed Data Quality Assessment Across CORD-MI Consortia.[doi:10.3205/22gmds116](https://www.egms.de/static/en/meetings/gmds2022/22gmds116.shtml)
  
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

[Here](https://github.com/KaisTahar/cordDqChecker-MIM/tree/methods_dqTools) are examples of DQ assessments using `dqLib`
- [cordDQCheck.R](https://github.com/KaisTahar/cordDqChecker-MIM/blob/methods_dqTools/Local/cordDqChecker.R): A reporting script for DQ assessment in CORD-MI
- Here you can see [the resulting files](https://github.com/KaisTahar/cordDqChecker-MIM/tree/methods_dqTools/Local/Data/Export)

## Note
The default data quality dimensions are completeness, plausibility, uniqueness and concordance. However, this framework allows the user to select desired DQ dimensions and metrics as well as to generate user defined annual reports.

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
