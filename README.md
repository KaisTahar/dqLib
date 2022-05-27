# `dqLib`

`dqLib` is an R package for data quality assessment and reporting. 
`dqLib` provides methods for calculating data quality metrics and generating reports on data quality, especially in [`CORD-MI`](https://www.medizininformatik-initiative.de/de/CORD).

Acknowledgement: This work was done within the “Collaboration on Rare Diseases” of the Medical Informatics Initiative (CORD-MI) funded by the German Federal Ministry of Education and Research (BMBF), under grant number: FKZ-01ZZ1911R

## Data Quality Metrics
- The following data quality dimensions and indicatos are implemented:

  | Dimension  | Indicator Name|
  | ------------- | ------------- |
  | completeness  | missing_item_rate, missing_value_rate, orphaCoding_completeness_rate  |
  | plausibility  | outlier_rate, orphaCoding_plausibility_rate |
  | uniqueness | rdCase_uniqueness_rate, duplication_rate|
  | concordance | orphaCoding_relativeFrequency, unique_rdCase_relativeFrequency|
  
- The following references are required to assess the quality of RD documentation:

  - Current Version of Alpha-ID-SE Terminology [1]
  - A reference for tracer diagnoses such as the list provided in [2].
  
    [1]   BfArM - Alpha-ID-SE [Internet]. [cited 2022 May 23]. Available from: https://www.bfarm.de/EN/Code-systems/Terminologies/Alpha-ID-SE/_node.html 
    
    [2]   List of Tracer Diagnoses Extracted from Alpha-ID-SE Terminology [Internet]. 2022 [cited 2022May 24]. Available from:  https://doi.org/21.11101/0000-0007-F6DF-9
------------------------------------------------------------------------

## Installation

You can install `dqLib` from local folder with:

``` r
devtools::install_local("./dqLib")
```
You can also install it directly from github with:

``` r
devtools::install_github("https://github.com/medizininformatik-initiative/dqLib")
```
## Example

Here are [examples](https://github.com/medizininformatik-initiative/cord-dq-checker) for data quality analysis and reporting using this package
- [cordDQCheck.R](https://github.com/medizininformatik-initiative/cord-dq-checker/blob/master/Local/cordDqChecker.R) for generating data quality reports in CORD-MI.
- Here you can see [the resulting files](https://github.com/medizininformatik-initiative/cord-dq-checker/tree/master/Local/Data/Export)

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
