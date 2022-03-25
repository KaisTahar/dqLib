# `dqLib`

`dqLib` is an R package for data quality analysis and reporting. 
`dqLib` provides methods for calculating data quality metrics and generating data quality reports.

## Data Quality Metrics
- The following data quality dimensions and indicatos are implemented:

  | Dimension  | Indicator Name|
  | ------------- | ------------- |
  | completeness  | missing_item_rate, missing_value_rate, orphaCoding_completeness_rate  |
  | plausibility  | outlier_rate, orphaCoding_plausibility_rate |
  | uniqueness | rdCase_uniqueness_rate|
  | concordance | orphaCoding_relativeFrequency, unique_rdCase_relativeFrequency|
  
- The following references are required to assess the quality of orphacoding:

  - Current Version of Alpha-ID-SE list [1]
  - Tracer diagnosis list such as Hamburger list [2] extended with relation types of ICD-10 to Orpha codes. 
  - Here are [Examples](https://github.com/KaisTahar/dqLib/tree/master/refData) of required references that can be easily used or updated.

    [1] DIMDI/Alpha-ID-SE list: www.dimdi.de
    
    [2] Schulz M et alt:. Prävalenz seltener Erkrankungen in der ambulanten Versorgung in Deutschland im Zeitraum 2008 bis 2011, Versorgungsatlas-Bericht. 2015;15/13
  

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

Here are [examples](https://github.com/medizininformatik-initiative/cord-dq-checker) for data quality analysis and reporting using this package
- [cordDQCheck.R](https://github.com/medizininformatik-initiative/cord-dq-checker/blob/master/cordDqChecker.R) for generating data quality reports in CORD-MI.
- Here you can see [the resulting files](https://github.com/medizininformatik-initiative/cord-dq-checker/tree/master/Data/Export)

## Citation

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
