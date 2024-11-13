# dqLib 1.20.0

**New changes and tests**

- This version was tested and validated using electronic clinical trial (ECT) data on cardiovascular diseases (CVDs)
- New generic function for DQ assessments across multiple domains using generic indicators
- New generic indicator for assessing the Semantic Plausibility
- New function to detect common data quality issues that may arise in the context of CVDs
- New functions to enable the specification of range rules using spreadsheets and the detection of outliers based on the predefined rules
- New functions that enable users to define missing data rules using spreadsheets and apply the predefined rules
- New functions to handle metadata and visualize detected outliers
- Improved function to check missing data values. The new version supports missing rules and coded missing values. This functionality is particularly crucial in the context of clinical trials
- Improved functions for semantic enrichment and reporting
- Improved function to set global variables
- Replacement of deprecated code
- Update the package documentation

**Full set of changes:** [v1.5.0...v1.20.0](https://github.com/KaisTahar/dqLib/compare/v1.5.0...v1.20.0)


# dqLib 1.5.0

**New changes and tests**

- This version was validated using EHR-based real-world data on rare diseases (RDs)
- Added function for semantic enrichment to improve the quality and clarity of generated reports
- Improvement of the functionality for generating DQ reports and the layout of the created reports
- Updated package documentation

**Full set of changes:** [v1.3.1...v1.5.0](https://github.com/KaisTahar/dqLib/compare/v1.3.1...v1.5.0)

# dqLib 1.3.1

**New changes and tests**
- This version was successfully tested using synthetic data across multiple hospitals. The conceptual framework for harmonized DQ assessments and the findings from the distributed DQ assessments have been published under [DOI: 10.1055/a-2006-1018](https://www.thieme-connect.com/products/ejournals/abstract/10.1055/a-2006-1018)
- Implemented functions for assessing data completeness, plausibility, and uniqueness were validated using real-world EHR data, and the results have been published under [DOI: 10.3233/SHTI230121](https://ebooks.iospress.nl/doi/10.3233/SHTI230121)
- Added new functions to assess the completeness of cases and subjects
- Added a new function to evaluate the concordance indicator
- Fixed bug
- Updated package documentation

**Full set of changes:** [v1.0.0...v1.3.1](https://github.com/KaisTahar/dqLib/compare/v1.0.0...v1.3.1)

# dqLib 1.0.0

- First version of the data quality library (dqLib), which was tested independently using synthetic data
- Test results indicated the correctness of the implemented data quality (DQ) indicators as reported in [DOI: 10.3205/22gmds116](https://www.egms.de/static/en/meetings/gmds2022/22gmds116.shtml)
