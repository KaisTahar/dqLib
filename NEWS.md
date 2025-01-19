# dqLib 1.32.0

**New changes and tests**

- This release was tested and validated using electronic health record (EHR) data on rare diseases (RDs) and electronic clinical trial (ECT) data on cardiovascular diseases (CVDs)
- New generic functions for assessing data quality (DQ) regarding semantic plausibility. The implemented functions enable the detection of contradictions based on predefined mathematical and logical rules
- New generic function to classify the detected DQ issues into different reports. These new reports offer more detailed information to explain the detected DQ issues and assist users trace them back to their sources and underlying causes
- New function to enable the specification of logical and mathematical rules using spreadsheets. These rule metadata are not dependent on the specific spreadsheets and data items employed
- New functions to count and visualize detected contradictions. Exemplary visualizations and DQ reports are available in the readme
- New function to report metrics on patient records, such as the number of patients with detected DQ issues
- New functions to provide environment variables and format date values
- Improved function to detect common DQ issues in CVD data
- Improved function to set environment variables
- Updated package documentation

**Full set of changes:** [v1.20.0...v1.32.0](https://github.com/KaisTahar/dqLib/compare/v1.20.0...v1.32.0)


# dqLib 1.20.0

**New changes and tests**

- This version was tested and validated using ECT data on CVDs
- New generic function for DQ assessments across multiple domains using generic indicators
- New generic indicator for assessing the Semantic Plausibility
- New function to detect common DQ issues that may arise in the context of CVDs
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

- This version was validated using EHR-based real-world data on RDs
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
- Test results indicated the correctness of the implemented DQ indicators as reported in [DOI: 10.3205/22gmds116](https://www.egms.de/static/en/meetings/gmds2022/22gmds116.shtml)
