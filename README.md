# <p align="center"> Data Quality Library (dqLib): An R Package for Traceable and Explainable Assessments of Clinical Data Quality </p>

## 1. Description
The Data Quality Library (`dqLib`) is an R package for traceable and explainable data quality (DQ) assessment. It offers generic methods for calculating DQ metrics and generating reports on detected DQ issues, especially in clinical care and research. This package also provides specific functions for reporting on DQ issues that may arise in the context of cardiovascular diseases (CVDs), and rare diseases (RDs). The reports provide adequate information to explain the detected DQ issues and assist users in tracing them back to their sources and underlying causes. Moreover, the latest release enables the detection and visualization of plausibility issues based on predefined logical and mathematical rules. To improve usability, this version allows users to specify DQ rules using spreadsheets. Exemplary visualizations and DQ reports are available in section 4, while further details on the developed functions are given in the [news](https://github.com/KaisTahar/dqLib/blob/master/NEWS.md).

## 2. Installation

You can install `dqLib` directly from GitHub by running the following command:

``` r 
devtools::install_github("https://github.com/KaisTahar/dqLib") 
```

To install `dqLib`, you can also clone the code repository of the desired [version](https://github.com/KaisTahar/dqLib/tags) or download it, and then run the following command from the local folder:

``` r
devtools::install_local("./dqLib")
```

## 3. DQ Metrics and Reports
`dqLib` provides multiple metrics and reporting functions to analyze different aspects of DQ. The implemented functions enable users to select appropriate indicators and generate customized DQ reports. The following generic DQ Indicators are already implemented:
<table>
    <thead>
        <tr>
            <th colspan="2">DQ Indicator </th>
            <th rowspan=2>DQ Dimension</th>
        </tr>
       <tr>
            <th>Abbreviation </th>
            <th>Name </th>
       </tr>
    </thead>
    <tbody>
        <tr>
            <td>dqi_co_icr</td>
            <td >Item Completeness Rate</td>
            <td rowspan=3>completeness</td>
        </tr>
        <tr>
            <td>dqi_co_vcr</td>
            <td>Value Completeness Rate</td>
        </tr>
	 <tr>
            <td>dqi_co_scr</td>
            <td>Subject Completeness Rate</td>
        </tr>
        <tr>
            <td>dqi_pl_rpr</td>
           <td > Range Plausibility Rate </td>
           <td rowspan=2>Plausibility</td>
        </tr>
            <td>dqi_pl_spr</td>
           <td > Semantic Plausibility Rate </td>
        </tr>
    </tbody>
</table>

<br />  In addition to indicators, the reports include relevant parameters and offer adequate information to help users address the detected DQ issues. `dqLib` provides functions to report on the following DQ issues and related parameters:
  
  | Abbreviation | DQ Parameter | Description |
  |-----|--------------------------- | ------------|
  |  im_misg | missing mandatory data items |  number of missing mandatory data items|
  |  vm_misg | missing mandatory data values| number of missing mandatory data values|
  |  s_inc | incomplete subjects |  number of incomplete subject records|
  |  vo | outlier values | number of detected outlier values  |
  |  vc | contradictory values | number of detected contradictory data values  |
  
<br /> `dqLib` also provides functions to assess the following specific indicators for RD data: 
<table>
    <thead>
        <tr>
            <th colspan="2"> DQ Indicator </th>
            <th rowspan=2> DQ Dimension</th>
        </tr>
       <tr>
            <th>Abbreviation </th>
            <th>Name </th>
       </tr>
    </thead>
    <tbody>
       <tr>
            <td> dqi_un_cur </td>
            <td > RD Case unambiguity Rate </td>
             <td rowspan=2 > Uniqueness</td>
        </tr>
         <tr>
            <td> dqi_un_cdr </td>
            <td > RD Case Dissimilarity Rate </td>
        </tr>
        <tr>
            <td>dqi_co_icr</td>
           <td >Orphacoding Completeness Rate </td>
            <td >Completeness</td>
        </tr>
        <tr>
            <td> dqi_pl_opr </td>
           <td > Orphacoding Plausibility Rate </td>
          <td >Plausibility</td>
        </tr>
        <tr>
            <td> dqi_cc_rvl </td>
            <td> Concordance with Reference Values from Literature </td>
            <td >Concordance</td>
        </tr>
    </tbody>
</table>

<br /> Moreover, `dqLib` enables annual assessments of selected DQ parameters. The following RD-specific metrics are already implemented: 
  | Abbreviation | DQ Parameter | Description |
  |-----|--------------------------- | ------------|
  |  rdCase | RD cases | number of RD cases |
  |  orphaCase | Orpha cases | number of available orpha-coded cases|
  |  tracerCase | tracer cases |  number of tracer cases |
  |  rdCase_rel | RD cases rel. frequency| relative frequency of RD cases |
  |  orphaCase_rel | Orpha cases rel. frequency| relative frequency of Orpha cases normalized to 100.000 inpatient cases |
  |  tracerCase_rel | tracer cases rel. frequency| relative frequency of tracer cases normalized to 100.000 inpatient cases  |
  |  tracerCase_rel_min | minimal tracer cases in reference values| min. rel. frequency of tracer cases normalized to 100.000 inpatient cases found in the literature |
  |  tracerCase_rel_max  | maximal tracer cases in reference values| max. rel. frequency of tracer cases normalized to 100.000 inpatient cases found in the literature   |
  |  vm_case_misg | missing mandatory data values in case module |  number of missing mandatory data values in the case module |
  |  rdCase_amb | ambiguous RD cases | number of ambiguous RD cases |
  |  rdCase_dup | duplicated RD cases |  number of duplicated RD cases |
  |  oc_misg | missing Orphacodes |  number of missing Orphacodes by tracer diagnoses |
  |  link_ip | implausible links | number of implausible ICD-10-GM/OC links |

  
The following references are required to assess the quality of RD documentation: (1) Current Version of Alpha-ID-SE Terminology [1] and (2) a reference for tracer diagnoses such as the list provided in [2].

  [1] BfArM - Alpha-ID-SE [Internet]. [cited 2022 May 23]. Available from: [BfArM](https://www.bfarm.de/EN/Code-systems/Terminologies/Alpha-ID-SE/_node.html)
 
  [2] Tahar et al. Rare Diseases in Hospital Information Systems — An Interoperable Methodology for Distributed Data Quality Assessments. Methods Inf Med. 2023 Sep;62(3/4):71–89. [DOI: 10.1055/a-2006-1018](https://www.thieme-connect.com/products/ejournals/abstract/10.1055/a-2006-1018)

## 4. Examples
- [CordDqChecker](https://github.com/KaisTahar/cordDqChecker/tree/bmc_dqTools): A reporting tool for DQ assessment on RD data implemented using `dqLib`. The code repository of `CordDqChecker` includes some [examples](https://github.com/KaisTahar/cordDqChecker/tree/bmc_dqTools/Local/Data/Export) of DQ reports generated using synthetic data.
- [CvdDqChecker](https://github.com/KaisTahar/cvdDqChecker): A reporting tool for assessing the quality of CVD data. This tool was also implemented using `dqLib`. The [./Export](https://github.com/KaisTahar/cvdDqChecker/tree/master/Data/Export) folder contains exemplary DQ reports and visualizations.

## 5. Notes

- To cite `dqLib`, please use the CITATION file in the folder `./inst`.

- Acknowledgment: This work was funded by the German Centre for Cardiovascular Research (DZHK), grant number 81X1300117, and the "Collaboration on Rare Diseases" of the Medical Informatics Initiative (CORD-MI) under grant number: 01ZZ1911R, FKZ-01ZZ1911R.
