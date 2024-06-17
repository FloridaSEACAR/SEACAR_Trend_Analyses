# SEACAR Atlas Python Scripts

## Generating Charts for use on the Atlas Website

### seacarcharts.py

main() function has three function calls to generate specific types of charts:

- createDiscreteWaterQualityCharts
- createContinuousWaterQualityCharts
- createAcreageCharts

Each of those functions will usually have a way to specify a single MA or Parameter to decrease work during development/testing

#### Global Variables

- DEBUG - True for testing and saving locally
- imgDirectory - Directory charts should be saved
- pngquant - Installed PNGQuant executable @ <https://pngquant.org/>

## Generation of Summary Text

### IndicatorSummarySQL/

Folder containing the SQL stored procedures that are used to generate indicator summary text for most of the Habitats. See README.md for details.

### analysisIndicatorSummaries.py

Generates summary text for the "State of the Indicator" column in the "Status and Trend Summary for each Habitat & Indicator" HTML table on Managed Area overview pages. Output is saved to "/data/analysisResults" folder with files starting with "SEACAR_IndicatorSummaries_".

### analysisParameterFindings.py

Generates Excel spreadsheet versions of R-generated output data files with human readable text summaries of each MA and Parameter. We scripted this to generate textual summaries of parameter-level analysis results to give to managers of specific Managed Areas.

### INPUT Results data files that are processed

- data\analysisResults\WQ_Discrete_All_KendallTau_Stats.txt
- data\analysisResults\WQ_Continuous_All_KendallTau_Stats.txt
- data\analysisResults\SAV_BBpct_LMEresults_All.txt
- data\analysisResults\Oyster_All_GLMM_Stats.txt
- data\analysisResults\Nekton_SpeciesRichness_MA_Overall_Stats.txt

### OUTPUT files

- data\analysisResults\SEACAR_AnalysisResults_YYYYMMDD_WC.xlsx
- data\analysisResults\SEACAR_AnalysisResults_YYYYMMDD_SAV.xlsx
- data\analysisResults\SEACAR_AnalysisResults_YYYYMMDD_OY.xlsx
- data\analysisResults\SEACAR_AnalysisResults_YYYYMMDD_NEK.xlsx

## exportManagedAreaHabitatIndicatorTable.py

Generates an Excel spreadsheet version of the current ManagedArea_Habitat_Indicator table which should exactly match the "State of the Indicator" column in the "Status and Trend Summary for each Habitat & Indicator" HTML table on Managed Area overview pages.

## Supporting Files

- analysis.py: data classes and data access queries used to get/manipulate analyse result data for generation of text summaries
- database.py: global database connection string used in all DB queries
- managedAreas.py: data classes and data access queries related to Managed Areas
- parameters.py:  data classes and data access queries related to WQ Parameters
- _matplotlibrc.default: sample Matplotlib configuration file
- seacar_atlas.mplstyle: SEACAR Atlas' Matplotlib configuration file
