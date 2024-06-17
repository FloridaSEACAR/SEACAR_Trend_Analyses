from ast import AsyncFunctionDef
from math import nan
import os, sys, time
from pydoc import describe
import subprocess
from datetime import date, datetime, timedelta
from regex import F
import slugify
import numpy as np
import pandas as pd
import pyodbc
from dataclasses import Field, dataclass
import parameters
import managedAreas
import analysis
import database



# GLOBALS (putting them here for now)

reportDir = os.path.join(os.getcwd(), 'data\\analysisResults\\')
nonSeacarWebsiteAreaIDs = [24, 32, 36, 42, 46]
fieldOnlyParams = ['Water Temperature', 'pH', 'Dissolved Oxygen', 'Dissolved Oxygen Saturation', 'Secchi Depth']
labOnlyParams = ['Total Suspended Solids', 'Total Nitrogen', 'Total Phosphorus', 'Chlorophyll a, Uncorrected for Pheophytin', 'Chlorophyll a, Corrected for Pheophytin', 'Colored Dissolved Organic Matter']


# DEBUG = True
# if DEBUG:
#     print('debug')

def main():
    print(f'App Started.')

    CreateTrendText()

    print(f'App Finished.')


def CreateTrendText():
    """
    Generates Excel spreadsheet versions of R-generated output data files with human readable text summaries of each MA and Parameter.
    
    Processes R-generated analysis results output files (e.g. "WQ_Discrete_All_KendallTau_Stats.txt").
    Creates Excel spreadsheets with filenames starting with "SEACAR_AnalysisResults_"
    """
    
    
    areaDict = managedAreas.getManagedAreasAsDict()
    paramDict = parameters.getParametersAsDict()
    
    
    # Results data that are processed are:
    # data\analysisResults\WQ_Discrete_All_KendallTau_Stats.txt
    # data\analysisResults\WQ_Continuous_All_KendallTau_Stats.txt
    # data\analysisResults\SAV_BBpct_LMEresults_All.txt
    # data\analysisResults\Oyster_All_GLMM_Stats.txt
    # data\analysisResults\Nekton_SpeciesRichness_MA_Overall_Stats.txt

    ProcessResults_WC()
    ProcessResults_SAV()
    ProcessResults_OY()
    ProcessResults_NEK()



def ProcessResults_OY():
    # 6/12/23 - switching to Oyster_All_GLMM_Stats.txt
    df = pd.read_csv(os.path.join(reportDir, 'Oyster_All_GLMM_Stats.txt'), delimiter='|')
    # print(df.head(5))

    df = df[~df.AreaID.isin(nonSeacarWebsiteAreaIDs)]

    df = df.astype({
        'ManagedAreaName': str,
        'ParameterName': str,
        'SizeClass': str,
        'ShellType': str,
        'HabitatType': str,
        'Programs': str,
        'ProgramIDs': str,
    })

    def createOysterFindingsText(row):

        if pd.isna(row.ParameterName) or row.ParameterName == "nan":
            return ''
        
        if pd.isna(row.ModelEstimate):
            return f'Insufficient data was available to assess long-term trends for {row.ParameterName.lower()} in {row.ManagedAreaName}.'

        text = ''

        if row.ParameterName.upper() == 'Density'.upper():

            increasing = row.ModelEstimate > 0
            trendPresent = False if (row.LowerConfidence < 0 and row.UpperConfidence > 0) else True
            trendStatus = 'no significant change'
            if trendPresent:
                trendStatus = 'an increase' if increasing else 'a decrease'

            text = f'Live oyster density within {row.ManagedAreaName} has shown {trendStatus} between {int(row.EarliestLiveDate)} and {int(row.LatestLiveDate)}.'

        elif row.ParameterName.upper() == 'Shell Height'.upper():
            increasing = row.ModelEstimate > 0
            trendPresent = False if (row.LowerConfidence < 0 and row.UpperConfidence > 0) else True
            trendStatus = 'showed no significant change'
            if trendPresent:
                trendStatus = 'significantly increased' if increasing else 'significantly decreased'
            text = f'Between {int(row.EarliestLiveDate)} and {int(row.LatestLiveDate)}, shell height of {row.ShellType.lower()} in the {row.SizeClass} size class {trendStatus}.'

        elif row.ParameterName.upper() == 'Percent Live'.upper():
            increasing = row.ModelEstimate > 0
            trendPresent = False if (row.LowerConfidence < 0 and row.UpperConfidence > 0) else True
            trendStatus = 'no significant change'
            if trendPresent:
                trendStatus = 'an increase' if increasing else 'a decrease'

            text = f'Between {int(row.EarliestLiveDate)} and {int(row.LatestLiveDate)} data shows {trendStatus} in the proportion of live oysters within {row.ManagedAreaName}.'

        else:
            text = 'WARNING: An unknown [ParameterName] was found.'
        
        return text

    df['TrendText'] = df.apply(lambda x: createOysterFindingsText(x), axis=1).astype(str)

    df = df[(df.ParameterName != np.nan) & (df.ParameterName != 'nan')]

    df.replace(np.nan, '', regex=True, inplace=True)
    df.replace('nan', '', regex=False, inplace=True)

    # print(df.head(30))
    output_filename = os.path.join(reportDir, f'SEACAR_AnalysisResults_{date.today().isoformat().replace("-","")}_OY.xlsx')

    df.to_excel(os.path.join(reportDir, output_filename), sheet_name='OY', float_format='%.9f', index=False, na_rep='')
    
    print(f'Output file saved [{output_filename}]')



def ProcessResults_NEK():
    df = pd.read_csv(os.path.join(reportDir, 'Nekton_SpeciesRichness_ManagedArea_Overall_Stats.txt'), delimiter='|')
    #print(df.head(5))

    df = df[~df.AreaID.isin(nonSeacarWebsiteAreaIDs)]

    df = df.astype({
        'ManagedAreaName': str,
        'GearType': str,
        'GearSize_m': float,
        'ParameterName': str,
        'N_Years': 'Int32',
        'EarliestYear': 'Int32',
        'LatestYear': 'Int32',
        'N_Data': 'Int32',
    })

    def createNektonFindingsText(row):

        if pd.isna(row.ParameterName) or row.ParameterName == "nan":
            return 'EXCEPTION: Unrecognized Parameter'
        
        if pd.isna(row.N_Years) or row.N_Years <= 0:
            return ''
        
        minValueStr = '0' if row.Min != 0.0 else f'{row.Min:.1f}'
        text = f'Between {row.EarliestYear} and {row.LatestYear} annual average Nekton richness per 100 square meters in {row.ManagedAreaName} was {row.Mean:.2f} species, with a maximum of {row.Max:.1f} and a minimum of {"0" if row.Min == 0.0 else f"{row.Min:.1f}"}.'
        
        return text

    df['TrendText'] = df.apply(lambda x: createNektonFindingsText(x), axis=1).astype(str)

    # df.replace(np.nan, '', regex=True)
    df = df.replace('nan', '', regex=False)

    # print(df.head(30))
    output_filename = os.path.join(reportDir, f'SEACAR_AnalysisResults_{date.today().isoformat().replace("-","")}_NEK.xlsx')

    df.to_excel(os.path.join(reportDir, output_filename), sheet_name='NEK', float_format='%.9f', index=False, na_rep='')
    
    print(f'Output file saved [{output_filename}]')



def ProcessResults_WC():
    dfDisc = pd.read_csv(os.path.join(reportDir, 'WQ_Discrete_All_KendallTau_Stats.txt'), delimiter='|')
    dfCont = pd.read_csv(os.path.join(reportDir, 'WQ_Continuous_All_KendallTau_Stats.txt'), delimiter='|')

    # CONT CLEANUP - 11/15/22 - data contains records with no Program Info and no Relative Depth
    # 11/16/22 - don't be cleanin'! we want to keep these so we can see/show which areas/programs don't have which param data
    # print(f'dfCont # Rows: {len(dfCont.index)}')
    # dfCont = dfCont[ pd.isna(dfCont['RelativeDepth']) == False ]
    # print(f'dfCont # Rows: {len(dfCont.index)}')

    # New column for both
    dfDisc['SamplingFrequency'] = 'Discrete'
    dfCont['SamplingFrequency'] = 'Continuous'

    # Add columns to Disc
    dfDisc['ProgramID'] = np.nan
    dfDisc['ProgramName'] = np.nan
    dfDisc['ProgramLocationID'] = np.nan

    # Add columns to Cont
    dfCont['ActivityType'] = np.nan


    # NOTE: the following were needed when DISC had Website column but CONT did not
    # dfCont['Website'] = np.nan
    # contParamsToIncludeOnWebsite = ['Dissolved Oxygen', 'Dissolved Oxygen Saturation', 'pH', 'Salinity', 'Turbidity', 'Water Temperature']
    # dfCont['Website'] = dfCont.apply(lambda row: 1 if row.ParameterName in contParamsToIncludeOnWebsite else 0, axis=1)
    # print(f'dfCont.ParameterName.unique(): {dfCont.ParameterName.unique()}')
    # print(f'dfCont.Website.unique(): {dfCont.Website.unique()}')


    df = pd.concat([dfDisc, dfCont])

    # print(f'df.SamplingFrequency.unique(): {df.SamplingFrequency.unique()}')
    # print(f'df.ActivityType.unique(): {df.ActivityType.unique()}')
    # print(f'df.RelativeDepth.unique(): {df.RelativeDepth.unique()}')
    # print(f'df.Website.unique(): {df.Website.unique()}')

    # Cleanup
    df['RelativeDepth'] = df['RelativeDepth'].str.replace('bottom', 'Bottom')
    df['RelativeDepth'] = df['RelativeDepth'].str.replace('surface', 'Surface')

    # print(f'df.RelativeDepth.unique(): {df.RelativeDepth.unique()}')


    # Manually reorder columns
    #print(df.columns)
    df = df[['AreaID', 'ManagedAreaName', 'SamplingFrequency', 'ProgramID', 'ProgramName', 'ProgramLocationID', 'ParameterName', 'RelativeDepth',
       'ActivityType','N_Data', 'N_Years', 'EarliestYear',
       'LatestYear', 'LastSampleDate', 'SufficientData', 'Median', 'Independent', 'tau', 'p',
       'SennSlope', 'SennIntercept', 'ChiSquared', 'pChiSquared', 'Trend', 'Website']]

    df = df[~df.AreaID.isin(nonSeacarWebsiteAreaIDs)]
    
    #print(df.head(30))

    df = df.astype({
        'ProgramID': 'Int32',
        'N_Data': 'Int32',
        'N_Years': 'Int32',
        'EarliestYear': 'Int32',
        'LatestYear': 'Int32',
        'Website': 'boolean',
    })

    def getParamText(fullParameterName):
        replacementDict = {
            'Chlorophyll a uncorrected for pheophytin': 'chlorophyll-a uncorrected',
            'Chlorophyll a corrected for pheophytin': 'chlorophyll-a corrected',
            'Colored dissolved organic matter, CDOM': 'CDOM', 
            'Dissolved Oxygen': 'dissolved oxygen (mg/L)',
            'Dissolved Oxygen Saturation': 'dissolved oxygen (% saturation)', 
            'Salinity': 'salinity', 
            'Secchi Depth':'Secchi depth', 
            'Total Nitrogen':'total nitrogen',
            'Total Phosphorus':'total phosphorus', 
            'Total Suspended Solids, TSS':'TSS', 
            'Turbidity':'turbidity',
            'Water Temperature':'water temperature', 
            'pH':'pH'
        }
        return replacementDict[fullParameterName] if fullParameterName in replacementDict else fullParameterName

    def getPreParamText(fullParameterName):
        replacementDict = {
            'Total Suspended Solids, TSS':'concentration of ', 
        }
        return replacementDict[fullParameterName] if fullParameterName in replacementDict else ''

    def getPostParamText(fullParameterName):
        replacementDict = {
            'Colored dissolved organic matter, CDOM': ' in the water column has', 
            'Total Nitrogen':' concentrations have',
            'Total Phosphorus':' concentrations have', 
            'Total Suspended Solids, TSS':' in the water column has', 

            'Chlorophyll a uncorrected for pheophytin': ' has',
            'Chlorophyll a corrected for pheophytin': ' has',
            'Dissolved Oxygen': ' has',
            'Dissolved Oxygen Saturation': ' has', 
            'Salinity': ' has', 
            'Secchi Depth':' has', 
            'Turbidity':' has',
            'Water Temperature':' has', 
            'pH':' has'
        }
        return replacementDict[fullParameterName] if fullParameterName in replacementDict else ''

    def createTrendText(row):
        if pd.isna(row.SufficientData):
                return ''

        # 12/5/22: We're now processing Discrete data at ALL depths (All/Surface/Bottom)
        if row.SamplingFrequency == 'Discrete':
        # if row.SamplingFrequency == 'Discrete' and (row.RelativeDepth == 'Surface' or row.RelativeDepth == 'All'):
            
            if (row.SufficientData is False) or \
            (row.ActivityType == 'Field' and row.ParameterName in labOnlyParams) or \
            ((row.ActivityType == 'Sample' or row.ActivityType == 'Lab') and row.ParameterName in fieldOnlyParams):
                return ''

            text = f'Between {row.EarliestYear} and {row.LatestYear}, {getPreParamText(row.ParameterName)}{getParamText(row.ParameterName)}{getPostParamText(row.ParameterName)} {GetTrendChangeText(row.Trend, pastTense=True)} in {row.ManagedAreaName}.'
            return text

        else:
            if (row.SufficientData is False):
                return ''

            text = f'Between {row.EarliestYear} and {row.LatestYear}, {getPreParamText(row.ParameterName)}{getParamText(row.ParameterName)}{getPostParamText(row.ParameterName)} {GetTrendChangeText(row.Trend, pastTense=True)} at station "{row.ProgramLocationID}" in {row.ManagedAreaName}.'
            return text
    
    df['TrendText'] = df.apply(lambda x: createTrendText(x), axis=1).astype(str)

    # df = df.replace(np.nan, '', regex=True)
    df = df.replace('nan', '', regex=False)

    # print(df.head(4))
    output_filename = os.path.join(reportDir, f'SEACAR_AnalysisResults_{date.today().isoformat().replace("-","")}_WC.xlsx')

    df.to_excel(output_filename, sheet_name='WaterColumn', float_format='%.9f', index=False, na_rep='')

    print(f'Output file saved [{output_filename}]')



def ProcessResults_SAV():
    df = pd.read_csv(os.path.join(reportDir, 'SAV_BBpct_LMEresults_All.txt'), delimiter='|')
    #print(df.head(30))

    df = df[~df.AreaID.isin(nonSeacarWebsiteAreaIDs)]

    df = df.astype({
        'ManagedAreaName': str,
        'Species': str,
        'ParameterName': str,
        'N_Programs': 'Int32',
        'ProgramIDs': str,
        'N_Data': 'Int32',
        'N_Years': 'Int32',
        'EarliestYear': 'Int32',
        'LatestYear': 'Int32',
        'SufficientData': 'boolean',
        # 'Model_Failed': 'boolean',
    })
    
    df['Trend'] = df.apply(lambda x: calcLmeTrend(x.SufficientData, x.LME_Slope, x.p), axis=1).astype('Int64')

    def GetSpeciesName(species):
        if 'SAV' in species:
            return 'total SAV'
        elif 'spp.' in species:
            return species
        else:
            return species.lower()

    def createTrendText(row):
        if pd.isna(row.Trend):
            return ''

        text = f'Between {row.EarliestYear} and {row.LatestYear} data from monitoring efforts show {GetTrendChangeText(row.Trend)} in the estimated percent cover of {GetSpeciesName(row.Species)} within {row.ManagedAreaName}.'
        return text
    
    df['TrendText'] = df.apply(lambda x: createTrendText(x), axis=1).astype(str)

    # df.replace(np.nan, '', regex=True, inplace=True)
    # df.replace('nan', '', regex=False, inplace=True)
    # df = df.replace(np.nan, '', regex=True)
    # df = df.fillna('')
    df = df.replace('nan', '', regex=False)

    # print(df.head(30))
    output_filename = os.path.join(reportDir, f'SEACAR_AnalysisResults_{date.today().isoformat().replace("-","")}_SAV.xlsx')

    df.to_excel(os.path.join(reportDir, output_filename), sheet_name='SAV', float_format='%.9f', index=False, na_rep='')
    
    print(f'Output file saved [{output_filename}]')



def calcLmeTrend(sufficientData, slope, p):

    if pd.isna(sufficientData) or pd.isna(slope) or pd.isna(p):
        return
    
    if sufficientData is False:
        return

    return 0 if p>0.05 else (-1 if slope<0.0 else 1)



def GetTrendChangeText(trend, opposite=False, pastTense=False):
    increasingText = 'an increase' if not pastTense else 'increased'
    noChangeText = 'no significant change' if not pastTense else 'shown no significant change'
    decreasingText = 'a decrease' if not pastTense else 'decreased'

    if trend < 0:
        return decreasingText if not opposite else increasingText
    elif trend > 0:
        return increasingText if not opposite else decreasingText
    else:
        return noChangeText



if __name__ == '__main__':

    st = time.perf_counter()

    main()

    elapsed = time.perf_counter() - st
    elapsed_units = "seconds"
    if elapsed > 60:
        elapsed = elapsed/60.
        elapsed_units = "minutes"

    print(f'{__file__} executed in {elapsed:0.1f} {elapsed_units}')
