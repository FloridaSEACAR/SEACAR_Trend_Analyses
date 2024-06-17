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


ACREAGE_INCLUDE_C_CAP = 'C-CAP'
ACREAGE_INCLUDE_Y = 'Y'

nonSeacarWebsiteAreaIDs = [24, 32, 36, 42, 46]


# DEBUG = True
# if DEBUG:
#     print('debug')

def main():
    print(f'App Started.')

    generateIndicatorSummaries()

    print(f'App Finished.')


def generateIndicatorSummaries():
    """
    Generates summary text for the "State of the Indicator" column in the 
    "Status and Trend Summary for each Habitat & Indicator" HTML table on Managed Area overview pages. 
    Output is saved to "/data/analysisResults" folder with files starting with "SEACAR_IndicatorSummaries_".
    In early 2023, some of this logic was moved to SQL Server stored procedures.
    """
    
    areaDict = managedAreas.getManagedAreasAsDict()
    paramDict = parameters.getParametersAsDict()

    print(f'Number of Managed Areas: {len(areaDict)}')
    print(f'Number of Parameters: {len(paramDict)}')

    dfCombinedParameterIndicator = analysis.getCombinedParameterIndicator()



    # Processing WC - Nekton #
    ##########################

    dfNektonResults = analysis.getNektonAnalysesResults()
    dfNektonResults = dfNektonResults[~dfNektonResults.AreaID.isin(nonSeacarWebsiteAreaIDs)]
    dfSummaries_WC_Nekton = GenerateSummaries_WC_Nekton(dfNektonResults, areaDict, paramDict, dfCombinedParameterIndicator)
    
    # dfSummaries_WC_Nekton.to_excel(os.path.join(reportDir, f'SEACAR_IndicatorSummaries_{date.today().isoformat().replace("-","")}_NEK.xlsx'), sheet_name='NEK', float_format='%.9f', index=False, na_rep='')
    # print(dfSummaries_WC_Nekton)



    # Processing SAV - Spp. Richness #
    ##################################
    dfSAVSummary = analysis.getTotalSAVResultsSummary()
    dfSAVResults = analysis.getTotalSAVResults()
    percentCoverParamId = 24
    dfSAVSummaryPC = dfSAVSummary[dfSAVSummary.ParameterID == percentCoverParamId]
    dfSAVResultsPC = dfSAVResults[dfSAVResults.ParameterID == percentCoverParamId]
    dfCPI_SAV_PC = dfCombinedParameterIndicator[dfCombinedParameterIndicator.ParameterID == percentCoverParamId]
    cpi = dfCPI_SAV_PC.iloc[0]
    dfSummaries_SAV_SppRichness = GenerateSummaries_SAV_SppRichness(dfSAVSummaryPC, dfSAVResultsPC, areaDict, paramDict, cpi)
    #print(dfSummaries_SAV_SppRichness)




    # Processing Oysters #
    ######################

    # FOR NOW I'M GOING TO MANUALLY MANIPULATE ANALYSISPARAMETERFINDINGS FOR OY INTO INDICATORSUMMARIES FORMAT
    # dfSummaries_OY = GenerateSummaries_OY(areaDict, paramDict, cpi)




    # Processing Water Column #
    ###########################

    dfDiscSummary = analysis.getDiscreteWCResultsSummary()
    dfContSummary = analysis.getContinuousWCResultsSummary()
    dfResults = analysis.getWCAnalysesResults()
    
    # print(f'dfDiscSummary has [{len(dfDiscSummary.index)}] rows')
    # print(f'dfContSummary has [{len(dfContSummary.index)}] rows')
    # print(f'dfResults has [{len(dfResults.index)}] rows')

    # print(dfDiscSummary.head(2))
    # print(dfContSummary.head(2))
    # print(dfResults.head(2))


    # Processing Nutrients (which are discrete only) (15/TN and 19/TP) together (as one) #
    ######################################################################################
    nutrientParamIds = [15, 19]
    dfNutSummary = dfDiscSummary[dfDiscSummary.ParameterID.isin(nutrientParamIds)]
    dfNutResults = dfResults[dfResults.ParameterID.isin(nutrientParamIds)]
    dfSummaries_WC_Nut = GenerateSummaries_WC_Nutrients(dfNutSummary, dfNutResults, areaDict, paramDict)


    # Processing WC - Water Clarity - Secchi Depth (11) #
    #####################################################
    dfWCSummary = dfDiscSummary[dfDiscSummary.ParameterID == 11]
    dfWCResults = dfResults[dfResults.ParameterID == 11]
    dfSummaries_WC_WC = GenerateSummaries_WC_WC(dfWCSummary, dfWCResults, areaDict, paramDict)
    
    
    # Process WQ Indicators having Discrete+Continuous Data #
    #########################################################
    wqParamIdsWithDiscAndCont = [1, 2, 3, 4, 5, 7, 15]

    # Added 15/TN only for DO comparison logic
    #wqParamIdsWithDiscAndCont = wqParamIdsWithDiscAndCont.append(100)

    dfBothDiscSummary = dfDiscSummary[dfDiscSummary.ParameterID.isin(wqParamIdsWithDiscAndCont)]
    dfBothContSummary = dfContSummary[dfContSummary.ParameterID.isin(wqParamIdsWithDiscAndCont)]
    dfBothResults = dfResults[dfResults.ParameterID.isin(wqParamIdsWithDiscAndCont)]
    dfSummaries_WC_WQ_DiscCont = GenerateSummaries_WC_WQ_BothDiscCont(dfBothDiscSummary, dfBothContSummary, dfBothResults, areaDict, paramDict, dfCombinedParameterIndicator)
    # dfSummaries_WC_WQ_DiscCont.to_excel(os.path.join(reportDir, f'SEACAR_IndicatorSummaries_{date.today().isoformat().replace("-","")}_WC_WQ_DiscCont.xlsx'), sheet_name='WC_WQ_DiscCont', float_format='%.9f', index=False, na_rep='')


    dfDiscSummaryTURB = dfDiscSummary[dfDiscSummary.ParameterID == 7]
    dfContSummaryTURB = dfContSummary[dfContSummary.ParameterID == 7]
    dfResultsTURB = dfResults[dfResults.ParameterID == 7]
    dfSummaries_SAV_TURB = GenerateSummaries_SAV_TURB(dfDiscSummaryTURB, dfContSummaryTURB, dfResultsTURB, areaDict, paramDict, dfCombinedParameterIndicator[dfCombinedParameterIndicator.ParameterID == 7])



    dfIndicatorSummaries = pd.concat([dfSummaries_WC_Nut, dfSummaries_WC_WC, dfSummaries_WC_Nekton, dfSummaries_SAV_SppRichness, dfSummaries_WC_WQ_DiscCont, dfSummaries_SAV_TURB])
    dfIndicatorSummaries.to_excel(os.path.join(reportDir, f'SEACAR_IndicatorSummaries_{date.today().isoformat().replace("-","")}.xlsx'), sheet_name='IndicatorSummaries', float_format='%.9f', index=False, na_rep='')



# WC/SAV - Water Clarity: 7/TURB
# Number of possible trend logic responses: 1
def GenerateSummaries_SAV_TURB(dfSummaryDisc, dfSummaryCont, dfResultsBoth, areaDict, paramDict, dfCPITurb):

    fullAreaIdList = areaDict.keys()
    areaIdListResults =  dfResultsBoth.AreaID.unique()
    paramIdListResults =  dfResultsBoth.ParameterID.unique()

    print(f'Numer of MAs: {len(areaIdListResults)}')
    print(f'Numer of Parameters: {len(paramIdListResults)}')
    print(f'dfResults length = {len(dfResultsBoth.index)} rows')

    dfSummaries = GetEmptySummaryDataframe()

    for areaId in fullAreaIdList:
        ma = areaDict.get(areaId)

        # Continuous Parameters: 7/TURB
        PARAM_ID_TURB = 7

        paramTURB = paramDict.get(PARAM_ID_TURB)

        cpi = dfCPITurb.iloc[0]
        indicatorName = cpi.IndicatorName.lower()

        summary = ''


        dfSummaryD = dfSummaryDisc[(dfSummaryDisc.AreaID == areaId)]
        dfSummaryC = dfSummaryCont[(dfSummaryCont.AreaID == areaId)]
        dfResults = dfResultsBoth[(dfResultsBoth.AreaID == areaId)]

        if len(dfSummaryD.index) > 0:
            ds = dfSummaryD.iloc[0]
            if ds.Sufficient > 0:
                if ds.MinTrend == ds.MaxTrend:
                    summary = f"Water clarity impacts the health of submerged resources and is monitored using several measures, one of which is turbidity. Within {ma.shortName}, turbidity readings from grab samples indicate {GetTrendChangeText(ds.AverageTrend, opposite=True, pastTense=False)} in water clarity between {int(ds.EarliestYear)} and {int(ds.MostRecentYear)}."
                else:
                    summary = f'*** TODO: handle case when there are multiple discrete TURBIDITY records and trends are not equal ***'
                
            else:
                summary = f'Insufficient data was available to assess long-term trends for water clarity in {ma.shortName}.'
        else:
            summary = f'Data for water clarity is needed for {ma.shortName}.'

        # hard-coding IDs and NAMEs
        dfSummaries.loc[len(dfSummaries.index)] = [6, 'Submerged Aquatic Vegetation', -99, 'Water Clarity', ma.id, ma.name, summary, '', 'SAV-WC-1']
        #dfSummaries.loc[len(dfSummaries.index)] = [cpi.HabitatID, cpi.HabitatName, cpi.IndicatorID, cpi.IndicatorName, ma.id, ma.name, summary]
        summary = ''

    return dfSummaries


# Continuous WQ Parameters: 1/DO 2/SAL 3/TEMP 4/PH 5/DOS        (15/TN added only for DO-related logic)
# Number of possible trend logic responses: 3
def GenerateSummaries_WC_WQ_BothDiscCont(dfSummaryDisc, dfSummaryCont, dfResultsBoth, areaDict, paramDict, dfCPI):
    print(f'\nGenerateSummaries_WC_WQ_BothDiscCont() -- BEGIN')

    fullAreaIdList = areaDict.keys()
    areaIdListResults =  dfResultsBoth.AreaID.unique()
    paramIdListResults =  dfResultsBoth.ParameterID.unique()

    print(f'Numer of MAs: {len(areaIdListResults)}')
    print(f'Numer of Parameters: {len(paramIdListResults)}')

    print(f'dfResults length = {len(dfResultsBoth.index)} rows')


    dfSummaries = GetEmptySummaryDataframe()


    for areaId in fullAreaIdList:
        ma = areaDict.get(areaId)
        
        # print(f'Processing {ma.name} ({ma.id})')


        # Continuous Parameters: 1/DO 2/SAL 3/TEMP 4/PH 5/DOS
        PARAM_ID_DO = 1
        PARAM_ID_SAL = 2
        PARAM_ID_TEMP = 3
        PARAM_ID_PH = 4
        PARAM_ID_DOS = 5
        # PARAM_ID_TURB = 7
        PARAM_ID_TN = 15

        paramDO = paramDict.get(PARAM_ID_DO)
        paramSAL = paramDict.get(PARAM_ID_SAL)
        paramTEMP = paramDict.get(PARAM_ID_TEMP)
        paramPH = paramDict.get(PARAM_ID_PH)
        paramDOS = paramDict.get(PARAM_ID_DOS)
        # paramTURB = paramDict.get(PARAM_ID_TURB)
        paramTN = paramDict.get(PARAM_ID_TN)

        dfCPIforAnyWQ = dfCPI[dfCPI.ParameterID == PARAM_ID_DO]
        cpi = dfCPIforAnyWQ.iloc[0]
        indicatorName = cpi.IndicatorName.lower()

        summary = ''

        # Habitat: Water Column
        # Indicator: Water Quality

        # Currently summary logic options are/concern:
        #   (1) ... DO concentrations from discrete monitoring have <trend>
        #   (2) Long-term discrete data shows... TEMP trend + SAL trend + DO trend + PH TREND
        #   (3) dataloggers at [#] sites collect near-continuous data. [StationID] data has shown [trend] in SAL



        dfSummaryD = dfSummaryDisc[(dfSummaryDisc.AreaID == areaId)]
        dfSummaryC = dfSummaryCont[(dfSummaryCont.AreaID == areaId)]
        dfResults = dfResultsBoth[(dfResultsBoth.AreaID == areaId)]

        # if ma.id == 7:
        #     print(dfSummaryD)
        #     print(dfSummaryC)
        #     print(dfResults)
            
        # (1)
        #####
        dsDO = dfSummaryD[dfSummaryD.ParameterID == PARAM_ID_DO]
        csDO = dfSummaryC[dfSummaryC.ParameterID == PARAM_ID_DO]
        rbDO = dfResults[dfResults.ParameterID == PARAM_ID_DO]

        dsTN = dfSummaryD[dfSummaryD.ParameterID == PARAM_ID_TN]
        dsTEMP = dfSummaryD[dfSummaryD.ParameterID == PARAM_ID_TEMP]
        dsSAL = dfSummaryD[dfSummaryD.ParameterID == PARAM_ID_SAL]

        if len(dsDO.index) > 0:
            ds = dsDO.iloc[0]
            if ds.Sufficient > 0:
                if ds.MinTrend == ds.MaxTrend:
                    # print(f'ds.MinTrend == ds.MaxTrend: {ds.AverageTrend}')
                    if ds.AverageTrend != 0:
                        # summary = f"Dissolved oxygen (DO) concentrations are influenced by multiple indicators that affect water's capacity to hold oxygen. For instance, increases in water temperature can decrease DO concentrations. Since {int(ds.EarliestYear)}, dissolved oxygen (DO) concentrations, from discrete monitoring, have {GetTrendChangeText(ds.AverageTrend, pastTense=True)}."
                        # summary = f"Dissolved oxygen (DO) concentrations are influenced by multiple indicators that affect water's capacity to hold oxygen. For instance, increases in water temperature can decrease DO concentrations. Since {int(ds.EarliestYear)}, dissolved oxygen (DO) concentrations, from discrete monitoring, have {GetTrendChangeText(ds.AverageTrend, pastTense=True)}."
                        summary = f"Since {int(ds.EarliestYear)}, dissolved oxygen (DO) concentrations, from discrete monitoring, have {GetTrendChangeText(ds.AverageTrend, pastTense=True)}."
                        doComparisonIntroText = "DO concentrations are influenced by multiple indicators that affect water's capacity to hold oxygen."
                        
                        dsNit = dsTN.iloc[0]
                        dsTemp = dsTEMP.iloc[0]
                        dsSal = dsSAL.iloc[0]

                        if dsNit.Sufficient > 0 and dsNit.MinTrend == dsNit.MaxTrend and dsNit.AverageTrend != 0:
                            if dsNit.AverageTrend * -1.0 == ds.AverageTrend:
                                # print(f'GOOD ---- DO Trend [{ds.AverageTrend}]; TN Trend [{dsNit.AverageTrend}]  (AreaID = {ma.id})')
                                tnText = 'increased' if dsNit.AverageTrend > 0 else 'decreased'
                                doText = 'decrease' if ds.AverageTrend < 0 else 'increase'
                                summary = summary + f' {doComparisonIntroText} For instance, {tnText} nitrogen levels can {doText} DO concentrations.'
                            # else:
                            #     print(f'BAD ---- DO Trend [{ds.AverageTrend}]; TN Trend [{dsTemp.AverageTrend}] (AreaID = {ma.id})')
                                # summary = summary + f' [Sufficient TN data, but TN trend was the same as DO trend]'
                            #print('KEITH - TN')
                            # print(dsTemp)
                            
                        elif dsTemp.Sufficient > 0 and dsTemp.MinTrend == dsTemp.MaxTrend and dsTemp.AverageTrend != 0:
                            if dsTemp.AverageTrend * -1.0 == ds.AverageTrend:
                                # print(f'GOOD ---- DO Trend [{ds.AverageTrend}]; Temp Trend [{dsTemp.AverageTrend}] (AreaID = {ma.id})')
                                tempText = 'increases' if dsTemp.AverageTrend > 0 else 'decreases'
                                doText = 'decrease' if ds.AverageTrend < 0 else 'increase'
                                summary = summary + f' {doComparisonIntroText} For instance, {tempText} in water temperature can {doText} DO concentrations.'
                            # else:
                            #     print(f'BAD ---- DO Trend [{ds.AverageTrend}]; Temp Trend [{dsTemp.AverageTrend}] (AreaID = {ma.id})')
                                # summary = summary + f' [Sufficient TEMP data, but TEMP trend was the same as DO trend]'
                            #summary = summary + f' For instance, {GetTrendChangeText(ds.AverageTrend, pastTense=True)}'
                            #print('KEITH - TEMP')
                            # print(dsTemp)

                        elif dsSal.Sufficient > 0 and dsSal.MinTrend == dsSal.MaxTrend and dsSal.AverageTrend != 0:
                            if dsSal.AverageTrend * -1.0 == ds.AverageTrend:
                                # print(f'GOOD ---- DO Trend [{ds.AverageTrend}]; SAL Trend [{dsSal.AverageTrend}] (AreaID = {ma.id})')
                                salText = 'decreased' if dsSal.AverageTrend < 0 else 'increased'
                                doText = 'increase' if ds.AverageTrend > 0 else 'decrease'
                                summary = summary + f' {doComparisonIntroText} For instance, {salText} salinity levels can {doText} DO concentrations.'
                            # else:
                            #     print(f'BAD ---- DO Trend [{ds.AverageTrend}]; SAL Trend [{dsSal.AverageTrend}] (AreaID = {ma.id})')
                                # summary = summary + f' [Sufficient SAL data, but SAL trend was the same as DO trend]'


                        # else:
                        #     print(f'KEITH - WHAT TO DO????   (AreaID = {ma.id})')
                            
                        # return
                    
                else:
                    summary = f'*** TODO: handle case when multiple DO records and trends are not equal  (AreaID = {ma.id}) ***'
                
                if summary != '':
                    dfSummaries.loc[len(dfSummaries.index)] = [cpi.HabitatID, cpi.HabitatName, cpi.IndicatorID, cpi.IndicatorName, ma.id, ma.name, summary, '', 'WC-WQ-1']

                # print(summary)
                summary = ''


        # (2)
        #####
        dsTEMP = dfSummaryD[dfSummaryD.ParameterID == PARAM_ID_TEMP]
        # csTEMP = dfSummaryC[dfSummaryC.ParameterID == PARAM_ID_TEMP]
        # rbTEMP = dfResults[dfResults.ParameterID == PARAM_ID_TEMP]
        dsSAL = dfSummaryD[dfSummaryD.ParameterID == PARAM_ID_SAL]
        # csSAL = dfSummaryC[dfSummaryC.ParameterID == PARAM_ID_SAL]
        # rbSAL = dfResults[dfResults.ParameterID == PARAM_ID_SAL]
        dsPH = dfSummaryD[dfSummaryD.ParameterID == PARAM_ID_PH]
        # csPH = dfSummaryC[dfSummaryC.ParameterID == PARAM_ID_PH]
        # rbPH = dfResults[dfResults.ParameterID == PARAM_ID_PH]

        DiscSuffCountDO = 0 if (True if len(dsDO.index) == 0 else False) else dsDO.iloc[0].Sufficient
        DiscSuffCountTEMP = 0 if (True if len(dsTEMP.index) == 0 else False) else dsTEMP.iloc[0].Sufficient
        DiscSuffCountSAL = 0 if (True if len(dsSAL.index) == 0 else False) else dsSAL.iloc[0].Sufficient
        DiscSuffCountPH = 0 if (True if len(dsPH.index) == 0 else False) else dsPH.iloc[0].Sufficient

            
        # summaryTemplate = 'Long-term discrete data shows water temperature has [increased/decreased/ shown no significant change], salinity has [increased/decreased/shown no significant change], dissolved oxygen has [increased/decreased/shown no significant change], and pH has [increased/decreased/shown no significant change] in [MA].'
        # summaryTemplate = 'Long-term discrete data shows [TEMP], [SAL], [DO], and [PH] in [MA].'

        summary = 'Long-term discrete data shows '
        paramSummaries = []
        if DiscSuffCountTEMP > 0:
            paramSummaries.append(f'{paramTEMP.name.lower()} has {GetTrendChangeText(dsTEMP.iloc[0].AverageTrend, pastTense=True)}')
        if DiscSuffCountSAL > 0:
            paramSummaries.append(f'{paramSAL.name.lower()} has {GetTrendChangeText(dsSAL.iloc[0].AverageTrend, pastTense=True)}')
        if DiscSuffCountDO > 0:
            paramSummaries.append(f'{paramDO.name.lower()} has {GetTrendChangeText(dsDO.iloc[0].AverageTrend, pastTense=True)}')
        if DiscSuffCountPH > 0:
            paramSummaries.append(f'{paramPH.name} has {GetTrendChangeText(dsPH.iloc[0].AverageTrend, pastTense=True)}')
        
        if (DiscSuffCountTEMP + DiscSuffCountSAL + DiscSuffCountDO + DiscSuffCountPH) > 0:
            summary = summary + ', '.join(paramSummaries[:-1]) + ', and ' + paramSummaries[-1] + f' in {ma.shortName}.'
            dfSummaries.loc[len(dfSummaries.index)] = [cpi.HabitatID, cpi.HabitatName, cpi.IndicatorID, cpi.IndicatorName, ma.id, ma.name, summary, '', 'WC-WQ-2']
        summary = ''
        

        # (3)
        #####
        csSAL = dfSummaryC[dfSummaryC.ParameterID == PARAM_ID_SAL]
        crSAL = dfResults[(dfResults.ParameterID == PARAM_ID_SAL) & (dfResults.SamplingFrequency == 'Continuous')]
        ContSuffCountSAL = 0 if (True if len(csSAL.index) == 0 else False) else csSAL.iloc[0].Sufficient
        if ContSuffCountSAL > 0:
            sal = csSAL.iloc[0]
            salTrendCount = sal.DistinctTrends
            salTrendMin = sal.MinTrend
            salTrendMax = sal.MaxTrend
            salTrendAvg = sal.AverageTrend
            salYearMin = sal.EarliestYear
            salYearMax = sal.MostRecentYear

            summary = f"Dataloggers are deployed at {ContSuffCountSAL} site{'' if ContSuffCountSAL==1 else 's'} within {ma.shortName} and collect near-continuous data on multiple parameters. "
            stnSummaries = []
            
            # when do we need to list out each individual station and its trend?
            # - when there's only 1 station
            # - when there are multiple stations and the min trend != max trend 
            if (ContSuffCountSAL > 1 and salTrendMin != salTrendMax) or (ContSuffCountSAL == 1):
                sufficientResults = crSAL[crSAL.SufficientData == 1]

                posSummaries = []
                notSummaries = []
                negSummaries = []
                
                for index, row in sufficientResults.iterrows():
                    # sText = f"Station {row.ProgramLocationID} data has shown {GetTrendChangeText(int(row.Trend), pastTense=False)} in {paramSAL.name.lower()} levels between {int(row.EarliestYear)} and {int(row.LatestYear)}."
                    sText = f"Station {row.ProgramLocationID.upper()} data has shown {GetTrendChangeText(int(row.Trend), pastTense=False)} in {paramSAL.name.lower()} levels between {int(row.EarliestYear)} and {int(row.LatestYear)}."

                    if int(row.Trend) > 0:
                        posSummaries.append(sText)
                    elif int(row.Trend) < 0:
                        negSummaries.append(sText);
                    else:
                        notSummaries.append(sText)
                
                stnSummaries.extend(posSummaries)
                stnSummaries.extend(notSummaries)
                stnSummaries.extend(negSummaries)

                # stnSummaries.append(f"Station {row.ProgramLocationID} data has shown {GetTrendChangeText(int(row.Trend), pastTense=False)} in {paramSAL.name.lower()} levels between {int(row.EarliestYear)} and {int(row.LatestYear)}.")
            
            else:
                stnSummaries.append(f"All station data has shown {GetTrendChangeText(int(salTrendAvg), pastTense=False)} in {paramSAL.name.lower()} levels between {int(salYearMin)} and {int(salYearMax)}.")

            summary = summary + ' '.join(stnSummaries)
            dfSummaries.loc[len(dfSummaries.index)] = [cpi.HabitatID, cpi.HabitatName, cpi.IndicatorID, cpi.IndicatorName, ma.id, ma.name, summary, '', 'WC-WQ-3']
            summary = ''

        
        # Did any of the above logic generate indicator summary text? If NOT, then generate a no-data / insufficient data summary.
        areaSummaryCount = len(dfSummaries[(dfSummaries.ManagedAreaID == areaId)].index)
        if areaSummaryCount == 0:
            print(f'**** No indicator summary text generated for {ma.shortName} ({ma.id})')
            indicatorParameterText = 'all discrete water quality parameters'
            if len(dfResults[dfResults.SufficientData == True]) > 0:
                # indicatorParameterText = 'water quality'
                # FROM ABOVE:
                #   (1) ... DO concentrations from discrete monitoring have <trend>
                #   (2) Long-term discrete data shows... TEMP trend + SAL trend + DO trend + PH TREND
                #   (3) dataloggers at [#] sites collect near-continuous data. [StationID] data has shown [trend] in SAL
                # indicatorParameterText = 'water quality' # per 2023_07_Logic_ManagedAreaHabitatIndicator_V2.xlsx they want an exact list of indicator/parameters
                # indicatorParameterText = 'discrete DO concentrations; long-term discrete water temperature, salinity, DO and pH; and near-continuous salinity levels'
                summary = f'Insufficient data was available to assess long-term trends for {indicatorParameterText} in {ma.shortName}.'
            else:
                #summary = f'Data for water quality is needed for {ma.shortName}.'
                summary = f'Insufficient data was available to assess long-term trends for {indicatorParameterText} in {ma.shortName}.'
                
            dfSummaries.loc[len(dfSummaries.index)] = [cpi.HabitatID, cpi.HabitatName, cpi.IndicatorID, cpi.IndicatorName, ma.id, ma.name, summary, '', 'WC-WQ-0']
            summary = ''
    
    # print(dfSummaries)
    
    print(f'GenerateSummaries_WC_WQ_BothDiscCont() -- END')
    
    return dfSummaries


# SAV - Percent Cover - 24
# Number of possible trend logic responses: 1
def GenerateSummaries_SAV_SppRichness(dfSummary, dfResults, areaDict, paramDict, cpi):
    print(f'\nGenerateSummaries_SAV_SppRichness() -- BEGIN')

    areaIdList =  dfSummary.AreaID.unique()
    paramIdList =  dfSummary.ParameterID.unique()

    print(f'{len(areaIdList)} Managed Areas provided: {areaIdList}')
    print(f'{len(paramIdList)} Parameters provided: {paramIdList}')
    print(f'len(dfSummary): {len(dfSummary.index)} rows')
    print(f'len(dfResults): {len(dfResults.index)} rows')

    parameterId = cpi.ParameterID
    param = paramDict.get(parameterId)
    #indicatorName = cpi.IndicatorName.lower()
    indicatorName = 'percent cover'   # override

    dfSummaries = GetEmptySummaryDataframe()
    summary = ''
    
    # Combined_SAV_Analysis does ensure all Managed Areas so lets iterate through the full list
    for ma in areaDict.values():

        if ma.id in areaIdList:

            s = dfSummary[(dfSummary.AreaID == ma.id) & (dfSummary.ParameterID == parameterId)].iloc[0]
            dfR = dfResults[(dfResults.AreaID == ma.id) & (dfResults.ParameterID == parameterId)]

            if s.HasTotalSeagrass == 1 and not pd.isna(s.TotalSeagrassTrend):

                summary = f'Between {int(s.TotalSeagrassEarliestYear)} and {int(s.TotalSeagrassLatestYear)} data from monitoring efforts show {GetTrendChangeText(s.TotalSeagrassTrend)} in the estimated median percent cover of seagrasses within {ma.shortName}.'
            
            elif s.HasTotalSAV == 1 and not pd.isna(s.TotalSAVTrend):

                # summary = f'Between {int(s.TotalSAVEarliestYear)} and {int(s.TotalSAVLatestYear)} data from monitoring efforts show {GetTrendChangeText(s.TotalSAVTrend)} in the percent cover of submerged aquatic vegetation within {ma.shortName}.'
                summary = f'Between {int(s.TotalSAVEarliestYear)} and {int(s.TotalSAVLatestYear)} data from monitoring efforts show {GetTrendChangeText(s.TotalSAVTrend)} in the estimated median percent cover of submerged aquatic vegetation within {ma.shortName}.'
            
            else:
                
                # print(f'MA[{ma.id}]')
                #print(s)
                if (
                    (s.SppCountSufficientWithTrendNonTotal == s.SppCountDecreaseNonTotal) or 
                    (s.SppCountSufficientWithTrendNonTotal == s.SppCountNoChangeNonTotal) or
                    (s.SppCountSufficientWithTrendNonTotal == s.SppCountIncreaseNonTotal)
                ):
                    dfSppOnlyResults = dfR[(dfR.Species != 'Total seagrass') & (dfR.Species != 'Total SAV')]
                    dfSppOnlyWithTrend = dfSppOnlyResults[(dfSppOnlyResults.SufficientData == True) & (~dfSppOnlyResults.p.isna())]
                    p = dfSppOnlyWithTrend.iloc[0].p
                    slope = dfSppOnlyWithTrend.iloc[0].LME_Slope
                    trendValue = GetTrendValue(p, slope)
                    earliestYear = int(min(dfSppOnlyWithTrend.EarliestYear.unique()))
                    latestYear = int(max(dfSppOnlyWithTrend.LatestYear.unique()))
                    summary = f'Within {ma.shortName} all modeled seagrass species have shown {GetTrendChangeText(trendValue)} between {earliestYear} and {latestYear}.'

                else:
                    dfSppOnlyResults = dfR[(dfR.Species != 'Total seagrass') & (dfR.Species != 'Total SAV')].copy()
                    dfSppOnlyWithTrend = dfSppOnlyResults[(dfSppOnlyResults.SufficientData == True) & (~dfSppOnlyResults.p.isna())].copy()

                    dfSppOnlyWithTrend['TrendValue'] = dfSppOnlyWithTrend.apply(lambda row: GetTrendValue(row.p, row.LME_Slope), axis=1)
                    
                    earliestYear = int(min(dfSppOnlyWithTrend.EarliestYear.unique()))
                    latestYear = int(max(dfSppOnlyWithTrend.LatestYear.unique()))

                    dfCounts = dfSppOnlyWithTrend.groupby(['TrendValue'])['TrendValue'].count()

                    summary = f'Between {earliestYear} and {latestYear}, {len(dfSppOnlyWithTrend.index)} species of seagrass were reported'

                    dfInc = dfCounts[dfCounts.index == 1].reset_index(name='Count')
                    dfNoTrend = dfCounts[dfCounts.index == 0].reset_index(name='Count')
                    dfDec = dfCounts[dfCounts.index == -1].reset_index(name='Count')


                    if len(dfInc.index) > 0:
                        summary = summary + f' and {dfInc.iloc[0].Count} showed {GetTrendChangeText(1)} in cover'

                    if len(dfNoTrend.index) > 0:
                        summary = summary + f' and {dfNoTrend.iloc[0].Count} showed {GetTrendChangeText(0)} in cover'

                    if len(dfDec.index) > 0:
                        summary = summary + f' and {dfDec.iloc[0].Count} showed {GetTrendChangeText(-1)} in cover'

                    summary = summary + '.'

        else:

            summary = f'Data for {indicatorName} is needed for {ma.shortName}.'

        dfSummaries.loc[len(dfSummaries.index)] = [cpi.HabitatID, cpi.HabitatName, cpi.IndicatorID, cpi.IndicatorName, ma.id, ma.name, summary, '', 'SAV-PC-1']

    print(f'GenerateSummaries_SAV_SppRichness() -- END')

    return dfSummaries


# WC - Nekton - Spp. Richness (Presence) - 36
# Number of possible trend logic responses: 1
def GenerateSummaries_WC_Nekton(df, areaDict, paramDict, dfCombinedParameterIndicator):
    print(f'\nGenerateSummaries_WC_Nekton -- BEGIN')
    #df = pd.read_csv(os.path.join(dataDir, 'Nekton_SpeciesRichness_ManagedArea_Overall_Stats.txt'), delimiter='|')

    paramId = 36    # Nekton Spp. Richness (Presence)
    param = paramDict.get(paramId)

    # print(param)
    # Parameter(id=36, name='Presence', code='Pres_Nekton', units='Y/N', precision=2, sampleFraction=None, minValue=nan, maxValue=nan)

    dfCPI = dfCombinedParameterIndicator[dfCombinedParameterIndicator.ParameterID == paramId]
    # print(dfCPI)
    # ParameterID  IndicatorID  DisplayOrder  Active ParameterName IndicatorName  HabitatID   HabitatName
    # 30           36            9           100   False      Presence        Nekton          7  Water Column
    cpi = dfCPI.iloc[0]
    # indicatorName = cpi.IndicatorName.lower()
    indicatorName = 'Nekton richness' # override

    #print(df.head(5))

    df = df.astype({
        'GearType': str,
        'GearSize_m': float,
        'N_Years': 'Int32',
        'EarliestYear': 'Int32',
        'LatestYear': 'Int32',
        'N_Data': 'Int32',
    })

    dfSummaries = GetEmptySummaryDataframe()

    # print(df)
        
    for index, row in df.iterrows():
        ma = areaDict.get(row.AreaID)
        summary = ''

        if row.SufficientData == False:
            if pd.isna(row.N_Years) or row.N_Years <= 0:
                summary = f'Data for {indicatorName} is needed for {ma.shortName}.'
            else:
                summary = f'Insufficient data was available to assess long-term trends for {indicatorName} in {ma.shortName}. Five years of data are required to assess long-term trends for {indicatorName}. {ma.shortName} only has {int(row.N_Years)} year{"s" if row.N_Years > 1 else ""} of data as of {int(row.LatestYear)}.'
        
        else:
            minValueStr = '0' if row.Min != 0.0 else f'{row.Min:.1f}'
            summary = f'Between {row.EarliestYear} and {row.LatestYear} annual {row.GearType.lower()} surveys showed average Nekton richness per 100 square meters was {row.Mean:.2f} species, with a maximum of {row.Max:.1f} species per 100 square meters in {int(row.Year_MaxRichness)} and a minimum of {"0" if row.Min == 0.0 else f"{row.Min:.1f}"} species per 100 square meters in {int(row.Year_MinRichness)}.'

        dfSummaries.loc[len(dfSummaries.index)] = [cpi.HabitatID, cpi.HabitatName, cpi.IndicatorID, cpi.IndicatorName, ma.id, ma.name, summary, '', 'WC-NEK-1'] 


    print(f'\nGenerateSummaries_WC_Nekton -- END')

    return dfSummaries


# 15/TN 19/TP
def GenerateSummaries_WC_Nutrients(dfSummary, dfResults, areaDict, paramDict):
    print(f'\nGenerateSummaries_WC_Nutrients() -- BEGIN')

    areaIdList =  dfSummary.AreaID.unique()
    paramIdList =  dfSummary.ParameterID.unique()

    print(f'{len(areaIdList)} Managed Areas provided: {areaIdList}')
    print(f'{len(paramIdList)} Parameters provided: {paramIdList}')
    print(f'len(dfSummary): {len(dfSummary.index)} rows')
    print(f'len(dfResults): {len(dfResults.index)} rows')

    dfSummaries = GetEmptySummaryDataframe()

    for areaId in areaIdList:
        ma = areaDict.get(areaId)

        tnParamId = 15
        tpParamId = 19

        tnSummary = dfSummary[(dfSummary.AreaID == areaId) & (dfSummary.ParameterID == tnParamId)]
        tpSummary = dfSummary[(dfSummary.AreaID == areaId) & (dfSummary.ParameterID == tpParamId)]

        # print(tnSummary)
        # print(tpSummary)

        tnResults = dfResults[(dfResults.AreaID == areaId) & (dfResults.ParameterID == tnParamId)]
        tpResults = dfResults[(dfResults.AreaID == areaId) & (dfResults.ParameterID == tpParamId)]

        if (len(tnResults.index) + len(tpResults.index)) != 2:
            print(f'**** EXCEPTION **** Something funny with TN/TP results -- MA[{str(areaId).zfill(2)}]')
            continue
        
        tn = tnResults.iloc[0]
        tp = tpResults.iloc[0]

        # print(tn)

        summary = ''

        if (~tn.SufficientData & ~tp.SufficientData):

            if (tn.N_Years == 0 & tp.N_Years == 0):
                summary = f'Data for nutrients is needed for {ma.shortName}.'
            else:
                # summary = f'Insufficient data was available to assess long-term trends for nutrients in {ma.shortName}. Ten years of data are required to assess long-term trends for nutrients. {ma.shortName} only has {int(max(tn.N_Years, tp.N_Years))} years of data as of {int(max(tn.LatestYear, tp.LatestYear))}.'
                summary = f'Insufficient data was available to assess long-term trends for nutrients in {ma.shortName}.' # per 2023_07_Logic_ManagedAreaHabitatIndicator_V2.xlsx

        else:

            latestYear = int(max(tn.LatestYear, tp.LatestYear))
            earliestYear = int(min(tn.EarliestYear, tp.EarliestYear))
            relYearsLatest = int(datetime.now().year - latestYear)
            relYearsEarliest = int(datetime.now().year - earliestYear)

            if (tn.SufficientData & tp.SufficientData):

                introTimeframe = f'Over the past {relYearsEarliest} years' if (relYearsLatest - latestYear) <= 2.0 else f'Between {earliestYear} and {latestYear}'

                if (tn.Trend == tp.Trend):
                    summary = f'{introTimeframe}, nutrient monitoring in {ma.shortName} has shown {GetTrendChangeText(tn.Trend)} in {"both" if tn.Trend != 0 else "either"} total nitrogen and total phosphorus concentrations.'
                else:
                    summary = f'{introTimeframe}, nutrient monitoring in {ma.shortName} has shown {GetTrendChangeText(tn.Trend)} in total nitrogen and {GetTrendChangeText(tp.Trend)} in total phosphorus concentrations.'
                    
            else:
                summary = f''

                suffDataParam = tn if tn.SufficientData else tp
                insuffDataParam = tn if not tn.SufficientData else tp

                # processing Sufficient Data first

                latestYear = int(suffDataParam.LatestYear)
                earliestYear = int(suffDataParam.EarliestYear)
                relYearsLatest = int(datetime.now().year - latestYear)
                relYearsEarliest = int(datetime.now().year - earliestYear)

                introTimeframe = f'Over the past {relYearsEarliest} years' if (relYearsLatest - latestYear) <= 2.0 else f'Between {earliestYear} and {latestYear}'

                summary = f'{introTimeframe}, nutrient monitoring in {ma.shortName} has shown {GetTrendChangeText(suffDataParam.Trend)} in {suffDataParam.ParameterName.lower()} concentrations.'
                
                
                # then tack on a message about the parameter with insufficient data
                summary = summary + ' '
                if (insuffDataParam.N_Years == 0):
                    summary = summary + f'Data for {insuffDataParam.ParameterName.lower()} is needed for {ma.shortName}.'
                else:
                    # summary = summary + f'Insufficient data was available to assess long-term trends for {insuffDataParam.ParameterName.lower()} in {ma.shortName}. Ten years of data are required to assess long-term trends for nutrients. {ma.shortName} only has {int(insuffDataParam.N_Years)} years of data as of {int(insuffDataParam.LatestYear)}.'
                    summary = summary + f'Insufficient data was available to assess long-term trends for {insuffDataParam.ParameterName.lower()} in {ma.shortName}.'


        dfSummaries.loc[len(dfSummaries.index)] = [tn.HabitatID, tn.HabitatName, tn.IndicatorID, tn.IndicatorName, ma.id, ma.name, summary, '', 'WC-NUT-1']

    print(f'nGenerateSummaries_WC_Nutrients() -- END')

    return dfSummaries


# 11/SEC
def GenerateSummaries_WC_WC(dfSummary, dfResults, areaDict, paramDict):
    print(f'\nGenerateSummaries_WC_WC() -- BEGIN')

    areaIdList =  dfSummary.AreaID.unique()
    paramIdList =  dfSummary.ParameterID.unique()

    print(f'{len(areaIdList)} Managed Areas provided: {areaIdList}')
    print(f'{len(paramIdList)} Parameters provided: {paramIdList}')
    print(f'len(dfSummary): {len(dfSummary.index)} rows')
    print(f'len(dfResults): {len(dfResults.index)} rows')

    dfSummaries = GetEmptySummaryDataframe()

    for areaId in areaIdList:
        ma = areaDict.get(areaId)
        for paramId in paramIdList:
            param = paramDict.get(paramId)

            dfParamSummary = dfSummary[(dfSummary.AreaID == areaId) & (dfSummary.ParameterID == paramId)]
            dfParamResults = dfResults[(dfResults.AreaID == areaId) & (dfResults.ParameterID == paramId)]

            summary = ''

            secSummary = dfParamSummary.iloc[0]
            secResults = dfParamResults.iloc[0]

            indicatorName = secResults.IndicatorName.lower()

            # print(f'MA[{str(secResults.AreaID).zfill(2)}] I[{str(secResults.IndicatorID).zfill(2)}] P[{str(secResults.ParameterID).zfill(2)}] - R[{secResults.Trend}] S[{secResults.SufficientData}]')

            if secResults.SufficientData == False:

                if secResults.N_Years == 0:
                    #summary = f'No Secchi disk data is available for {indicatorName} in {ma.shortName}.'
                    summary = f'Data for water clarity is needed for {ma.shortName}.'
                else:
                    summary = f'Insufficient Secchi disk data was available to assess long-term trends for {indicatorName} in {ma.shortName}. Ten years of data are required to assess long-term trends for {indicatorName}. {ma.shortName} only has {int(secResults.N_Years)} years of data as of {int(secResults.LatestYear)}.'
            
            else:

                trendString = 'decreased' if secResults.Trend == -1 else 'increased'
                if secResults.Trend == 0:
                    trendString = 'shown no significant change'

                summary = f'Based on Secchi disk measurements, water clarity within the estuary has {trendString} between {int(secResults.EarliestYear)} and {int(secResults.LatestYear)}.'

            dfSummaries.loc[len(dfSummaries.index)] = [secResults.HabitatID, secResults.HabitatName, secResults.IndicatorID, secResults.IndicatorName, ma.id, ma.name, summary, '', 'WC-WC-1'] 
    
    print(f'GenerateSummaries_WC_WC() -- END')

    return dfSummaries



def GetTrendValue(p:float, slope:float):
    return 0 if p > 0.05 else (-1 if slope < 0.0 else 1)


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


def GetEmptySummaryDataframe():
    dfSummaries = pd.DataFrame(columns=['HabitatID', 'HabitatName', 'IndicatorID', 'IndicatorName', 'ManagedAreaID', 'ManagedAreaName', 'IndicatorSummary', 'OptionPriority', 'OptionID'])
    dfSummaries = dfSummaries.astype({
        'HabitatID': 'Int32',
        'HabitatName': str,
        'IndicatorID': 'Int32',
        'IndicatorName': str,
        'ManagedAreaID': 'Int32',
        'ManagedAreaName': str,
        'IndicatorSummary': str,
        'OptionPriority': str,
        'OptionID': str,
    })
    return dfSummaries




if __name__ == '__main__':

    st = time.perf_counter()

    main()

    elapsed = time.perf_counter() - st
    elapsed_units = "seconds"
    if elapsed > 60:
        elapsed = elapsed/60.
        elapsed_units = "minutes"

    print(f'{__file__} executed in {elapsed:0.1f} {elapsed_units}')

