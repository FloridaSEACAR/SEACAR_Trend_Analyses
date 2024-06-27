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
import re



# GLOBALS (putting them here for now)

reportDir = os.path.join(os.getcwd(), 'data\\analysisResults\\')


nonSeacarWebsiteAreaIDs = [24, 32, 36, 42, 46]


# DEBUG = True
# if DEBUG:
#     print('debug')

def main():
    """
    Generates an Excel spreadsheet version of the current ManagedArea_Habitat_Indicator table.
    """

    dfMAHI = analysis.getManagedArea_Habitat_Indicator()
    
    # print(dfMAHI.head(5))
    
    def strip_timestamp(text):
        return re.sub(r'<small\b[^>]*>.*?</small>', '', text)
    
    def strip_html(text):
        return re.sub('<[^<]+?>', '', text)
        
    def replace_with_unicode(text):
        return re.sub('&ge;', u"\u2265", text)
    
    dfMAHI.IndicatorState = dfMAHI.IndicatorState.apply(strip_timestamp)
    
    # print(dfMAHI.head(5))
    
    dfMAHI.IndicatorState = dfMAHI.IndicatorState.apply(strip_html)
    
    dfMAHI.IndicatorState = dfMAHI.IndicatorState.apply(replace_with_unicode)
    
    # print(dfMAHI.head(5))
    
    # sort
    dfMAHI = dfMAHI.sort_values(by=['Habitat', 'Indicator', 'ManagedAreaID'])
      
    
    outputFilePath = os.path.join(reportDir, f'SEACAR_ManagedAreaHabitatIndicator_{date.today().isoformat().replace("-","")}.xlsx')
    dfMAHI.to_excel(outputFilePath, sheet_name='MA_Habitat_Indicators', float_format='%.9f', index=False, na_rep='')
    
    print(f'Exported MAHI table to [{outputFilePath}]')


if __name__ == '__main__':

    st = time.perf_counter()

    main()

    elapsed = time.perf_counter() - st
    elapsed_units = "seconds"
    if elapsed > 60:
        elapsed = elapsed/60.
        elapsed_units = "minutes"

    print(f'{__file__} executed in {elapsed:0.1f} {elapsed_units}')


