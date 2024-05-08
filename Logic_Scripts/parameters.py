import pandas as pd
import pyodbc
from sqlalchemy import create_engine
from sqlalchemy.engine import URL
from dataclasses import dataclass
from enum import Enum, auto
import database

pyodbc.pooling = False

@dataclass
class Parameter:
    id: int
    name: str
    code: str
    units: str
    precision: int
    sampleFraction: str
    minValue: float
    maxValue: float


def getParameters():

    connection_url = URL.create("mssql+pyodbc", query={"odbc_connect": database.CONNECTION_STRING})
    engine = create_engine(connection_url)
    # conn = pyodbc.connect()
    parameters = []

    try:
        sql = f'SELECT * FROM Combined_Parameter'
        dfResults = pd.read_sql(sql, engine)
        engine.dispose()

        for _index, p in dfResults.iterrows():

            param = Parameter(
                p.ParameterID,
                p.ParameterName,
                p.ParameterCode,
                p.Units,
                p.Precision,
                p.SampleFraction,
                p.GraphMinValue,
                p.GraphMaxValue
            )
            parameters.append(param)

    finally:
        engine.dispose()
    
    return parameters


def getParametersAsDict():

    parameters = getParameters()
    return {p.id: p for p in parameters}


class AutoName(Enum):
    def _generate_next_value_(name, start, count, last_values):
        return name

@dataclass
class RelativeDepth(AutoName):
    Bottom = auto()
    Surface = auto()
    Both = auto()
    All = auto()
@dataclass
class SamplingFrequency(AutoName):
    Continuous = auto()
    Discrete = auto()
@dataclass
class FieldLab(AutoName):
    Field = auto()
    Lab = auto()
    Both = auto()

@dataclass
class ParameterVisualization:
    id: int
    parameterId: int
    # parameter: str
    name: str
    code: str
    typeId: int
    type: str
    creator: str
    displayName: str
    units: str
    precision: int
    sampleFraction: str
    # displayUnitsHtml: str
    # displayUnitsPython: str
    minValue: float
    maxValue: float
    data: str
    relativeDepth: RelativeDepth # Bottom|Surface|Both
    samplingFrequency: SamplingFrequency # Continuous|Discrete
    fieldLab: FieldLab  # Field|Lab|Both

def getParameterVisualizations():

    connection_url = URL.create("mssql+pyodbc", query={"odbc_connect": database.CONNECTION_STRING})
    engine = create_engine(connection_url)
    # conn = pyodbc.connect()
    visualizations = []

    try:
        sql = f'EXECUTE [usp_charts_ParameterVisualization_list]'
        dfResults = pd.read_sql(sql, engine)
        engine.dispose()

        for _index, pv in dfResults.iterrows():

            viz = ParameterVisualization(
                pv.ParameterVisId, pv.ParameterID, pv.ParameterName, pv.ParameterCode, 
                pv.VisualizationTypeID, pv.VisualizationType, pv.Creator, 
                pv.DisplayName, pv.Units,
                pv.Precision, pv.SampleFraction, 
                pv.GraphMinValue, pv.GraphMaxValue, 
                pv.Data,
                None, #RelativeDepth(pv.RelativeDepth), 
                None, #SamplingFrequency(pv.SamplingFrequency), 
                None #FieldLab(pv.FieldLab) or None
            )

            if pv.RelativeDepth is not None:
                if pv.RelativeDepth != '':
                    viz.relativeDepth = RelativeDepth(pv.RelativeDepth)

            if pv.SamplingFrequency is not None:
                if pv.SamplingFrequency != '':
                    viz.samplingFrequency = SamplingFrequency(pv.SamplingFrequency)

            if pv.FieldLab is not None:
                if pv.FieldLab != '':
                    viz.fieldLab = FieldLab(pv.FieldLab)
                
            visualizations.append(viz)

    finally:
        engine.dispose()
    
    return visualizations

def getDiscreteWaterColumnVisualizations():
    visualizations = getParameterVisualizations()

    ReleventParameterIdList = [1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 13, 15, 19, 36]

    results = []
    for pv in visualizations:
        if (pv.parameterId in ReleventParameterIdList) and ('Python' in pv.creator) and (pv.samplingFrequency is SamplingFrequency.Discrete):
            results.append(pv)
        
    return results

def getContinuousWaterColumnVisualizations():
    visualizations = getParameterVisualizations()

    ReleventParameterIdList = [1, 2, 3, 4, 5, 7]

    results = []
    for pv in visualizations:
        if (pv.parameterId in ReleventParameterIdList) and ('Python' in pv.creator) and (pv.samplingFrequency is SamplingFrequency.Continuous):
            results.append(pv)
        
    return results

def getAcreageVisualizations():
    visualizations = getParameterVisualizations()

    ReleventParameterIdList = [25, 37, 45, 48]

    results = []
    for pv in visualizations:
        if (pv.parameterId in ReleventParameterIdList) and ('Python' in pv.creator):
            results.append(pv)
        
    return results