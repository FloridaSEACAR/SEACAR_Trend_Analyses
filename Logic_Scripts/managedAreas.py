import pandas as pd
import pyodbc
from sqlalchemy import create_engine
from sqlalchemy.engine import URL
from dataclasses import dataclass
import database

pyodbc.pooling = False

@dataclass
class ManagedArea:
    id: int
    name: str
    shortName: int
    regionId: int
    regionName: str

def getManagedAreas():
    
    connection_url = URL.create("mssql+pyodbc", query={"odbc_connect": database.CONNECTION_STRING})
    engine = create_engine(connection_url)
    # conn = pyodbc.connect()

    managedAreas = []

    try:
        sql = f'EXECUTE usp_charts_ManagedAreas_list'
        dfResults = pd.read_sql(sql, engine)
        engine.dispose()

        for _index, ma in dfResults.iterrows():

            managedArea = ManagedArea(
                ma.ManagedAreaId, ma.LongName, ma.ShortName, ma.RegionId, ma.RegionName
            )
            managedAreas.append(managedArea)

    finally:
        engine.dispose()
    
    return managedAreas


def getManagedAreasAsDict():

    managedAreas = getManagedAreas()
    return {ma.id: ma for ma in managedAreas}