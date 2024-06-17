from unittest import result
import pandas as pd
import pyodbc
from sqlalchemy import create_engine
from sqlalchemy.engine import URL
from dataclasses import dataclass
import parameters
import database

pyodbc.pooling = False

@dataclass
class WQDiscreteResults:
    overview: pd.DataFrame
    raw: pd.DataFrame


def getWQDiscreteResults(parameterId, activityType):
    # Dec 2022, KB:
    # All discrete wq params are RelativeDepth = "All" except Salinity (11) which is "Surface/All"
    # Also, all params are Lab OR Field except for Salinity which is "Both/Combined/All"
    #
    # This method should only return a single record. The following shouldn't return anything:
    #   SELECT AreaID, ParameterID, RelativeDepth, ActivityType, COUNT(*)
    #   FROM Combined_WQ_WC_NUT_Analysis
    #   WHERE Website=1
    #   GROUP BY AreaID, ParameterID, RelativeDepth, ActivityType
    #   HAVING COUNT(*) > 1
    
    activityTypeWhereClause = '1 = 1'
    if activityType is parameters.FieldLab.Field:
        activityTypeWhereClause = f"ActivityType = 'Field'"
    elif activityType is parameters.FieldLab.Lab:
        # Some records still use SAMPLE instead of LAB
        activityTypeWhereClause = f"(ActivityType = 'Lab' OR ActivityType = 'Sample')"

    connection_url = URL.create("mssql+pyodbc", query={"odbc_connect": database.CONNECTION_STRING})
    engine = create_engine(connection_url)
    dfResults = None

    try:
        sql = f'''
            SELECT *
            FROM Combined_WQ_WC_NUT_Analysis
            WHERE (RelativeDepth = 'All' OR (ParameterID = 11 AND RelativeDepth = 'Surface')) 
                AND ParameterID = {parameterId} 
                AND {activityTypeWhereClause} 
                AND Website = 1
            ORDER BY AreaID
        '''
        
        dfResults = pd.read_sql(sql, engine)

        engine.dispose()

    finally:
        engine.dispose()
    
    return dfResults



def getMonthlyAveragedPlotData(areaId, parameterId, relativeDepth, activityType):
    # print(f'getMonthlyAveragedPlotData({areaId}, {parameterId}, {relativeDepth}, {activityType})')

    assert areaId > 0, f'Invalid input parameter [areaId]'
    assert parameterId > 0, f'Invalid input parameter [parameterId]'
    assert relativeDepth and isinstance(relativeDepth, parameters.RelativeDepth), f'Invalid input parameter [relativeDepth]'
    assert activityType and isinstance(activityType, parameters.FieldLab), f'Invalid input parameter [activityType]'

    relativeDepthWhereClause = '1 = 1'
    activityTypeWhereClause = '1 = 1'

    if activityType is not parameters.FieldLab.Both:
        if activityType is parameters.FieldLab.Lab:
            activityTypeWhereClause = f"ActivityType IN ('Lab', 'Sample')"
        else:
            activityTypeWhereClause = f"ActivityType IN ('Field')"
    
    if (relativeDepth is not parameters.RelativeDepth.All and relativeDepth is not parameters.RelativeDepth.Both):
        relativeDepthWhereClause = f"RelativeDepth = '{'Surface' if relativeDepth is parameters.RelativeDepth.Surface else 'Bottom'}'"

    results = None

    connection_url = URL.create("mssql+pyodbc", query={"odbc_connect": database.CONNECTION_STRING})
    engine = create_engine(connection_url)
    
    try:
        sql = f'''
            SELECT AreaID, ParameterID, RelativeDepth, ActivityType, SampleDate, ResultValue, DateAdded
            FROM [Combined_WQ_Discrete_web_monthly] 
            WHERE AreaID = {areaId} AND ParameterID = {parameterId} AND {relativeDepthWhereClause} AND {activityTypeWhereClause}
        '''

        # print(sql)

        results = pd.read_sql(sql, engine)

        engine.dispose()

    finally:
        engine.dispose()
    
    return results


def getFullPlotData(areaId, parameterId, relativeDepth, activityType):
    # print(f'getFullPlotData({areaId}, {parameterId}, {relativeDepth}, {activityType})')

    assert areaId > 0, f'Invalid input parameter [areaId]'
    assert parameterId > 0, f'Invalid input parameter [parameterId]'
    assert relativeDepth and isinstance(relativeDepth, parameters.RelativeDepth), f'Invalid input parameter [relativeDepth]'
    assert activityType and isinstance(activityType, parameters.FieldLab), f'Invalid input parameter [activityType]'

    relativeDepthWhereClause = '1 = 1'
    activityTypeWhereClause = '1 = 1'

    if activityType is not parameters.FieldLab.Both:
        if activityType is parameters.FieldLab.Lab:
            activityTypeWhereClause = f"ActivityType IN ('Lab', 'Sample')"
        else:
            activityTypeWhereClause = f"ActivityType IN ('Field')"
    
    if (relativeDepth is not parameters.RelativeDepth.All and relativeDepth is not parameters.RelativeDepth.Both):
        relativeDepthWhereClause = f"RelativeDepth = '{'Surface' if relativeDepth is parameters.RelativeDepth.Surface else 'Bottom'}'"

    results = None

    connection_url = URL.create("mssql+pyodbc", query={"odbc_connect": database.CONNECTION_STRING})
    engine = create_engine(connection_url)
    
    try:
        sql = f'''
            SELECT AreaID, ParameterID, RelativeDepth, ActivityType, SampleDate, ResultValue, DateAdded
            FROM [Combined_WQ_Discrete_web] 
            WHERE AreaID = {areaId} AND ParameterID = {parameterId} AND {relativeDepthWhereClause} AND {activityTypeWhereClause}
        '''
        results = pd.read_sql(sql, engine)

        engine.dispose()

    finally:
        engine.dispose()
    
    return results


def getAcreageData():

    results = None

    connection_url = URL.create("mssql+pyodbc", query={"odbc_connect": database.CONNECTION_STRING})
    engine = create_engine(connection_url)
    
    try:
        sql = f'''
            SELECT		analysis.LandCoverHabitat,
                        analysis.AreaID,
                        analysis.ManagedAreaName, 
                        analysis.Analysis_Include_YN, 
                        dup.LandCoverGroup, 
                        analysis.Year, 
                        SampleDate = CONVERT(datetime, '1/1/'+CONVERT(varchar,analysis.Year)),
                        [TotalHectares] = Sum(Round([ResultValue],1))
            FROM		Combined_Acreage_Analysis_Detail AS analysis 
                        INNER JOIN [TUP-WI-SQLDB].seacar.dbo.Combined_Acreage_dup AS dup 
                            ON (analysis.Year = dup.Year) 
                                AND (analysis.LandCoverHabitat = dup.LandCoverHabitat) 
                                AND (analysis.AreaID = dup.AreaID) 
                                AND (analysis.ProgramID = dup.ProgramID)
            WHERE		(UPPER(analysis.Analysis_Include_YN) <> 'N') AND analysis.AreaID NOT IN (42)
            GROUP BY	analysis.LandCoverHabitat, analysis.AreaID, analysis.ManagedAreaName, analysis.Analysis_Include_YN, dup.LandCoverGroup, analysis.Year
            ORDER BY	analysis.LandCoverHabitat, analysis.AreaID, analysis.ManagedAreaName, analysis.Analysis_Include_YN, dup.LandCoverGroup DESC, analysis.Year;
        '''
        results = pd.read_sql(sql, engine)

        engine.dispose()

    finally:
        engine.dispose()
    
    return results

def getContinuousWQData(areaId, paramId):

    results = None

    connection_url = URL.create("mssql+pyodbc", query={"odbc_connect": database.CONNECTION_STRING})
    engine = create_engine(connection_url)
    
    try:
        # sql = f'''
        #     SELECT		RowID, ParameterID, SampleDate, ProgramLocationID,
        #                 RelativeDepth = CASE
        #                     WHEN RelativeDepth='bottom' THEN 'Bottom'
		# 					WHEN RelativeDepth='surface' THEN 'Surface'
        #                     ELSE RelativeDepth
        #                 END,
        #                 ResultValue, SampleFraction, DateAdded, AreaID
        #     FROM		Combined_WQ_Cont_web
        #     WHERE		AreaID = {areaId} AND ParameterID = {paramId};
        # '''
        dailyAvgSql = f'''
            SELECT		SampleDate, AreaID, ParameterID, ProgramLocationID,
                        RelativeDepth = CASE
                            WHEN RelativeDepth='bottom' THEN 'Bottom'
                            WHEN RelativeDepth='surface' THEN 'Surface'
                            ELSE RelativeDepth
                        END,
                        ResultValue = AVG(ResultValue)
                        FROM		Combined_WQ_Cont_web
                        WHERE		AreaID = {areaId} AND ParameterID = {paramId} AND RelativeDepth IS NOT NULL
                        GROUP BY	SampleDate, AreaID, ParameterID, ProgramLocationID, RelativeDepth;
        '''
        results = pd.read_sql(dailyAvgSql, engine)

        engine.dispose()

    finally:
        engine.dispose()
    
    return results


def getContinuousWQMonthlyData(areaId, paramId):

    results = None

    connection_url = URL.create("mssql+pyodbc", query={"odbc_connect": database.CONNECTION_STRING})
    engine = create_engine(connection_url)

    # 7/19/23 - KB - Switched to [Combined_WQ_Cont_export_web_monthly] instead of [Combined_WQ_Cont_web_monthly]
    try:
        sql = f'''
            SELECT		SampleDate, AreaID, ParameterID, ProgramLocationID,
                        RelativeDepth = CASE
                            WHEN RelativeDepth='bottom' THEN 'Bottom'
                            WHEN RelativeDepth='surface' THEN 'Surface'
                            ELSE RelativeDepth
                        END,
                        ResultValue
                        FROM		Combined_WQ_Cont_export_web_monthly
                        WHERE		AreaID = {areaId} AND ParameterID = {paramId} AND RelativeDepth IS NOT NULL;
        '''
        results = pd.read_sql(sql, engine)

        engine.dispose()

    finally:
        engine.dispose()
    
    return results


def getContinuousWQTrendData(areaId, paramId):

    results = None

    connection_url = URL.create("mssql+pyodbc", query={"odbc_connect": database.CONNECTION_STRING})
    engine = create_engine(connection_url)
    try:
        sql = f'''
            SELECT		*
            FROM		Combined_WQ_WC_NUT_cont_Analysis
            WHERE		AreaID = {areaId} AND ParameterID = {paramId} AND RelativeDepth IS NOT NULL AND Website = 1;
        '''
        results = pd.read_sql(sql, engine)

        engine.dispose()

    finally:
        engine.dispose()
    
    return results


def getContinuousWQTrendData_withMonthYearMinMax(areaId, paramId):

    results = None

    connection_url = URL.create("mssql+pyodbc", query={"odbc_connect": database.CONNECTION_STRING})
    engine = create_engine(connection_url)
    try:
        sql = f'''
            ;WITH cteWQ_Cont AS (
                SELECT AreaID, ParameterID, ProgramLocationID, RelativeDepth, MonthYearMin = MIN(SampleDate), MonthYearMax = MAX(SampleDate)
                FROM Combined_WQ_Cont_export_web_monthly
                GROUP BY AreaID, ParameterID, ProgramLocationID, RelativeDepth
            )
            SELECT b.*, MonthYearMin, MonthYearMax
            FROM cteWQ_Cont a
            INNER JOIN Combined_WQ_WC_NUT_cont_Analysis b ON a.AreaID = b.AreaID AND a.ParameterID = b.ParameterID AND a.RelativeDepth = b.RelativeDepth AND a.ProgramLocationID = b.ProgramLocationID

            WHERE		a.AreaID = {areaId} AND a.ParameterID = {paramId} AND a.RelativeDepth IS NOT NULL AND b.Website = 1;
        '''
        results = pd.read_sql(sql, engine)

        engine.dispose()

    finally:
        engine.dispose()
    
    return results



def getDiscreteWCResultsSummary():
    
    results = None
    connection_url = URL.create("mssql+pyodbc", query={"odbc_connect": database.CONNECTION_STRING})
    engine = create_engine(connection_url)
    try:
        sql = f'''
            EXECUTE usp_trends_getDiscreteWCSummaries
        '''
        results = pd.read_sql(sql, engine)

        engine.dispose()

    finally:
        engine.dispose()
    
    return results

def getContinuousWCResultsSummary():
    
    results = None
    connection_url = URL.create("mssql+pyodbc", query={"odbc_connect": database.CONNECTION_STRING})
    engine = create_engine(connection_url)
    try:
        sql = f'''
            EXECUTE usp_trends_getContinuousWCSummaries
        '''
        results = pd.read_sql(sql, engine)

        engine.dispose()

    finally:
        engine.dispose()
    
    return results


def getWCAnalysesResults():
    
    results = None
    connection_url = URL.create("mssql+pyodbc", query={"odbc_connect": database.CONNECTION_STRING})
    engine = create_engine(connection_url)
    try:
        sql = f'''
            EXECUTE usp_trends_getWCAnalysesResults
        '''
        results = pd.read_sql(sql, engine)

        engine.dispose()

    finally:
        engine.dispose()
    
    return results

def getCombinedParameterIndicator():
    
    results = None
    connection_url = URL.create("mssql+pyodbc", query={"odbc_connect": database.CONNECTION_STRING})
    engine = create_engine(connection_url)
    try:
        sql = f'''
            SELECT * FROM vw_Combined_Parameter_Indicator
        '''
        results = pd.read_sql(sql, engine)

        engine.dispose()

    finally:
        engine.dispose()
    
    return results


def getNektonAnalysesResults():
    
    results = None
    connection_url = URL.create("mssql+pyodbc", query={"odbc_connect": database.CONNECTION_STRING})
    engine = create_engine(connection_url)
    try:
        sql = f'''
            SELECT * FROM Combined_NEKTON_Analysis
        '''
        results = pd.read_sql(sql, engine)

        engine.dispose()

    finally:
        engine.dispose()
    
    return results


def getOysterAnalysesResults():
    
    results = None
    connection_url = URL.create("mssql+pyodbc", query={"odbc_connect": database.CONNECTION_STRING})
    engine = create_engine(connection_url)
    try:
        sql = f'''
            SELECT * FROM Combined_OYSTER_Analysis
        '''
        results = pd.read_sql(sql, engine)

        engine.dispose()

    finally:
        engine.dispose()
    
    return results


def getTotalSAVResultsSummary():
    
    results = None
    connection_url = URL.create("mssql+pyodbc", query={"odbc_connect": database.CONNECTION_STRING})
    engine = create_engine(connection_url)
    try:
        sql = f"""

SELECT	AreaID, ParameterID,
		Results = COUNT(*),
		HasTotalSAV = (SELECT COUNT(*) FROM Combined_SAV_Analysis 
						WHERE AreaID = a.AreaID AND ParameterID = a.ParameterID AND Species = 'Total SAV'),
		TotalSAVTrend = (
			SELECT Trend = CASE 
				WHEN p IS NULL THEN NULL
				WHEN p IS NOT NULL AND p <= 0.05 THEN
					CASE
						WHEN LME_Slope < 0 THEN -1
						WHEN LME_Slope > 0 THEN 1
					END
				WHEN p IS NOT NULL AND p > 0.05 THEN 0
			END
			FROM Combined_SAV_Analysis 
			WHERE AreaID = a.AreaID AND ParameterID = a.ParameterID AND Species = 'Total SAV'
		),
		TotalSAVEarliestYear = (SELECT EarliestYear FROM Combined_SAV_Analysis 
								WHERE AreaID = a.AreaID AND ParameterID = a.ParameterID AND Species = 'Total SAV'),
		TotalSAVLatestYear = (SELECT LatestYear FROM Combined_SAV_Analysis 
								WHERE AreaID = a.AreaID AND ParameterID = a.ParameterID AND Species = 'Total SAV'),
		HasTotalSeagrass = (SELECT COUNT(*) FROM Combined_SAV_Analysis 
								WHERE AreaID = a.AreaID AND ParameterID = a.ParameterID AND Species = 'Total seagrass'),
		TotalSeagrassTrend = (
			SELECT Trend = CASE 
				WHEN p IS NULL THEN NULL
				WHEN p IS NOT NULL AND p <= 0.05 THEN
					CASE
						WHEN LME_Slope < 0 THEN -1
						WHEN LME_Slope > 0 THEN 1
					END
				WHEN p IS NOT NULL AND p > 0.05 THEN 0
			END
			FROM Combined_SAV_Analysis 
			WHERE AreaID = a.AreaID AND ParameterID = a.ParameterID AND Species = 'Total seagrass'
		),
		TotalSeagrassEarliestYear = (SELECT EarliestYear FROM Combined_SAV_Analysis 
								WHERE AreaID = a.AreaID AND ParameterID = a.ParameterID AND Species = 'Total seagrass'),
		TotalSeagrassLatestYear = (SELECT LatestYear FROM Combined_SAV_Analysis 
								WHERE AreaID = a.AreaID AND ParameterID = a.ParameterID AND Species = 'Total seagrass'),
		AverageSufficiencyNonTotal = (SELECT AVG(CONVERT(decimal, SufficientData)) FROM Combined_SAV_Analysis 
								WHERE AreaID = a.AreaID AND ParameterID = a.ParameterID AND Species NOT LIKE '%total%'),
		SppCountDecreaseNonTotal = (SELECT COUNT(*) FROM Combined_SAV_Analysis 
								WHERE AreaID = a.AreaID AND ParameterID = a.ParameterID AND SufficientData = 1 AND LME_Slope < 0 AND p <= 0.05 AND Species NOT LIKE '%total%'),
		SppCountNoChangeNonTotal = (SELECT COUNT(*) FROM Combined_SAV_Analysis 
								WHERE AreaID = a.AreaID AND ParameterID = a.ParameterID AND SufficientData = 1 AND p > 0.05 AND Species NOT LIKE '%total%'),
		SppCountIncreaseNonTotal = (SELECT COUNT(*) FROM Combined_SAV_Analysis 
								WHERE AreaID = a.AreaID AND ParameterID = a.ParameterID AND SufficientData = 1 AND LME_Slope > 0 AND p <= 0.05 AND Species NOT LIKE '%total%'),
		SppCountSufficientWithTrendNonTotal = (SELECT COUNT(*) FROM Combined_SAV_Analysis 
								WHERE AreaID = a.AreaID AND ParameterID = a.ParameterID AND SufficientData = 1 AND p IS NOT NULL AND Species NOT LIKE '%total%'),
		SppCountSufficientNonTotal = (SELECT COUNT(*) FROM Combined_SAV_Analysis 
								WHERE AreaID = a.AreaID AND ParameterID = a.ParameterID AND SufficientData = 1 AND Species NOT LIKE '%total%'),
		SppCountInsufficientNonTotal = (SELECT COUNT(*) FROM Combined_SAV_Analysis 
								WHERE AreaID = a.AreaID AND ParameterID = a.ParameterID AND SufficientData = 0 AND Species NOT LIKE '%total%'),
		SppCountTotalNonTotal = (SELECT COUNT(*) FROM Combined_SAV_Analysis 
								WHERE AreaID = a.AreaID AND ParameterID = a.ParameterID AND Species NOT LIKE '%total%')
FROM		Combined_SAV_Analysis a
GROUP BY	AreaID, a.ParameterID
        """
        results = pd.read_sql(sql, engine)

        engine.dispose()

    finally:
        engine.dispose()
    
    return results


def getTotalSAVResults():
    
    results = None
    connection_url = URL.create("mssql+pyodbc", query={"odbc_connect": database.CONNECTION_STRING})
    engine = create_engine(connection_url)
    try:
        sql = f'''
            SELECT * FROM Combined_SAV_Analysis
        '''
        results = pd.read_sql(sql, engine)

        engine.dispose()

    finally:
        engine.dispose()
    
    return results


def getManagedArea_Habitat_Indicator():
    
    results = None
    connection_url = URL.create("mssql+pyodbc", query={"odbc_connect": database.CONNECTION_STRING})
    engine = create_engine(connection_url)
    try:
        sql = f'''
            SELECT * FROM vw_ManagedArea_Habitat_Indicator
        '''
        results = pd.read_sql(sql, engine)

        engine.dispose()

    finally:
        engine.dispose()
    
    return results