USE [seacar_atlas]
GO
/****** Object:  StoredProcedure [dbo].[usp_analysis_trends_python_oyster_concat_update]    Script Date: 9/8/2023 10:04:21 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO




ALTER PROCEDURE [dbo].[usp_analysis_trends_python_oyster_concat_update]
AS
BEGIN
	SET NOCOUNT ON;

	-- EXEC [usp_analysis_trends_python_oyster_concat_update]


	DECLARE @htmlTimestamp varchar(300) = CONCAT('<small class="float-right mr-3" style="color:#ccd">', FORMAT(GETDATE(), 'M.d.yy'), '</small>');
	DECLARE @addTimestamp bit = 0;
	DECLARE @ts varchar(300) = CASE @addTimestamp WHEN 1 THEN @htmlTimestamp END


	IF OBJECT_ID('tempdb..#SizeClassResults') IS NOT NULL
	DROP TABLE #SizeClassResults;

	;WITH ctePivotData AS (
		SELECT		a.AreaID,
					L75				= MIN(CASE ShellType WHEN 'Live Oyster Shells' THEN ShellType END), 
					L75_size		= MIN(CASE ShellType WHEN 'Live Oyster Shells' THEN SizeClass END), 
					L75_estimate	= MIN(CASE ShellType WHEN 'Live Oyster Shells' THEN ModelEstimate END), 
					L75_begYear		= MIN(CASE ShellType WHEN 'Live Oyster Shells' THEN EarliestLiveDate END), 
					L75_endYear		= MIN(CASE ShellType WHEN 'Live Oyster Shells' THEN LatestLiveDate END),
					L75_suff		= MIN(CASE ShellType WHEN 'Live Oyster Shells' THEN SufficientData + 0 END),
					L75_sig			= MIN(CASE ShellType WHEN 'Live Oyster Shells' THEN Significant + 0 END),

					D75				= MIN(CASE ShellType WHEN 'Dead Oyster Shells' THEN ShellType END), 
					D75_size		= MIN(CASE ShellType WHEN 'Dead Oyster Shells' THEN SizeClass END), 
					D75_estimate	= MIN(CASE ShellType WHEN 'Dead Oyster Shells' THEN ModelEstimate END),  
					D75_begYear		= MIN(CASE ShellType WHEN 'Dead Oyster Shells' THEN EarliestLiveDate END),
					D75_endYear		= MIN(CASE ShellType WHEN 'Dead Oyster Shells' THEN LatestLiveDate END),
					D75_suff		= MIN(CASE ShellType WHEN 'Dead Oyster Shells' THEN SufficientData + 0 END),
					D75_sig			= MIN(CASE ShellType WHEN 'Dead Oyster Shells' THEN Significant + 0 END)
		FROM		Combined_OYSTER_Analysis a
		WHERE		a.ParameterID IN (28)
		AND			a.SizeClass = '>75mm'
		AND			a.HabitatType = 'Natural'
		GROUP BY	a.AreaID
	)
	SELECT	a.*, b.ShortName
	INTO	#SizeClassResults 
	FROM	ctePivotData a
	INNER JOIN ManagedArea b ON a.AreaID = b.ManagedAreaID;
	--SELECT * FROM #SizeClassResults;


	-- TESTING
	--UPDATE #SizeClassResults SET L75_suff = 1, L75_sig=1 WHERE AreaID = 3;
	--UPDATE #SizeClassResults SET L75_suff = 1, L75_sig=1, L75_estimate=L75_estimate*-1.0, D75_sig=1 WHERE AreaID = 19;
	--UPDATE #SizeClassResults SET L75_sig = 1 WHERE AreaID = 20;
	--UPDATE #SizeClassResults SET L75_suff = 1, L75_sig=1, L75_estimate=1.05 WHERE AreaID = 22;
	--UPDATE #SizeClassResults SET L75_suff = 1, L75_sig=1, L75_estimate=1.234 WHERE AreaID = 34;


	;WITH cteResults AS (
		SELECT	
			liveSummary = 
				CASE 
					WHEN a.L75 IS NOT NULL AND L75_suff = 1
					THEN
						CONCAT(
								'Between ', a.L75_begYear, ' and ', a.L75_endYear, ', the size of large oysters (&ge;75mm size class) ',
								CASE 
									WHEN a.L75_sig = 0
									THEN 
										'showed no significant change.'
									WHEN a.L75_sig = 1
									THEN
										CASE
										WHEN a.L75_estimate < 0
										THEN
											CONCAT('decreased at an average rate of ', FORMAT(a.L75_estimate, 'N2'), 'mm per year.')
										ELSE
											CONCAT('increased at an average rate of ', FORMAT(a.L75_estimate, 'N2'), 'mm per year.')
										END
								END
						)
					WHEN (a.L75 IS NOT NULL AND L75_suff = 0) OR (a.L75 IS NULL AND a.D75 IS NOT NULL)
					THEN 
						'Insufficient data were available to estimate a size trend for large live oysters (&ge;75mm size class).'
				END,
			deadSummary = 
				CASE 
					WHEN (a.L75 IS NOT NULL AND a.L75_suff = 1) and (a.D75 IS NOT NULL AND a.D75_suff = 1)
					THEN 
						CONCAT (
								'Additional historical evidence of large oyster sizes from measurements of dead oyster shells ', 
								CASE 
									WHEN	(a.L75_sig = 0 AND a.D75_sig = 0) 
											OR 
											(
												(a.L75_sig = 1 AND a.D75_sig = 1)
												AND
												(
													(L75_estimate < 0 AND D75_estimate < 0)
													OR
													(L75_estimate > 0 AND D75_estimate > 0)
												)
											)
									THEN 
										'supports'
									ELSE
										'complicates'
								END,
								' this finding.'	
						)
				END,
				a.*
		FROM	#SizeClassResults a
	)

	--SELECT		AreaID, ShortName, mahi.HabitatID, mahi.IndicatorID, mahi.HasData,
	--			L75, L75_size, L75_estimate, L75_begYear, L75_endYear, L75_suff, L75_sig, 
	--			D75, D75_size, D75_estimate, D75_begYear, D75_endYear, D75_suff, D75_sig, 
	--			Trend = CASE 
	--				WHEN (UPPER(r.liveSummary) LIKE '%INSUFFICIENT%') THEN NULL
	--				ELSE -99
	--			END,
	--			IndicatorState = CASE 
	--				WHEN liveSummary NOT LIKE '%Insufficient%' THEN CONCAT(liveSummary, ' ', deadSummary)
	--				ELSE liveSummary 
	--			END
	--FROM		ManagedArea_Habitat_Indicator	mahi
	--INNER JOIN	cteResults						r ON r.AreaID = mahi.ManagedAreaID
	--WHERE		mahi.HabitatID = 4
	--			AND mahi.IndicatorID = 16;

	UPDATE		mahi
	SET			Trend = CASE 
					WHEN (UPPER(r.liveSummary) LIKE '%INSUFFICIENT%') THEN NULL
					ELSE -99
				END,
				IndicatorState = CONCAT(@ts, '<p>', 
									CASE 
										WHEN liveSummary NOT LIKE '%Insufficient%' THEN CONCAT(liveSummary, ' ', deadSummary)
										ELSE liveSummary 
									END, 
									'</p>')
	FROM		ManagedArea_Habitat_Indicator		mahi
	INNER JOIN	cteResults						r ON r.AreaID = mahi.ManagedAreaID
	WHERE		mahi.HabitatID = 4
				AND mahi.IndicatorID = 16;


	--SELECT * FROM vw_ManagedArea_Habitat_Indicator WHERE IndicatorID = 16;
	-- SELECT * FROM vw_Combined_Parameter_Indicator WHERE ParameterID IN (28);

END
