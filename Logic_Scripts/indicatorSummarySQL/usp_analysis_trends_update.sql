USE [seacar_atlas]
GO
/****** Object:  StoredProcedure [dbo].[usp_analysis_trends_update]    Script Date: 9/8/2023 10:01:53 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


ALTER PROCEDURE [dbo].[usp_analysis_trends_update]
AS
BEGIN
	SET NOCOUNT ON;

    -- EXECUTE usp_analysis_trends_update;

	EXECUTE usp_analysis_trends_acreage_update;

	EXECUTE usp_analysis_trends_cw_update;

	EXECUTE usp_analysis_trends_coral_update;

	EXECUTE usp_analysis_trends_nekton_update;

	-- OY density, percent live, and shell height (NO DATA and INSUFFICIENT DATA)
	EXECUTE usp_analysis_trends_oyster_update;
	-- OY-Density, OY-PercentLive
	EXECUTE usp_analysis_trends_python_oyster_update;
	-- OY-ShellHeight
	EXECUTE usp_analysis_trends_python_oyster_concat_update;

	-- WC-NUT, WC-WC, WC-NEK, SAV-PC, WC-WQ, SAV-WC
	--  Loads/processes output from Python analysisIndicatorSummaries.py (SEACAR_IndicatorSummaries_YYYYMMDD.xlsx)
	EXECUTE usp_analysis_trends_python_update;



END
