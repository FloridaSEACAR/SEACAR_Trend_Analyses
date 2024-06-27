from ast import AsyncFunctionDef
from ctypes import alignment
from math import nan
import os, sys, time
import subprocess
from pydoc import describe
from datetime import date, datetime, timedelta
from regex import F
import slugify
import numpy as np
import pandas as pd
import re
# from dataclasses import Field, dataclass
import seaborn as sns
import matplotlib.pyplot as plt
import matplotlib.lines as lines
from matplotlib.dates import DateFormatter
from matplotlib.patches import Patch
from matplotlib.lines import Line2D
from matplotlib import dates, rcParams, patches
from matplotlib.legend_handler import HandlerLine2D, HandlerTuple
import parameters
import managedAreas
import analysis

from PIL import Image


# full list of rcParams @ https://matplotlib.org/stable/api/matplotlib_configuration_api.html#matplotlib.rcParams
# rcParams that are ignored in style sheets @ https://matplotlib.org/stable/api/style_api.html#matplotlib.style.use
plt.style.use("seacar_atlas.mplstyle")


# GLOBALS (putting them here for now)
imgDirectory = ( "U:\Misc_Projects\SEACAR_FDEP\SEACAR_Atlas\Charts\WaterQualityStaticCharts" )
pngquant = "C:\\apps\\pngquant\\pngquant.exe"
blueColor = "#314963"  # '#314963'
seacarColors = [
    "#005396",
    "#0088B1",
    "#00ADAE",
    "#65CCB3",
    "#AEE4C1",
    "#FDEBA8",
    "#F8CD6D",
    "#F5A800",
    "#F17B00",
    "#FFFFFF",
    "#CCCCCC",
    "#444444",
]

imgFmt = "png"
endDate = datetime.today()

ACREAGE_INCLUDE_C_CAP = "C-CAP"
ACREAGE_INCLUDE_Y = "Y"
LABEL_LAB_ONLY = "Lab only"

OPTIMIZE_PNG = False # don't optimize b/c we need to rename/resize them later and we'll optimize AFTER that

DEBUG = True
if DEBUG:
    # from PIL import Image
    imgDirectory = "c:\\temp\\seacar-atlas-charts"


def main():
    """
    This is the main function of the application. It orchestrates the creation of various SEACAR Atlas charts.
    """
    
    print(f"App Started.")

    createDiscreteWaterQualityCharts()
    createContinuousWaterQualityCharts()
    # createAcreageCharts()
          

    # return

    print(f"App Finished.")


# region ACREAGE_CHARTS


def createAcreageCharts():
    """
    Create acreage charts for different land cover habitats and managed areas.

    This function generates acreage charts based on land cover habitat and managed area data.
    It processes a list of visualizations (defined in DB), filters the data, and creates charts for each
    combination of land cover habitat and managed area.
    """
    
    areaDict = managedAreas.getManagedAreasAsDict()
    paramDict = parameters.getParametersAsDict()
    visualizations = parameters.getAcreageVisualizations()

    paramIdToLandCoverHabitatDict = {
        25: "SAV",
        37: "OY",
        45: "CR",
        48: "CW",
    }

    print(f"\n== ACREAGE CHARTS ==")
    print(f"Number of Visualizations to Process: {len(visualizations)}")

    dfAllData = analysis.getAcreageData()
    print(f"All Data Total Rows: {len(dfAllData.index)}")

    # TESTING
    # # ParameterVisualization Filter
    # pv10 = visualizations.pop(0)
    # visualizations.clear()
    # visualizations.append(pv10)
    #
    # # Managed Area Filter
    # dfAllData = dfAllData[dfAllData.AreaID == 6]
    #
    # # Parameter Filter
    # visualizations = filter(lambda x: x.parameterId == 25, visualizations)

    for pv in visualizations:

        # for this PV, does the "Data" column mention C-CAP?
        Analysis_Include_YN_Filter = ACREAGE_INCLUDE_Y
        createTwoCharts = False
        if ACREAGE_INCLUDE_C_CAP in pv.data:
            createTwoCharts = True  # Need to create two charts: Y and CCAP
            Analysis_Include_YN_Filter = ACREAGE_INCLUDE_C_CAP

        landCoverHabitat = paramIdToLandCoverHabitatDict[pv.parameterId]

        dfData = dfAllData[dfAllData.LandCoverHabitat == landCoverHabitat].copy()

        if len(dfData.index) == 0:
            print(f"\nNo Acreage Data for {landCoverHabitat}")
            continue

        print(f"\nAcreage data for [{landCoverHabitat}]: {len(dfData.index)} records")

        # dfData = dfData[dfData.Analysis_Include_YN == Analysis_Include_YN_Filter]
        # print(f'Acreage data for [{landCoverHabitat} + {Analysis_Include_YN_Filter}]: {len(dfData.index)} records')

        # if (len(dfData.index) == 0):
        #     print(f'No Acreage Data for [{landCoverHabitat} + {Analysis_Include_YN_Filter}]')
        #     continue

        areaIdList = dfData.AreaID.unique()
        for areaId in areaIdList:

            dfChartData = dfData[dfData.AreaID == areaId].copy()
            # print(dfChartData.head(10))

            # TESTING
            createStackedAcreageChart(
                areaDict.get(areaId),
                paramDict.get(pv.parameterId),
                pv,
                landCoverHabitat,
                "BOTH",
                dfChartData,
            )



def createStackedAcreageChart(
    area, param, pv, landCoverHabitat, analysis_Include_YN_Filter, data
):
    """
    Create stacked acreage charts based on input data.

    This function generates a stacked acreage chart for a specific managed area, parameter, parameter visualization (pv),
    land cover habitat, analysis include YN filter, and input data. It processes the data, creates the chart, and
    saves it as an image file.

    Args:
        area (Area): The managed area for which the chart is created.
        param (Parameter): The parameter associated with the parameter visualization.
        pv (ParameterVisualization): The parameter visualization configuration.
        landCoverHabitat (str): The land cover habitat for which the chart is created.
        analysis_Include_YN_Filter (str): The analysis include YN filter value.
        data (DataFrame): The input data for chart generation.

    Returns:
        None
    """
    
    print(
        f"Creating Stacked Acreage Chart for A[{area.id}] P[{param.id}] PV[{pv.id}] H[{landCoverHabitat}] AIYNF[{analysis_Include_YN_Filter}] with {len(data.index)} rows"
    )

    data.SampleDate = pd.to_datetime(data.SampleDate)

    show_legend = True
    hatchStyle = ""
    acreageChartEdgeColor = (0.2, 0.2, 0.2)
    acreageChartLineWidth = 0.5
    legendTitle = "Program"
    landCoverGroups = data.LandCoverGroup.unique()

    PVsWithMultipleLandCoverGroups = [10]
    chartTitlePrefix = ""
    title_prefix_dict = {
        10: "Coastal Wetland ",
        14: "Coral ",
        15: "Oyster ",
        19: "SAV ",
    }
    chartTitlePrefix = title_prefix_dict[pv.id]

    if pv.id in PVsWithMultipleLandCoverGroups:
        hatchStyle = "\\\\"
        show_legend = True
        acreageChartEdgeColor = (0, 0, 0, 0.4)
        legendTitle = "Type"
    else:
        hatchStyle = "\\\\"
        acreageChartEdgeColor = (0, 0, 0, 0.4)
    # elif len(landCoverGroups) > 1:
    #     show_legend = True

    chartTitleSuffix = ""
    sns.set_palette(seacarColors)
    # ax = sns.scatterplot(data=data, x="SampleDate", y="ResultValue", hue="Program Location", style="Program Location", markers=markers, style_order=programLocations, **scatter_kwargs)

    ax = None

    dfY = data[data.Analysis_Include_YN == ACREAGE_INCLUDE_Y]
    dfCCAP = data[data.Analysis_Include_YN == ACREAGE_INCLUDE_C_CAP]

    # print(dfY)
    # print(dfCCAP)

    pd.options.mode.chained_assignment = None
    if pv.id in PVsWithMultipleLandCoverGroups:
        dfCCAP.LandCoverGroup = dfCCAP.LandCoverGroup.apply(lambda x: f"{x} (C-CAP)")
    else:
        dfY.LandCoverGroup = dfY.LandCoverGroup.apply(lambda x: f"Other Programs")
        dfCCAP.LandCoverGroup = dfCCAP.LandCoverGroup.apply(lambda x: f"C-CAP")
    pd.options.mode.chained_assignment = "raise"

    pivotY = pd.pivot_table(
        data=dfY, index=["Year"], columns=["LandCoverGroup"], values=["TotalHectares"]
    )
    pivotCCAP = pd.pivot_table(
        data=dfCCAP,
        index=["Year"],
        columns=["LandCoverGroup"],
        values=["TotalHectares"],
    )

    pivotY.columns = pivotY.columns.get_level_values(level=1)
    pivotCCAP.columns = pivotCCAP.columns.get_level_values(level=1)

    # print(pivotY)
    # print(pivotCCAP)

    yearsY = pivotY.index.array
    yearsCCAP = pivotCCAP.index.array
    allyears = np.unique(np.concatenate((yearsY, yearsCCAP)))

    # print(f'yearsY={len(yearsY)} yearsCCAP={len(yearsCCAP)} allyears={len(allyears)}')

    for y in allyears:
        if y not in pivotY.index and len(pivotY.index) > 0:
            if param.id == 48:
                pivotY.loc[y] = {"Invasives": nan, "Mangroves": nan, "Marsh": nan}
            else:
                pivotY.loc[y] = {"Other Programs": nan}
        if y not in pivotCCAP.index and len(pivotCCAP.index) > 0:
            if param.id == 48:
                pivotCCAP.loc[y] = {
                    "Invasives (C-CAP)": nan,
                    "Mangroves (C-CAP)": nan,
                    "Marsh (C-CAP)": nan,
                }
            else:
                pivotCCAP.loc[y] = {"C-CAP": nan}

    pivotY = pivotY.sort_index(ascending=True)
    pivotCCAP = pivotCCAP.sort_index(ascending=True)

    # pivotY.columns = [col.replace('TotalHectares', '') for col in pivotY.columns]
    # pivotCCAP.columns = [col.replace('TotalHectares', '') for col in pivotCCAP.columns]

    # print(pivotCCAP)

    fig, ax = plt.subplots()

    plt.setp(ax.patches, linewidth=0.3)

    # maybe, if we need to add space between grouped
    # https://stackoverflow.com/questions/66913686/how-to-get-spacing-between-grouped-bar-plot-in-python

    # gotta be a better/easier way...
    # marsh,mang,inv: 0, 1, 2 (orig)
    # other,ccap: 0, 1 (orig)
    colorDict = {
        "Marsh": seacarColors[1],
        "Mangroves": seacarColors[2],
        "Invasives": seacarColors[0],
        "Marsh (C-CAP)": seacarColors[1],
        "Mangroves (C-CAP)": seacarColors[2],
        "Invasives (C-CAP)": seacarColors[0],
        "SAV": seacarColors[0],
        "SAV (C-CAP)": seacarColors[1],
        "OY": seacarColors[0],
        "OY (C-CAP)": seacarColors[1],
        "CR": seacarColors[0],
        "CR (C-CAP)": seacarColors[1],
        "Other Programs": seacarColors[1],
        "C-CAP Program": seacarColors[2],
        "C-CAP": seacarColors[2],
    }

    if len(pivotY.index) > 0:
        pivotY.plot(
            kind="bar",
            stacked=True,
            width=0.3,
            ax=ax,
            position=1,
            linewidth=acreageChartLineWidth,
            edgecolor=acreageChartEdgeColor,
            color=colorDict,
        )
    if len(pivotCCAP.index) > 0:
        pivotCCAP.plot(
            kind="bar",
            stacked=True,
            width=0.3,
            ax=ax,
            position=0,
            linewidth=acreageChartLineWidth,
            edgecolor=acreageChartEdgeColor,
            color=colorDict,
            hatch=hatchStyle,
        )

    # plt.setp(ax.patches, linewidth=0.4)

    ax.tick_params(axis="y")
    plt.setp(
        ax.get_xticklabels(),
        rotation=-45,
        ha="left",
        rotation_mode="anchor",
        color=(0.2, 0.2, 0.2),
    )
    plt.setp(ax.spines.values(), color=(0.2, 0.2, 0.2), linewidth=0.5)

    # def change_width(ax, new_value) :
    #     for patch in ax.patches :
    #         current_width = patch.get_width()
    #         diff = current_width - new_value

    #         # we change the bar width
    #         patch.set_width(new_value)

    #         # we recenter the bar
    #         patch.set_x(patch.get_x() + diff * .5)

    # change_width(ax, .25)

    if show_legend:
        # plt.legend(bbox_to_anchor=(1.02, 0.5), loc='center left', title="Habitat", frameon=False)
        plt.legend(
            bbox_to_anchor=(1.02, 0.5), loc="center left", ncol=1, title=legendTitle, alignment="left"
        )
    else:
        plt.legend([], [], frameon=False)

    superTitle = f"{chartTitlePrefix}{pv.displayName}{chartTitleSuffix}"
    superTitle = superTitle.replace("Acreage", "Areal Extent").replace(
        "by habitat", "by Type"
    )
    plt.suptitle(superTitle, color=blueColor)
    # plt.title(f'{area.name}', color=blueColor, fontsize=10)
    plt.title(f"{area.name}", color=blueColor)

    plt.xlabel("Year")
    plt.ylabel(f"{getSentenceCase(param.name)}{getUnitsLabel(param.units)}")

    imgFilename = f"{imgDirectory}/ma-{area.id}-pv-{pv.id}.png"

    if True:
        plt.savefig(imgFilename, bbox_inches="tight")
        plt.close("all")
        
        # if OPTIMIZE_PNG:
        #     subprocess.run([pngquant, "--force", "--strip",  "--speed=3", "--quality=65-80", "--ext=.png", imgFilename], stdout=subprocess.DEVNULL, timeout=5)

    else:
        plt.show()

    plt.close("all")


# endregion


# region WQ_CONTINUOUS


def createContinuousWaterQualityCharts():
    areaDict = managedAreas.getManagedAreasAsDict()
    paramDict = parameters.getParametersAsDict()

    visualizations = parameters.getContinuousWaterColumnVisualizations()
    # print(visualizations)
    
    # FOR TESTING PURPOSES - Running a single Param or MA
    #####################################################
    # # ParameterVisualization Filter
    # pv = next((x for x in visualizations if x.parameterId == 7), None)
    # visualizations.clear()
    # visualizations.append(pv)
    # print(visualizations)
    #
    # Managed Area Filter
    # ma_id = 30
    # areaDict = {ma_id: areaDict.get(ma_id)}
    #
    # # Parameter Filter
    # visualizations = filter(lambda x: x.parameterId == 7, visualizations)
    
    print(f"\n== CONTINUOUS WQ CHARTS ==")
    print(f"Number of Visualizations to Process: {len(visualizations)}")

    for pv in visualizations:
        for areaId in areaDict:
            # df = analysis.getContinuousWQData(areaId, pv.parameterId)
            dfPlotData = analysis.getContinuousWQMonthlyData(areaId, pv.parameterId)
            
            # dfTrendData = analysis.getContinuousWQTrendData(areaId, pv.parameterId)
            dfTrendData = analysis.getContinuousWQTrendData_withMonthYearMinMax(areaId, pv.parameterId)
            
            
            # Save data if/when needed for external use
            # dfPlotData.to_csv("c:\\temp\\\seacar-atlas-charts\\plot-data.csv")
            # dfTrendData.to_csv("c:\\temp\\\seacar-atlas-charts\\trend-data.csv")
            
            skipping_message = " - SKIPPING" if len(dfPlotData.index) == 0 else ""
            print(
                f"MA-{areaId} PV-{pv.id} ({pv.parameterId} | {pv.name}) : {len(dfPlotData.index)} records, {len(dfTrendData.index)} program trend data records {skipping_message}"
            )

            if len(dfPlotData.index) == 0:
                continue
            
            createCWQChart(
                areaDict.get(areaId),
                paramDict.get(pv.parameterId),
                pv,
                dfPlotData,
                dfTrendData,
            )


def createCWQChart(area, param, pv, plotData, trendData):
    depth = pv.relativeDepth
    units = param.units
    # if units == 'Degrees C':
    #     units = '$^\circ$C'

    plotData["SampleDateNum"] = dates.datestr2num(plotData["SampleDate"])
    plotData.SampleDate = pd.to_datetime(plotData.SampleDate)

    marker_size_scatter = 30  # 25
    marker_size_legend = 5
    marker_line_width = 0.5  # 0.8
    marker_legend_scale = 3.0 #2.8
    marker_edge_color = "#333333"
    marker_line_alpha = 1.0
    trend_line_width = 2.0
    trend_line_color = "#000099" # "#000000"
    trend_line_alpha = 0.8

    trendData["RelativeDepth"] = trendData["RelativeDepth"].apply(
        lambda x: "Bottom" if x.upper() == "BOTTOM" else "Surface"
    )

    plotData["Program Location"] = (
        plotData.ProgramLocationID + " - " + plotData.RelativeDepth
    )
    trendData["Program Location"] = (
        trendData.ProgramLocationID + " - " + trendData.RelativeDepth
    )
    trendData = trendData[
        trendData["Program Location"].isin(plotData["Program Location"].unique())
    ]
    programLocations = sorted(plotData["Program Location"].unique())

    if len(programLocations) > 10:
        print(
            f"** Trimming the Number of ProgramLocations ({len(programLocations)}) to 10 **"
        )
        top10 = (
            plotData.groupby("Program Location")
            .size()
            .reset_index(name="SampleCount")
            .sort_values(by=["SampleCount"], ascending=False)
            .head(10)
        )
        plotData = plotData[
            plotData["Program Location"].isin(top10["Program Location"])
        ]
        trendData = trendData[
            trendData["Program Location"].isin(top10["Program Location"])
        ]
        programLocations = sorted(plotData["Program Location"].unique())

    rowCount = len(plotData.index)

    scatter_kwargs = {
        "s": marker_size_scatter,  # 40, #150 if rowCount > 500 else 40,
        "linewidth": marker_line_width,  # .4
        "edgecolor": marker_edge_color,
        "alpha": marker_line_alpha,
    }

    plotData = plotData.sort_values(by=["Program Location", "SampleDate"])
    # ax = sns.scatterplot(data=data, x="SampleDate", y="ResultValue", hue="RelativeDepth", style="ProgramLocationID", **scatter_kwargs)
    markers = ["o", "s", "^", "D", "P", "X", "*", "v", "<", ">", "p", "h"]
    # palette = ['#00374f', '#007c99', '#00c9db', '#006481', '#00aec6', '#00ffff', '#00374f', '#007c99', '#00c9db', '#006481', '#00aec6', '#00ffff']
    
    # sns.set_palette(seacarColors)
    greyColors = [
        "#CCCCCC",
        "#CCCCCC",
        "#CCCCCC",
        "#CCCCCC",
        "#CCCCCC",
        "#CCCCCC",
        "#CCCCCC",
        "#CCCCCC",
        "#CCCCCC",
        "#CCCCCC",
        "#CCCCCC",
        "#CCCCCC",
    ]
    sns.set_palette(greyColors)
    
    ax = sns.scatterplot(
        data=plotData,
        x="SampleDate",
        y="ResultValue",
        hue="Program Location",
        style="Program Location",
        markers=markers[:len(programLocations)],
        style_order=programLocations,
        **scatter_kwargs,
    )
    
    # show lines
    # plt.plot(plotData["SampleDate"], plotData["ResultValue"], color='black', linewidth=0.5, alpha=0.5, zorder=0)
    
    scatter_handles, scatter_labels = ax.get_legend_handles_labels()
    # print(f'\n{len(scatter_handles)} ScatterPlot Handles: {scatter_handles}')
    # print(f'\n{len(scatter_labels)} ScatterPlot Labels: {scatter_labels}\n')
    
    # handles1, labels1 = ax.get_legend_handles_labels()
    # print('\n1:')
    # print(handles1, labels1)

    # TREND LINES
    trendData = trendData[trendData.SufficientData == True]
    # original dash configs from: https://matplotlib.org/stable/gallery/lines_bars_and_markers/linestyles.html
    seacar_dashes = [
        (1, 0),  # SOLID
        (1, 2), # DOTTED
        (3, 3), # DASHED
        (3, 5), # LOOSELY DASHED
        (1, 3), # LOOSELY DOTTED
        (3, 2, 1, 2), # DASHDOTTED
        (3, 3, 1, 3, 1, 3), # DASHDOTDOTTED
        (0, 5, 3), # LONG DASH WITH OFFSET
        (3, 4, 1, 4), # LOOSELY DASHDOTTED
        (3, 5, 1, 5, 1, 5), # LOOSELY DASHDOTDOTTED
        (1, 1), # DENSELY DOTTED
        (3, 1), # DENSELY DASHED
        (3, 1, 1, 1), # DENSELY DASHDOTTED
        (3, 1, 1, 1, 1, 1), # DENSELY DASHDOTDOTTED
    ]
    
    dfTrendLines = pd.DataFrame(columns=["SampleDate", "ResultValue", "Program Location"])
    for i, row in trendData.iterrows():
        # if not np.isnan(row.SennSlope) and not np.isnan(row.p) and row.p <= 0.05:
        if not np.isnan(row.SennSlope) and not pd.isnull(row.p) and row.p <= 0.05:
            trendLineData = getSenContTrendLineData(
                # row, f"{row['Program Location']} Trend"
                row, f"{row['Program Location']}"
            )
            
            # dfTrendLines = dfTrendLines.append(trendLineData, ignore_index=True)
            dfTrendLines = pd.concat([dfTrendLines, trendLineData], axis=0, ignore_index=True)
    
    dfTrendLines = dfTrendLines.sort_values(by=["Program Location", "SampleDate"])
    
    programTrendLocations = dfTrendLines["Program Location"].unique()
    programTrendCount = len(programTrendLocations)
    if programTrendCount > 0:
        ax2 = sns.lineplot(
                    data=dfTrendLines,
                    x="SampleDate",
                    y="ResultValue",
                    style="Program Location",
                    linewidth=trend_line_width,
                    color=trend_line_color,
                    alpha=trend_line_alpha,
                    zorder=1,
                    dashes=seacar_dashes[:programTrendCount],
                    # legend=True
                )
    
    ax.tick_params(axis="y")
    plt.setp(
        ax.get_xticklabels(),
        rotation=-45,
        ha="left",
        rotation_mode="anchor",
        color=(0.2, 0.2, 0.2),
    )
    plt.setp(ax.spines.values(), color=(0.2, 0.2, 0.2), linewidth=0.5)


    
    trend_handles, trend_labels = ax.get_legend_handles_labels()
    
    for h in scatter_handles:
        if isinstance(h, Line2D):
            h.set_color(trend_line_color)
        else:
            h.set_edgecolor(marker_edge_color)
            h.set_linewidth(marker_line_width)
            h.set_alpha(marker_line_alpha)
    
    for h in trend_handles:
        if isinstance(h, Line2D):
            h.set_color(trend_line_color)
        else:
            h.set_edgecolor(marker_edge_color)
            h.set_linewidth(marker_line_width)
            h.set_alpha(marker_line_alpha)
    
    handleTuples = []
    labelTuples = []
    
    empty_handle = patches.Rectangle((0,0), 1, 1, fill=False, edgecolor='none', visible=False)
    handler_base_kwargs = {"xpad":-0.2, "ypad":-0.2}
    
    
    
    # print(f'\n{len(handles)} Handles: {handles}')
    # print(f'\n{len(labels)} Labels: {labels}')
    # print(f'\n{len(programLocations)} Program Locations: {programLocations}')
    # if programTrendCount > 0:
    #     print(f'{programTrendCount} Program Location Trends: {programTrendLocations}\n')

    if programTrendCount > 0:
        trend_labels = trend_labels[len(scatter_labels):]
        trend_handles = trend_handles[len(scatter_labels):]
        
    for i in range(len(scatter_labels)):
        label = scatter_labels[i]
        handle = (scatter_handles[i], empty_handle)
        if label in trend_labels:
            handle = (scatter_handles[i], trend_handles[trend_labels.index(label)])
        labelTuples.append(label)
        handleTuples.append(handle)
    
    leg = plt.legend(
        handles=handleTuples,
        labels=labelTuples,
        handler_map={tuple: HandlerTuple(ndivide=1, pad=0.1, **handler_base_kwargs)},
        bbox_to_anchor=(1.02, 0.5),
        loc="center left",
        ncol=1,
        title="Program location",
        alignment="left",
        markerscale=marker_legend_scale,
    )
    # leg._legend_box.align = "left"

    # if show_legend:
    #     leg = plt.legend(bbox_to_anchor=(0.5, -0.1), loc='upper center', ncol=len(activityTypes), handles=handles, labels=labels, frameon=False)

    suptitle = f"{param.name} - Continuous"
    plt.suptitle(f"{suptitle}", color=blueColor)
    plt.title(area.name, color=blueColor)

    plt.xlabel("Year")
    plt.ylabel(f"{getSentenceCase(param.name)}{getUnitsLabel(units)}")

    imgFilename = f"{imgDirectory}/ma-{area.id}-pv-{pv.id}.png"
    # plt.savefig(imgFilename, bbox_extra_artists=(leg), bbox_inches='tight')
    plt.savefig(imgFilename, bbox_inches="tight")
    
    # if OPTIMIZE_PNG:
    #     subprocess.run([pngquant, "--force", "--strip",  "--speed=3", "--quality=65-80", "--ext=.png", imgFilename], stdout=subprocess.DEVNULL, timeout=5)

    # plt.show()

    # img = Image.open(imgFilename)
    # img.show()

    plt.close("all")


# endregion


def getSenContTrendLineData(data, seriesName):
    """
    Generate trendline data for continuous WQ data series.

    This function calculates a trendline for a continuous WQ data series based on the provided data and
    series name. It computes the beginning and ending values of the trendline based on the earliest and
    latest years in the input data.

    Returns:
        DataFrame: A DataFrame containing the trendline data with columns: "SampleDate", "ResultValue",
                    and "Program Location".
    """
    
    # ax = sns.scatterplot(data=plotData, x="SampleDate", y="ResultValue", hue="Program Location", style="Program Location", markers=markers, style_order=programLocations, **scatter_kwargs)
    
    begYear = date(data.EarliestYear, 1, 1)
    endYear = date(data.LatestYear, 1, 1)
    relYears = endYear.year - begYear.year
    
    if 'MonthYearMin' in data.index:
        begYear = pd.to_datetime(data.MonthYearMin)
        endYear = pd.to_datetime(data.MonthYearMax)
        
    senSlope = data.SennSlope
    senIntercept = data.SennIntercept


    begVal = (senSlope * 0) + senIntercept
    endVal = (senSlope * relYears) + senIntercept

    trendData = [[begYear, begVal, seriesName], [endYear, endVal, seriesName]]

    dfTrendLineData = pd.DataFrame(
        trendData, columns=["SampleDate", "ResultValue", "Program Location"]
    )
    dfTrendLineData.SampleDate = pd.to_datetime(dfTrendLineData.SampleDate)
    dfTrendLineData.ResultValue = dfTrendLineData.ResultValue.astype("float64")
    dfTrendLineData["Program Location"] = dfTrendLineData["Program Location"].astype(
        "string"
    )

    return dfTrendLineData


# region WQ_DISCRETE


def createDiscreteWaterQualityCharts():
    """
    Creates discrete water quality charts for all managed areas and parameters.
    """
    
    areaDict = managedAreas.getManagedAreasAsDict()
    paramDict = parameters.getParametersAsDict()

    # allParameterVisualizations = parameters.getParameterVisualizations()
    visualizations = parameters.getDiscreteWaterColumnVisualizations()

    print(f"\n== DISCRETE WQ CHARTS ==")
    print(f"Number of Visualizations to Process: {len(visualizations)}")

    # TESTING
    # # ParameterVisualization Filter
    # print(visualizations)
    # pv = next((x for x in visualizations if x.parameterId == 2), None)
    # visualizations.clear()
    # visualizations.append(pv)

    for pv in visualizations:

        # TESTING
        # if pv.parameterId != 13:
        #     continue

        df = analysis.getWQDiscreteResults(pv.parameterId, pv.fieldLab)

        print(
            f"\nWorking on PV[{pv.id}] P[{pv.parameterId}] Code[{pv.code}] RD[{pv.relativeDepth}] SF[{pv.samplingFrequency}] FL[{pv.fieldLab}]: {len(df.index)} records"
        )

        areaIdList = df.AreaID.unique()
        for areaId in areaIdList:

            # TESTING
            # if areaId != 43:
            #     continue

            dfArea = df[df.AreaID == areaId].copy()

            # Dec 2022: there's really only a single row now (All or Lab Only or Field Only)
            if len(dfArea.index) == 0:
                print(f"@@ NO DATA @@")
                continue
            elif len(dfArea.index) > 1:
                print(f"\n@@@@ TOO MUCH DATA - this is REALLY unexpected @@@@\n")
                continue

            # there's only one row, get it
            row = dfArea.iloc[0].copy()
            dfRow = dfArea.iloc[[0]].copy()

            data = None
            slopeData = None

            if row.SufficientData == True:
                data = analysis.getMonthlyAveragedPlotData(
                    row.AreaID, row.ParameterID, pv.relativeDepth, pv.fieldLab
                )
            else:
                data = analysis.getFullPlotData(
                    row.AreaID, row.ParameterID, pv.relativeDepth, pv.fieldLab
                )

            print(
                f"MA[{row.AreaID}] PV[{pv.id}] P[{row.ParameterID}] Code[{pv.code}] RD[{row.RelativeDepth}] AT[{pv.fieldLab.name}] SD[{row.SufficientData}] p-value[{row.p}]: {len(data.index)} records"
            )

            # KB 20220909: fixed clause to only draw trend line when significant
            if not np.isnan(row.SennSlope) and not np.isnan(row.p) and row.p <= 0.05:
                slopeData = dfRow

            if len(data.index) == 0:
                # skip for now
                print(f"** NO DATA TO PLOT **")
                continue

            # alter data.ActivityType to make sure we're using "Lab" instead of "Sample"
            data.ActivityType = data.ActivityType.apply(
                lambda x: x.replace("Lab", LABEL_LAB_ONLY)
            )
            data.ActivityType = data.ActivityType.apply(
                lambda x: x.replace("Sample", LABEL_LAB_ONLY)
            )
            dfRow.ActivityType = dfRow.ActivityType.apply(
                lambda x: x.replace("Lab", LABEL_LAB_ONLY)
            )
            if row.ActivityType == "Lab":
                row.ActivityType = LABEL_LAB_ONLY

            area = areaDict.get(row.AreaID)
            param = paramDict.get(pv.parameterId)

            createDWQChart(area, param, pv, data, slopeData)

    return


def createDWQChart(area, param, pv, data, slopeData):
    """Generates and saves a single Discrete Water Quality chart for the provided data

    Args:
        area (Area): The managed area for which the chart is created.
        param (Parameter): The parameter being charted.
        pv (ParameterVisualization): The parameter visualization configuration.
        data (DataFrame): The input data containing sample date values.
        slopeData (DataFrame): The slope and intercept data used to calculate the trendline.
    """

    units = param.units

    data["SampleDateNum"] = dates.datestr2num(data["SampleDate"])
    data.SampleDate = pd.to_datetime(data.SampleDate)

    activityTypes = data.ActivityType.unique()

    marker_size_scatter = 25
    marker_size_legend = 5
    marker_line_width = 0.8
    marker_face_color = "#cccccc"
    marker_edge_color = "#333333"
    marker_line_alpha = 1.0
    trend_line_width = 2.0
    trend_line_color = "#000099"
    trend_line_alpha = 0.8
    trend_line_dashes_solid = (1, 0)
    trend_line_dashes_dashed = (3, 1)
    ticklabel_color = (0.2, 0.2, 0.2)

    scatter_kwargs = {
        "s": marker_size_scatter,
        "linewidth": marker_line_width,
        "edgecolor": marker_edge_color,
        "alpha": marker_line_alpha,
    }

    show_legend = True
    # trendLines = []
    # trendline_dashes_dict = {
    #     'Field': trend_line_dashes_solid
    # }

    if len(activityTypes) > 1:
        plotColors = {
            "Field": marker_face_color,
            "Sample": marker_face_color,
            "Lab": marker_face_color,
            LABEL_LAB_ONLY: marker_face_color,
        }
        plotMarkers = {"Field": "o", "Sample": "^", "Lab": "^", LABEL_LAB_ONLY: "^"}
        plotOrder = ["Field", "Sample", "Lab", LABEL_LAB_ONLY]
        # trendline_dashes_dict['Lab'] = trend_line_dashes_dashed
        # trendline_dashes_dict[LABEL_LAB_ONLY] = trend_line_dashes_dashed
    else:
        plotColors = {
            "Field": marker_face_color,
            "Sample": marker_face_color,
            "Lab": marker_face_color,
            LABEL_LAB_ONLY: marker_face_color,
        }
        plotMarkers = "o" if activityTypes[0] == "Field" else "^"
        plotOrder = ["Field"] if activityTypes[0] == "Field" else [LABEL_LAB_ONLY]
        # trendline_dashes_dict['Lab'] = trend_line_dashes_solid
        # trendline_dashes_dict[LABEL_LAB_ONLY] = trend_line_dashes_solid

    ax = sns.scatterplot(
        data=data,
        x="SampleDate",
        y="ResultValue",
        hue="ActivityType",
        style="ActivityType",
        hue_order=plotOrder,
        palette=plotColors,
        markers=plotMarkers,
        legend="full" if show_legend else False,
        **scatter_kwargs,
    )

    ax.tick_params(axis="y")
    plt.setp(
        ax.get_xticklabels(),
        rotation=-45,
        ha="left",
        rotation_mode="anchor",
        color=ticklabel_color,
    )
    plt.setp(ax.spines.values(), color=ticklabel_color, linewidth=0.5)

    show_sen_trend_line = False
    if isinstance(slopeData, pd.DataFrame):
        show_sen_trend_line = True

        dfTrendLineData = getSenTrendLineData(
            data, slopeData, slopeData.iloc[0].ActivityType
        )
        sns.lineplot(
            data=dfTrendLineData,
            x="SampleDate",
            y="ResultValue",
            style="ActivityType",
            linewidth=trend_line_width,
            color=trend_line_color,
            alpha=trend_line_alpha,
            zorder=1,
        )

        # for atype in activityTypes:
        #     if len(slopeData[slopeData.ActivityType == atype].index) > 0:
        #         dfTrendLineData = getSenTrendLineData(data, slopeData, atype)
        #         trendLines.append(atype)

        #         sns.lineplot(data=dfTrendLineData, x='SampleDate', y='ResultValue', style='ActivityType', linewidth=trend_line_width, dashes=trendline_dashes_dict, color=trend_line_color, alpha=trend_line_alpha, zorder=1)
        #     else:
        #         # TODO: handle this case
        #         print(f'@@ NOT ADDING TREND LINE FOR A[{area.id}] PV[{pv.id}] atype[{atype}] slopData.ActivityType[{slopeData.head(1).ActivityType}]')

    # needed in order to remove the legend item added for STYLE parameter
    handles, labels = ax.get_legend_handles_labels()
    handles = handles[1:]
    labels = labels[1:]

    # leg = plt.legend(bbox_to_anchor=(1.02, 0.5), loc='center left', ncol=len(activityTypes))

    if show_legend:

        legend_elements = []

        if "Field" in activityTypes:
            legend_elements.append(
                Line2D(
                    [0],
                    [0],
                    marker="o",
                    color="w",
                    label="Field",
                    linewidth=marker_line_width,
                    markerfacecolor=marker_face_color,
                    markersize=marker_size_legend,
                )
            )

        if "Lab" in activityTypes or LABEL_LAB_ONLY in activityTypes:
            legend_elements.append(
                Line2D(
                    [0],
                    [0],
                    marker="^",
                    color="w",
                    label=LABEL_LAB_ONLY,
                    linewidth=marker_line_width,
                    markerfacecolor=marker_face_color,
                    markersize=marker_size_legend,
                )
            )

        if show_sen_trend_line:
            legend_elements.append(
                Line2D(
                    [0],
                    [0],
                    color=trend_line_color,
                    linewidth=trend_line_width,
                    label="Trend",
                    alpha=trend_line_alpha,
                )
            )
        # if 'Field' in trendLines:
        #     legend_elements.append(Line2D([0], [0], color=trend_line_color, linewidth=trend_line_width, dashes=trendline_dashes_dict['Field'], label='Field trend', alpha=trend_line_alpha))

        # if 'Lab' in activityTypes or LABEL_LAB_ONLY in trendLines:
        #     legend_elements.append(Line2D([0], [0], color=trend_line_color, linewidth=trend_line_width, dashes=trendline_dashes_dict['Lab'], label=f'{LABEL_LAB_ONLY} trend', alpha=trend_line_alpha))

        for le in legend_elements:
            le.set_markeredgewidth(marker_line_width)
            le.set_markeredgecolor(marker_edge_color)
            le.set_alpha(trend_line_alpha)

        # leg = plt.legend(bbox_to_anchor=(1.02, 0.5), loc='center left', ncol=1, handles=legend_elements, title="$\\bf{Data\ Type}$", frameon=False)
        leg = plt.legend(
            bbox_to_anchor=(1.02, 0.5),
            loc="center left",
            ncol=1,
            handles=legend_elements,
            title="Data type",
            alignment="left"
        )

    # suptitle = f'{param.name}{getUnitsLabel(units)} - Discrete, Surface'
    titleRelativeDepthAppend = " - Discrete, All Depths"
    if param.id == 11:
        titleRelativeDepthAppend = ""
    suptitle = f"{getTitleCase(param.name)}{titleRelativeDepthAppend}"
    plt.suptitle(f"{suptitle}", color=blueColor)
    plt.title(f"{area.name}", color=blueColor)

    plt.xlabel("Year")
    plt.ylabel(f"{getSentenceCase(param.name)}{getUnitsLabel(units)}")

    imgFilename = f"{imgDirectory}/ma-{area.id}-pv-{pv.id}.png"
    # plt.savefig(imgFilename, bbox_extra_artists=(leg), bbox_inches='tight')
    plt.savefig(imgFilename, bbox_inches="tight")

    # if OPTIMIZE_PNG:
    #     subprocess.run([pngquant, "--force", "--strip",  "--speed=3", "--quality=65-80", "--ext=.png", imgFilename], stdout=subprocess.DEVNULL, timeout=5)

    plt.close("all")


def getSenTrendLineData(data, slopeData, activityType):
    """
    Generate trendline data for a given set of data points and slope information.

    This function calculates a trendline based on the provided data points, slope, and intercept values,
    and returns the trendline data as a DataFrame. It computes the beginning and ending values of the trendline
    based on the minimum and maximum sample dates in the input data.

    Args:
        data (DataFrame): The input data containing sample date values.
        slopeData (DataFrame): The slope and intercept data used to calculate the trendline.
        activityType (str): The type of activity associated with the trendline data.

    Returns:
        DataFrame: A DataFrame containing the trendline data with columns: "SampleDate", "ResultValue", and "ActivityType".
    """

    begYear = data.SampleDate.min()
    endYear = data.SampleDate.max()
    senSlope = slopeData.SennSlope
    senIntercept = slopeData.SennIntercept

    relYears = endYear.year - begYear.year

    begVal = (senSlope * 0) + senIntercept
    endVal = (senSlope * relYears) + senIntercept

    trendData = [[begYear, begVal, activityType], [endYear, endVal, activityType]]

    dfTrendLineData = pd.DataFrame(
        trendData, columns=["SampleDate", "ResultValue", "ActivityType"]
    )
    dfTrendLineData.SampleDate = pd.to_datetime(dfTrendLineData.SampleDate)
    dfTrendLineData.ResultValue = dfTrendLineData.ResultValue.astype("float64")
    dfTrendLineData.ActivityType = dfTrendLineData.ActivityType.astype("string")

    return dfTrendLineData


# endregion


def getUnitsLabel(units):
    """
    Returns a string for use within a chart title/subtitle/axis; handles which Units shouldn't be printed
    Contains a leading space with the original unit string (unless not to be shown) wrapped in parentheses (as specified)
    """
    if units is None:
        return ""

    # u = f'{units}'
    u = units
    unitReplacementDict = {"None": "", "NULL": "", "Degrees C": "$^\circ$C"}
    if u in unitReplacementDict:
        u = unitReplacementDict[u]
        if u == "":
            return ""

    return f" ({u})"

WORD_REPLACE_ALL_CAPS = {
        "(n)":"(N)", 
        "nh3":"NH3", 
        "nh4":"NH4", 
        "no2":"NO2", 
        "po4":"PO4", 
        "cdom":"CDOM", 
        "fdom":"FDOM", 
        "tkn":"TKN", 
        "tss":"TSS", 
        "braun":"Braun", 
        "blanquet":"Blanquet", 
        "kjeldahl": "Kjeldahl"
    }

def getTitleCase(sentence):
    """
    Convert a sentence to title case while preserving specific word replacements.

    This function takes a sentence as input and converts it to title case, ensuring that specific words
    specified in the WORD_REPLACE_ALL_CAPS dictionary are replaced with their appropriate title case
    versions. It also handles special cases like converting "Ph" to "pH" and adjusting capitalization for
    certain words.

    Args:
        sentence (str): The input sentence to be converted to title case.

    Returns:
        str: The input sentence in title case.
    """
    
    new = sentence
    new = new.title() 
    
    for word, replacement in WORD_REPLACE_ALL_CAPS.items():
        # new = new.replace(word, replacement)
        compiled = re.compile(re.escape(word), re.IGNORECASE)
        new = compiled.sub(replacement, new)
    
    if new == "Ph":
        new = "pH"
    
    new = new.replace(" A ", " a ").replace(" A, ", " a, ").replace(" For ", " for ")
        
    separators = ["- ", ]
    
    for separator in separators:
        if new.find(separator) > 0:
            parts = new.split(separator)
            new_parts = []
            for part in parts:
                new_parts.append(getTitleCase(part))
            new = separator.join(new_parts)
    
    return new

def getSentenceCase(sentence):
    """
    Convert a sentence to "sentence case" while preserving specific word replacements.

    This function takes a sentence as input and converts it to sentence case, ensuring that specific words
    specified in the WORD_REPLACE_ALL_CAPS dictionary are replaced with their appropriate sentence case
    versions. It also handles special cases like converting "Ph" to "pH" and adjusting capitalization for
    certain words.

    Args:
        sentence (str): The input sentence to be converted to sentence case.

    Returns:
        str: The input sentence in sentence case.
    """
    
    new = sentence
    new = new.capitalize()
    
    for word, replacement in WORD_REPLACE_ALL_CAPS.items():
        # new = new.replace(word, replacement)
        compiled = re.compile(re.escape(word), re.IGNORECASE)
        new = compiled.sub(replacement, new)
    
    if new == "Ph":
        new = "pH"
    
    separators = ["- ", ]
    
    for separator in separators:
        if new.find(separator) > 0:
            parts = new.split(separator)
            new_parts = []
            for part in parts:
                new_parts.append(getSentenceCase(part))
            new = separator.join(new_parts)
    
    return new


if __name__ == "__main__":
    """
    Entry point for the script's execution.

    This block of code serves as the entry point for the script's execution when it is run as a standalone
    program. It measures the execution time of the `main` function and prints the elapsed time upon completion.
    """

    st = time.perf_counter()

    main()

    elapsed = time.perf_counter() - st
    elapsed_units = "seconds"
    if elapsed > 60:
        elapsed = elapsed / 60.0
        elapsed_units = "minutes"

    print(f"{__file__} executed in {elapsed:0.1f} {elapsed_units}")
