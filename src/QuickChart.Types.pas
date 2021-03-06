{*******************************************************}
{                                                       }
{         quickchart.oi Pascal Client Library           }
{             Developer: Joathan Theiller               }
{                  Copyright(c) 2022                    }
{    https://github.com/jtheiller/quickchart-pascal     }
{                                                       }
{*******************************************************}

unit QuickChart.Types;

interface

type
  TChartType = (bar,
                line,
                radar,
                pie,
                doughnut,
                polar,
                scatter,
                bubble,
                radialGauge,
                boxViolin,
                sparklines,
                progressBar,
                //MixedChart,
                QRCodes,
                GraphViz,
                WordClouds);

  TChartPosition = (top,
                    left,
                    bottom,
                    right,
                    start,
                    center,
                    &end);

  TChartAxisType = (linear,
                    logarithmic,
                    category,
                    time);

  TChartFontStyle = (normal,
                    bold,
                    italic);

implementation

end.
