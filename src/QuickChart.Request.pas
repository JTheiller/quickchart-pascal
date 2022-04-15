unit QuickChart.Request;

interface

uses
  QuickChart;

type
  TQuickChartRequest = class
  private
    _QuickChart: TQuickChart;
    const HOST_API = 'https://quickchart.io/';
    const URI_CHART = 'chart';
  public
    constructor Create(AQuickChart: TQuickChart);
    property QuickChart: TQuickChart read _QuickChart;
    function GetUrl: String;
  end;

implementation

uses
  System.Net.URLClient, System.SysUtils;

{ TQuickChartRequest }

constructor TQuickChartRequest.Create(AQuickChart: TQuickChart);
begin
  _QuickChart := AQuickChart;
end;

function TQuickChartRequest.GetUrl: String;
var
  LURI: TURI;
begin
  LURI := TURI.Create( HOST_API + URI_CHART );

  if QuickChart.width > 0 then
     LURI.AddParameter('width',QuickChart.width.ToString);

  if QuickChart.width > 0 then
     LURI.AddParameter('height',QuickChart.height.ToString);

  if QuickChart.devicePixelRatio > 0 then
     LURI.AddParameter('devicePixelRatio',QuickChart.devicePixelRatio.ToString);

  if QuickChart.backgroundColor <> EmptyStr then
     LURI.AddParameter('backgroundColor',QuickChart.backgroundColor);

  if QuickChart.format <> EmptyStr then
     LURI.AddParameter('format',QuickChart.format);

  if QuickChart.encoding <> EmptyStr then
     LURI.AddParameter('encoding',QuickChart.encoding);

  if QuickChart.version <> EmptyStr then
     LURI.AddParameter('version',QuickChart.version);

//  if QuickChart.chartInfo <> EmptyStr then
//     AURI.AddParameter('c',QuickChart.chartInfo);

  Result := Concat(LURI.ToString,'&c=',QuickChart.chartInfo);
end;

end.
