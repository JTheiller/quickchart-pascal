{*******************************************************}
{                                                       }
{         quickchart.oi Pascal Client Library           }
{             Developer: Joathan Theiller               }
{                  Copyright(c) 2022                    }
{    https://github.com/jtheiller/quickchart-pascal     }
{                                                       }
{*******************************************************}

unit QuickChart;

interface

uses
  System.Classes, Vcl.Graphics,
  QuickChart.Chart;

type
  TQuickChart = class
  private
    Fwidth: Integer;
    Fheight: Integer;
    FdevicePixelRatio: Double;
    FbackgroundColor: String;
    Fformat: String;
    Fencoding: String;
    Fversion: String;
    Fchart: String;
    FImage: TGraphic;
    function GetImage: TGraphic;
    function GetUrl: String;
  protected
    property Image: TGraphic read GetImage;
  public
    constructor Create;
    destructor Destroy; override;
    class function New: TQuickChart;
    property width: Integer read Fwidth;
    property height: Integer read Fheight;
    property devicePixelRatio: Double read FdevicePixelRatio;
    property backgroundColor: String read FbackgroundColor;
    property format: String read Fformat;
    property encoding: String read Fencoding;
    property version: String read Fversion;
    property chartInfo: String read Fchart;
    property url: String read GetUrl;

    function SetDefault: TQuickChart;
    function SetWidth(const Value: Integer): TQuickChart;
    function SetHeight(const Value: Integer): TQuickChart;
    function SetDevicePixelRatio(const Value: Double): TQuickChart;
    function SetBackgroundColor(const Value: String): TQuickChart;
    function SetFormat(const Value: String): TQuickChart;
    function SetEncoding(const Value: String): TQuickChart;
    function SetVersion(const Value: String): TQuickChart;

    function SetChart(const Value: String): TQuickChart; overload;
    function SetChart(const ChartDTO: TQuickChartDTO): TQuickChart; overload;
    function Download(var Stream: TMemoryStream): TQuickChart; overload;
    function Download(Picture: TPicture): TQuickChart; overload;
    function Download(FileName: String): TQuickChart; overload;
  end;

implementation

uses
  System.SysUtils, System.StrUtils, Vcl.Imaging.pngimage,
  QuickChart.Request, QuickChart.Download;

{ TQuickChart }

constructor TQuickChart.Create;
begin
  Inherited Create;
  SetDefault;
end;

destructor TQuickChart.Destroy;
begin
  if Assigned(FImage) then
     FImage.Free;
  inherited Destroy;
end;

function TQuickChart.SetDefault: TQuickChart;
begin
  SetWidth(0);
  SetHeight(0);
  SetDevicePixelRatio(0);
  SetBackgroundColor(EmptyStr);
  SetFormat(EmptyStr);
  SetEncoding(EmptyStr);
  SetVersion(EmptyStr);
  SetChart(EmptyStr);
end;

class function TQuickChart.New: TQuickChart;
begin
  Result := TQuickChart.Create;
end;

function TQuickChart.GetImage: TGraphic;
begin
  if not Assigned(FImage) then
     FImage := TPngImage.Create;
  Result := FImage;
end;

function TQuickChart.GetUrl: String;
var
  LRequest: TQuickChartRequest;
begin
  LRequest := TQuickChartRequest.Create( Self );
  try
    Result := LRequest.GetUrl;
  finally
    LRequest.Free;
  end;
end;

function TQuickChart.SetWidth(const Value: Integer): TQuickChart;
begin
  Result := Self;
  Fwidth := Value;
end;

function TQuickChart.SetHeight(const Value: Integer): TQuickChart;
begin
  Result := Self;
  Fheight := Value;
end;

function TQuickChart.SetDevicePixelRatio(const Value: Double): TQuickChart;
begin
  Result := Self;
  FdevicePixelRatio := Value;
end;

function TQuickChart.SetBackgroundColor(const Value: String): TQuickChart;
begin
  Result := Self;
  FbackgroundColor := Value;
end;

function TQuickChart.SetFormat(const Value: String): TQuickChart;
begin
  Result := Self;
  Fformat := Value;
end;

function TQuickChart.SetEncoding(const Value: String): TQuickChart;
begin
  Result := Self;
  Fencoding := Value;
end;

function TQuickChart.SetVersion(const Value: String): TQuickChart;
begin
  Result := Self;
  Fversion := Value;
end;

function TQuickChart.SetChart(const Value: String): TQuickChart;
begin
  Result := Self;
  Fchart := Value;
end;

function TQuickChart.SetChart(const ChartDTO: TQuickChartDTO): TQuickChart;
begin
  Result := Self;
  SetChart(ChartDTO.AsJson)
end;

function TQuickChart.Download(var Stream: TMemoryStream): TQuickChart;
begin
  Result := Self;

  Stream := TQuickChartDownload.GetAsStream( Url );
end;

function TQuickChart.Download(Picture: TPicture): TQuickChart;
var
  LStream: TMemoryStream;
begin
  Result := Self;

  Download(LStream);
  try
    Image.LoadFromStream( LStream );
    Picture.Graphic := Image;
  finally
    LStream.Free;
  end;
end;

function TQuickChart.Download(FileName: String): TQuickChart;
var
  LStream: TMemoryStream;
begin
  Result := Self;

  Download(LStream);
  try
    LStream.SaveToFile( FileName );
  finally
    LStream.Free;
  end;
end;

end.
