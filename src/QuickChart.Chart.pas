{*******************************************************}
{                                                       }
{         quickchart.oi Pascal Client Library           }
{             Developer: Joathan Theiller               }
{                  Copyright(c) 2022                    }
{    https://github.com/jtheiller/quickchart-pascal     }
{                                                       }
{*******************************************************}

unit QuickChart.Chart;

interface

uses
  System.Generics.Collections,
  QuickChart.Types,
  Rest.Json;

type
  TDataDTO = class;
  TDatasetDTO = class;
  TOptions = class;
  TTitle = class;

  TQuickChartDTO = class
  private
    FType: TChartType;
    FData: TDataDTO;
    FOptions: TOptions;
  published
    property &Type: TChartType read FType write FType;
    property Data: TDataDTO read FData;
    property Options: TOptions read FOptions;
  public
    constructor Create;
    destructor Destroy; override;
    class function New: TQuickChartDTO;
    function SetType(Value: TChartType): TQuickChartDTO;
    function AsJson: String;
  end;

  TDataDTO = class
  private
    FDatasets: TArray<TDatasetDTO>;
    FLabels: TArray<String>;
    function AddDataset: TDatasetDTO;
  published
    property Datasets: TArray<TDatasetDTO> read FDatasets;
    property Labels: TArray<String> read FLabels write FLabels;
  public
    constructor Create;
    destructor Destroy; override;
    function LabelNew(Value: String): TDataDTO; overload;
    function LabelNew(Values: Array of String): TDataDTO; overload;
    function DatasetNew: TDatasetDTO;
    function DatasetAdd(DatasetDTO: TDatasetDTO): TDataDTO;
  end;

  TDatasetDTO = class
  private
    FLabel: String;
    FData: TArray<Double>;
  public
    constructor Create;
    class function New: TDatasetDTO;
    property &Label: String read FLabel write FLabel;
    property Data: TArray<Double> read FData;
    function SetLabel(Value: String): TDatasetDTO;
    function DataNew(Value: Double): TDatasetDTO; overload;
    function DataNew(Values: Array of Double): TDatasetDTO; overload;
  end;

  TOptions = class
  private
    FTitle: TTitle;
  published
    property Title: TTitle read FTitle;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TTitle = class
  private
    FDisplay: Boolean;
    FPosition: TChartPosition;
    FFontSize: Integer;
    FFontFamily: String;
    FFontColor: String;
    FFontStyle: TChartFontStyle;
    FPadding: Integer;
    FLineHeight: Double;
    FText: String;
  published
    property Display: Boolean read FDisplay;
    property Position: TChartPosition read FPosition;
    property FontSize: Integer read FFontSize;
    property FontFamily: String read FFontFamily;
    property FontColor: String read FFontColor;
    property FontStyle: TChartFontStyle read FFontStyle;
    property Padding: Integer read FPadding;
    property LineHeight: Double read FLineHeight;
    property Text: string read FText;
  public
    constructor Create;
    function SetDisplay(Value: Boolean): TTitle;
    function SetPosition(Value: TChartPosition): TTitle;
    function SetFontSize(Value: Integer): TTitle;
    function SetFontFamily(Value: String): TTitle;
    function SetFontColor(Value: String): TTitle;
    function SetFontStyle(Value: TChartFontStyle): TTitle;
    function SetPadding(Value: Integer): TTitle;
    function SetLineHeight(Value: Double): TTitle;
    function SetText(Value: String): TTitle;
  end;

implementation

{ TQuickChartDTO }

constructor TQuickChartDTO.Create;
begin
  inherited;
  FData := TDataDTO.Create;
  FOptions := TOptions.Create;
end;

destructor TQuickChartDTO.Destroy;
begin
  FOptions.Free;
  FData.Free;
  inherited;
end;

class function TQuickChartDTO.New: TQuickChartDTO;
begin
  Result := TQuickChartDTO.Create;
end;

function TQuickChartDTO.SetType(Value: TChartType): TQuickChartDTO;
begin
  Result := Self;
  FType := Value;
end;

function TQuickChartDTO.AsJson: String;
begin
  Result := TJson.ObjectToJsonString(Self,[]);
end;

{ TDataDTO }

constructor TDataDTO.Create;
begin
  //
end;

destructor TDataDTO.Destroy;
var
  I: Integer;
begin
  SetLength(FLabels,0);

  for I := High(FDatasets) downto Low(FDatasets)  do
    FDatasets[I].Free;
  SetLength(FDatasets,0);

  inherited Destroy;
end;

function TDataDTO.LabelNew(Values: array of String): TDataDTO;
var
  A: String;
begin
  Result := Self;

  for A in Values do
    LabelNew(A);
end;

function TDataDTO.DatasetAdd(DatasetDTO: TDatasetDTO): TDataDTO;
begin
  Result := Self;
  FDatasets := FDatasets + [DatasetDTO];
end;

function TDataDTO.DatasetNew: TDatasetDTO;
begin
  Result := TDatasetDTO.Create;
  DatasetAdd(Result);
end;

function TDataDTO.LabelNew(Value: String): TDataDTO;
begin
  Result := Self;
  Labels := Labels + [Value];
end;

function TDataDTO.AddDataset: TDatasetDTO;
begin
  Result := TDatasetDTO.Create;
end;

{ TDatasetDTO }

constructor TDatasetDTO.Create;
begin

end;

function TDatasetDTO.DataNew(Value: Double): TDatasetDTO;
begin
  Result := Self;
  FData := FData + [Value];
end;

function TDatasetDTO.DataNew(Values: array of Double): TDatasetDTO;
var
  A: Double;
begin
  Result := Self;

  for A in Values do
    DataNew(A);
end;

class function TDatasetDTO.New: TDatasetDTO;
begin
  Result := TDatasetDTO.Create;
end;

function TDatasetDTO.SetLabel(Value: String): TDatasetDTO;
begin
  Result := Self;
  FLabel := Value;
end;

{ TOptions }

constructor TOptions.Create;
begin
  inherited;
  FTitle := TTitle.Create;
end;

destructor TOptions.Destroy;
begin
  FTitle.Free;
  inherited;
end;

{ TTitle }

constructor TTitle.Create;
begin
  SetDisplay(False);
  SetFontSize(12);
  SetFontFamily('sans-serif');
  SetFontColor('#666666');
  SetFontStyle(bold);
  SetPadding(10);
  SetLineHeight(1.2);
end;

function TTitle.SetDisplay(Value: Boolean): TTitle;
begin
  Result := Self;
  FDisplay := Value;
end;

function TTitle.SetFontColor(Value: String): TTitle;
begin
  Result := Self;
  FFontColor := Value;
end;

function TTitle.SetFontFamily(Value: String): TTitle;
begin
  Result := Self;
  FFontFamily := Value;
end;

function TTitle.SetFontSize(Value: Integer): TTitle;
begin
  Result := Self;
  FFontSize := Value;
end;

function TTitle.SetFontStyle(Value: TChartFontStyle): TTitle;
begin
  Result := Self;
  FFontStyle := Value;
end;

function TTitle.SetLineHeight(Value: Double): TTitle;
begin
  Result := Self;
  FLineHeight := Value;
end;

function TTitle.SetPadding(Value: Integer): TTitle;
begin
  Result := Self;
  FPadding := Value;
end;

function TTitle.SetPosition(Value: TChartPosition): TTitle;
begin
  Result := Self;
  FPosition := Value;
end;

function TTitle.SetText(Value: String): TTitle;
begin
  Result := Self;
  FText := Value;
end;

end.
