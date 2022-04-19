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
  TLayout = class;
  TPadding = class;
  TLegend = class;
  TLabels = class;

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
    FLayout: TLayout;
    FLegend: TLegend;
  published
    property Title: TTitle read FTitle;
    property Layout: TLayout read FLayout;
    property Legend: TLegend read FLegend;
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

  TLayout = class
  private
    FPadding: TPadding;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TPadding = class
  private
    FLeft: Integer;
    FRight: Integer;
    FTop: Integer;
    FBottom: Integer;
  public
    function SetLeft(Value: Integer): TPadding;
    function SetRight(Value: Integer): TPadding;
    function SetTop(Value: Integer): TPadding;
    function SetBottom(Value: Integer): TPadding;
  end;

  TLegend = class
  private
    FDisplay: Boolean;
    FPosition: TChartPosition;
    FAlign: TChartPosition;
    FFullWidth: Boolean;
    FReverse: Boolean;
    FLabels: TLabels;
  public
    constructor Create;
    destructor Destroy; override;
    function SetDisplay(Value: Boolean): TLegend;
    function SetPosition(Value: TChartPosition): TLegend;
    function SetAlign(Value: TChartPosition): TLegend;
    function SetFullWidth(Value: Boolean): TLegend;
    function SetReverse(Value: Boolean): TLegend;
  end;

  TLabels = class
  private
    FBoxWidth: Integer;
    FFontSize: Integer;
    FFontFamily: String;
    FFontColor: String;
    FFontStyle: TChartFontStyle;
    FPadding: Integer;
    FUsePointStyle: Boolean;
  public
    constructor Create;
    function SetBoxWidth(Value: Integer): TLabels;
    function SetFontSize(Value: Integer): TLabels;
    function SetFontFamily(Value: String): TLabels;
    function SetFontColor(Value: String): TLabels;
    function SetFontStyle(Value: TChartFontStyle): TLabels;
    function SetPadding(Value: Integer): TLabels;
    function SetUsePointStyle(Value: Boolean): TLabels;
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
  FLayout := TLayout.Create;
  FLegend := TLegend.Create;
end;

destructor TOptions.Destroy;
begin
  FLegend.Free;
  FLayout.Free;
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

{ TLayout }

constructor TLayout.Create;
begin
  FPadding := TPadding.Create;
end;

destructor TLayout.Destroy;
begin
  FPadding.Free;
  inherited;
end;

{ TLegend }

constructor TLegend.Create;
begin
  SetDisplay(True);
  SetPosition(Top);
  SetAlign(center);
  SetFullWidth(True);
  FLabels := TLabels.Create;
end;

destructor TLegend.Destroy;
begin
  FLabels.Free;
  inherited;
end;

function TLegend.SetAlign(Value: TChartPosition): TLegend;
begin
  Result := Self;
  FAlign := Value;
end;

function TLegend.SetDisplay(Value: Boolean): TLegend;
begin
  Result := Self;
  FDisplay := Value;
end;

function TLegend.SetFullWidth(Value: Boolean): TLegend;
begin
  Result := Self;
  FFullWidth := Value;
end;

function TLegend.SetPosition(Value: TChartPosition): TLegend;
begin
  Result := Self;
  FPosition := Value;
end;

function TLegend.SetReverse(Value: Boolean): TLegend;
begin
  Result := Self;
  FReverse := Value;
end;

{ TPadding }

function TPadding.SetBottom(Value: Integer): TPadding;
begin
  Result := Self;
  FBottom := Value;
end;

function TPadding.SetLeft(Value: Integer): TPadding;
begin
  Result := Self;
  FLeft := Value;
end;

function TPadding.SetRight(Value: Integer): TPadding;
begin
  Result := Self;
  FRight := Value;
end;

function TPadding.SetTop(Value: Integer): TPadding;
begin
  Result := Self;
  FTop := Value;
end;

{ TLabels }

constructor TLabels.Create;
begin
  SetBoxWidth(40);
  SetFontSize(12);
  SetFontFamily('sans-serif');
  SetFontColor('#666666');
  SetFontStyle(normal);
  SetPadding(10);
  SetUsePointStyle(False);
end;

function TLabels.SetBoxWidth(Value: Integer): TLabels;
begin
  Result := Self;
  FBoxWidth := Value;
end;

function TLabels.SetFontColor(Value: String): TLabels;
begin
  Result := Self;
  FFontColor := Value;
end;

function TLabels.SetFontFamily(Value: String): TLabels;
begin
  Result := Self;
  FFontFamily := Value;
end;

function TLabels.SetFontSize(Value: Integer): TLabels;
begin
  Result := Self;
  FFontSize := Value;
end;

function TLabels.SetFontStyle(Value: TChartFontStyle): TLabels;
begin
  Result := Self;
  FFontStyle := Value;
end;

function TLabels.SetPadding(Value: Integer): TLabels;
begin
  Result := Self;
  FPadding := Value;
end;

function TLabels.SetUsePointStyle(Value: Boolean): TLabels;
begin
  Result := Self;
  FUsePointStyle := Value;
end;

