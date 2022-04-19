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
  TGridLines = class;
  TScales = class;
  TAxes = class;
  TXAxes = class;
  TYAxes = class;
  TTime = class;
  TDisplayFormats = class;
  TAngleLines = class;
  TPointLabels = class;
  TTicks = class;
  TScaleLabel = class;

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
    FScales: TScales;
  published
    property Title: TTitle read FTitle;
    property Layout: TLayout read FLayout;
    property Legend: TLegend read FLegend;
    property Scales: TScales read FScales;
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

  TScales = class
  private
    FXAxes: TArray<TXAxes>;
    FYAxes: TArray<TYAxes>;
  published
    property XAxes: TArray<TXAxes> read FXAxes;
    property YAxes: TArray<TYAxes> read FYAxes;
  public
    constructor Create;
    destructor Destroy; override;
    function XAxesAdd(Axes: TXAxes): TScales;
    function YAxesAdd(Axes: TYAxes): TScales;
  end;

  TAxes = class
  private
    FId: String;
    FDisplay: Boolean;
    FPosition: TChartPosition;
    FType: TChartAxisType;
    FStacked: Boolean;
    FTime: TTime;
    FDistribution: TChartAxisType;
    FGridLines: TGridLines;
    FAngleLines: TAngleLines;
    FPointLabels: TPointLabels;
    FTicks: TTicks;
    FScaleLabel: TScaleLabel;
  published
    property Id: String read FId;
    property Display: Boolean read FDisplay;
    property Position: TChartPosition read FPosition;
    property &Type: TChartAxisType read FType;
    property Stacked: Boolean read FStacked;
    property Time: TTime read FTime;
    property Distribution: TChartAxisType read FDistribution;
    property GridLines: TGridLines read FGridLines;
  public
    constructor Create;
    destructor Destroy; override;
    class function New: TAxes;
    function SetId(Value: String): TAxes;
    function SetDisplay(Value: Boolean): TAxes;
    function SetPosition(Value: TChartPosition): TAxes;
    function SetType(Value: TChartAxisType): TAxes;
    function SetStacked(Value: Boolean): TAxes;
    function SetDistribution(Value: TChartAxisType): TAxes;
  end;

  TXAxes = class(TAxes)
  private
  public
  end;

  TYAxes = class(TAxes)
  private
  public
  end;

  TTime = class
  private
    FUnit: Boolean;
    FStepSize: Integer;
    FDisplayFormats: TDisplayFormats;
  public
    constructor Create;
    destructor Destroy;
    function SetUnit(Value: Boolean): TTime;
    function SetStepSize(Value: Integer): TTime;
  end;

  TDisplayFormats = class
  private
    FMillisecond: String;
    FSecond: String;
    FMinute: String;
    FHour: String;
    FDay: String;
    FWeek: String;
    FMonth: String;
    FQuarter: String;
    FYear: String;
  public
    constructor Create;
    function SetMillisecond(Value: String): TDisplayFormats;
    function SetSecond(Value: String): TDisplayFormats;
    function SetMinute(Value: String): TDisplayFormats;
    function SetHour(Value: String): TDisplayFormats;
    function SetDay(Value: String): TDisplayFormats;
    function SetWeek(Value: String): TDisplayFormats;
    function SetMonth(Value: String): TDisplayFormats;
    function SetQuarter(Value: String): TDisplayFormats;
    function SetYear(Value: String): TDisplayFormats;
  end;

  TGridLines = class
  private
    FDisplay: Boolean;
    FColor: String;
    FLineWidth: Integer;
    FDrawBorder: Boolean;
    FDrawOnChartArea: Boolean;
    FDrawTicks: Boolean;
    FTickMarkLength: Integer;
    FZeroLineWidth: Integer;
    FZeroLineColor: String;
  public
    constructor Create;
    destructor Destroy; override;
    function SetDisplay(Value: Boolean): TGridLines;
    function SetColor(Value: String): TGridLines;
    function SetLineWidth(Value: Integer): TGridLines;
    function SetDrawBorder(Value: Boolean): TGridLines;
    function SetDrawOnChartArea(Value: Boolean): TGridLines;
    function SetDrawTicks(Value: Boolean): TGridLines;
    function SetTickMarkLength(Value: Integer): TGridLines;
    function SetZeroLineWidth(Value: Integer): TGridLines;
    function SetZeroLineColor(Value: String): TGridLines;
  end;

  TAngleLines = class
  private
    FDisplay: Boolean;
    FColor: String;
    FLineWidth: Integer;
  public
    constructor Create;
    function SetDisplay(Value: Boolean): TAngleLines;
    function SetColor(Value: String): TAngleLines;
    function SetLineWidth(Value: Integer): TAngleLines;
  end;

  TPointLabels = class
  private
    FDisplay: Boolean;
    FFontColor: String;
    FFontSize: Integer;
    FFontStyle: TChartFontStyle;
  public
    constructor Create;
    function SetDisplay(Value: Boolean): TPointLabels;
    function SetFontColor(Value: String): TPointLabels;
    function SetFontSize(Value: Integer): TPointLabels;
    function SetFontStyle(Value: TChartFontStyle): TPointLabels;
  end;

  TTicks = class
  private
    FDisplay: Boolean;
    FFontSize: Integer;
    FFontFamily: String;
    FFontColor: String;
    FFontStyle: TChartFontStyle;
    FPadding: Integer;
    FStepSize: Variant;
    FMinRotation: Integer;
    FMaxRotation: Integer;
    FMirror: Boolean;
    FReverse: Boolean;
  public
    constructor Create;
    function SetDisplay(Value: Boolean): TTicks;
    function SetFontSize(Value: Integer): TTicks;
    function SetFontFamily(Value: String): TTicks;
    function SetFontColor(Value: String): TTicks;
    function SetFontStyle(Value: TChartFontStyle): TTicks;
    function SetPadding(Value: Integer): TTicks;
    function SetStepSize(Value: Variant): TTicks;
    function SetMinRotation(Value: Integer): TTicks;
    function SetMaxRotation(Value: Integer): TTicks;
    function SetMirror(Value: Boolean): TTicks;
    function SetReverse(Value: Boolean): TTicks;
  end;

  TScaleLabel = class
  private
    FDisplay: Boolean;
    FLabelString: String;
    FLineHeight: Double;
    FFontColor: String;
    FFontFamily: String;
    FFontSize: Integer;
    FFontStyle: TChartFontStyle;
    FPadding: Integer;
  public
    constructor Create;
    function SetDisplay(Value: Boolean): TScaleLabel;
    function SetLabelString(Value: String): TScaleLabel;
    function SetLineHeight(Value: Double): TScaleLabel;
    function SetFontColor(Value: String): TScaleLabel;
    function SetFontFamily(Value: String): TScaleLabel;
    function SetFontSize(Value: Integer): TScaleLabel;
    function SetFontStyle(Value: TChartFontStyle): TScaleLabel;
    function SetPadding(Value: Integer): TScaleLabel;
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
  FScales := TScales.Create;
end;

destructor TOptions.Destroy;
begin
  FScales.Free;
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

{ TScales }

constructor TScales.Create;
begin

end;

destructor TScales.Destroy;
var
  I: Integer;
begin
  for I := High(FXAxes) downto Low(FXAxes)  do
    FXAxes[I].Free;
  SetLength(FXAxes,0);

  for I := High(FYAxes) downto Low(FYAxes)  do
    FYAxes[I].Free;
  SetLength(FYAxes,0);
  inherited;
end;

function TScales.XAxesAdd(Axes: TXAxes): TScales;
begin
  Result := Self;
  FXAxes := FXAxes + [Axes];
end;

function TScales.YAxesAdd(Axes: TYAxes): TScales;
begin
  Result := Self;
  FYAxes := FYAxes + [Axes];
end;

{ TAxes }

constructor TAxes.Create;
begin
  SetDisplay(True);
  FTime := TTime.Create;
  FGridLines := TGridLines.Create;
  FAngleLines := TAngleLines.Create;
  FPointLabels := TPointLabels.Create;
  FTicks := TTicks.Create;
  FScaleLabel := TScaleLabel.Create;
end;

destructor TAxes.Destroy;
begin
  FScaleLabel.Free;
  FTicks.Free;
  FPointLabels.Free;
  FAngleLines.Free;
  FGridLines.Free;
  FTime.Free;
  inherited;
end;

class function TAxes.New: TAxes;
begin
  Result := TAxes.Create;
end;

function TAxes.SetId(Value: String): TAxes;
begin
  Result := Self;
  FId := Value;
end;

function TAxes.SetDisplay(Value: Boolean): TAxes;
begin
  Result := Self;
  FDisplay := Value;
end;

function TAxes.SetDistribution(Value: TChartAxisType): TAxes;
begin
  Result := Self;
  FDistribution := Value;
end;

function TAxes.SetPosition(Value: TChartPosition): TAxes;
begin
  Result := Self;
  FPosition := Value;
end;

function TAxes.SetType(Value: TChartAxisType): TAxes;
begin
  Result := Self;
  FType := Value;
end;

function TAxes.SetStacked(Value: Boolean): TAxes;
begin
  Result := Self;
  FStacked := Value;
end;

{ TGridLines }

constructor TGridLines.Create;
begin
  SetDisplay(True);
  SetLineWidth(1);
  SetDrawBorder(True);
  SetDrawOnChartArea(True);
  SetDrawTicks(True);
  SetTickMarkLength(10);
  SetZeroLineWidth(1);
end;

destructor TGridLines.Destroy;
begin

  inherited;
end;

function TGridLines.SetColor(Value: String): TGridLines;
begin
  Result := Self;
  FColor := Value;
end;

function TGridLines.SetDisplay(Value: Boolean): TGridLines;
begin
  Result := Self;
  FDisplay := Value;
end;

function TGridLines.SetDrawBorder(Value: Boolean): TGridLines;
begin
  Result := Self;
  FDrawBorder := Value;
end;

function TGridLines.SetDrawOnChartArea(Value: Boolean): TGridLines;
begin
  Result := Self;
  FDrawOnChartArea := Value;
end;

function TGridLines.SetDrawTicks(Value: Boolean): TGridLines;
begin
  Result := Self;
  FDrawTicks := Value;
end;

function TGridLines.SetLineWidth(Value: Integer): TGridLines;
begin
  Result := Self;
  FLineWidth := Value;
end;

function TGridLines.SetTickMarkLength(Value: Integer): TGridLines;
begin
  Result := Self;
  FTickMarkLength := Value;
end;

function TGridLines.SetZeroLineColor(Value: String): TGridLines;
begin
  Result := Self;
  FZeroLineColor := Value;
end;

function TGridLines.SetZeroLineWidth(Value: Integer): TGridLines;
begin
  Result := Self;
  FZeroLineWidth := Value;
end;

{ TAngleLines }

constructor TAngleLines.Create;
begin
  SetDisplay(True);
  SetLineWidth(1);
end;

function TAngleLines.SetColor(Value: String): TAngleLines;
begin
  Result := Self;
  FColor := Value;
end;

function TAngleLines.SetDisplay(Value: Boolean): TAngleLines;
begin
  Result := Self;
  FDisplay := Value;
end;

function TAngleLines.SetLineWidth(Value: Integer): TAngleLines;
begin
  Result := Self;
  FLineWidth := Value;
end;

{ TPointLabels }

constructor TPointLabels.Create;
begin
  SetDisplay(True);
  SetFontColor('#666');
  SetFontSize(10);
end;

function TPointLabels.SetDisplay(Value: Boolean): TPointLabels;
begin
  Result := Self;
  FDisplay := Value;
end;

function TPointLabels.SetFontColor(Value: String): TPointLabels;
begin
  Result := Self;
  FFontColor := Value;
end;

function TPointLabels.SetFontSize(Value: Integer): TPointLabels;
begin
  Result := Self;
  FFontSize := Value;
end;

function TPointLabels.SetFontStyle(Value: TChartFontStyle): TPointLabels;
begin
  Result := Self;
  FFontStyle := Value;
end;

{ TTicks }

constructor TTicks.Create;
begin
  SetDisplay(True);
  SetFontSize(12);
  SetMaxRotation(50);
  SetFontFamily('sans-serif');
  SetFontColor('#666666');
  SetFontStyle(normal);
end;

function TTicks.SetDisplay(Value: Boolean): TTicks;
begin
  Result := Self;
  FDisplay := Value;
end;

function TTicks.SetFontColor(Value: String): TTicks;
begin
  Result := Self;
  FFontColor := Value;
end;

function TTicks.SetFontFamily(Value: String): TTicks;
begin
  Result := Self;
  FFontFamily := Value;
end;

function TTicks.SetFontSize(Value: Integer): TTicks;
begin
  Result := Self;
  FFontSize := Value;
end;

function TTicks.SetFontStyle(Value: TChartFontStyle): TTicks;
begin
  Result := Self;
  FFontStyle := Value;
end;

function TTicks.SetMaxRotation(Value: Integer): TTicks;
begin
  Result := Self;
  FMaxRotation := Value;
end;

function TTicks.SetMinRotation(Value: Integer): TTicks;
begin
  Result := Self;
  FMinRotation := Value;
end;

function TTicks.SetMirror(Value: Boolean): TTicks;
begin
  Result := Self;
  FMirror := Value;
end;

function TTicks.SetPadding(Value: Integer): TTicks;
begin
  Result := Self;
  FPadding := Value;
end;

function TTicks.SetReverse(Value: Boolean): TTicks;
begin
  Result := Self;
  FReverse := Value;
end;

function TTicks.SetStepSize(Value: Variant): TTicks;
begin
  Result := Self;
  FStepSize := Value;
end;

{ TScaleLabel }

constructor TScaleLabel.Create;
begin
  SetDisplay(False);
  SetLabelString('Axis label');
  SetLineHeight(1.2);
  SetFontColor('#666666');
  SetFontFamily('sans-serif');
  SetFontSize(12);
  SetFontStyle(Normal);
  SetPadding(4);
end;

function TScaleLabel.SetDisplay(Value: Boolean): TScaleLabel;
begin
  Result := Self;
  FDisplay := Value;
end;

function TScaleLabel.SetFontColor(Value: String): TScaleLabel;
begin
  Result := Self;
  FFontColor := Value;
end;

function TScaleLabel.SetFontFamily(Value: String): TScaleLabel;
begin
  Result := Self;
  FFontFamily := Value;
end;

function TScaleLabel.SetFontSize(Value: Integer): TScaleLabel;
begin
  Result := Self;
  FFontSize := Value;
end;

function TScaleLabel.SetFontStyle(Value: TChartFontStyle): TScaleLabel;
begin
  Result := Self;
  FFontStyle := Value;
end;

function TScaleLabel.SetLabelString(Value: String): TScaleLabel;
begin
  Result := Self;
  FLabelString := Value;
end;

function TScaleLabel.SetLineHeight(Value: Double): TScaleLabel;
begin
  Result := Self;
  FLineHeight := Value;
end;

function TScaleLabel.SetPadding(Value: Integer): TScaleLabel;
begin
  Result := Self;
  FPadding := Value;
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

{ TDisplayFormats }

constructor TDisplayFormats.Create;
begin
  SetMillisecond('h:mm:ss.SSS a');
  SetSecond('h:mm:ss a');
  SetMinute('h:mm a');
  SetHour('hA');
  SetDay('MMM D');
  SetWeek('ll');
  SetMonth('MMM YYYY');
  SetQuarter('[Q]Q - YYYY');
  SetYear('YYYY');
end;

function TDisplayFormats.SetDay(Value: String): TDisplayFormats;
begin
  Result := Self;
  FDay := Value;
end;

function TDisplayFormats.SetHour(Value: String): TDisplayFormats;
begin
  Result := Self;
  FHour := Value;
end;

function TDisplayFormats.SetMillisecond(Value: String): TDisplayFormats;
begin
  Result := Self;
  FMillisecond := Value;
end;

function TDisplayFormats.SetMinute(Value: String): TDisplayFormats;
begin
  Result := Self;
  FMinute := Value;
end;

function TDisplayFormats.SetMonth(Value: String): TDisplayFormats;
begin
  Result := Self;
  FMonth := Value;
end;

function TDisplayFormats.SetQuarter(Value: String): TDisplayFormats;
begin
  Result := Self;
  FQuarter:= Value;
end;

function TDisplayFormats.SetSecond(Value: String): TDisplayFormats;
begin
  Result := Self;
  FSecond := Value;
end;

function TDisplayFormats.SetWeek(Value: String): TDisplayFormats;
begin
  Result := Self;
  FWeek := Value;
end;

function TDisplayFormats.SetYear(Value: String): TDisplayFormats;
begin
  Result := Self;
  FYear := Value;
end;

{ TTime }

constructor TTime.Create;
begin
  SetStepSize(1);
  FDisplayFormats := TDisplayFormats.Create;
end;

destructor TTime.Destroy;
begin
  FDisplayFormats.Free;
end;

function TTime.SetStepSize(Value: Integer): TTime;
begin
  Result := Self;
  FStepSize := Value;
end;

function TTime.SetUnit(Value: Boolean): TTime;
begin
  Result := Self;
  FUnit := Value;
end;

end.
