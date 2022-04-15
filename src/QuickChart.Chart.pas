unit QuickChart.Chart;

interface

uses
  System.Generics.Collections,
  QuickChart.Types,
  Rest.Json;

type
  TDataDTO = class;
  TDatasetDTO = class;

  TQuickChartDTO = class
  private
    FData: TDataDTO;
    FType: TChartType;
  published
    property Data: TDataDTO read FData;
    property &Type: TChartType read FType write FType;
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
    FLabels: TArray<string>;
    function AddDataset: TDatasetDTO;
  published
    property Datasets: TArray<TDatasetDTO> read FDatasets;
    property Labels: TArray<string> read FLabels write FLabels;
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
    FLabel: string;
    FData: TArray<Double>;
  public
    constructor Create;
    class function New: TDatasetDTO;
    property &Label: string read FLabel write FLabel;
    property Data: TArray<Double> read FData;
    function SetLabel(Value: String): TDatasetDTO;
    function DataNew(Value: Double): TDatasetDTO; overload;
    function DataNew(Values: Array of Double): TDatasetDTO; overload;
  end;

implementation

{ TQuickChartDTO }

constructor TQuickChartDTO.Create;
begin
  inherited;
  FData := TDataDTO.Create;
end;

destructor TQuickChartDTO.Destroy;
begin
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

end.
