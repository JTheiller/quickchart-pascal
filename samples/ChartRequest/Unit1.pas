unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Buttons,
  Vcl.Samples.Spin;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    btnDownload: TBitBtn;
    Panel2: TPanel;
    Image1: TImage;
    Panel3: TPanel;
    Label1: TLabel;
    Panel4: TPanel;
    Label2: TLabel;
    Panel5: TPanel;
    SpinEditWidth: TSpinEdit;
    SpinEditHeigth: TSpinEdit;
    Label3: TLabel;
    Label4: TLabel;
    SpinEditDevicePixelRatio: TSpinEdit;
    Label5: TLabel;
    EditBrackGroundColorHex: TEdit;
    Label6: TLabel;
    procedure btnDownloadClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  QuickChart,
  QuickChart.Chart, QuickChart.Types;

{$R *.dfm}

procedure TForm1.btnDownloadClick(Sender: TObject);
var
  LChartDTO: TQuickChartDTO;
begin
  LChartDTO := TQuickChartDTO.New;

  LChartDTO
    .Data
      .LabelNew(['January','February','March','April','May'])
      .DatasetAdd( TDatasetDTO
                     .New
                       .SetLabel('Dogs')
                       .DataNew( [50, 60, 70, 180, 190] )
                     )
      .DatasetAdd( TDatasetDTO
                     .New
                       .SetLabel('Cats')
                       .DataNew( [100, 200, 300, 400, 500] )
                     );

  //Load object ChartDTO
  TQuickChart
    .New
      .SetWidth(SpinEditWidth.Value)
      .SetHeight(SpinEditHeigth.Value)
      .SetDevicePixelRatio(SpinEditDevicePixelRatio.Value)
      .SetBackgroundColor(EditBrackGroundColorHex.Text)
      .SetChart( LChartDTO ) //<----- ObjectDTO
      .Download( Image1.Picture )
    .Free;

  LChartDTO.Free;
end;

end.
