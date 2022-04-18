{*******************************************************}
{                                                       }
{         quickchart.oi Pascal Client Library           }
{             Developer: Joathan Theiller               }
{                  Copyright(c) 2022                    }
{    https://github.com/jtheiller/quickchart-pascal     }
{                                                       }
{*******************************************************}

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
    Memo1: TMemo;
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
    Label7: TLabel;
    EditFileName: TEdit;
    PanelTitle: TPanel;
    Label8: TLabel;
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
  QuickChart;

{$R *.dfm}

procedure TForm1.btnDownloadClick(Sender: TObject);
const
  JSON_TEXT ='{'
            +'  "type":"bar",'
            +'  "data":{'
            +'    "labels":["January","February","March","April","May"],'
            +'    "datasets":['
            +'      {'
            +'        "label":"Dogs",'
            +'        "data":[50,60,70,180,190]'
            +'      },'
            +'      {'
            +'        "label":"Cats",'
            +'        "data":[100,200,300,400,500'
            +'        ]'
            +'      }'
            +'    ]'
            +'  }'
            +'}';
begin
  TQuickChart
    .New
      .SetWidth(SpinEditWidth.Value)
      .SetHeight(SpinEditHeigth.Value)
      .SetDevicePixelRatio(SpinEditDevicePixelRatio.Value)
      .SetBackgroundColor(EditBrackGroundColorHex.Text)
      .SetChart( Memo1.Text )
      .Download( Image1.Picture )   //Optional
      .Download( EditFileName.Text )//Optional
    .Free;
end;

end.
