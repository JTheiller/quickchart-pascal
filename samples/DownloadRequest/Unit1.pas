unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Buttons,
  Vcl.Imaging.pngimage;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    ImageChart: TImage;
    btnDownload: TBitBtn;
    MemoUrl: TMemo;
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
  QuickChart.Download;

{$R *.dfm}

procedure TForm1.btnDownloadClick(Sender: TObject);
var
  LStream: TMemoryStream;
  LImgPNG: TPngImage;
begin
  LStream := TQuickChartDownload.GetAsStream( MemoUrl.Text );
  try
    LImgPNG := TPngImage.Create;
    try
      LImgPNG.LoadFromStream(LStream);
      ImageChart.Picture.Graphic := LImgPNG;
    finally
      LImgPNG.Free;
    end;
  finally
    LStream.Free;
  end;
end;

end.
