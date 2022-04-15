program Project1;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  QuickChart in '..\..\src\QuickChart.pas',
  QuickChart.Types in '..\..\src\QuickChart.Types.pas',
  QuickChart.Chart in '..\..\src\QuickChart.Chart.pas',
  QuickChart.Download in '..\..\src\QuickChart.Download.pas',
  QuickChart.Request in '..\..\src\QuickChart.Request.pas';

{$R *.res}

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
