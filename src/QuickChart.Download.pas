unit QuickChart.Download;

interface

uses
  System.Classes;

type
  TQuickChartDownload = class
  public
    class function GetAsStream(AUrl: String): TMemoryStream;
  end;

implementation

uses
  Winapi.Windows, Winapi.WinInet;

{ QuickChartDownload }

class function TQuickChartDownload.GetAsStream(AUrl: String): TMemoryStream;
var
  hSession     : HINTERNET;
  hService     : HINTERNET;
  lpBuffer     : array[0..1023] of Byte;
  dwBytesRead  : DWORD;
  dwBytesAvail : DWORD;
  dwTimeOut    : DWORD;
begin
  Result := TMemoryStream.Create;

  hSession := InternetOpen('usersession', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  if not Assigned(hSession) then Exit;
  try
    hService := InternetOpenUrl(hSession, PChar(AUrl), nil, 0, 0, 0);
    if hService = nil then
      Exit;
    try
      dwTimeOut := 60000;
      InternetSetOption(hService, INTERNET_OPTION_RECEIVE_TIMEOUT, @dwTimeOut, SizeOf(dwTimeOut));
      if InternetQueryDataAvailable(hService, dwBytesAvail, 0, 0) then
      repeat
        if not InternetReadFile(hService, @lpBuffer[0], SizeOf(lpBuffer), dwBytesRead) then
          Break;
        if dwBytesRead <> 0 then
          Result.WriteBuffer(lpBuffer[0], dwBytesRead);
      until dwBytesRead = 0;
    finally
      InternetCloseHandle(hService);
    end;
  finally
    InternetCloseHandle(hSession);
    Result.Position := 0;
  end;
end;

end.
