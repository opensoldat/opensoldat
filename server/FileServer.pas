unit FileServer;

interface

uses
  sysutils, Classes, fphttpserver, strutils;

type
  THTTPServer = class(TFPHTTPServer)
  public
    property Address;
    property ConnectionCount;
  end;

  THTTPServerThread = class(TThread)
  private
    FServer: THTTPServer;
  public
    constructor Create(AAddress: AnsiString; APort: Word; const OnRequest: THTTPServerRequestHandler);
    procedure Execute; override;
    procedure DoTerminate; override;
    property Server: THTTPServer read FServer;
  end;

  TFileServer = class(TDataModule)
  public
    procedure DoHandleRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
  end;

procedure StartFileServer;
procedure StopFileServer;

var
  FServerThread: THTTPServerThread;
  FFileServer: TFileServer;

implementation

uses
  Server{$IFDEF STEAM}, Steam{$ENDIF}, Version;

constructor THTTPServerThread.Create(AAddress: AnsiString; APort: Word; const OnRequest: THTTPServerRequestHandler);
begin
  inherited Create(False);
  FServer := THTTPServer.Create(nil);
  FServer.Threaded := True;
  FServer.Address := AAddress;
  FServer.Port := APort;
  FServer.OnRequest := OnRequest;
  FServer.AcceptIdleTimeout := 1000;
  FServer.FreeOnRelease;
  Self.FreeOnTerminate := False;
end;

procedure THTTPServerThread.Execute;
begin
  try
    try
      FServer.Active := True;
    finally
      if FServer <> nil then FreeAndNil(FServer);
    end;
  except
    on E: Exception do
    begin
      WriteLn('[FileServer] Error: ' + E.Message);
    end;
  end;
end;

procedure THTTPServerThread.DoTerminate;
begin
  if Assigned(FServer) then
    FServer.Active := False;
  inherited DoTerminate;
end;

procedure StartFileServer;
var
  Port: Word;
begin
  if not Assigned(FServerThread) then
  begin
    if fileserver_port.Value = 0 then
      Port := net_port.Value + 10
    else
      Port := fileserver_port.Value;

    FServerThread := THTTPServerThread.Create(fileserver_ip.Value, Port, FFileServer.DoHandleRequest);
    WriteLn('[FileServer] Starting fileserver on ' + fileserver_ip.Value + ':' + IntToStr(Port));
  end;
end;

procedure StopFileServer;
begin
  if Assigned(FServerThread) then
  begin
    WriteLn('[FileServer] Stopping fileserver');
    try
      FServerThread.DoTerminate;
      FreeAndNil(FServerThread);
    except
      on e: Exception do
      begin
        WriteLn('[FileServer] Error while stopping: ' + E.Message);
      end;
    end;
  end;
end;

{$push}{$warn 5024 off}
procedure TFileServer.DoHandleRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
var
  F: TFileStream;
  Uri: AnsiString;
  {$IFDEF STEAM}
  Path: array[0..4096] of Char;
  Sr: TSearchRec;
  Split: TStringArray;
  ItemID: uint64;
  WorkshopDownload: Boolean = False;
  {$ENDIF}
  ServePath: String;
begin
  Uri := ARequest.URI;

  if (ARequest.UserAgent <> 'opensoldatclient/' + OPENSOLDAT_VERSION) or (ARequest.Method <> 'GET') then
  begin
    AResponse.Free;
    Exit;
  end;

  ServePath := UserDirectory + Uri;
  {$IFDEF STEAM}
  if Uri.StartsWith('/maps/workshop/') then
  begin
    Split := Uri.Split('/');
    if Length(Split) >= 4 then
    begin
      ItemID := StrToIntDef(Split[3].Replace('.smap', ''), 0);
      if ItemID > 0 then
      begin
        if SteamAPI.UGC.GetItemInstallInfo(ItemID, nil, @Path, 4096, nil) then
        begin
          if FindFirst(Path + '/*.smap', faAnyFile - faDirectory, sr) = 0 then
          begin
            ServePath := Path + '/' + Sr.Name;
            WorkshopDownload := True;
          end;
        end;
      end;
    end;
  end;
  {$ENDIF}

  if ((AnsiStartsText('/mods', Uri) and AnsiEndsText('.smod', Uri)) or
    (AnsiStartsText('/maps', Uri) and AnsiEndsText('.smap', Uri))) {$IFDEF STEAM} or WorkshopDownload {$ENDIF} then
  begin
    WriteLn('[FileServer] File request: ' + Uri + ' by ' + ARequest.RemoteAddress);
    try
      F := TFileStream.Create(ServePath, fmOpenRead or fmShareDenyWrite);
      try
        AResponse.SetCustomHeader('content-disposition', 'attachment; filename="' + ExtractFileName(F.FileName) + '"');
        AResponse.ContentType := 'application/octet-stream';
        AResponse.ContentLength := F.Size;
        AResponse.ContentStream := F;
        AResponse.SendContent;
        AResponse.ContentStream := Nil;
      except
        on e: Exception do
        begin
          WriteLn('[FileServer] HTTP Error: ' + E.Message);
          AResponse.Free;
          F.Free;
        end;
    end;
    except
      on e: Exception do
      begin
        WriteLn('[FileServer] File open error: ' + E.Message);
        AResponse.Free;
        F.Free;
      end;
    end;
  end else
    AResponse.Free;

end;
{$pop}

end.
