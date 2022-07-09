unit FileClient;

interface

uses
  SysUtils, Classes, sha1, strutils, fphttpclient,
  GameRendering, Constants, Version;

const
  MAX_DL_SIZE = 150000000;  // max download size in bytes

type
  TDownloadThread = Class(TThread)
    private
      FURL: String;
      FFilename: String;
      FChecksum: TSHA1Digest;
      FErrorMsg: String;
      FStatus: Byte;
      FProgress: Byte;
      FDownloadPos: Int64;
      FDownloadSize: Int64;
      Client: TFPHTTPClient;

      procedure SetStatus;
      procedure SetError;
      procedure DoProgress(Sender: TObject; Const ContentLength, CurrentPos : Int64);
      procedure OnFinished;
      procedure DummySync;
    protected
      procedure Execute; override;
    public
      constructor Create(DownloadURL: String; Name: String; CheckSum: TSHA1Digest);
      procedure CancelDownload;
      destructor Destroy; override;
  end;

var
  DownloadRetry: Byte = 0;

implementation

uses
  Client, Util;

constructor TDownloadThread.Create(DownloadURL: String; Name: String; Checksum: TSHA1Digest);
begin
  inherited Create(False);
  FURL := DownloadURL;
  FFilename := AnsiReplaceStr(Name, '..', '');
  FChecksum := Checksum;
  FreeOnTerminate := True;
end;

procedure TDownloadThread.CancelDownload;
begin
  Client.Terminate;
end;

procedure TDownloadThread.SetError;
begin
  ShowMessage('Download error: ' + WideString(FErrorMsg));
  MainConsole.Console('Download error: ' + FErrorMsg, DEBUG_MESSAGE_COLOR);
end;

procedure TDownloadThread.SetStatus;
begin
  RenderGameInfo(WideString(Format('Downloading %s - %d%% (%s/%s)',
    [ExtractFileName(FFilename), FProgress, GetSize(FDownloadPos), GetSize(FDownloadSize)])));
end;

{$push}{$warn 5024 off}
procedure TDownloadThread.DoProgress(Sender: TObject; const ContentLength, CurrentPos: Int64);
var
  OldProgress: Int64;
begin
  if (ContentLength > MAX_DL_SIZE) or (CurrentPos > MAX_DL_SIZE) then
  begin
    FStatus := 3;
    Client.Terminate;
    Exit;
  end;
  if ContentLength > 0 then
  begin
    OldProgress := FProgress;
    FProgress := Round((CurrentPos / ContentLength) * 100);
    FDownloadSize := ContentLength;
    FDownloadPos := CurrentPos;
    if FProgress <> OldProgress then
      Synchronize(SetStatus);
  end;
end;
{$pop}

procedure TDownloadThread.Execute;
begin
  Client := TFPHTTPClient.Create(Nil);
  with Client do
  try
    try
      AddHeader('User-Agent', 'opensoldatclient/' + OPENSOLDAT_VERSION);
      AllowRedirect := False;
      IOTimeout := 1000;
      OnDataReceived := DoProgress;
      CreateDirIfMissing(ExtractFilePath(FFilename));
      Get(FURL, FFilename);
      if FStatus = 3 then
        raise Exception.Create('The requested file is too large to download directly');
      if not Sha1Match(Sha1File(FFilename, 4096), FCheckSum) then
        raise Exception.Create('Checksum mismatch');
      FStatus := 1;
    except
      on E: Exception do
      begin
        DeleteFile(FFileName);
        FErrorMsg := E.Message;
        Synchronize(SetError);
      end;
    end;
  finally
    Client.Terminate;
  end;

  { We can't Synchronize a method that calls JoinServer, because it starts the
    infinite game loop. As such, execution will never be returned to DownloadThread,
    and we won't be able to destroy it properly. Instead, we Queue the method
    that calls JoinServer, and then rely on a hack to make sure that Queued method
    gets executed before we destroy DownloadThread. We need the DownloadThread
    instance to be initialized, because we access its properties in OnFinished. }
  Queue(OnFinished);
  Synchronize(DummySync);
end;

// Must be executed from main thread.
procedure TDownloadThread.OnFinished;
begin
  Inc(DownloadRetry);
  if DownloadRetry = 1 then
    if FStatus = 1 then
      JoinServer;
end;

destructor TDownloadThread.Destroy;
begin
  Client.Free;
  inherited Destroy;
end;

procedure TDownloadThread.DummySync;
begin
end;

end.
