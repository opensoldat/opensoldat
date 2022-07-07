{*******************************************************}
{                                                       }
{       PascalCore unit for OPENSOLDAT                  }
{                                                       }
{       Copyright (c) 2012 Tomasz Kolosowski            }
{                                                       }
{ PascalCore defines a class that should be extended by }
{ any script using PascalScript                         }
{                                                       }
{*******************************************************}
unit PascalCore;

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses
  Classes, Script, SysUtils, uPSComponent;

type
  TPascalCore = class(TScript)
  private
    // pascal script instance
    FPascalScript: TPSScript;
  protected
    {$push}{$warn 3018 off} // Hide "Constructor should be public"
    constructor Create(Dir: string);
    {$pop}
  public
    destructor Destroy(); override;
    // PascalScript instance published for inheriting class and interface functions
    // (for instance ScriptCoreInterface unit)
    property PascalScript: TPSScript read FPascalScript write FPascalScript;
    procedure ThreadFunc(const Parameters: array of Variant; FuncName: string);
  end;

type
  TThreadScript = class(TThread)
  private
    FFuncName: string;
    FParams: array of Variant;
    FScript: TPascalCore;
  protected
    procedure Execute; override;  // Main thread execution
  public
    constructor Create(Script: TPascalCore; Parameters: array of Variant;
      FuncName: string);
  end;

implementation

uses
  ScriptDispatcher;

constructor TPascalCore.Create(Dir: string);
begin
  inherited Create;
  Self.FPascalScript := TPSScript.Create(nil);
  Self.FDir := Dir + '/';
end;

destructor TPascalCore.Destroy();
begin
  Self.FPascalScript.Free;
  inherited;
end;

procedure TPascalCore.ThreadFunc(const Parameters: array of Variant;
  FuncName: string);
begin
  TThreadScript.Create(self, Parameters, FuncName);
end;

constructor TThreadScript.Create(Script: TPascalCore;
  Parameters: array of Variant; FuncName: string);
var
  I: Integer;
begin
  try
    inherited Create(False);  // Create thread suspended
      {$IFDEF MSWINDOWS}
      Priority := TThreadPriority(tpNormal);  // Set Priority Level
      {$ENDIF}
    FreeOnTerminate := True;  // Thread Free Itself when terminated

    FFuncName := FuncName;
    FScript := Script;
    SetLength(FParams, High(Parameters) + 1);

    for I := 0 to High(Parameters) do
    begin
      FParams[I] := Parameters[I];
    end;
  except
    on E: Exception do
      WriteLn(' [*] ' + FScript.Name +
        ' -> Exception raised on CreateThread(' + FuncName + ')');
  end;
end;

procedure TThreadScript.Execute;  // Main execution for thread
begin
  try
    FScript.PascalScript.ExecuteFunction(Self.FParams, Self.FFuncName
        {$IFDEF LEGACY_PASCALSCRIPT},0{$ENDIF}
      );
  except
    on e: Exception do
    begin
      ScrptDispatcher.WriteInfo('Script thread error: ' + e.Message);
      self.Terminate;  // set terminated flag for checking
    end;
  end;
end;

end.
