{*******************************************************}
{                                                       }
{       GameStrings Unit for OPENSOLDAT                 }
{                                                       }
{       Copyright (c) 2003 Michal Marcinkowski          }
{                                                       }
{*******************************************************}

unit GameStrings;

interface

uses
  GetText, Classes, PhysFS;

var
  TranslationFile: TMOFile;

function InitTranslation(Filename: String): Boolean;
function _(InputText: WideString): WideString; overload;
function _(InputText: AnsiString): WideString; overload;
procedure DeInitTranslation();

implementation

function InitTranslation(Filename: String): Boolean;
var
  TranslationStream: TStream;
begin
  Result := False;
  TranslationStream := PHYSFS_readAsStream(PChar(Filename));
  if TranslationStream.Size <> 0 then
  begin
    try
      TranslationFile := TMOFile.Create(TranslationStream);
      Result := True;
    except
      Result := False;
    end;
  end;
  TranslationStream.Free;
end;

function _(InputText: WideString): WideString; overload;
var
  Translation: WideString;
begin
  if Assigned(TranslationFile) then
  begin
    Translation := WideString(TranslationFile.Translate(String(InputText)));
    if Translation = '' then
      Translation := InputText;
  end else
    Translation := InputText;
  Result := Translation;
end;

function _(InputText: AnsiString): WideString; overload;
var
  Translation: AnsiString;
begin
  if Assigned(TranslationFile) then
  begin
    Translation := TranslationFile.Translate(InputText);
    if Translation = '' then
      Translation := InputText;
  end else
    Translation := InputText;
  Result := WideString(Translation);
end;

procedure DeInitTranslation();
begin
  TranslationFile.Free;
end;

end.
