{*************************************************************}
{                                                             }
{       MyInterfacedObject Unit for OpenSoldat                }
{                                                             }
{       Copyright (c) 2020-2023 OpenSoldat contributors       }
{                                                             }
{*************************************************************}

unit MyInterfacedObject;

interface


type
  TMyInterfacedObject = class(TObject, IInterface)
    protected
      function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
      function _AddRef: Integer; stdcall;
      function _Release: Integer; stdcall;
    end;


implementation


function TMyInterfacedObject.QueryInterface(const IID: TGUID; out Obj): HRESULT;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TMyInterfacedObject._AddRef: Integer;
begin
  Result := -1;   // -1 indicates no reference counting is taking place
end;

function TMyInterfacedObject._Release: Integer;
begin
  Result := -1;  // -1 indicates no reference counting is taking place
end;

end.
