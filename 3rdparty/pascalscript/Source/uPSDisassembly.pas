

unit uPSDisassembly;
{$I PascalScript.inc}

interface
uses
  uPSRuntime, uPSUtils, sysutils;

function IFPS3DataToText(const Input: tbtstring; var Output: string): Boolean;
implementation

type
  TMyPSExec = class(TPSExec)
    function ImportProc(const Name: ShortString; proc: TIFExternalProcRec): Boolean; override;
  end;

function Debug2Str(const s: string): string;
var
  i: Integer;
begin
  result := '';
  for i := 1 to length(s) do
  begin
    if (s[i] < #32) or (s[i] > #128) then
      result := result + '\'+inttohex(ord(s[i]), 2)
    else if s[i] = '\' then
      result := result + '\\'
    else
      result := result + s[i];
  end;

end;

function SpecImportProc(Sender: TObject; p: TIFExternalProcRec): Boolean; forward;

function FloatToStr(Value: Extended): string;
begin
  try
    Result := SysUtils.FloatToStr(Value);
  except
    Result := 'NaNa';
  end;
end;


function IFPS3DataToText(const Input: tbtstring; var Output: string): Boolean;
var
  I: TMyPSExec;

  procedure Writeln(const s: string);
  begin
    Output := Output + s + #13#10;
  end;
  function BT2S(P: PIFTypeRec): string;
  var
    i: Longint;
  begin
    case p.BaseType of
      btU8: Result := 'U8';
      btS8: Result := 'S8';
      btU16: Result := 'U16';
      btS16: Result := 'S16';
      btU32: Result := 'U32';
      btS32: Result := 'S32';
      {$IFNDEF PS_NOINT64}bts64: Result := 'S64'; {$ENDIF}
      btChar: Result := {$IFDEF PS_PANSICHAR}'AnsiChar'{$ELSE}'Char'{$ENDIF};
      {$IFNDEF PS_NOWIDESTRING}
      btWideChar: Result := 'WideChar';
      btWideString: Result := 'WideString';
      {$ENDIF}
      btSet: Result := 'Set';
      btSingle: Result := 'Single';
      btDouble: Result := 'Double';
      btExtended: Result := 'Extended';
      btString: Result := 'String';
      btRecord:
        begin
          Result := 'Record(';
          for i := 0 to TPSTypeRec_Record(p).FieldTypes.Count-1 do
          begin
            if i <> 0 then Result := Result+',';
            Result := Result + BT2S(PIFTypeRec(TPSTypeRec_Record(p).FieldTypes[i]));
          end;
          Result := Result + ')';
        end;
      btArray: Result := 'Array of '+BT2S(TPSTypeRec_Array(p).ArrayType);
      btResourcePointer: Result := 'ResourcePointer';
      btPointer: Result := 'Pointer';
      btVariant: Result := 'Variant';
      btClass: Result := 'Class';
      btProcPtr: Result := 'ProcPtr';
      btStaticArray: Result := 'StaticArray['+inttostR(TPSTypeRec_StaticArray(p).Size)+'] of '+BT2S(TPSTypeRec_Array(p).ArrayType);
      btPChar: Result := 'PChar';
      btCurrency: Result := 'Currency';
      btUnicodeString: Result := 'UnicodeString';
      btInterface: Result := 'Interface';
      btType: Result := 'Type';
      btEnum: Result := 'Enum';
      btExtClass: Result := 'ExtClass';
    else
      Result := 'Unknown '+inttostr(p.BaseType);
    end;
  end;
  procedure WriteTypes;
  var
    T: Longint;
  begin
    Writeln('[TYPES]');
    for T := 0 to i.FTypes.Count -1 do
    begin
      if PIFTypeRec(i.FTypes[t]).ExportName <> '' then
        Writeln('Type ['+inttostr(t)+']: '+bt2s(PIFTypeRec(i.FTypes[t]))+' Export: '+PIFTypeRec(i.FTypes[t]).ExportName)
      else
        Writeln('Type ['+inttostr(t)+']: '+bt2s(PIFTypeRec(i.FTypes[t])));
    end;
  end;
  procedure WriteVars;
  var
    T: Longint;
    function FindType(p: Pointer): Cardinal;
    var
      T: Longint;
    begin
      Result := Cardinal(-1);
      for T := 0 to i.FTypes.Count -1 do
      begin
        if p = i.FTypes[t] then begin
          result := t;
          exit;
        end;
      end;
    end;
  begin
    Writeln('[VARS]');
    for t := 0 to i.FGlobalVars.count -1 do
    begin
      Writeln('Var ['+inttostr(t)+']: '+ IntToStr(FindType(PIFVariant(i.FGlobalVars[t])^.FType)) + ' '+ bt2s(PIFVariant(i.FGlobalVars[t])^.Ftype) + ' '+ PIFVariant(i.FGlobalVars[t])^.Ftype.ExportName);
    end;
  end;

  procedure WriteProcs;
  var
    t: Longint;
    procedure WriteProc(proc: TPSProcRec);
    var
      sc, CP: Cardinal;
      function ReadData(var Data; Len: Cardinal): Boolean;
      begin
        if CP + Len <= TPSInternalProcRec(PROC).Length then begin
          Move(TPSInternalProcRec(Proc).Data[CP], Data, Len);
          CP := CP + Len;
          Result := True;
        end else Result := False;
      end;
      function ReadByte(var B: Byte): Boolean;
      begin
        if CP < TPSInternalProcRec(Proc).Length then begin
          b := TPSInternalProcRec(Proc).Data^[cp];
          Inc(CP);
          Result := True;
        end else Result := False;
      end;

      function ReadLong(var B: Cardinal): Boolean;
      begin
        if CP + 3 < TPSInternalProcRec(Proc).Length then begin
          b := Cardinal((@TPSInternalProcRec(Proc).Data[CP])^);
          Inc(CP, 4);
          Result := True;
        end else Result := False;
      end;
      function ReadWriteVariable: string;
      var
        VarType: byte;
        L1, L2: Cardinal;
        function ReadVar(FType: Cardinal): string;
        var
          F: PIFTypeRec;
          b: byte;
          w: word;
          l: Cardinal;
          {$IFNDEF PS_NOINT64}ff: Int64;{$ENDIF}
          e: extended;
          ss: single;
          d: double;
          s: ansistring;
          c: char;
          {$IFNDEF PS_NOWIDESTRING}
          wc: WideChar;
          ws: WideString;
          {$ENDIF}

        begin
          result := '';
          F:= i.FTypes[Ftype];
          if f = nil then exit;
          case f.BaseType of
            btProcPtr: begin if not ReadData(l, 4) then exit; Result := 'PROC: '+inttostr(l); end;
            btU8: begin if not ReadData(b, 1) then exit; Result := IntToStr(tbtu8(B)); end;
            btS8: begin if not ReadData(b, 1) then exit; Result := IntToStr(tbts8(B)); end;
            btU16: begin if not ReadData(w, 2) then exit; Result := IntToStr(tbtu16(w)); end;
            btS16: begin if not ReadData(w, 2) then exit; Result := IntToStr(tbts16(w)); end;
            btU32: begin if not ReadData(l, 4) then exit; Result := IntToStr(tbtu32(l)); end;
            btS32: begin if not ReadData(l, 4) then exit; Result := IntToStr(tbts32(l)); end;
            {$IFNDEF PS_NOINT64}bts64: begin if not ReadData(ff, 8) then exit; Result := IntToStr(ff); end;{$ENDIF}
            btSingle: begin if not ReadData(ss, Sizeof(tbtsingle)) then exit; Result := FloatToStr(ss); end;
            btDouble: begin if not ReadData(d, Sizeof(tbtdouble)) then exit; Result := FloatToStr(d); end;
            btExtended: begin if not ReadData(e, Sizeof(tbtextended)) then exit; Result := FloatToStr(e); end;
            btPChar, btString: begin if not ReadData(l, 4) then exit; SetLength(s, l); if not readData(s[1], l) then exit; Result := MakeString(s); end;
            btSet:
              begin
                SetLength(s, TPSTypeRec_Set(f).aByteSize);
                if not ReadData(s[1], length(s)) then exit;
                result := MakeString(s);

              end;
            btChar: begin if not ReadData(c, 1) then exit; Result := '#'+IntToStr(ord(c)); end;
            {$IFNDEF PS_NOWIDESTRING}
            btWideChar: begin if not ReadData(wc, 2) then exit; Result := '#'+IntToStr(ord(wc)); end;
            btWideString: begin if not ReadData(l, 4) then exit; SetLength(ws, l); if not readData(ws[1], l*2) then exit; Result := MakeWString(ws); end;
            {$ENDIF}
          end;
        end;
        function AddressToStr(a: Cardinal): String;
        begin
          if a < PSAddrNegativeStackStart then
            Result := 'GlobalVar['+inttostr(a)+']'
          else
            Result := 'Base['+inttostr(Longint(A-PSAddrStackStart))+']';
        end;

      begin
        Result := '';
        if not ReadByte(VarType) then Exit;
        case VarType of
          0:
          begin

            if not ReadLong(L1) then Exit;
            Result := AddressToStr(L1);
          end;
          1:
          begin
            if not ReadLong(L1) then Exit;
            Result := '['+ReadVar(l1)+']';
          end;
          2:
          begin
            if not ReadLong(L1) then Exit;
            if not ReadLong(L2) then Exit;
            Result := AddressToStr(L1)+'.['+inttostr(l2)+']';
          end;
          3:
          begin
            if not ReadLong(l1) then Exit;
            if not ReadLong(l2) then Exit;
            Result := AddressToStr(L1)+'.'+AddressToStr(l2);
          end;
        end;
      end;

    var
      b: Byte;
      s: string;
      DP, D1, D2, d3, d4: Cardinal;

    begin
      CP := 0;
      sc := 0;
      while true do
      begin
        DP := cp;
        if not ReadByte(b) then Exit;
        case b of
          CM_A:
          begin
            {$IFDEF FPC}
            Output := Output + ' ['+inttostr(dp)+'] ASSIGN '+ ReadWriteVariable;
            Output := Output + ', ' + ReadWriteVariable + #13#10;
            {$ELSE}
            Writeln(' ['+inttostr(dp)+'] ASSIGN '+ReadWriteVariable+ ', ' + ReadWriteVariable);
            {$ENDIF}
          end;
          CM_CA:
          begin
            if not ReadByte(b) then exit;
            case b of
            0: s:= '+';
            1: s := '-';
            2: s := '*';
            3: s:= '/';
            4: s:= 'MOD';
            5: s:= 'SHL';
            6: s:= 'SHR';
            7: s:= 'AND';
            8: s:= 'OR';
            9: s:= 'XOR';
            else
              exit;
            end;
            Writeln(' ['+inttostr(dp)+'] CALC '+ReadWriteVariable+ ' '+s+' ' + ReadWriteVariable);
          end;
          CM_P:
          begin
            Inc(sc);
            Writeln(' ['+inttostr(dp)+'] PUSH '+ReadWriteVariable + ' // '+inttostr(sc));
          end;
          CM_PV:
          begin
            Inc(sc);
            Writeln(' ['+inttostr(dp)+'] PUSHVAR '+ReadWriteVariable + ' // '+inttostr(sc));
          end;
          CM_PO:
          begin
            Dec(Sc);
            Writeln(' ['+inttostr(dp)+'] POP // '+inttostr(sc));
          end;
          Cm_C:
          begin
            if not ReadLong(D1) then exit;
            Writeln(' ['+inttostr(dp)+'] CALL '+inttostr(d1));
          end;
          Cm_PG:
          begin
            if not ReadLong(D1) then exit;
            Writeln(' ['+inttostr(dp)+'] POP/GOTO currpos + '+IntToStr(d1)+' ['+IntToStr(CP+d1)+']');
          end;
          Cm_P2G:
          begin
            if not ReadLong(D1) then exit;
            Writeln(' ['+inttostr(dp)+'] POP2/GOTO currpos + '+IntToStr(d1)+' ['+IntToStr(CP+d1)+']');
          end;
          Cm_G:
          begin
            if not ReadLong(D1) then exit;
            Writeln(' ['+inttostr(dp)+'] GOTO currpos + '+IntToStr(d1)+' ['+IntToStr(CP+d1)+']');
          end;
          Cm_CG:
          begin
            if not ReadLong(D1) then exit;
            Writeln(' ['+inttostr(dp)+'] COND_GOTO currpos + '+IntToStr(d1)+' '+ReadWriteVariable+' ['+IntToStr(CP+d1)+']');
          end;
          Cm_CNG:
          begin
            if not ReadLong(D1) then exit;
            Writeln(' ['+inttostr(dp)+'] COND_NOT_GOTO currpos + '+IntToStr(d1)+' '+ReadWriteVariable+' ['+IntToStr(CP+d1)+']');
          end;
          Cm_R: Writeln(' ['+inttostr(dp)+'] RET');
          Cm_ST:
          begin
            if not ReadLong(d1) or not readLong(d2) then exit;
            Writeln(' ['+inttostr(dp)+'] SETSTACKTYPE Base['+inttostr(d1)+'] '+inttostr(d2));
          end;
          Cm_Pt:
          begin
            Inc(sc);
            if not ReadLong(D1) then exit;
            Writeln(' ['+inttostr(dp)+'] PUSHTYPE '+inttostr(d1) + '('+BT2S(TPSTypeRec(I.FTypes[d1]))+') // '+inttostr(sc));
          end;
          CM_CO:
          begin
            if not readByte(b) then exit;
            case b of
              0: s := '>=';
              1: s := '<=';
              2: s := '>';
              3: s := '<';
              4: s := '<>';
              5: s := '=';
              else exit;
            end;
            Writeln(' ['+inttostr(dp)+'] COMPARE into '+ReadWriteVariable+': '+ReadWriteVariable+' '+s+' '+ReadWriteVariable);
          end;
          Cm_cv:
          begin
            Writeln(' ['+inttostr(dp)+'] CALLVAR '+ReadWriteVariable);
          end;
          Cm_inc:
          begin
            Writeln(' ['+inttostr(dp)+'] INC '+ReadWriteVariable);
          end;
          Cm_dec:
          begin
            Writeln(' ['+inttostr(dp)+'] DEC '+ReadWriteVariable);
          end;
          cm_sp:
          begin
            Writeln(' ['+inttostr(dp)+'] SETPOINTER '+ReadWriteVariable+': '+ReadWriteVariable);
          end;
          cm_spc:
          begin
            Writeln(' ['+inttostr(dp)+'] SETCOPYPOINTER '+ReadWriteVariable+': '+ReadWriteVariable);
          end;
          cm_in:
          begin
            Writeln(' ['+inttostr(dp)+'] INOT '+ReadWriteVariable);
          end;
          cm_bn:
          begin
            Writeln(' ['+inttostr(dp)+'] BNOT '+ReadWriteVariable);
          end;
          cm_vm:
          begin
            Writeln(' ['+inttostr(dp)+'] MINUS '+ReadWriteVariable);
          end;
          cm_sf:
           begin
             s := ReadWriteVariable;
             if not ReadByte(b) then exit;
             if b = 0 then
               Writeln(' ['+inttostr(dp)+'] SETFLAG '+s)
             else
               Writeln(' ['+inttostr(dp)+'] SETFLAG NOT '+s);
           end;
           cm_fg:
           begin
             if not ReadLong(D1) then exit;
             Writeln(' ['+inttostr(dp)+'] FLAGGOTO currpos + '+IntToStr(d1)+' ['+IntToStr(CP+d1)+']');
           end;
           cm_puexh:
           begin
             if not ReadLong(D1) then exit;
             if not ReadLong(D2) then exit;
             if not ReadLong(D3) then exit;
             if not ReadLong(D4) then exit;
             Writeln(' ['+inttostr(dp)+'] PUSHEXCEPTION '+inttostr(d1)+' '+inttostr(d2)+' '+inttostr(d3)+' '+inttostr(d4));
           end;
           cm_poexh:
           begin
             if not ReadByte(b) then exit;
             Writeln(' ['+inttostr(dp)+'] POPEXCEPTION '+inttostr(b));
           end;
        else
          begin
            Writeln(' Disasm Error');
            Break;
          end;
        end;
      end;
    end;

  begin
    Writeln('[PROCS]');
    for t := 0 to i.FProcs.Count -1 do
    begin
      if TPSProcRec(i.FProcs[t]).ClassType = TIFExternalProcRec then
      begin
        if TPSExternalProcRec(i.FProcs[t]). Decl = '' then
          Writeln('Proc ['+inttostr(t)+']: External: '+TPSExternalProcRec(i.FProcs[t]).Name)
        else
          Writeln('Proc ['+inttostr(t)+']: External Decl: '+Debug2Str(TIFExternalProcRec(i.FProcs[t]).Decl) + ' ' + TIFExternalProcRec(i.FProcs[t]).Name);
      end else begin
        if TPSInternalProcRec(i.FProcs[t]).ExportName <> '' then
        begin
          Writeln('Proc ['+inttostr(t)+'] Export: '+TPSInternalProcRec(i.FProcs[t]).ExportName+' '+TPSInternalProcRec(i.FProcs[t]).ExportDecl);
        end else
          Writeln('Proc ['+inttostr(t)+']');
        Writeproc(i.FProcs[t]);
      end;
    end;
  end;

begin
  Result := False;
  try
    I := TMyPSExec.Create;
    I.AddSpecialProcImport('', @SpecImportProc, nil);

    if not I.LoadData(Input) then begin
      I.Free;
      Exit;
    end;
    Output := '';
    WriteTypes;
    WriteVars;
    WriteProcs;
    I.Free;
  except
    exit;
  end;
  result := true;
end;

{ TMyIFPSExec }

function MyDummyProc(Caller: TPSExec; p: TIFExternalProcRec; Global, Stack: TPSStack): Boolean;
begin
  Result := False;
end;


function TMyPSExec.ImportProc(const Name: ShortString;
  proc: TIFExternalProcRec): Boolean;
begin
  Proc.ProcPtr := MyDummyProc;
  result := true;
end;

function SpecImportProc(Sender: TObject; p: TIFExternalProcRec): Boolean;
begin
  p.ProcPtr := MyDummyProc;
  Result := True;
end;

end.
