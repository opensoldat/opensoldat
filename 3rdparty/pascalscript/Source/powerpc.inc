{ implementation of the powerpc osx abi for function calls in pascal script
  Copyright (c) 2007 by Henry Vermaak (henry.vermaak@gmail.com) }

{$ifndef darwin}
  {$fatal This code is Darwin specific at the moment!}
{$endif}

{$ifndef cpu32}
  {$fatal This code is 32bit specific at the moment!}
{$endif}
// FPC does not always push a copy of r3 - r10 to the stack (24(r1) - 52(r1)
// The API states that the space must be reserved, if the regiser contents are not placed there.
{$define NeedRegCopyOnStack}
const
  rtINT = 0;
  rtINT64 = 1;
  rtFLOAT = 2;

type
  Trint = array[1..8] of dword;
  Trfloat = array[1..13] of double;

{$goto on}
{ define labels }
label
  rfloat_loop,
  stack_loop,
  load_regs,
  int_result,
  int64_result,
  float_result,
  asmcall_end;

{ call a function from a pointer }
{ resulttype: 0 = int, 1 = int64, 2 = float }
function ppcasmcall(rint: Trint; rfloat: Trfloat; proc, stack: pointer; stacksize, resulttype: integer): pointer; assembler; nostackframe;
asm
	mflr	r0
	stw	r0, 8(r1)

	{ save non-volatile register/s - make sure the stack size is sufficient! }
	stw	r31, -4(r1)	{ stacksize }

	stwu	r1, -240(r1)	{ create stack }

	{ get all the params into the stack }
	stw	r3, 48(r1)	{ rint }
	stw	r4, 52(r1)	{ rfloat }
	stw	r5, 56(r1)	{ proc }
	stw	r6, 60(r1)	{ stack }
	stw	r7, 64(r1)	{ stacksize }
	stw	r8, 68(r1)	{ resulttype }
	{ result is stored in 72(r1) and 76(r1) (if returning int64) }

	{ write rint array into stack }
	lwz	r2, 48(r1)	{ rint }
	lfd	f0, 0(r2)
	stfd	f0, 80(r1)	{ rint[1], rint[2] }
	lfd	f0, 8(r2)
	stfd	f0, 88(r1)	{ rint[3], rint[4] }
	lfd	f0, 16(r2)
	stfd	f0, 96(r1)	{ rint[5], rint[6] }
	lfd	f0, 24(r2)
	stfd	f0, 104(r1)	{ rint[7], rint[8] }

	{ write rfloat array into stack }
	lwz	r2, 52(r1)	{ rfloat }
	addi	r4, r1, 112	{ rfloat[1] from here upwards (8 bytes apart) }
	subi	r2, r2, 8	{ src }
	subi	r4, r4, 8	{ dest }
	li	r3, 13		{ counter }

rfloat_loop:
	subic.	r3, r3, 1	{ dec counter }
	lfdu	f0, 8(r2)	{ load rfloat[x] + update }
	stfdu	f0, 8(r4)	{ store rfloat[x] + update }
	bne	cr0, rfloat_loop

	{ create new stack }
	mflr	r0
	stw	r0, 8(r1)
	mr	r12, r1		{ remember previous stack to fill in regs later }

	lwz	r31, 64(r12)	{ load stacksize into r31 }
	neg	r3, r31		{ negate }
	stwux	r1, r1, r3	{ create new stack }

	{ build up the stack here }
	mr	r3, r31		{ counter }
	subic.	r3, r3, 24	{ don't write first 24 }
	ble	cr0, load_regs	{ don't fill in stack if there is none }

	lwz	r2, 60(r12)	{ pointer to stack }
	addi	r2, r2, 24	{ start of params }
	subi	r2, r2, 1	{ src }

	addi	r4, r1, 24	{ start of params }
	subi	r4, r4, 1	{ dest }

stack_loop:
	subic.	r3, r3, 1	{ dec counter }
	lbzu	r5, 1(r2)	{ load stack + update }
	stbu	r5, 1(r4)	{ store stack + update }
	bne	cr0, stack_loop

load_regs:			{ now load the registers from the previous stack in r12 }
	lwz	r3, 80(r12)
	lwz	r4, 84(r12)
	lwz	r5, 88(r12)
	lwz	r6, 92(r12)
	lwz	r7, 96(r12)
	lwz	r8, 100(r12)
	lwz	r9, 104(r12)
	lwz	r10, 108(r12)

	lfd	f1, 112(r12)
	lfd	f2, 120(r12)
	lfd	f3, 128(r12)
	lfd	f4, 136(r12)
	lfd	f5, 144(r12)
	lfd	f6, 152(r12)
	lfd	f7, 160(r12)
	lfd	f8, 168(r12)
	lfd	f9, 176(r12)
	lfd	f10, 184(r12)
	lfd	f11, 192(r12)
	lfd	f12, 200(r12)
	lfd	f13, 208(r12)

	{ now call this function }
	lwz	r2, 56(r12)	{ proc }
	mtctr	r2		{ move to ctr }
	bctrl			{ branch and link to ctr }

	{ restore stack - use stacksize in r31 }
	add	r1, r1, r31
	lwz	r0, 8(r1)
	mtlr	r0

	{ check resulttype and put appropriate pointer into r3 }
	lwz	r2, 68(r1)		{ resulttype }
	cmpwi	cr0, r2, 0		{ int result? }
	beq	cr0, int_result		{ branch if equal }

	cmpwi	cr0, r2, 1		{ single result? }
	beq	cr0, int64_result	{ branch if equal }


float_result:			{ the result is a double}
	stfd	f1, 72(r1)	{ write f1 to result on stack }
	b	asmcall_end


int64_result:			{ the result is a single }
	stw	r3, 72(r1)	{ write high dword to result on stack }
	stw	r4, 76(r1)	{ write low dword to result on stack }
	b	asmcall_end


int_result:			{ the result is dword }
	stw	r3, 72(r1)	{ write r3 to result on stack }


asmcall_end:			{ epilogue }
	addi	r3, r1, 72	{ pointer to result on the stack }
	addi	r1, r1, 240	{ restore stack }

	{ restore non-volatile register/s }
	lwz	r31, -4(r1)

	lwz	r0, 8(r1)
	mtlr	r0
	blr
end;

function FlipHiLo(v: byte): byte;
var
  i: integer;
  j, k: byte;
begin
  Result := 0;
  k := $80;
  j := $01;
  for i := 0 to 7 do begin
    if (v and k) <> 0 then
      Result := Result or j;
    k := k div 2;
    j := j * 2;
  end;
end;

function TPSExec.InnerfuseCall(_Self, Address: Pointer; CallingConv: TPSCallingConvention; Params: TPSList; res: PPSVariantIFC): Boolean;
var
  rint: Trint;			{ registers r3 to r10 }
  rfloat: Trfloat;		{ registers f1 to f13 }
  st: packed array of byte;	{ stack }
  i, j, n, m, rindex, findex, stindex: integer;
  fvar: PPSVariantIFC;
  IsConstructor: Boolean;
  fSetHelper: dword;
  fSetP1, fsetP2: PByte;

  { add a dword to stack }
  procedure addstackdword(value: dword);
  begin
    {$ifndef NeedRegCopyOnStack}
    if stindex = 0 then begin
      stindex := 56; // leave empty space for registers on stack
    end;
    {$endif}
    setlength(st, stindex+4);
    pdword(@st[stindex])^ := value;
    inc(stindex, 4);
  end;

  { add a float to stack }
  procedure addstackfloat(value: pointer; size: integer);
  begin
    {$ifndef NeedRegCopyOnStack}
    if stindex = 0 then begin
      stindex := 56; // leave empty space for registers on stack
    end;
    {$endif}
    setlength(st, stindex + (size * 4));
    if size = 1
      then psingle(@st[stindex])^ := single(value^)
      else pdouble(@st[stindex])^ := double(value^);
    inc(stindex, size*4);
  end;

  { add to the general registers or overflow to stack }
  procedure addgen(value: dword);
  begin
    if rindex <= 8
      then begin
        rint[rindex] := value;
        inc(rindex);
        {$ifdef NeedRegCopyOnStack}
        addstackdword(value);
        {$endif}
      end
      else begin
        addstackdword(value);
      end;
  end;
  { add to the float registers or overflow to stack }
  { size = 1 for single, 2 for double }
  procedure addfloat(value: pointer; size: integer);
  begin
    if findex <= 13
      then begin
        if size = 1
          then rfloat[findex] := single(value^)
          else rfloat[findex] := double(value^);
        inc(findex);
        inc(rindex, size);
        {$ifdef NeedRegCopyOnStack}
        addstackfloat(value, size);
        {$endif}
      end
      else begin
        addstackfloat(value, size);
      end;
  end;

begin
  if (Integer(CallingConv) and 64) <> 0 then begin
    IsConstructor := true;
    CAllingConv := TPSCallingConvention(Integer(CallingConv) and not 64);
  end else IsConstructor := false;

  rindex := 1;
  findex := 1;
  {$ifdef NeedRegCopyOnStack}
  stindex := 24;
  {$else}
  stindex := 0; // do not create a stack, if only registers are used
  {$endif}
  setlength(st, stindex);
  Result := False;

  if assigned(_Self) then begin
    addgen(dword(_Self));
  end;

  { the pointer of the result needs to be passed first in the case of some result types }
  if assigned(res)
    then begin
      case res.atype.basetype of
        btStaticArray, btRecord, btString: addgen(dword(res.dta));
      end;
    end;

  { process all parameters }
  for i := 0 to Params.Count-1 do begin
    if Params[i] = nil
      then Exit;
    fvar := Params[i];

    { cook dynamic arrays - fpc stores size-1 at @array-4 }
    if (fvar.aType.BaseType = btArray)
      then dec(pdword(pointer(fvar.dta^)-4)^);

    if fvar.varparam
      then begin  { var param }
        case fvar.aType.BaseType of
          { add var params here }
          btArray, btVariant, btSet, btStaticArray, btRecord, btInterface, btClass, {$IFNDEF PS_NOWIDESTRING} btWideString, btWideChar, {$ENDIF}
          btU8, btS8, btU16, btS16, btU32, btS32, btSingle, btDouble, btExtended, btString, btPChar, btChar, btCurrency
          {$IFNDEF PS_NOINT64}, bts64{$ENDIF}: addgen(dword(fvar.dta));  { TODO: test all }
          else begin
            writeln(stderr, 'Parameter type not recognised!');
            Exit;
          end;
        end;  { case }
      end else begin  { not a var param }
        case fvar.aType.BaseType of
//          btArray, btVariant, btSet, btStaticArray, btRecord, btInterface, btClass, {$IFNDEF PS_NOWIDESTRING} btWideString, btWideChar, {$ENDIF}
//          btU8, btS8, btU16, btS16, btU32, btS32, btSingle, btDouble, btExtended, btString, btPChar, btChar, btCurrency
//          {$IFNDEF PS_NOINT64}, bts64{$ENDIF}: writeln('normal param');

          { add normal params here }
          btString:                            addgen(dword(pstring(fvar.dta)^));
          btU8, btS8:                          addgen(dword(pbyte(fvar.dta)^));
          btU16, BtS16:                        addgen(dword(pword(fvar.dta)^));
          btU32, btS32:                        addgen(dword(pdword(fvar.dta)^));
          btSingle:                            addfloat(fvar.dta, 1);
          btDouble, btExtended:                addfloat(fvar.dta, 2);
          btPChar:                             addgen(dword(ppchar(fvar.dta)^));
          btChar:                              addgen(dword(pchar(fvar.dta)^));
          {$IFNDEF PS_NOINT64}bts64:{$ENDIF}   begin
                                                 addgen(dword(pint64(fvar.dta)^ shr 32));
                                                 addgen(dword(pint64(fvar.dta)^ and $ffffffff));
                                               end;
          btStaticArray:                       addgen(dword(fvar.dta));
          btRecord:                            for j := 0 to (fvar.atype.realsize div 4)-1 do
                                                 addgen(pdword(fvar.dta + j*4)^);
          btArray:                             addgen(dword(fvar.dta^));
          btSet: begin

              fSetP1 := fvar.dta;
              fSetP2 := @fSetHelper;
              fSetHelper := 0;
              for n := 1 to TPSTypeRec_Set(fvar.aType).aByteSize do
              begin
                fSetP2^ := fliphilo(fSetP1^);
                inc(fSetP1);
                inc(fSetP2);
                if n and 3 = 0
                then begin
                  addgen(fSetHelper);
                  fSetP2 := @fSetHelper;
                  fSetHelper := 0;
                end;
              end;
              if TPSTypeRec_Set(fvar.aType).aByteSize and 3 <> 0
              then addgen(fSetHelper);

            end;

          { TODO add and test }
{          btVariant, btSet, btInterface, btClass  }

          else begin
            writeln(stderr, 'Parameter type not implemented!');
            Exit;
          end;
        end;  { case }
      end;  { else }
  end;  { for }

  if (stindex mod 16) <> 0 then begin
    stindex := stindex + 16 - (stindex mod 16);
    setlength(st, stindex);
  end;

  if not assigned(res)
    then begin
      ppcasmcall(rint, rfloat, address, st, stindex, rtINT);  { ignore return }
    end
    else begin
      case res.atype.basetype of
        { add result types here }
        btString:                ppcasmcall(rint, rfloat, address, st, stindex, rtINT);
        btU8, btS8:              pbyte(res.dta)^ := byte(pdword(ppcasmcall(rint, rfloat, address, st, stindex, rtINT))^);
        btU16, btS16:            pword(res.dta)^ := word(pdword(ppcasmcall(rint, rfloat, address, st, stindex, rtINT))^);
        btU32, btS32:            pdword(res.dta)^ := pdword(ppcasmcall(rint, rfloat, address, st, stindex, rtINT))^;
        btSingle:                psingle(res.dta)^ := pdouble(ppcasmcall(rint, rfloat, address, st, stindex, rtFLOAT))^;
        btDouble, btExtended:    pdouble(res.dta)^ := pdouble(ppcasmcall(rint, rfloat, address, st, stindex, rtFLOAT))^;
        btPChar:                 ppchar(res.dta)^ := pchar(pdword(ppcasmcall(rint, rfloat, address, st, stindex, rtINT))^);
        btChar:                  pchar(res.dta)^ := char(pdword(ppcasmcall(rint, rfloat, address, st, stindex, rtINT))^);
        btStaticArray, btRecord: ppcasmcall(rint, rfloat, address, st, stindex, rtINT);
        btArray:                 res.dta := ppcasmcall(rint, rfloat, address, st, stindex, rtINT);

        { TODO add and test }

        else begin
          writeln(stderr, 'Result type not implemented!');
          exit;
        end;  { else }
      end;  { case }
    end;

  { cook dynamic arrays - fpc stores size-1 at @array-4 }
  for i := 0 to Params.Count-1 do begin
    fvar := Params[i];
    if (fvar.aType.BaseType = btArray)
      then inc(pdword(pointer(fvar.dta^)-4)^);
   end;

  Result := True;
end;
