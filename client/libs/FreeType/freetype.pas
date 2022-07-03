// -------------------------------------------------------------------------- //
// FreeType bindings (based on 2.11.1 for x86/x64)

unit FreeType;
{$NOTES OFF}
{$MINENUMSIZE 4}

interface

const
  {$IFDEF MSWINDOWS}
  FTLIB = 'freetype.dll';
  {$ELSE}
  FTLIB = 'freetype.so';
  {$ENDIF}

// -------------------------------------------------------------------------- //
//                             TYPES & CONSTANTS                              //
// -------------------------------------------------------------------------- //
// ftconfig.h

type
  // fix for delphi versions
  {$IFNDEF FPC}
  {$if CompilerVersion<=18.5}
  FT_Long  = LongInt;
  UFT_Long = LongWord;
  {$ifend}
  {$ENDIF}

  FT_UInt32_ptr = ^FT_UInt32;

  FT_Int16  = SmallInt;
  FT_UInt16 = Word;
  FT_Int32  = LongInt;
  FT_UInt32 = LongWord;
  {$if defined(cpu64) and not(defined(win64) and defined(cpux86_64))}
  FT_Long = int64;
  FT_ULong = qword;
  FT_Pos = int64;
  {$ELSE}
  FT_Long = longint;
  FT_ULong = longword;
  FT_Pos = longint;
  {$ENDIF}
// -------------------------------------------------------------------------- //
// ftsystem.h

type
  FT_Memory       = ^FT_MemoryRec;
  FT_Alloc_Func   = function(memory: FT_Memory; size: FT_Long): Pointer; cdecl;
  FT_Free_Func    = procedure(memory: FT_Memory; block: Pointer); cdecl;
  FT_Realloc_Func = function(memory: FT_Memory; cur_size, new_size: FT_Long; block: Pointer): Pointer; cdecl;

  FT_MemoryRec = record
    user:    Pointer;
    alloc:   FT_Alloc_Func;
    free:    FT_Free_Func;
    realloc: FT_Realloc_Func;
  end;

  FT_Stream = ^FT_StreamRec;

  FT_StreamDesc = record
    case Integer of
      0: (val: FT_Long);
      1: (ptr: Pointer);
  end;

  FT_Stream_IoFunc = function(stream: FT_Stream; offset: FT_ULong; buffer: PByte; count: FT_ULong): FT_ULong; cdecl;
  FT_Stream_CloseFunc = procedure(stream: FT_Stream); cdecl;

  FT_StreamRec = record
    base:       PByte;
    size:       FT_ULong;
    pos:        FT_ULong;
    descriptor: FT_StreamDesc;
    pathname:   FT_StreamDesc;
    read:       FT_Stream_IoFunc;
    close:      FT_Stream_CloseFunc;
    memory:     FT_Memory;
    cursor:     PByte;
    limit:      PByte;
  end;

// -------------------------------------------------------------------------- //
// ftimage.h

type
  FT_Pixel_Mode = (
    FT_PIXEL_MODE_NONE = 0,
    FT_PIXEL_MODE_MONO,
    FT_PIXEL_MODE_GRAY,
    FT_PIXEL_MODE_GRAY2,
    FT_PIXEL_MODE_GRAY4,
    FT_PIXEL_MODE_LCD,
    FT_PIXEL_MODE_LCD_V,
    FT_PIXEL_MODE_BGRA,
    FT_PIXEL_MODE_MAX
  );

  FT_Glyph_Format = (
    FT_GLYPH_FORMAT_NONE      = $00000000,
    FT_GLYPH_FORMAT_COMPOSITE = $636f6d70,  // ['c', 'o', 'm', 'p']
    FT_GLYPH_FORMAT_BITMAP    = $62697473,  // ['b', 'i', 't', 's']
    FT_GLYPH_FORMAT_OUTLINE   = $6f75746c,  // ['o', 'u', 't', 'l']
    FT_GLYPH_FORMAT_PLOTTER   = $706c6f74   // ['p', 'l', 'o', 't']
  );

const
  FT_OUTLINE_CONTOURS_MAX    = $7fff;
  FT_OUTLINE_POINTS_MAX      = $7fff;

  FT_OUTLINE_NONE            = $0000;
  FT_OUTLINE_OWNER           = $0001;
  FT_OUTLINE_EVEN_ODD_FILL   = $0002;
  FT_OUTLINE_REVERSE_FILL    = $0004;
  FT_OUTLINE_IGNORE_DROPOUTS = $0008;
  FT_OUTLINE_SMART_DROPOUTS  = $0010;
  FT_OUTLINE_INCLUDE_STUBS   = $0020;
  FT_OUTLINE_OVERLAP         = $0040;
  FT_OUTLINE_HIGH_PRECISION  = $0100;
  FT_OUTLINE_SINGLE_PASS     = $0200;

  FT_CURVE_TAG_ON            = 1;
  FT_CURVE_TAG_CONIC         = 0;
  FT_CURVE_TAG_CUBIC         = 2;
  FT_CURVE_TAG_HAS_SCANMODE  = 4;
  FT_CURVE_TAG_TOUCH_X       = 8;
  FT_CURVE_TAG_TOUCH_Y       = 16;
  FT_CURVE_TAG_TOUCH_BOTH    = FT_CURVE_TAG_TOUCH_X or FT_CURVE_TAG_TOUCH_Y;

  FT_RASTER_FLAG_DEFAULT     = 0;
  FT_RASTER_FLAG_AA          = 1;
  FT_RASTER_FLAG_DIRECT      = 2;
  FT_RASTER_FLAG_CLIP        = 4;
  FT_RASTER_FLAG_SDF         = 8;

type
  FT_Vector_ptr        = ^FT_Vector;
  FT_Raster_ptr        = ^FT_Raster;
  FT_Span_ptr          = ^FT_Span;
  FT_Raster_Params_ptr = ^FT_Raster_Params;


  FT_Vector = record
    x: FT_Pos;
    y: FT_Pos;
  end;

  FT_BBox = record
    xMin, yMin: FT_Pos;
    xMax, yMax: FT_Pos;
  end;

  FT_Bitmap = record
    rows:         LongWord;
    width:        LongWord;
    pitch:        LongInt;
    buffer:       PByte;
    num_grays:    Word;
    pixel_mode:   Byte;
    palette_mode: Byte;
    palette:      Pointer;
  end;

  FT_Outline = record
    n_contours:  SmallInt;
    n_points:    SmallInt;
    points:     ^FT_Vector;
    tags:       ^ShortInt;
    contours:   ^SmallInt;
    flags:       LongInt;
  end;

  FT_Outline_MoveToFunc  = function(to_: FT_Vector_ptr; user: Pointer): LongInt; cdecl;
  FT_Outline_LineToFunc  = function(to_: FT_Vector_ptr; user: Pointer): LongInt; cdecl;
  FT_Outline_ConicToFunc = function(ctrl, to_: FT_Vector_ptr; user: Pointer): LongInt; cdecl;
  FT_Outline_CubicToFunc = function(ctrl1, ctrl2, to_: FT_Vector_ptr; user: Pointer): LongInt; cdecl;

  FT_Outline_MoveTo_Func  = FT_Outline_MoveToFunc;
  FT_Outline_LineTo_Func  = FT_Outline_LineToFunc;
  FT_Outline_ConicTo_Func = FT_Outline_ConicToFunc;
  FT_Outline_CubicTo_Func = FT_Outline_CubicToFunc;

  FT_Outline_Funcs = record
    move_to:  FT_Outline_MoveToFunc;
    line_to:  FT_Outline_LineToFunc;
    conic_to: FT_Outline_ConicToFunc;
    cubic_to: FT_Outline_CubicToFunc;
    shift:    LongInt;
    delta:    FT_Pos;
  end;

  FT_Raster     = ^FT_RasterRec;
  FT_RasterRec  =  record end;

  FT_Span = record
    x:        SmallInt;
    len:      Word;
    coverage: Byte;
  end;

  FT_SpanFunc = procedure(y, count: LongInt; spans: FT_Span_ptr; user: Pointer); cdecl;
  FT_Raster_Span_Func = FT_SpanFunc;

  FT_Raster_BitTest_Func = function(y, x: LongInt; user: Pointer): LongInt; cdecl;
  FT_Raster_BitSet_Func  = procedure(y, x: LongInt; user: Pointer); cdecl;

  FT_Raster_Params = record
    target:      ^FT_Bitmap;
    source:       Pointer;
    flags:        LongInt;
    gray_spans:   FT_SpanFunc;
    black_spans:  FT_SpanFunc;
    bit_test:     FT_Raster_BitTest_Func;
    bit_set:      FT_Raster_BitSet_Func;
    user:         Pointer;
    clip_box:     FT_BBox;
  end;

  FT_Raster_NewFunc     = function(memory: Pointer; raster: FT_Raster_ptr): LongInt; cdecl;
  FT_Raster_DoneFunc    = procedure(raster: FT_Raster); cdecl;
  FT_Raster_ResetFunc   = procedure(raster: FT_Raster; pool_base: PByte; pool_size: FT_ULong); cdecl;
  FT_Raster_SetModeFunc = function(raster: FT_Raster; mode: FT_ULong; args: Pointer): LongInt; cdecl;
  FT_Raster_RenderFunc  = function(raster: FT_Raster; params: FT_Raster_Params_ptr): LongInt; cdecl;

  FT_Raster_New_Func      = FT_Raster_NewFunc;
  FT_Raster_Done_Func     = FT_Raster_DoneFunc;
  FT_Raster_Reset_Func    = FT_Raster_ResetFunc;
  FT_Raster_Set_Mode_Func = FT_Raster_SetModeFunc;
  FT_Raster_Render_Func   = FT_Raster_RenderFunc;

  FT_Raster_Funcs = record
    glyph_format:    FT_Glyph_Format;
    raster_new:      FT_Raster_NewFunc;
    raster_reset:    FT_Raster_ResetFunc;
    raster_set_mode: FT_Raster_SetModeFunc;
    raster_render:   FT_Raster_RenderFunc;
    raster_done:     FT_Raster_DoneFunc;
  end;

// -------------------------------------------------------------------------- //
// fttypes.h

type
  FT_String_ptr = ^FT_String;
  FT_Int_ptr    = ^FT_Int;
  FT_UInt_ptr   = ^FT_UInt;
  FT_Fixed_ptr  = ^FT_Fixed;
  FT_Matrix_ptr = ^FT_Matrix;

  FT_Bool    =  Byte;
  FT_FWord   =  SmallInt;
  FT_UFWord  =  Word;
  FT_Char    =  ShortInt;
  FT_Byte    =  Byte;
  FT_Bytes   = ^FT_Byte;
  FT_Tag     =  FT_UInt32;
  FT_String  =  AnsiChar;
  FT_Short   =  SmallInt;
  FT_UShort  =  Word;
  FT_Int     =  LongInt;
  FT_UInt    =  LongWord;
  FT_F2Dot14 =  SmallInt;
  FT_F26Dot6 =  FT_Long;
  FT_Fixed   =  FT_Long;
  FT_Error   =  LongInt;
  FT_Pointer =  Pointer;

  FT_UnitVector = record
    x: FT_F2Dot14;
    y: FT_F2Dot14;
  end;

  FT_Matrix = record
    xx, xy: FT_Fixed;
    yx, yy: FT_Fixed;
  end;

  FT_Data = record
    ptr:    ^FT_Byte;
    length:  FT_Int;
  end;

  FT_Generic_Finalizer = procedure(obj: Pointer); cdecl;

  FT_Generic = record
    data:      Pointer;
    finalizer: FT_Generic_Finalizer;
  end;

  FT_ListNode = ^FT_ListNodeRec;
  FT_List     = ^FT_ListRec;

  FT_ListNodeRec = record
    prev: FT_ListNode;
    next: FT_ListNode;
    data: Pointer;
  end;

  FT_ListRec = record
    head: FT_ListNode;
    tail: FT_ListNode;
  end;

// -------------------------------------------------------------------------- //
// fterrdef.h

const
  FT_Err_Ok                            = $00;
  FT_Err_Cannot_Open_Resource          = $01;
  FT_Err_Unknown_File_Format           = $02;
  FT_Err_Invalid_File_Format           = $03;
  FT_Err_Invalid_Version               = $04;
  FT_Err_Lower_Module_Version          = $05;
  FT_Err_Invalid_Argument              = $06;
  FT_Err_Unimplemented_Feature         = $07;
  FT_Err_Invalid_Table                 = $08;
  FT_Err_Invalid_Offset                = $09;
  FT_Err_Array_Too_Large               = $0A;
  FT_Err_Missing_Module                = $0B;
  FT_Err_Missing_Property              = $0C;
  FT_Err_Invalid_Glyph_Index           = $10;
  FT_Err_Invalid_Character_Code        = $11;
  FT_Err_Invalid_Glyph_Format          = $12;
  FT_Err_Cannot_Render_Glyph           = $13;
  FT_Err_Invalid_Outline               = $14;
  FT_Err_Invalid_Composite             = $15;
  FT_Err_Too_Many_Hints                = $16;
  FT_Err_Invalid_Pixel_Size            = $17;
  FT_Err_Invalid_Handle                = $20;
  FT_Err_Invalid_Library_Handle        = $21;
  FT_Err_Invalid_Driver_Handle         = $22;
  FT_Err_Invalid_Face_Handle           = $23;
  FT_Err_Invalid_Size_Handle           = $24;
  FT_Err_Invalid_Slot_Handle           = $25;
  FT_Err_Invalid_CharMap_Handle        = $26;
  FT_Err_Invalid_Cache_Handle          = $27;
  FT_Err_Invalid_Stream_Handle         = $28;
  FT_Err_Too_Many_Drivers              = $30;
  FT_Err_Too_Many_Extensions           = $31;
  FT_Err_Out_Of_Memory                 = $40;
  FT_Err_Unlisted_Object               = $41;
  FT_Err_Cannot_Open_Stream            = $51;
  FT_Err_Invalid_Stream_Seek           = $52;
  FT_Err_Invalid_Stream_Skip           = $53;
  FT_Err_Invalid_Stream_Read           = $54;
  FT_Err_Invalid_Stream_Operation      = $55;
  FT_Err_Invalid_Frame_Operation       = $56;
  FT_Err_Nested_Frame_Access           = $57;
  FT_Err_Invalid_Frame_Read            = $58;
  FT_Err_Raster_Uninitialized          = $60;
  FT_Err_Raster_Corrupted              = $61;
  FT_Err_Raster_Overflow               = $62;
  FT_Err_Raster_Negative_Height        = $63;
  FT_Err_Too_Many_Caches               = $70;
  FT_Err_Invalid_Opcode                = $80;
  FT_Err_Too_Few_Arguments             = $81;
  FT_Err_Stack_Overflow                = $82;
  FT_Err_Code_Overflow                 = $83;
  FT_Err_Bad_Argument                  = $84;
  FT_Err_Divide_By_Zero                = $85;
  FT_Err_Invalid_Reference             = $86;
  FT_Err_Debug_OpCode                  = $87;
  FT_Err_ENDF_In_Exec_Stream           = $88;
  FT_Err_Nested_DEFS                   = $89;
  FT_Err_Invalid_CodeRange             = $8A;
  FT_Err_Execution_Too_Long            = $8B;
  FT_Err_Too_Many_Function_Defs        = $8C;
  FT_Err_Too_Many_Instruction_Defs     = $8D;
  FT_Err_Table_Missing                 = $8E;
  FT_Err_Horiz_Header_Missing          = $8F;
  FT_Err_Locations_Missing             = $90;
  FT_Err_Name_Table_Missing            = $91;
  FT_Err_CMap_Table_Missing            = $92;
  FT_Err_Hmtx_Table_Missing            = $93;
  FT_Err_Post_Table_Missing            = $94;
  FT_Err_Invalid_Horiz_Metrics         = $95;
  FT_Err_Invalid_CharMap_Format        = $96;
  FT_Err_Invalid_PPem                  = $97;
  FT_Err_Invalid_Vert_Metrics          = $98;
  FT_Err_Could_Not_Find_Context        = $99;
  FT_Err_Invalid_Post_Table_Format     = $9A;
  FT_Err_Invalid_Post_Table            = $9B;
  FT_Err_DEF_In_Glyf_Bytecode          = $9C;
  FT_Err_Missing_Bitmap                = $9D;
  FT_Err_Syntax_Error                  = $A0;
  FT_Err_Stack_Underflow               = $A1;
  FT_Err_Ignore                        = $A2;
  FT_Err_No_Unicode_Glyph_Name         = $A3;
  FT_Err_Glyph_Too_Big                 = $A4;
  FT_Err_Missing_Startfont_Field       = $B0;
  FT_Err_Missing_Font_Field            = $B1;
  FT_Err_Missing_Size_Field            = $B2;
  FT_Err_Missing_Fontboundingbox_Field = $B3;
  FT_Err_Missing_Chars_Field           = $B4;
  FT_Err_Missing_Startchar_Field       = $B5;
  FT_Err_Missing_Encoding_Field        = $B6;
  FT_Err_Missing_Bbx_Field             = $B7;
  FT_Err_Bbx_Too_Big                   = $B8;
  FT_Err_Corrupted_Font_Header         = $B9;
  FT_Err_Corrupted_Font_Glyphs         = $BA;

// -------------------------------------------------------------------------- //
// freetype.h

type
  FT_Encoding = (
    FT_ENCODING_NONE           = $00000000,
    FT_ENCODING_MS_SYMBOL      = $73796d62,          // ['s', 'y', 'm', 'b']
    FT_ENCODING_UNICODE        = $756e6963,          // ['u', 'n', 'i', 'c']
    FT_ENCODING_SJIS           = $736a6973,          // ['s', 'j', 'i', 's']
    FT_ENCODING_PRC            = $67622020,          // ['g', 'b', ' ', ' ']
    FT_ENCODING_BIG5           = $62696735,          // ['b', 'i', 'g', '5']
    FT_ENCODING_WANSUNG        = $77616e73,          // ['w', 'a', 'n', 's']
    FT_ENCODING_JOHAB          = $6a6f6861,          // ['j', 'o', 'h', 'a']
    FT_ENCODING_GB2312         = FT_ENCODING_PRC,
    FT_ENCODING_MS_SJIS        = FT_ENCODING_SJIS,
    FT_ENCODING_MS_GB2312      = FT_ENCODING_GB2312,
    FT_ENCODING_MS_BIG5        = FT_ENCODING_BIG5,
    FT_ENCODING_MS_WANSUNG     = FT_ENCODING_WANSUNG,
    FT_ENCODING_MS_JOHAB       = FT_ENCODING_JOHAB,
    FT_ENCODING_ADOBE_STANDARD = $41444f42,          // ['A', 'D', 'O', 'B']
    FT_ENCODING_ADOBE_EXPERT   = $41444245,          // ['A', 'D', 'B', 'E']
    FT_ENCODING_ADOBE_CUSTOM   = $41444243,          // ['A', 'D', 'B', 'C']
    FT_ENCODING_ADOBE_LATIN_1  = $6c617431,          // ['l', 'a', 't', '1']
    FT_ENCODING_OLD_LATIN_2    = $6c617432,          // ['l', 'a', 't', '2']
    FT_ENCODING_APPLE_ROMAN    = $61726d6e           // ['a', 'r', 'm', 'n']
  );

  FT_Size_Request_Type = (
    FT_SIZE_REQUEST_TYPE_NOMINAL = 0,
    FT_SIZE_REQUEST_TYPE_REAL_DIM,
    FT_SIZE_REQUEST_TYPE_BBOX,
    FT_SIZE_REQUEST_TYPE_CELL,
    FT_SIZE_REQUEST_TYPE_SCALES,
    FT_SIZE_REQUEST_TYPE_MAX
  );

  FT_Render_Mode = (
    FT_RENDER_MODE_NORMAL = 0,
    FT_RENDER_MODE_LIGHT,
    FT_RENDER_MODE_MONO,
    FT_RENDER_MODE_LCD,
    FT_RENDER_MODE_LCD_V,
    FT_RENDER_MODE_SDF,
    FT_RENDER_MODE_MAX
  );

  FT_Kerning_Mode = (
    FT_KERNING_DEFAULT = 0,
    FT_KERNING_UNFITTED,
    FT_KERNING_UNSCALED
  );

const
  FT_FACE_FLAG_SCALABLE                  = 1 shl  0;
  FT_FACE_FLAG_FIXED_SIZES               = 1 shl  1;
  FT_FACE_FLAG_FIXED_WIDTH               = 1 shl  2;
  FT_FACE_FLAG_SFNT                      = 1 shl  3;
  FT_FACE_FLAG_HORIZONTAL                = 1 shl  4;
  FT_FACE_FLAG_VERTICAL                  = 1 shl  5;
  FT_FACE_FLAG_KERNING                   = 1 shl  6;
  FT_FACE_FLAG_FAST_GLYPHS               = 1 shl  7;
  FT_FACE_FLAG_MULTIPLE_MASTERS          = 1 shl  8;
  FT_FACE_FLAG_GLYPH_NAMES               = 1 shl  9;
  FT_FACE_FLAG_EXTERNAL_STREAM           = 1 shl 10;
  FT_FACE_FLAG_HINTER                    = 1 shl 11;
  FT_FACE_FLAG_CID_KEYED                 = 1 shl 12;
  FT_FACE_FLAG_TRICKY                    = 1 shl 13;
  FT_FACE_FLAG_COLOR                     = 1 shl 14;
  FT_FACE_FLAG_VARIATION                 = 1 shl 15;

  FT_STYLE_FLAG_ITALIC                   = 1;
  FT_STYLE_FLAG_BOLD                     = 2;

  FT_OPEN_MEMORY                         = $01;
  FT_OPEN_STREAM                         = $02;
  FT_OPEN_PATHNAME                       = $04;
  FT_OPEN_DRIVER                         = $08;
  FT_OPEN_PARAMS                         = $10;

  FT_LOAD_DEFAULT                        = 0;
  FT_LOAD_NO_SCALE                       = 1 shl  0;
  FT_LOAD_NO_HINTING                     = 1 shl  1;
  FT_LOAD_RENDER                         = 1 shl  2;
  FT_LOAD_NO_BITMAP                      = 1 shl  3;
  FT_LOAD_VERTICAL_LAYOUT                = 1 shl  4;
  FT_LOAD_FORCE_AUTOHINT                 = 1 shl  5;
  FT_LOAD_CROP_BITMAP                    = 1 shl  6;
  FT_LOAD_PEDANTIC                       = 1 shl  7;
  FT_LOAD_IGNORE_GLOBAL_ADVANCE_WIDTH    = 1 shl  9;
  FT_LOAD_NO_RECURSE                     = 1 shl 10;
  FT_LOAD_IGNORE_TRANSFORM               = 1 shl 11;
  FT_LOAD_MONOCHROME                     = 1 shl 12;
  FT_LOAD_LINEAR_DESIGN                  = 1 shl 13;
  FT_LOAD_NO_AUTOHINT                    = 1 shl 15;
  FT_LOAD_COLOR                          = 1 shl 20;
  FT_LOAD_COMPUTE_METRICS                = 1 shl 21;
  FT_LOAD_BITMAP_METRICS_ONLY            = 1 shl 22;
  FT_LOAD_ADVANCE_ONLY                   = 1 shl  8;
  FT_LOAD_SBITS_ONLY                     = 1 shl 14;

  FT_LOAD_TARGET_NORMAL                  = (Integer(FT_RENDER_MODE_NORMAL) and 15) shl 16;
  FT_LOAD_TARGET_LIGHT                   = (Integer(FT_RENDER_MODE_LIGHT)  and 15) shl 16;
  FT_LOAD_TARGET_MONO                    = (Integer(FT_RENDER_MODE_MONO)   and 15) shl 16;
  FT_LOAD_TARGET_LCD                     = (Integer(FT_RENDER_MODE_LCD)    and 15) shl 16;
  FT_LOAD_TARGET_LCD_V                   = (Integer(FT_RENDER_MODE_LCD_V)  and 15) shl 16;

  FT_SUBGLYPH_FLAG_ARGS_ARE_WORDS        = $001;
  FT_SUBGLYPH_FLAG_ARGS_ARE_XY_VALUES    = $002;
  FT_SUBGLYPH_FLAG_ROUND_XY_TO_GRID      = $004;
  FT_SUBGLYPH_FLAG_SCALE                 = $008;
  FT_SUBGLYPH_FLAG_XY_SCALE              = $040;
  FT_SUBGLYPH_FLAG_2X2                   = $080;
  FT_SUBGLYPH_FLAG_USE_MY_METRICS        = $200;

  FT_FSTYPE_INSTALLABLE_EMBEDDING        = $000;
  FT_FSTYPE_RESTRICTED_LICENSE_EMBEDDING = $002;
  FT_FSTYPE_PREVIEW_AND_PRINT_EMBEDDING  = $004;
  FT_FSTYPE_EDITABLE_EMBEDDING           = $008;
  FT_FSTYPE_NO_SUBSETTING                = $100;
  FT_FSTYPE_BITMAP_EMBEDDING_ONLY        = $200;

type
  FT_Library_ptr   = ^FT_Library;
  FT_Face_ptr      = ^FT_Face;
  FT_Open_Args_ptr = ^FT_Open_Args;
  FT_Parameter_ptr = ^FT_Parameter;

  FT_Glyph_Metrics = record
    width:        FT_Pos;
    height:       FT_Pos;
    horiBearingX: FT_Pos;
    horiBearingY: FT_Pos;
    horiAdvance:  FT_Pos;
    vertBearingX: FT_Pos;
    vertBearingY: FT_Pos;
    vertAdvance:  FT_Pos;
  end;

  FT_Bitmap_Size = record
    height: FT_Short;
    width:  FT_Short;
    size:   FT_Pos;
    x_ppem: FT_Pos;
    y_ppem: FT_Pos;
  end;

  FT_Library   = ^FT_LibraryRec;
  FT_Module    = ^FT_ModuleRec;
  FT_Driver    = ^FT_DriverRec;
  FT_Renderer  = ^FT_RendererRec;
  FT_Face      = ^FT_FaceRec;
  FT_Size      = ^FT_SizeRec;
  FT_GlyphSlot = ^FT_GlyphSlotRec;
  FT_CharMap   = ^FT_CharMapRec;

  FT_LibraryRec  = record end;
  FT_ModuleRec   = record end;
  FT_DriverRec   = record end;
  FT_RendererRec = record end;

  FT_CharMapRec = record
    face:        FT_Face;
    encoding:    FT_Encoding;
    platform_id: FT_UShort;
    encoding_id: FT_UShort;
  end;

  FT_Face_Internal    = ^FT_Face_InternalRec;
  FT_Face_InternalRec =  record end;

  FT_FaceRec = record
    num_faces:            FT_Long;
    face_index:           FT_Long;
    face_flags:           FT_Long;
    style_flags:          FT_Long;
    num_glyphs:           FT_Long;
    family_name:         ^FT_String;
    style_name:          ^FT_String;
    num_fixed_sizes:      FT_Int;
    available_sizes:     ^FT_Bitmap_Size;
    num_charmaps:         FT_Int;
    charmaps:            ^FT_CharMap;
    generic:              FT_Generic;
    bbox:                 FT_BBox;
    units_per_EM:         FT_UShort;
    ascender:             FT_Short;
    descender:            FT_Short;
    height:               FT_Short;
    max_advance_width:    FT_Short;
    max_advance_height:   FT_Short;
    underline_position:   FT_Short;
    underline_thickness:  FT_Short;
    glyph:                FT_GlyphSlot;
    size:                 FT_Size;
    charmap:              FT_CharMap;
    driver:               FT_Driver;
    memory:               FT_Memory;
    stream:               FT_Stream;
    sizes_list:           FT_ListRec;
    autohint:             FT_Generic;
    extensions:           Pointer;
    internal:             FT_Face_Internal;
  end;

  FT_Size_Internal    = ^FT_Size_InternalRec;
  FT_Size_InternalRec =  record end;

  FT_Size_Metrics = record
    x_ppem:      FT_UShort;
    y_ppem:      FT_UShort;
    x_scale:     FT_Fixed;
    y_scale:     FT_Fixed;
    ascender:    FT_Pos;
    descender:   FT_Pos;
    height:      FT_Pos;
    max_advance: FT_Pos;
  end;

  FT_SizeRec = record
    face:     FT_Face;
    generic:  FT_Generic;
    metrics:  FT_Size_Metrics;
    internal: FT_Size_Internal;
  end;

  FT_SubGlyph    = ^FT_SubGlyphRec;
  FT_SubGlyphRec =  record end;

  FT_Slot_Internal    = ^FT_Slot_InternalRec;
  FT_Slot_InternalRec =  record end;

  FT_GlyphSlotRec = record
    lib:               FT_Library;
    face:              FT_Face;
    next:              FT_GlyphSlot;
    glyph_index:       FT_UInt;
    generic:           FT_Generic;
    metrics:           FT_Glyph_Metrics;
    linearHoriAdvance: FT_Fixed;
    linearVertAdvance: FT_Fixed;
    advance:           FT_Vector;
    format:            FT_Glyph_Format;
    bitmap:            FT_Bitmap;
    bitmap_left:       FT_Int;
    bitmap_top:        FT_Int;
    outline:           FT_Outline;
    num_subglyphs:     FT_UInt;
    subglyphs:         FT_SubGlyph;
    control_data:      Pointer;
    control_len:       FT_Long;
    lsb_delta:         FT_Pos;
    rsb_delta:         FT_Pos;
    other:             Pointer;
    internal:          FT_Slot_Internal;
  end;

  FT_Parameter = record
    tag:  FT_ULong;
    data: FT_Pointer;
  end;

  FT_Open_Args = record
    flags:        FT_UInt;
    memory_base: ^FT_Byte;
    memory_size:  FT_Long;
    pathname:    ^FT_String;
    stream:       FT_Stream;
    driver:       FT_Module;
    num_params:   FT_Int;
    params:      ^FT_Parameter;
  end;

  FT_Size_Request = ^FT_Size_RequestRec;

  FT_Size_RequestRec = record
    typ:            FT_Size_Request_Type;
    width:          FT_Long;
    height:         FT_Long;
    horiResolution: FT_UInt;
    vertResolution: FT_UInt;
  end;

// -------------------------------------------------------------------------- //
//                             MACROS & FUNCTIONS                             //
// -------------------------------------------------------------------------- //
// macros

function FT_CURVE_TAG(flag: ShortInt): ShortInt;
function FT_IS_EMPTY(list: FT_List): Boolean;
function FT_HAS_HORIZONTAL(face: FT_Face): Boolean;
function FT_HAS_VERTICAL(face: FT_Face): Boolean;
function FT_HAS_KERNING(face: FT_Face): Boolean;
function FT_IS_SCALABLE(face: FT_Face): Boolean;
function FT_IS_SFNT(face: FT_Face): Boolean;
function FT_IS_FIXED_WIDTH(face: FT_Face): Boolean;
function FT_HAS_FIXED_SIZES(face: FT_Face): Boolean;
function FT_HAS_GLYPH_NAMES(face: FT_Face): Boolean;
function FT_HAS_MULTIPLE_MASTERS(face: FT_Face): Boolean;
function FT_IS_NAMED_INSTANCE(face: FT_Face): Boolean;
function FT_IS_VARIATION(face: FT_Face): Boolean;
function FT_IS_CID_KEYED(face: FT_Face): Boolean;
function FT_IS_TRICKY(face: FT_Face): Boolean;
function FT_HAS_COLOR(face: FT_Face): Boolean;
function FT_LOAD_TARGET_MODE(x: FT_Int32): FT_Render_Mode;

// -------------------------------------------------------------------------- //
// functions

function  FT_Init_FreeType(alib: FT_Library_ptr): FT_Error; cdecl; external FTLIB;
function  FT_Done_FreeType(lib: FT_Library): FT_Error; cdecl; external FTLIB;
function  FT_New_Face(lib: FT_Library; filename: PAnsiChar; face_index: FT_Long; aface: FT_Face_ptr): FT_Error; cdecl; external FTLIB;
function  FT_New_Memory_Face(lib: FT_Library; file_base: FT_Bytes; file_size: FT_Long; face_index: FT_Long; aface: FT_Face_ptr): FT_Error; cdecl; external FTLIB;
function  FT_Open_Face(lib: FT_Library; args: FT_Open_Args_ptr; face_index: FT_Long; aface: FT_Face_ptr): FT_Error; cdecl; external FTLIB;
function  FT_Attach_File(face: FT_Face; filename: PAnsiChar): FT_Error; cdecl; external FTLIB;
function  FT_Attach_Stream(face: FT_Face; parameters: FT_Open_Args_ptr): FT_Error; cdecl; external FTLIB;
function  FT_Reference_Face(face: FT_Face): FT_Error; cdecl; external FTLIB;
function  FT_Done_Face(face: FT_Face): FT_Error; cdecl; external FTLIB;
function  FT_Select_Size(face: FT_Face; strike_index: FT_Int): FT_Error; cdecl; external FTLIB;
function  FT_Request_Size(face: FT_Face; req: FT_Size_Request): FT_Error; cdecl; external FTLIB;
function  FT_Set_Char_Size(face: FT_Face; char_width: FT_F26Dot6; char_height: FT_F26Dot6; horz_resolution: FT_UInt; vert_resolution: FT_UInt): FT_Error; cdecl; external FTLIB;
function  FT_Set_Pixel_Sizes(face: FT_Face; pixel_width: FT_UInt; pixel_height: FT_UInt): FT_Error; cdecl; external FTLIB;
function  FT_Load_Glyph(face: FT_Face; glyph_index: FT_UInt; load_flags: FT_Int32): FT_Error; cdecl; external FTLIB;
function  FT_Load_Char(face: FT_Face; char_code: FT_ULong; load_flags: FT_Int32): FT_Error; cdecl; external FTLIB;
procedure FT_Set_Transform(face: FT_Face; matrix: FT_Matrix_ptr; delta: FT_Vector_ptr); cdecl; external FTLIB;
procedure FT_Get_Transform(face: FT_Face; matrix: FT_Matrix_ptr; delta: FT_Vector_ptr); cdecl; external FTLIB;
function  FT_Render_Glyph(slot: FT_GlyphSlot; render_mode: FT_Render_Mode): FT_Error; cdecl; external FTLIB;
function  FT_Get_Kerning(face: FT_Face; left_glyph: FT_UInt; right_glyph: FT_UInt; kern_mode: FT_UInt; akerning: FT_Vector_ptr): FT_Error; cdecl; external FTLIB;
function  FT_Get_Track_Kerning(face: FT_Face; point_size: FT_Fixed; degree: FT_Int; akerning: FT_Fixed_ptr): FT_Error; cdecl; external FTLIB;
function  FT_Get_Glyph_Name(face: FT_Face; glyph_index: FT_UInt; buffer: FT_Pointer; buffer_max: FT_UInt): FT_Error; cdecl; external FTLIB;
function  FT_Get_Postscript_Name(face: FT_Face): PAnsiChar; cdecl; external FTLIB;
function  FT_Select_Charmap(face: FT_Face; encoding: FT_Encoding): FT_Error; cdecl; external FTLIB;
function  FT_Set_Charmap(face: FT_Face; charmap: FT_CharMap): FT_Error; cdecl; external FTLIB;
function  FT_Get_Charmap_Index(charmap: FT_CharMap): FT_Int; cdecl; external FTLIB;
function  FT_Get_Char_Index(face: FT_Face; charcode: FT_ULong): FT_UInt; cdecl; external FTLIB;
function  FT_Get_First_Char(face: FT_Face; agindex: FT_UInt_ptr): FT_ULong; cdecl; external FTLIB;
function  FT_Get_Next_Char(face: FT_Face; char_code: FT_ULong; agindex: FT_UInt_ptr): FT_ULong; cdecl; external FTLIB;
function  FT_Face_Properties(face: FT_Face; num_properties: FT_UInt; properties: FT_Parameter_ptr): FT_Error; cdecl; external FTLIB;
function  FT_Get_Name_Index(face: FT_Face; glyph_name: FT_String_ptr): FT_UInt; cdecl; external FTLIB;
function  FT_Get_SubGlyph_Info(glyph: FT_GlyphSlot; sub_index: FT_UInt; p_index: FT_Int_ptr; p_flags: FT_UInt_ptr; p_arg1: FT_Int_ptr; p_arg2: FT_Int_ptr; p_transform: FT_Matrix_ptr): FT_Error; cdecl; external FTLIB;
function  FT_Get_FSType_Flags(face: FT_Face): FT_UShort; cdecl; external FTLIB;
function  FT_Face_GetCharVariantIndex(face: FT_Face; charcode: FT_ULong; variantSelector: FT_ULong): FT_UInt; cdecl; external FTLIB;
function  FT_Face_GetCharVariantIsDefault(face: FT_Face; charcode: FT_ULong; variantSelector: FT_ULong): FT_Int; cdecl; external FTLIB;
function  FT_Face_GetVariantSelectors(face: FT_Face): FT_UInt32_ptr; cdecl; external FTLIB;
function  FT_Face_GetVariantsOfChar(face: FT_Face; charcode: FT_ULong): FT_UInt32_ptr; cdecl; external FTLIB;
function  FT_Face_GetCharsOfVariant(face: FT_Face; variantSelector: FT_ULong): FT_UInt32_ptr; cdecl; external FTLIB;
function  FT_MulDiv(a, b, c: FT_Long): FT_Long; cdecl; external FTLIB;
function  FT_MulFix(a, b: FT_Long): FT_Long; cdecl; external FTLIB;
function  FT_DivFix(a, b: FT_Long): FT_Long; cdecl; external FTLIB;
function  FT_RoundFix(a: FT_Fixed): FT_Fixed; cdecl; external FTLIB;
function  FT_CeilFix(a: FT_Fixed): FT_Fixed; cdecl; external FTLIB;
function  FT_FloorFix(a: FT_Fixed): FT_Fixed; cdecl; external FTLIB;
procedure FT_Vector_Transform(vec: FT_Vector_ptr; matrix: FT_Matrix_ptr); cdecl; external FTLIB;
procedure FT_Library_Version(lib: FT_Library; amajor: FT_Int_ptr; aminor: FT_Int_ptr; apatch: FT_Int_ptr); cdecl; external FTLIB;
function  FT_Face_CheckTrueTypePatents(face: FT_Face): FT_Bool; cdecl; external FTLIB;
function  FT_Face_SetUnpatentedHinting(face: FT_Face; val: FT_Bool): FT_Bool; cdecl; external FTLIB;

// -------------------------------------------------------------------------- //

implementation

function FT_CURVE_TAG(flag: ShortInt): ShortInt;
begin
  Result := flag and 3;
end;

function FT_IS_EMPTY(list: FT_List): Boolean;
begin
  Result := (list.head = nil);
end;

function FT_HAS_HORIZONTAL(face: FT_Face): Boolean;
begin
  Result := (face.face_flags and FT_FACE_FLAG_HORIZONTAL) <> 0;
end;

function FT_HAS_VERTICAL(face: FT_Face): Boolean;
begin
  Result := (face.face_flags and FT_FACE_FLAG_VERTICAL) <> 0;
end;

function FT_HAS_KERNING(face: FT_Face): Boolean;
begin
  Result := (face.face_flags and FT_FACE_FLAG_KERNING) <> 0;
end;

function FT_IS_SCALABLE(face: FT_Face): Boolean;
begin
  Result := (face.face_flags and FT_FACE_FLAG_SCALABLE) <> 0;
end;

function FT_IS_SFNT(face: FT_Face): Boolean;
begin
  Result := (face.face_flags and FT_FACE_FLAG_SFNT) <> 0;
end;

function FT_IS_FIXED_WIDTH(face: FT_Face): Boolean;
begin
  Result := (face.face_flags and FT_FACE_FLAG_FIXED_WIDTH) <> 0;
end;

function FT_HAS_FIXED_SIZES(face: FT_Face): Boolean;
begin
  Result := (face.face_flags and FT_FACE_FLAG_FIXED_SIZES) <> 0;
end;

function FT_HAS_GLYPH_NAMES(face: FT_Face): Boolean;
begin
  Result := (face.face_flags and FT_FACE_FLAG_GLYPH_NAMES) <> 0;
end;

function FT_HAS_MULTIPLE_MASTERS(face: FT_Face): Boolean;
begin
  Result := (face.face_flags and FT_FACE_FLAG_MULTIPLE_MASTERS) <> 0;
end;

function FT_IS_NAMED_INSTANCE(face: FT_Face): Boolean;
begin
  Result := (face.face_index and $7FFF0000) <> 0;
end;

function FT_IS_VARIATION(face: FT_Face): Boolean;
begin
  Result := (face.face_flags and FT_FACE_FLAG_VARIATION) <> 0;
end;

function FT_IS_CID_KEYED(face: FT_Face): Boolean;
begin
  Result := (face.face_flags and FT_FACE_FLAG_CID_KEYED) <> 0;
end;

function FT_IS_TRICKY(face: FT_Face): Boolean;
begin
  Result := (face.face_flags and FT_FACE_FLAG_TRICKY) <> 0;
end;

function FT_HAS_COLOR(face: FT_Face): Boolean;
begin
  Result := (face.face_flags and FT_FACE_FLAG_COLOR) <> 0;
end;

function FT_LOAD_TARGET_MODE(x: FT_Int32): FT_Render_Mode;
begin
  Result := FT_Render_Mode((x shr 16) and 15);
end;
{$NOTES ON}
end.
