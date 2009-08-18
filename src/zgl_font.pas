{
 * Copyright © Kemka Andrey aka Andru
 * mail: dr.andru@gmail.com
 * site: http://andru-kun.inf.ua
 *
 * This file is part of ZenGL
 *
 * ZenGL is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * ZenGL is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
}
unit zgl_font;

{$I zgl_config.cfg}

interface
uses
  zgl_types,
  zgl_textures,
  zgl_math_2d,
  zgl_file,
  zgl_memory;

const
  ZGL_FONT_INFO = 'ZGL_FONT_INFO';

  CP1251_TO_UTF8 : array[ 0..255 ] of WORD =
  ( 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,
    33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,
    63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,
    93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,
    117,118,119,120,121,122,123,124,125,126,127,1026,1027,8218,1107,8222,8230,8224,8225,8364,
    8240,1033,8249,1034,1036,1035,1039,1106,8216,8217,8220,8221,8226,8211,8212,8250,8482,1113,
    8250,1114,1116,1115,1119,160,1038,1118,1032,164,1168,166,167,1025,169,1028,171,172,173,174,
    1031,176,177,1030,1110,1169,181,182,183,1105,8470,1108,187,1112,1029,1109,1111,1040,1041,
    1042,1043,1044,1045,1046,1047,1048,1049,1050,1051,1052,1053,1054,1055,1056,1057,1058,1059,
    1060,1061,1062,1063,1064,1065,1066,1067,1068,1069,1070,1071,1072,1073,1074,1075,1076,1077,
    1078,1079,1080,1081,1082,1083,1084,1085,1086,1087,1088,1089,1090,1091,1092,1093,1094,1095,
    1096,1097,1098,1099,1100,1101,1102,1103 );

type
  zglPCharDesc = ^zglTCharDesc;
  zglTCharDesc = record
    Page      : WORD;
    Width     : Byte;
    Height    : Byte;
    ShiftX    : Integer;
    ShiftY    : Integer;
    ShiftP    : Integer;
    TexCoords : array[ 0..3 ] of zglTPoint2D;
end;

type
  zglPFont = ^zglTFont;
  zglTFont = record
    Count      : record
      Pages : WORD;
      Chars : WORD;
                 end;

    Pages      : array of zglPTexture;
    CharDesc   : array[ 0..65535 ] of zglPCharDesc;
    MaxHeight  : Integer;
    MaxShiftY  : Integer;
    Padding    : array[ 0..3 ] of Byte;

    Prev, Next : zglPFont;
end;

type
  zglPFontManager = ^zglTFontManager;
  zglTFontManager = record
    Count : DWORD;
    First : zglTFont;
end;

function  font_Add : zglPFont;
procedure font_Del( var Font : zglPFont );

function font_Load : zglPFont;
function font_LoadFromFile( const FileName : AnsiString ) : zglPFont;
function font_LoadFromMemory( const Memory : zglTMemory ) : zglPFont;

function font_GetUTF8ID( const Text : AnsiString; const Pos : Integer; const Shift : PInteger ) : DWORD;
function font_GetCP1251ID( const Text : AnsiString; const Pos : Integer; const Shift : PInteger ) : DWORD;

var
  managerFont : zglTFontManager;
  font_GetCID : function( const Text : AnsiString; const Pos : Integer; const Shift : PInteger ) : DWORD;

implementation
uses
  zgl_main,
  zgl_log;

var
  fntMem : zglTMemory;
  fntID  : array[ 0..12 ] of AnsiChar;

function font_Add;
begin
  Result := @managerFont.First;
  while Assigned( Result.Next ) do
    Result := Result.Next;

  zgl_GetMem( Pointer( Result.Next ), SizeOf( zglTFont ) );
  Result.Next.Prev     := Result;
  Result.Next.Next     := nil;
  Result := Result.Next;
  INC( managerFont.Count );
end;

procedure font_Del;
  var
    i : Integer;
begin
  if not Assigned( Font ) Then exit;

  for i := 0 to 65535 do
    if Assigned( Font.CharDesc[ i ] ) Then
      FreeMemory( Font.CharDesc[ i ] );
  if Assigned( Font.Prev ) Then
    Font.Prev.Next := Font.Next;
  if Assigned( Font.Next ) Then
    Font.Next.Prev := Font.Prev;
  FreeMemory( Font );
  DEC( managerFont.Count );

  Font := nil;
end;

function font_Load;
  var
    i : Integer;
    c : DWORD;
begin
  Result := font_Add;
  mem_Read( fntMem, Result.Count.Pages,  2 );
  mem_Read( fntMem, Result.Count.Chars,  2 );
  mem_Read( fntMem, Result.MaxHeight,    4 );
  mem_Read( fntMem, Result.MaxShiftY,    4 );
  mem_Read( fntMem, Result.Padding[ 0 ], 4 );
  SetLength( Result.Pages, Result.Count.Pages );
  for i := 0 to Result.Count.Chars - 1 do
    begin
      mem_Read( fntMem, c, 4 );
      zgl_GetMem( Pointer( Result.CharDesc[ c ] ), SizeOf( zglTCharDesc ) );
      {$IFDEF ENDIAN_BIG}
      forceNoSwap := TRUE;
      {$ENDIF}
      mem_Read( fntMem, Result.CharDesc[ c ].Page, 4 );
      {$IFDEF ENDIAN_BIG}
      forceNoSwap := FALSE;
      {$ENDIF}
      mem_Read( fntMem, Result.CharDesc[ c ].Width, 1 );
      mem_Read( fntMem, Result.CharDesc[ c ].Height, 1 );
      mem_Read( fntMem, Result.CharDesc[ c ].ShiftX, 4 );
      mem_Read( fntMem, Result.CharDesc[ c ].ShiftY, 4 );
      mem_Read( fntMem, Result.CharDesc[ c ].ShiftP, 4 );
      {$IFDEF ENDIAN_BIG}
      mem_Read( fntMem, Result.CharDesc[ c ].TexCoords[ 0 ].X, 4 );
      mem_Read( fntMem, Result.CharDesc[ c ].TexCoords[ 0 ].Y, 4 );
      mem_Read( fntMem, Result.CharDesc[ c ].TexCoords[ 1 ].X, 4 );
      mem_Read( fntMem, Result.CharDesc[ c ].TexCoords[ 1 ].Y, 4 );
      mem_Read( fntMem, Result.CharDesc[ c ].TexCoords[ 2 ].X, 4 );
      mem_Read( fntMem, Result.CharDesc[ c ].TexCoords[ 2 ].Y, 4 );
      mem_Read( fntMem, Result.CharDesc[ c ].TexCoords[ 3 ].X, 4 );
      mem_Read( fntMem, Result.CharDesc[ c ].TexCoords[ 3 ].Y, 4 );
      {$ELSE}
      mem_Read( fntMem, Result.CharDesc[ c ].TexCoords[ 0 ], SizeOf( zglTPoint2D ) * 4 );
      {$ENDIF}
  end;
  mem_Free( fntMem );
end;

function font_LoadFromFile;
begin
  Result := nil;
  if not file_Exists( FileName ) Then
    begin
      log_Add( 'Cannot read ' + FileName );
      exit;
    end;

  mem_LoadFromFile( fntMem, FileName );
  mem_Read( fntMem, fntID, 13 );
  if fntID <> ZGL_FONT_INFO Then
    begin
      log_Add( FileName + ' - it''s not a ZenGL font info file' );
      exit;
    end else
      Result := font_Load;
end;

function font_LoadFromMemory;
begin
  Result := nil;
  fntMem.Size     := Memory.Size;
  zgl_GetMem( fntMem.Memory, Memory.Size );
  fntMem.Position := Memory.Position;
  Move( Memory.Memory^, fntMem.Memory^, Memory.Size );

  mem_Read( fntMem, fntID, 13 );
  if fntID <> ZGL_FONT_INFO Then
    begin
      log_Add( 'Unable to determinate ZenGL font info: From Memory' );
      exit;
    end else
      Result := font_Load;
end;

function font_GetUTF8ID;
begin
  case Byte( Text[ Pos ] ) of
    0..127:
      begin
        Result := Byte( Text[ Pos ] );
        if Assigned( Shift ) Then
          Shift^ := Pos + 1;
      end;

    192..223:
      begin
        Result := ( Byte( Text[ Pos ] ) - 192 ) * 64 +
                  ( Byte( Text[ Pos + 1 ] ) - 128 );
        if Assigned( Shift ) Then
          Shift^ := Pos + 2;
      end;

    224..239:
      begin
        Result := ( Byte( Text[ Pos ] ) - 224 ) * 4096 +
                  ( Byte( Text[ Pos + 1 ] ) - 128 ) * 64 +
                  ( Byte( Text[ Pos + 2 ] ) - 128 );
        if Assigned( Shift ) Then
          Shift^ := Pos + 3;
      end;

    240..247:
      begin
        Result := ( Byte( Text[ Pos ] ) - 240 ) * 262144 +
                  ( Byte( Text[ Pos + 1 ] ) - 128 ) * 4096 +
                  ( Byte( Text[ Pos + 2 ] ) - 128 ) * 64 +
                  ( Byte( Text[ Pos + 3 ] ) - 128 );
        if Assigned( Shift ) Then
          Shift^ := Pos + 4;
      end;

    248..251:
      begin
        Result := ( Byte( Text[ Pos ] ) - 248 ) * 16777216 +
                  ( Byte( Text[ Pos + 1 ] ) - 128 ) * 262144 +
                  ( Byte( Text[ Pos + 2 ] ) - 128 ) * 4096 +
                  ( Byte( Text[ Pos + 3 ] ) - 128) * 64 +
                  ( Byte( Text[ Pos + 4 ] ) - 128 );
        if Assigned( Shift ) Then
          Shift^ := Pos + 5;
      end;

    252..253:
      begin
        Result := ( Byte( Text[ Pos ] ) - 252 ) * 1073741824 +
                  ( Byte( Text[ Pos + 1 ] ) - 128 ) * 16777216 +
                  ( Byte( Text[ Pos + 2 ] ) - 128 ) * 262144 +
                  ( Byte( Text[ Pos + 3 ] ) - 128 ) * 4096 +
                  ( Byte( Text[ Pos + 4 ] ) - 128 ) * 64 +
                  ( Byte( Text[ Pos + 5 ] ) - 128 );
        if Assigned( Shift ) Then
          Shift^ := Pos + 6;
      end;

    254..255:
      begin
        Result := 0;
        if Assigned( Shift ) Then
          Shift^ := Pos + 1;
      end;
  else
    Result := 0;
    if Assigned( Shift ) Then
      Shift^ := Pos + 1;
  end;
end;

function font_GetCP1251ID;
begin
  if Assigned( Shift ) Then
    Shift^ := Pos + 1;
  Result := CP1251_TO_UTF8[ Byte( Text[ Pos ] ) ];
end;


end.
