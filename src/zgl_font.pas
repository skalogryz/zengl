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
      mem_Read( fntMem, Result.CharDesc[ c ].Page, 4 );
      mem_Read( fntMem, Result.CharDesc[ c ].Width, 1 );
      mem_Read( fntMem, Result.CharDesc[ c ].Height, 1 );
      mem_Read( fntMem, Result.CharDesc[ c ].ShiftX, 4 );
      mem_Read( fntMem, Result.CharDesc[ c ].ShiftY, 4 );
      mem_Read( fntMem, Result.CharDesc[ c ].ShiftP, 4 );
      mem_Read( fntMem, Result.CharDesc[ c ].TexCoords[ 0 ], SizeOf( zglTPoint2D ) * 4 );
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
  case Byte( Text[ Pos ] ) of
    0..127: Result := Byte( Text[ Pos ] );
    192..255: Result := Byte( Text[ Pos ] ) + 848;
  else
    Result := 0;
  end;
end;

end.
