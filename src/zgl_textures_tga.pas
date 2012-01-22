{
 *  Copyright © Kemka Andrey aka Andru
 *  mail: dr.andru@gmail.com
 *  site: http://zengl.org
 *
 *  This file is part of ZenGL.
 *
 *  ZenGL is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as
 *  published by the Free Software Foundation, either version 3 of
 *  the License, or (at your option) any later version.
 *
 *  ZenGL is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with ZenGL. If not, see http://www.gnu.org/licenses/
}
unit zgl_textures_tga;

{$I zgl_config.cfg}

interface

uses
  zgl_file,
  zgl_memory;

const
  TGA_EXTENSION : UTF8String = 'TGA';

type
  zglPTGAHeader = ^zglTTGAHeader;
  zglTTGAHeader = packed record
    IDLength  : Byte;
    CPalType  : Byte;
    ImageType : Byte;
    CPalSpec  : packed record
      FirstEntry : Word;
      Length     : Word;
      EntrySize  : Byte;
    end;
    ImgSpec: packed record
      X      : Word;
      Y      : Word;
      Width  : Word;
      Height : Word;
      Depth  : Byte;
      Desc   : Byte;
    end;
end;

procedure tga_LoadFromFile( const FileName : UTF8String; var Data : Pointer; var W, H, Format : Word );
procedure tga_LoadFromMemory( const Memory : zglTMemory; var Data : Pointer; var W, H, Format : Word );

implementation
uses
  zgl_types,
  zgl_main,
  zgl_log,
  zgl_textures;

procedure tga_FlipVertically( var Data : Pointer; w, h : Integer );
  var
    i        : Integer;
    scanLine : Pointer;
begin
  GetMem( scanLine, w * 4 );

  for i := 0 to h shr 1 - 1 do
    begin
      Move( Pointer( Ptr( Data ) + i * w * 4 )^, scanLine^, w * 4 );
      Move( Pointer( Ptr( Data ) + ( h - i - 1 ) * w * 4 )^, Pointer( Ptr( Data ) + i * w * 4 )^, w * 4 );
      Move( scanLine^, Pointer( Ptr( Data ) + ( h - i - 1 ) * w * 4 )^, w * 4 );
    end;

  FreeMem( scanLine );
end;

procedure tga_FlipHorizontally( var Data : Pointer; w, h : Integer );
  var
    i, x     : Integer;
    scanLine : Pointer;
begin
  GetMem( scanLine, w * 4 );

  for i := 0 to h - 1 do
    begin
      Move( Pointer( Ptr( Data ) + i * w * 4 )^, scanLine^, w * 4 );
      for x := 0 to w - 1 do
        PLongWord( Ptr( Data ) +  i * w * 4 + x * 4 )^ := PLongWord( Ptr( scanLine ) + ( w - 1 - x ) * 4 )^;
    end;

  FreeMem( scanLine );
end;

function tga_RLEDecode( var tgaMem : zglTMemory; var Header : zglTTGAHeader; var Data : PByte ) : LongWord;
  var
    i, j      : Integer;
    pixelSize : Integer;
    packetHdr : Byte;
    packet    : array[ 0..3 ] of Byte;
    packetLen : Byte;
begin
  pixelSize := Header.ImgSpec.Depth shr 3;
  Result    := Header.ImgSpec.Width * Header.ImgSpec.Height * pixelSize;
  GetMem( Data, Result );

  i := 0;
  while i < Result do
    begin
      mem_Read( tgaMem, packetHdr, 1 );
      packetLen := ( packetHdr and $7F ) + 1;
      if ( packetHdr and $80 ) <> 0 Then
        begin
          mem_Read( tgaMem, packet[ 0 ], pixelSize );
          for j := 0 to ( packetLen * pixelSize ) - 1 do
            begin
              Data^ := packet[ j mod pixelSize ];
              INC( Data );
              INC( i );
            end;
        end else
          for j := 0 to ( packetLen * pixelSize ) - 1 do
            begin
              mem_Read( tgaMem, packet[ j mod pixelSize ], 1 );
              Data^ := packet[ j mod pixelSize ];
              INC( Data );
              INC( i );
            end;
    end;
  DEC( Data, i );

  Header.ImageType := Header.ImageType - 8;
end;

function tga_PaletteDecode( var tgaMem : zglTMemory; var Header : zglTTGAHeader; var Data : PByte; Palette : PByte ) : Boolean;
  var
    i, base : Integer;
    size    : Integer;
    entry   : Byte;
begin
  if ( Header.CPalType = 1 ) and ( Header.CPalSpec.EntrySize <> 24 ) Then
    begin
      log_Add( 'Unsupported color palette type in TGA-file!' );
      Result := FALSE;
      exit;
    end;

  size := Header.ImgSpec.Width * Header.ImgSpec.Height;
  base := Header.CPalSpec.FirstEntry;
  ReallocMem( Data, size * 3 );

  if Header.CPalType = 1 Then
    begin
      for i := size - 1 downto 0 do
        begin
          entry := PByte( Ptr( Data ) + i )^;
          PByte( Ptr( Data ) + i * 3 + 0 )^ := PByte( Ptr( Palette ) + entry * 3 + 0 - base )^;
          PByte( Ptr( Data ) + i * 3 + 1 )^ := PByte( Ptr( Palette ) + entry * 3 + 1 - base )^;
          PByte( Ptr( Data ) + i * 3 + 2 )^ := PByte( Ptr( Palette ) + entry * 3 + 2 - base )^;
        end;
    end else
      for i := size - 1 downto 0 do
        begin
          entry := PByte( Ptr( Data ) + i )^;
          PByte( Ptr( Data ) + i * 3 + 0 )^ := entry;
          PByte( Ptr( Data ) + i * 3 + 1 )^ := entry;
          PByte( Ptr( Data ) + i * 3 + 2 )^ := entry;
        end;

  Header.ImageType     := 2;
  Header.ImgSpec.Depth := 24;
  Header.CPalType      := 0;
  FillChar( Header.CPalSpec, SizeOf( Header.CPalSpec ), 0 );

  Result := TRUE;
end;

procedure tga_LoadFromFile( const FileName : UTF8String; var Data : Pointer; var W, H, Format : Word );
  var
    tgaMem : zglTMemory;
begin
  mem_LoadFromFile( tgaMem, FileName );
  tga_LoadFromMemory( tgaMem, Data, W, H, Format );
  mem_Free( tgaMem );
end;

procedure tga_LoadFromMemory( const Memory : zglTMemory; var Data : Pointer; var W, H, Format : Word );
  label _exit;
  var
    i, size    : Integer;
    tgaMem     : zglTMemory;
    tgaHeader  : zglTTGAHeader;
    tgaData    : PByte;
    tgaPalette : array of Byte;
begin
  tgaMem := Memory;
  mem_Read( tgaMem, tgaHeader, SizeOf( zglTTGAHeader ) );

  if tgaHeader.CPalType = 1 then
    begin
      with tgaHeader.CPalSpec do SetLength( tgaPalette, Length * EntrySize shr 3 );
      mem_Read( tgaMem, tgaPalette[ 0 ], Length( tgaPalette ) );
    end;

  if tgaHeader.ImageType >= 9 Then
    size := tga_RLEDecode( tgaMem, tgaHeader, tgaData )
  else
    begin
      size := tgaHeader.ImgSpec.Width * tgaHeader.ImgSpec.Height * ( tgaHeader.ImgSpec.Depth shr 3 );
      GetMem( tgaData, size );
      mem_Read( tgaMem, tgaData^, size );
    end;

  if tgaHeader.ImageType <> 2 Then
    if not tga_PaletteDecode( tgaMem, tgaHeader, tgaData, @tgaPalette[ 0 ] ) Then
      goto _exit;

  if tgaHeader.ImgSpec.Depth shr 3 = 3 Then
    begin
      GetMem( Data, tgaHeader.ImgSpec.Width * tgaHeader.ImgSpec.Height * 4 );
      for i := 0 to tgaHeader.ImgSpec.Width * tgaHeader.ImgSpec.Height - 1 do
        begin
          PByte( Ptr( Data ) + i * 4 + 2 )^ := PByte( Ptr( tgaData ) + 0 )^;
          PByte( Ptr( Data ) + i * 4 + 1 )^ := PByte( Ptr( tgaData ) + 1 )^;
          PByte( Ptr( Data ) + i * 4 + 0 )^ := PByte( Ptr( tgaData ) + 2 )^;
          PByte( Ptr( Data ) + i * 4 + 3 )^ := 255;
          INC( tgaData, 3 );
        end;
      DEC( tgaData, tgaHeader.ImgSpec.Width * tgaHeader.ImgSpec.Height * 3 );
    end else
      if tgaHeader.ImgSpec.Depth shr 3 = 4 Then
        begin
          GetMem( Data, tgaHeader.ImgSpec.Width * tgaHeader.ImgSpec.Height * 4 );
          for i := 0 to tgaHeader.ImgSpec.Width * tgaHeader.ImgSpec.Height - 1 do
            begin
              PByte( Ptr( Data ) + i * 4 + 2 )^ := PByte( Ptr( tgaData ) + 0 )^;
              PByte( Ptr( Data ) + i * 4 + 1 )^ := PByte( Ptr( tgaData ) + 1 )^;
              PByte( Ptr( Data ) + i * 4 + 0 )^ := PByte( Ptr( tgaData ) + 2 )^;
              PByte( Ptr( Data ) + i * 4 + 3 )^ := PByte( Ptr( tgaData ) + 3 )^;
              INC( tgaData, 4 );
            end;
          DEC( tgaData, tgaHeader.ImgSpec.Width * tgaHeader.ImgSpec.Height * 4 );
        end;

  W      := tgaHeader.ImgSpec.Width;
  H      := tgaHeader.ImgSpec.Height;
  Format := TEX_FORMAT_RGBA;

  if ( tgaHeader.ImgSpec.Desc and ( 1 shl 4 ) ) <> 0 Then
    tga_FlipHorizontally( Data, W, H );
  if ( tgaHeader.ImgSpec.Desc and ( 1 shl 5 ) ) <> 0 Then
    tga_FlipVertically( Data, W, H );

_exit:
  begin
    FreeMem( tgaData );
    SetLength( tgaPalette, 0 );
  end;
end;

{$IFDEF USE_TGA}
initialization
  zgl_Reg( TEX_FORMAT_EXTENSION,   @TGA_EXTENSION[ 1 ] );
  zgl_Reg( TEX_FORMAT_FILE_LOADER, @tga_LoadFromFile );
  zgl_Reg( TEX_FORMAT_MEM_LOADER,  @tga_LoadFromMemory );
{$ENDIF}

end.
