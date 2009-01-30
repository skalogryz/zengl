{
 * Copyright © Kemka Andrey aka Andru
 * mail: dr.andru@gmail.com
 * site: http://andru.2x4.ru
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
}
unit zgl_textures_tga;

{$I define.inc}

interface

uses
  zgl_file,
  zgl_memory,
  zgl_log;

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

procedure tga_Load( var pData : Pointer; var W, H : WORD );
procedure tga_LoadFromFile( const FileName : String; var pData : Pointer; var W, H : WORD );
procedure tga_LoadFromMemory( const Memory : zglTMemory; var pData : Pointer; var W, H : WORD );

procedure tga_FlipVertically( var Data : array of Byte; w, h, pixelSize : Integer );
procedure tga_FlipHorizontally( var Data : array of Byte; w, h, pixelSize : Integer );
procedure tga_RLEDecode;

implementation

var
  tgaMem     : zglTMemory;
  tgaHeader  : zglTTGAHeader;
  tgaData    : array of Byte;
  tgaDataA   : array of Byte;
  tgaPalette : array of Byte;

procedure tga_Load;
  label _exit;
  var
    i         : DWORD;
    n, base   : Integer;
    pixelSize : Integer;
    entry     : Byte;
begin
  mem_Read( tgaMem, tgaHeader, SizeOf( zglTTGAHeader ) );

  pixelSize := tgaHeader.ImgSpec.Depth shr 3;

  if tgaHeader.CPalType = 1 then
    begin
      with tgaHeader.CPalSpec do SetLength( tgaPalette, Length * EntrySize shr 3 );
      mem_Read( tgaMem, tgaPalette[ 0 ], Length( tgaPalette ) );
    end;

  if tgaHeader.ImageType >= 9 Then
    tga_RLEDecode
  else
    begin
      SetLength( tgaData, tgaHeader.ImgSpec.Width * tgaHeader.ImgSpec.Height * pixelSize );
      mem_Read( tgaMem, tgaData[ 0 ], Length( tgaData ) );
    end;

  if ( tgaHeader.ImgSpec.Desc and ( 1 shl 4 ) ) <> 0 Then
    tga_FlipHorizontally( tgaData, tgaHeader.ImgSpec.Width, tgaHeader.ImgSpec.Height, pixelSize );
  if ( tgaHeader.ImgSpec.Desc and ( 1 shl 5 ) ) <> 0 Then
    tga_FlipVertically( tgaData, tgaHeader.ImgSpec.Width, tgaHeader.ImgSpec.Height, pixelSize );

  if tgaHeader.ImageType <> 2 Then
    begin
      if ( tgaHeader.CPalType = 1 ) and ( tgaHeader.CPalSpec.EntrySize <> 24 ) Then
        begin
          log_Add( 'Unsupported color palette type in TGA-file!' );
          goto _exit;
        end;

      base := tgaHeader.CPalSpec.FirstEntry;
      n    := Length( tgaData );

      tgaHeader.ImageType     := 2;
      tgaHeader.ImgSpec.Depth := 24;
      tgaHeader.CPalType      := 0;
      FillChar ( tgaHeader.CPalSpec, SizeOf( tgaHeader.CPalSpec ), 0 );
      SetLength( tgaData, Length( tgaData ) * 3 );

      for i := n - 1 downto 0 do
        begin
          entry := tgaData[ i ];
          if tgaHeader.CPalType = 1 Then
            begin
              tgaData[ i * 3 + 0 ] := tgaPalette[ entry * 3 + 0 - base ];
              tgaData[ i * 3 + 1 ] := tgaPalette[ entry * 3 + 1 - base ];
              tgaData[ i * 3 + 2 ] := tgaPalette[ entry * 3 + 2 - base ];
            end else
              begin
                tgaData[ i * 3 + 0 ] := entry;
                tgaData[ i * 3 + 1 ] := entry;
                tgaData[ i * 3 + 2 ] := entry;
              end;
        end;
    end;

  if tgaHeader.ImgSpec.Depth shr 3 = 3 Then
    begin
      SetLength( tgaDataA, tgaHeader.ImgSpec.Width * tgaHeader.ImgSpec.Height * 4 );
      for i := 0 to tgaHeader.ImgSpec.Width * tgaHeader.ImgSpec.Height - 1 do
        begin
          tgaDataA[ i * 4 + 2 ] := tgaData[ i * 3 + 0 ];
          tgaDataA[ i * 4 + 1 ] := tgaData[ i * 3 + 1 ];
          tgaDataA[ i * 4 + 0 ] := tgaData[ i * 3 + 2 ];
          tgaDataA[ i * 4 + 3 ] := 255;
        end;
      pData := AllocMem( tgaHeader.ImgSpec.Width * tgaHeader.ImgSpec.Height * 4 );
      Move( Pointer( tgaDataA )^, pData^, tgaHeader.ImgSpec.Width * tgaHeader.ImgSpec.Height * 4 );
    end else
      if tgaHeader.ImgSpec.Depth shr 3 = 4 Then
        begin
          SetLength( tgaDataA, tgaHeader.ImgSpec.Width * tgaHeader.ImgSpec.Height * 4 );
          for i := 0 to tgaHeader.ImgSpec.Width * tgaHeader.ImgSpec.Height - 1 do
            begin
              tgaDataA[ i * 4 + 2 ] := tgaData[ i * 4 + 0 ];
              tgaDataA[ i * 4 + 1 ] := tgaData[ i * 4 + 1 ];
              tgaDataA[ i * 4 + 0 ] := tgaData[ i * 4 + 2 ];
              tgaDataA[ i * 4 + 3 ] := tgaData[ i * 4 + 3 ];
            end;
          pData := AllocMem( tgaHeader.ImgSpec.Width * tgaHeader.ImgSpec.Height * 4 );
          Move( Pointer( tgaDataA )^, pData^, tgaHeader.ImgSpec.Width * tgaHeader.ImgSpec.Height * 4 );
        end else
          pData := nil;
  W := tgaHeader.ImgSpec.Width;
  H := tgaHeader.ImgSpec.Height;

_exit:
  begin
    SetLength( tgaData, 0 );
    SetLength( tgaDataA, 0 );
    SetLength( tgaPalette, 0 );
    mem_Free( tgaMem );
  end;
end;

procedure tga_LoadFromFile;
begin
  mem_LoadFromFile( tgaMem, FileName );
  tga_Load( pData, W, H );
end;

procedure tga_LoadFromMemory;
begin
  tgaMem.Size     := Memory.Size;
  tgaMem.Memory   := Allocmem( Memory.Size );
  tgaMem.Position := Memory.Position;
  Move( Memory.Memory^, tgaMem.Memory^, Memory.Size );
  tga_Load( pData, W, H );
end;

procedure tga_FlipVertically;
  var
    i        : Integer;
    scanLine : array of Byte;
begin
  SetLength( scanLine, w * pixelSize );

  for i := 0 to h shr 1 - 1 do
    begin
      Move( Data[ i * w * pixelSize ], scanLine[ 0 ], w * pixelSize );
      Move( Data[ ( h - i - 1 ) * w * pixelSize ], Data[ i * w * pixelSize ], w * pixelSize );
      Move( scanLine[ 0 ], Data[( h - i - 1 ) * w * pixelSize ], w * pixelSize );
    end;
end;

procedure tga_FlipHorizontally;
  var
    scanLine : array of Byte;
    i, j, x  : Integer;
begin
  SetLength( scanLine, w * pixelSize );

  for i := 0 to h - 1 do
    begin
      Move( Data[ i * w * pixelSize ], scanLine[ 0 ], w * pixelSize );
      for x := 0 to w shr 1 do
        for j := 0 to pixelSize - 1 do
          scanLine[ x * pixelSize + j ] := scanLine[ ( w - 1 - x ) * pixelSize + j ];
    end;
end;

procedure tga_RLEDecode;
  var
    pixelSize : Integer;
    i, j, n   : Integer;
    packetHdr : Byte;
    packet    : array of Byte;
    packetLen : Byte;
begin
  pixelSize := tgaHeader.ImgSpec.Depth div 8;
  n         := tgaHeader.ImgSpec.Width * tgaHeader.ImgSpec.Height * pixelSize;
  SetLength( tgaData, n );

  SetLength( packet, pixelSize );

  i := 0;
  while i < n do
    begin
      mem_Read( tgaMem, packetHdr, 1 );
      packetLen := ( packetHdr and $7F ) + 1;
      if ( packetHdr and $80 ) <> 0 Then
        begin
          mem_Read( tgaMem, packet[ 0 ], pixelSize );
          for j := 0 to ( packetLen * pixelSize ) - 1 do
            begin
              tgaData[ i ] := packet[ j mod pixelSize ];
              INC( i );
            end;
        end else
          begin
            for j := 0 to (packetLen * pixelSize) - 1 do
              begin
                mem_Read( tgaMem, packet[ j mod pixelSize ], 1 );
                tgaData[ i ] := packet[ j mod pixelSize ];
                INC( i );
             end;
          end;
    end;

  tgaHeader.ImageType := tgaHeader.ImageType - 8;
end;

end.
