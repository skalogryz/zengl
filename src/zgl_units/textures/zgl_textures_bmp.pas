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
unit zgl_textures_bmp;

{$I define.inc}

interface

uses
  zgl_types,
  zgl_file,
  zgl_memory,
  zgl_log;
  
const
  BI_RGB       = 0;
  BI_RLE8      = 1;
  BI_RLE4      = 2;
  BI_BITFIELDS = 3;
  BI_JPEG      = 4;
  BI_PNG       = 5;
  
type
  zglPBMPFileHeader = ^zglTBmpFileHeader;
  zglTBMPFileHeader = packed record
    bfType      : WORD;
    bfSize      : DWORD;
    bfReserved1 : WORD;
    bfReserved2 : WORD;
    bfOffBits   : DWORD;
end;
  
type
  zglPBMPInfoHeader = ^zglTBMPInfoHeader;
  zglTBMPInfoHeader = packed record
    biSize          : DWORD;
    biWidth         : Integer;
    biHeight        : Integer;
    biPlanes        : Word;
    biBitCount      : Word;
    biCompression   : DWORD;
    biSizeImage     : DWORD;
    biXPelsPerMeter : Integer;
    biYPelsPerMeter : Integer;
    biClrUsed       : DWORD;
    biClrImportant  : DWORD;
end;

procedure bmp_LoadFromFile( FileName : PChar; var pData : Pointer; var W, H : WORD ); extdecl;

implementation

var
  bmpMem         : zglTMemory;
  bmpFileHeader  : zglTBmpFileHeader;
  bmpInfoHeader  : zglTBMPInfoHeader;
  bmpScanLine    : array of Byte;
  bmpData        : array of Byte;
  bmpPalette     : array of Byte;
  bmpPaletteSize : DWORD;

procedure bmp_LoadFromFile;
  label _exit;
  var
    i, j       : DWORD;
    Index      : Byte;
begin
  FillChar( bmpFileHeader, SizeOf( zglTBMPFileHeader ), 0 );
  FillChar( bmpInfoHeader, SizeOf( zglTBMPInfoHeader ), 0 );

  mem_LoadFromFile( bmpMem, FileName );
  mem_Read( bmpMem, bmpFileHeader, SizeOf( zglTBMPFileHeader ) );
  mem_Read( bmpMem, bmpInfoHeader, SizeOf( zglTBMPInfoHeader ) );

  if ( bmpFileHeader.bfType <> 19778 ) or ( bmpInfoHeader.biPlanes <> 1 ) Then goto _exit;

  SetLength( bmpData, bmpInfoHeader.biWidth * bmpInfoHeader.biHeight * 4 );
  SetLength( bmpScanLine, bmpInfoHeader.biWidth );
  
  if bmpInfoHeader.biBitCount < 24 Then
    begin
      bmpPaletteSize := bmpInfoHeader.biClrUsed;
      if bmpPaletteSize = 0 Then bmpPaletteSize := 256;

      SetLength( bmpPalette, bmpPaletteSize * 4 );
      mem_Read( bmpMem, bmpPalette[ 0 ], bmpPaletteSize * 4 );
      mem_Seek( bmpMem, bmpFileHeader.bfOffBits, FSM_SET );
    end;

  case bmpInfoHeader.biBitCount of
    4:
      begin
        for j := 0 to bmpInfoHeader.biHeight - 1 do
          begin
            mem_Read( bmpMem, bmpScanLine[ 0 ], bmpInfoHeader.biWidth div 2 );

            for i := 0 to bmpInfoHeader.biWidth - 1 do
              begin
                Index := bmpScanLine[ i div 2 ];

                if i mod 2 = 0 Then
                  Index := Index and 255
                else
                  Index := Index shr 4;

                bmpData[ j * bmpInfoHeader.biWidth * 4 + i * 4 + 2 ] := bmpPalette[ Index * 4 + 0 ];
                bmpData[ j * bmpInfoHeader.biWidth * 4 + i * 4 + 1 ] := bmpPalette[ Index * 4 + 1 ];
                bmpData[ j * bmpInfoHeader.biWidth * 4 + i * 4 + 0 ] := bmpPalette[ Index * 4 + 2 ];
                bmpData[ j * bmpInfoHeader.biWidth * 4 + i * 4 + 3 ] := 255;
              end;
          end;
      end;
    8:
      begin
        for j := 0 to bmpInfoHeader.biHeight - 1 do
          begin
            if bmpInfoHeader.biCompression = BI_RGB Then
              mem_Read( bmpMem, bmpScanLine[ 0 ], bmpInfoHeader.biWidth )
            else
              begin
                SetLength( bmpData, 0 );
                
                log_Add( 'Unsupport RLE Bitmaps' );
                goto _exit; // пока RLE не поддерживается...
              end;

            for i := 0 to bmpInfoHeader.biWidth - 1 do
              begin
                Index := bmpScanLine[ i ];
                bmpData[ j * bmpInfoHeader.biWidth * 4 + i * 4 + 2 ] := bmpPalette[ Index * 4 + 0 ];
                bmpData[ j * bmpInfoHeader.biWidth * 4 + i * 4 + 1 ] := bmpPalette[ Index * 4 + 1 ];
                bmpData[ j * bmpInfoHeader.biWidth * 4 + i * 4 + 0 ] := bmpPalette[ Index * 4 + 2 ];
                bmpData[ j * bmpInfoHeader.biWidth * 4 + i * 4 + 3 ] := 255;
              end;
            end;
      end;
    24:
      begin
        for j := 0 to bmpInfoHeader.biHeight - 1 do
          for i := 0 to bmpInfoHeader.biWidth - 1 do
            begin
              mem_Read( bmpMem, bmpData[ j * bmpInfoHeader.biWidth * 4 + i * 4 + 2 ], 1 );
              mem_Read( bmpMem, bmpData[ j * bmpInfoHeader.biWidth * 4 + i * 4 + 1 ], 1 );
              mem_Read( bmpMem, bmpData[ j * bmpInfoHeader.biWidth * 4 + i * 4 + 0 ], 1 );
              bmpData[ j * bmpInfoHeader.biWidth * 4 + i * 4 + 3 ] := 255;
            end;
      end;
    32:
      begin
        for j := 0 to bmpInfoHeader.biHeight - 1 do
          for i := 0 to bmpInfoHeader.biWidth - 1 do
            begin
              mem_Read( bmpMem, bmpData[ j * bmpInfoHeader.biWidth * 4 + i * 4 + 2 ], 4 );
              mem_Read( bmpMem, bmpData[ j * bmpInfoHeader.biWidth * 4 + i * 4 + 1 ], 1 );
              mem_Read( bmpMem, bmpData[ j * bmpInfoHeader.biWidth * 4 + i * 4 + 0 ], 1 );
              mem_Read( bmpMem, bmpData[ j * bmpInfoHeader.biWidth * 4 + i * 4 + 3 ], 1 );
            end;
      end;
  else
    goto _exit;
  end;

  if ( bmpInfoHeader.biBitCount = 4  ) or ( bmpInfoHeader.biBitCount = 8 ) or
     ( bmpInfoHeader.biBitCount = 16 ) or ( bmpInfoHeader.biBitCount = 24 ) or
     ( bmpInfoHeader.biBitCount = 32 ) Then
    begin
      pData := AllocMem( bmpInfoHeader.biWidth * bmpInfoHeader.biHeight * 4 );
      Move( Pointer( bmpData )^, pData^, bmpInfoHeader.biWidth * bmpInfoHeader.biHeight * 4 );
    end else
      pData := nil;
  W := bmpInfoHeader.biWidth;
  H := bmpInfoHeader.biHeight;

_exit:
  begin
    SetLength( bmpScanLine, 0 );
    SetLength( bmpData, 0 );
    SetLength( bmpPalette, 0 );
    mem_Free( bmpMem );
  end;
end;

end.
