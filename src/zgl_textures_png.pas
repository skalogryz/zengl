{
 *  Copyright © Kemka Andrey aka Andru
 *  mail: dr.andru@gmail.com
 *  site: http://andru-kun.inf.ua
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
unit zgl_textures_png;

{$I zgl_config.cfg}

interface
uses
  {$IFDEF FPC}
  zbase,
  paszlib,
  {$ELSE}
  zlibpas,
  {$ENDIF}

  zgl_file,
  zgl_memory;

const
  PNG_SIGNATURE : array[ 0..7 ] of AnsiChar = ( #137, #80, #78, #71, #13, #10, #26, #10 );

  PNG_FILTER_NONE    = 0;
  PNG_FILTER_SUB     = 1;
  PNG_FILTER_UP      = 2;
  PNG_FILTER_AVERAGE = 3;
  PNG_FILTER_PAETH   = 4;

  PNG_COLOR_GRAYSCALE      = 0;
  PNG_COLOR_RGB            = 2;
  PNG_COLOR_PALETTE        = 3;
  PNG_COLOR_GRAYSCALEALPHA = 4;
  PNG_COLOR_RGBALPHA       = 6;


type
  zglTPNGChunkName = array[ 0..3 ] of AnsiChar;

  zglPPNGChunk = ^zglTPNGChunk;
  zglTPNGChunk = record
    Name : zglTPNGChunkName;
    Size : Integer;
  end;

  zglTPNGHeader = packed record
    Width             : Integer;
    Height            : Integer;
    BitDepth          : Byte;
    ColorType         : Byte;
    CompressionMethod : Byte;
    FilterMethod      : Byte;
    InterlaceMethod   : Byte;
  end;

  PColor = ^TColor;
  TColor = record
    R, G, B, A : Byte;
  end;

procedure png_Load( var Data : Pointer; var W, H : Word );
procedure png_LoadFromFile( const FileName : String; var Data : Pointer; var W, H : Word );
procedure png_LoadFromMemory( const Memory : zglTMemory; var Data : Pointer; var W, H : Word );

procedure png_ReadIHDR( var Data : Pointer );
procedure png_ReadPLTE;
procedure png_ReadIDAT( var Data : Pointer );
procedure png_ReadtRNS;

procedure png_GetPixelInfo;

procedure png_DecodeNonInterlaced( var Data : Pointer );
procedure png_FilterRow;
function  png_DecodeIDAT( const Buffer : Pointer; const Bytes : Integer ) : Integer;

implementation
uses
  zgl_types,
  zgl_main,
  zgl_log;

var
  pngMem          : zglTMemory;
  pngFail         : Boolean;
  pngHeader       : zglTPNGHeader;
  pngHeaderOk     : Boolean;
  pngSignature    : array[ 0..7 ] of AnsiChar;
  pngChunk        : zglTPNGChunk;
  pngHasIDAT      : Boolean;
  pngHastRNS      : Boolean;
  pngPalette      : array[ 0..255, 0..2 ] of Byte;
  pngPaletteAlpha : Byte;

  pngZStream      : TZStream;
  pngZData        : Pointer;

  pngRowUsed      : Boolean = TRUE;
  pngRowSize      : LongWord;
  pngRowBuffer    : array[ Boolean ] of PByteArray;
  pngOffset       : LongWord;

  pngIDATEnd      : LongWord;

procedure png_Load;
  label _exit;
begin
  {$IFDEF ENDIAN_BIG}
  forceNoSwap := TRUE;
  {$ENDIF}
  mem_Read( pngMem, pngSignature[ 0 ], 8 );

  if pngSignature <> PNG_SIGNATURE Then
    begin
      log_Add( 'PNG - Invalid header' );
      goto _exit;
    end;

  repeat
    if pngFail Then goto _exit;

    mem_ReadSwap( pngMem, pngChunk.Size, 4 );
    mem_Read( pngMem, pngChunk.Name, 4 );

    if ( not pngHeaderOk ) and ( pngChunk.Name <> 'IHDR' ) Then
      begin
        log_Add( 'PNG - Header not found' );
        goto _exit;
      end;

    if ( pngHasIDAT and ( pngChunk.Name = 'IDAT' ) ) or ( pngChunk.Name = 'cHRM' ) Then
      begin
        mem_Seek( pngMem, pngChunk.Size + 4, FSM_CUR );
        continue;
      end;

    if pngChunk.Name = 'IDAT' Then pngHasIDAT := TRUE;

    if pngChunk.Name = 'IHDR' Then
      png_ReadIHDR( Data )
    else
      if pngChunk.Name = 'PLTE' Then
        png_ReadPLTE()
      else
        if pngChunk.Name = 'IDAT' Then
          png_ReadIDAT( Data )
        else
          if pngChunk.Name = 'tRNS' Then
            png_ReadtRNS()
          else
            mem_Seek( pngMem, pngChunk.Size, FSM_CUR );

    mem_Seek( pngMem, 4, FSM_CUR );
  until ( pngChunk.Name = 'IEND' );

  if not pngHasIDAT Then
    begin
      log_Add( 'PNG - Image data not found' );
      goto _exit;
    end;

  W := pngHeader.Width;
  H := pngHeader.Height;

_exit:
  begin
    mem_Free( pngMem );
    pngFail     := FALSE;
    pngHasIDAT  := FALSE;
    pngHastRNS  := FALSE;
    pngHeaderOk := FALSE;
    pngRowUsed  := TRUE;
    {$IFDEF ENDIAN_BIG}
    forceNoSwap := FALSE;
    {$ENDIF}
  end;
end;

procedure png_LoadFromFile;
begin
  mem_LoadFromFile( pngMem, FileName );
  png_Load( Data, W, H );
end;

procedure png_LoadFromMemory;
begin
  pngMem.Size := Memory.Size;
  zgl_GetMem( pngMem.Memory, Memory.Size );
  pngMem.Position := Memory.Position;
  Move( Memory.Memory^, pngMem.Memory^, Memory.Size );
  png_Load( Data, W, H );
end;

procedure png_ReadIHDR;
  var
    i : Integer;
begin
  if pngChunk.Size < SizeOf( zglTPNGHeader ) Then
    begin
      log_Add( 'PNG - Invalid header size' );
      pngFail := TRUE;
      exit;
    end;

  mem_ReadSwap( pngMem, pngHeader.Width, 4 );
  mem_ReadSwap( pngMem, pngHeader.Height, 4 );
  mem_Read( pngMem, pngHeader.BitDepth, 1 );
  mem_Read( pngMem, pngHeader.ColorType, 1 );
  mem_Read( pngMem, pngHeader.CompressionMethod, 1 );
  mem_Read( pngMem, pngHeader.FilterMethod, 1 );
  mem_Read( pngMem, pngHeader.InterlaceMethod, 1 );
  zgl_GetMem( Data, pngHeader.Width * pngHeader.Height * 4 );

  mem_Seek( pngMem, pngChunk.Size - SizeOf( zglTPNGHeader ), FSM_CUR );

  if ( pngHeader.CompressionMethod <> 0 ) Then
    begin
      log_Add( 'PNG - Invalid compression method' );
      pngFail := TRUE;
      exit;
    end;

  if pngHeader.InterlaceMethod <> 0 Then
    begin
      log_Add( 'PNG - Interlace not supported.' );
      pngFail := TRUE;
      exit;
    end;

  if pngHeader.ColorType = PNG_COLOR_GRAYSCALE Then
    for i := 0 to 255 do
      begin
        pngPalette[ i, 0 ] := i;
        pngPalette[ i, 1 ] := i;
        pngPalette[ i, 2 ] := i;
      end;
  pngHeaderOk := TRUE;
end;

procedure png_ReadPLTE;
begin
  mem_Read( pngMem, pngPalette[ 0 ], pngChunk.Size );
end;

procedure png_ReadIDAT;
begin
  zgl_GetMem( pngZData, 65535 );
  InflateInit_( pngZStream, zlib_version, SizeOf( TZStream ) );

  png_GetPixelInfo;
  pngIDATEnd := pngMem.Position + pngChunk.Size;

  zgl_GetMem( Pointer( pngRowBuffer[ FALSE ] ), pngRowSize + 1 );
  zgl_GetMem( Pointer( pngRowBuffer[ TRUE ] ), pngRowSize + 1 );

  png_DecodeNonInterlaced( Data );

  InflateEnd( pngZStream );
  FreeMem( pngZData, 65535 );

  FreeMem( pngRowBuffer[ FALSE ] );
  FreeMem( pngRowBuffer[ TRUE ] );
end;

procedure png_ReadtRNS;
begin
  mem_Read( pngMem, pngPaletteAlpha, 1 );
  pngHastRNS := TRUE;
end;

procedure png_GetPixelInfo;
begin
  case pngHeader.ColorType of
    PNG_COLOR_GRAYSCALE:
      begin
        pngRowSize := ( pngHeader.Width * pngHeader.BitDepth + 7 ) div 8;
        if pngHeader.BitDepth = 16 Then
          pngOffset := 2
        else
          pngOffset := 1;
      end;
    PNG_COLOR_PALETTE:
      begin
        pngRowSize := ( pngHeader.Width * pngHeader.BitDepth + 7 ) div 8;
        pngOffset  := 1;
      end;
    PNG_COLOR_RGB:
      begin
        pngRowSize := ( pngHeader.Width * pngHeader.BitDepth * 3) div 8;
        pngOffset  := 3 * pngHeader.BitDepth div 8;
      end;
    PNG_COLOR_GRAYSCALEALPHA:
      begin
        pngRowSize := ( pngHeader.Width * pngHeader.BitDepth * 2 ) div 8;
        pngOffset  := 2 * pngHeader.BitDepth div 8;
      end;
    PNG_COLOR_RGBALPHA:
      begin
        pngRowSize := ( pngHeader.Width * pngHeader.BitDepth * 4 ) div 8;
        pngOffset  := 4 * pngHeader.BitDepth div 8;
      end;
  else
    pngRowsize := 0;
    pngOffset  := 0;
  end;
end;

procedure png_CopyNonInterlacedRGB( Src, Dest : PByte );
  var
    i     : Integer;
    Color : PColor;
begin
  Color := Pointer( Dest );
  for i := 0 to pngHeader.Width - 1 do
    begin
      Color.R := Byte( Src^ ); INC( Src );
      Color.G := Byte( Src^ ); INC( Src );
      Color.B := Byte( Src^ ); INC( Src );
      Color.A := 255;
      INC( Color );
    end;
end;

procedure png_CopyNonInterlacedRGBAlpha( Src, Dest : PByte );
begin
  Move( Src^, Dest^, pngHeader.Width * 4 );
end;

procedure png_CopyNonInterlacedPalette( Src, Dest : PByte );
  var
    i : Integer;
    ByteData, N, K : Byte;
begin
  N := ( 8 div pngHeader.BitDepth );
  K := ( 8 - pngHeader.BitDepth );

  for i := 0 to pngHeader.Width - 1 do
    begin
      ByteData := PByteArray( Src )^[ i div N ];

      if pngHeader.BitDepth < 8 Then
        begin
          ByteData := ( ByteData Shr ( ( 8 - pngHeader.BitDepth ) - ( i mod N ) * pngHeader.BitDepth ) );
          ByteData := ByteData and ( $FF Shr K );
        end;

    Byte( Dest^ ) := pngPalette[ ByteData, 0 ]; INC( Dest );
    Byte( Dest^ ) := pngPalette[ ByteData, 1 ]; INC( Dest );
    Byte( Dest^ ) := pngPalette[ ByteData, 2 ]; INC( Dest );
    if ( pngPalette[ ByteData, 0 ] = pngPalette[ pngPaletteAlpha, 0 ] ) and
       ( pngPalette[ ByteData, 1 ] = pngPalette[ pngPaletteAlpha, 1 ] ) and
       ( pngPalette[ ByteData, 2 ] = pngPalette[ pngPaletteAlpha, 2 ] ) and pngHastRNS Then
      Byte( Dest^ ) := 0
    else
      Byte( Dest^ ) := 255;
    INC( Dest );
  end;
end;

procedure png_CopyNonInterlacedGrayscaleAlpha( Src, Dest : PByte );
  var
    i     : Integer;
    Color : PColor;
begin
  Color := Pointer( Dest );
  for i := 0 to pngHeader.Width - 1 do
    begin
      Color.R := Byte( Src^ );
      Color.G := Byte( Src^ );
      Color.B := Byte( Src^ ); INC( Src );
      Color.A := Byte( Src^ ); INC( Src );
      INC( Color );
    end;
end;

procedure png_DecodeNonInterlaced;
  var
    i     : Cardinal;
    CopyP : procedure( Src, Dest : PByte );
begin
  CopyP := nil;
  case pngHeader.ColorType of
    PNG_COLOR_RGB:
      if pngHeader.BitDepth = 8 Then CopyP := png_CopyNonInterlacedRGB;
    PNG_COLOR_PALETTE, PNG_COLOR_GRAYSCALE:
      if ( pngHeader.BitDepth = 1 ) or ( pngHeader.BitDepth = 4 ) or ( pngHeader.BitDepth = 8 ) Then CopyP := png_CopyNonInterlacedPalette;
    PNG_COLOR_RGBALPHA:
      if pngHeader.BitDepth = 8 Then CopyP := png_CopyNonInterlacedRGBAlpha;
    PNG_COLOR_GRAYSCALEALPHA:
      if pngHeader.BitDepth = 8 Then CopyP := png_CopyNonInterlacedGrayscaleAlpha;
  end;

  if not Assigned( CopyP ) Then
    begin
      log_Add( 'PNG - Unsupported ColorType' );
      pngFail := TRUE;
      exit;
    end;

  for i := pngHeader.Height downto 1 do
    begin
      png_DecodeIDAT( @pngRowBuffer[ pngRowUsed ][ 0 ], pngRowsize + 1 );
      if pngFail Then exit;

      png_FilterRow();

      CopyP( @pngRowBuffer[ pngRowUsed ][ 1 ], Pointer( Ptr( Data ) + pngHeader.Width * 4 * ( i - 1 ) ) );

      pngRowUsed := not pngRowUsed;
    end;
end;

procedure png_FilterRow;
  var
    i                          : Integer;
    Paeth                      : Byte;
    PP, Left, Above, AboveLeft : Integer;

  function PaethPredictor( a, b, c : Byte ) : Byte; {$IFDEF USE_INLINE} inline; {$ENDIF}
    var
      p, pa, pb, pc : Integer;
  begin
    p  := a + b - c;
    pa := abs( p - a );
    pb := abs( p - b );
    pc := abs( p - c );
    if ( pa <= pb ) and ( pa <= pc ) Then
      Result := a
    else
      if pb <= pc Then
        Result := b
      else
        Result := c;
  end;
begin
  case pngRowBuffer[ pngRowUsed ][ 0 ] of
    PNG_FILTER_NONE:;

    PNG_FILTER_SUB:
      for i := pngOffset + 1 to pngRowSize do
        pngRowBuffer[ pngRowUsed ][ i ] := ( pngRowBuffer[ pngRowUsed ][ i ] + pngRowBuffer[ pngRowUsed ][ i - pngOffset] ) and 255;

    PNG_FILTER_UP:
      for i := 1 to pngRowSize do
        pngRowBuffer[ pngRowUsed ][ i ] := ( pngRowBuffer[ pngRowUsed ][ i ] + pngRowBuffer[ not pngRowUsed ][ i ] ) and 255;

    PNG_FILTER_AVERAGE:
      for i := 1 to pngRowSize do
        begin
          Above := pngRowBuffer[ not pngRowUsed ][ i ];
          if i - 1 < pngOffset Then
            Left := 0
          else
            Left := pngRowBuffer[ pngRowUsed ][ i - pngOffset ];

          pngRowBuffer[ pngRowUsed ][ i ] := ( pngRowBuffer[ pngRowUsed ][ i ] + ( Left + Above ) div 2 ) and $FF;
        end;

    PNG_FILTER_PAETH:
      begin
        Left      := 0;
        AboveLeft := 0;

        for i := 1 to pngRowSize do
          Begin
            if i - 1 >= pngOffset Then
              begin
                Left      := pngRowBuffer[ pngRowUsed ][ i - pngOffset];
                AboveLeft := pngRowBuffer[ not pngRowUsed ][ i - pngOffset];
              end;

            Above := pngRowBuffer[ not pngRowUsed ][ i ];

            Paeth := pngRowBuffer[ pngRowUsed ][ i ];
            PP    := PaethPredictor( Left, Above, AboveLeft );

            pngRowBuffer[ pngRowUsed ][ i ] := ( Paeth + PP ) and $FF;
          end;
      end;
  end;
end;

function png_DecodeIDAT;
  var
    IDATHeader : zglTPNGChunkName;
begin
  Result := -1;
  with pngZStream do
    begin
      next_out  := Buffer;
      avail_out := Bytes;

      while avail_out > 0 do
        begin
          if ( pngMem.Position = pngIDATEnd ) and ( avail_out > 0 ) and ( avail_in = 0 ) Then
            begin
              mem_Seek( pngMem, 4, FSM_CUR );

              mem_ReadSwap( pngMem, pngIDATEnd, 4 );
              mem_Read( pngMem, IDATHeader, 4 );

              if IDATHeader <> 'IDAT' Then
                begin
                  log_Add( 'PNG - IDAT chunk expected' );
                  pngFail := TRUE;
                  exit;
                end;

              INC( pngIDATEnd, pngMem.Position );
            end;

          if avail_in = 0 Then
            begin
              if pngMem.Position + 65535 > pngIDATEnd Then
                avail_in := mem_Read( pngMem, pngZData^, pngIDATEnd - pngMem.Position )
              else
                avail_in := mem_Read( pngMem, pngZData^, 65535 );

              if avail_in = 0 Then
                begin
                  Result := Bytes - avail_out;
                  exit;
                end;

              next_in := pngZData;
            end;

          Result := Inflate( pngZStream, 0 );

          if Result < 0 Then
            begin
              log_Add( 'PNG - ZLib error: ' + {$IFDEF FPC} zError( Result ) {$ELSE} _z_errmsg[ Result ] {$ENDIF} );
              pngFail := TRUE;
              exit;
            end else
              Result := avail_In;
        end;
    end;
end;

initialization
  zgl_Reg( TEX_FORMAT_EXTENSION, PChar( 'png' ) );
  zgl_Reg( TEX_FORMAT_FILE_LOADER, @png_LoadFromFile );
  zgl_Reg( TEX_FORMAT_MEM_LOADER,  @png_LoadFromMemory );

end.
