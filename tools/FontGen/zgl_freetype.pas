{
 * Copyright © Kemka Andrey aka Andru
 * mail: dr.andru@gmail.com
 * site: http://andru-kun.ru
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
unit zgl_freetype;

interface
uses
  freetype,
  zgl_font;

function  fnt_Init : Boolean;
procedure fnt_Done;
function  fnt_Load( const FileName : String ) : Boolean;
procedure fnt_Free;
procedure fnt_SetCharSize( const Size : Integer );
procedure fnt_GenChars;
procedure fnt_FreeChars;
procedure fnt_PutChar( var pData : Pointer; const X, Y, ID : Integer );
procedure fnt_GenPages;
procedure fnt_Save( const FileName : String );

var
  fntDPI      : Integer = 96;
  fntSize     : Integer;
  fntPageSize : Integer = 512;
  fntCCount   : Integer = 13; // Символов по горизонтали и вертикали
  fntPadding  : array[ 0..3 ] of Byte = ( 4, 4, 4, 4 );

  fntLibrary : FT_Library;
  fntFace    : FT_Face;
  fntGlyphID : FT_UInt;
  fntGlyphs  : array of FT_Glyph;
  fntBitmaps : array of FT_Bitmap;

  fntChars   : array[ 0..65535 ] of Boolean;
  fntCharsID : array of WORD;
  fntCharsP  : array of Integer;

  Font : zglPFont;

implementation
uses
  zgl_types,
  zgl_main,
  zgl_log,
  zgl_textures,
  zgl_textures_tga,
  zgl_file,
  zgl_math_2d,
  zgl_utils;

function Max( v1, v2 : Integer ) : Integer;
begin
  if v1 > v2 Then
    Result := v1
  else
    Result := v2;
end;

function fnt_Init;
begin
  Result := FT_Init_FreeType( fntLibrary ) = 0;
  if not Result Then
    log_Add( 'ERROR: FT_Init_FreeType' );
end;

procedure fnt_Done;
begin
  FT_Done_FreeType( fntLibrary );
end;

function fnt_Load;
begin
  Result := FT_New_Face( fntLibrary, PChar( FileName ), 0, fntFace ) = 0;
  if not Result Then
    begin
      log_Add( 'ERROR: FT_New_Face for ' + FileName );
      exit;
    end;

  if Assigned( Font ) Then
    font_Del( Font );
  Font := font_Add;
end;

procedure fnt_Free;
begin
  FT_Done_Face( fntFace );
end;

procedure fnt_SetCharSize;
begin
  fntSize := Size;
  FT_Set_Char_Size( fntFace, 0, Size shl 6, fntDPI, fntDPI );
end;

procedure fnt_GenChars;
  var
    i, j : Integer;
begin
  SetLength( fntGlyphs,  Font.Count.Chars );
  SetLength( fntBitmaps, Font.Count.Chars );
  SetLength( fntCharsID, Font.Count.Chars );
  SetLength( fntCharsP,  Font.Count.Chars );
  j := 0;
  for i := 0 to 65535 do
    if fntChars[ i ] Then
      begin
        fntGlyphID := FT_Get_Char_Index( fntFace, i );

        if FT_Load_Glyph( fntFace, fntGlyphID, FT_LOAD_DEFAULT ) <> 0 Then
          log_Add( 'ERROR: FT_Load_Glyph for ' + u_IntToStr( i ) );
        if FT_Render_Glyph( fntFace.glyph, FT_RENDER_MODE_NORMAL ) <> 0 Then
          log_Add( 'ERROR: FT_Render_Glyph for ' + u_IntToStr( i )  );
        if FT_Get_Glyph( fntFace.glyph, fntGlyphs[ j ] ) <> 0 Then
          log_Add( 'ERROR: FT_Get_Glyph ' + u_IntToStr( i ) );
        if FT_Glyph_To_Bitmap( fntGlyphs[ j ], FT_RENDER_MODE_NORMAL, 0, 1 ) <> 0 Then
          log_Add( 'ERROR: FT_Glyph_To_Bitmap ' + u_IntToStr( i ) );

        fntBitmaps[ j ] := FT_BitmapGlyph( fntGlyphs[ j ] ).bitmap;
        fntCharsID[ j ] := i;
        fntCharsP[ j ]  := fntFace.glyph.advance.x shr 6;
        INC( j );
      end;
end;

procedure fnt_FreeChars;
  var
    i : Integer;
begin
  for i := 0 to Font.Count.Chars - 1 do
    FT_Done_Glyph( fntGlyphs[ i ] );
  SetLength( fntGlyphs,  0 );
  SetLength( fntBitmaps, 0 );
end;

procedure fnt_PutChar;
  var
    i, j : Integer;
    fw   : Integer;
    buff : Pointer;
begin
  fw   := fntBitmaps[ ID ].width;
  buff := fntBitmaps[ ID ].buffer;
  for i := 0 to fntBitmaps[ ID ].width - 1 do
    for j := 0 to fntBitmaps[ ID ].rows - 1 do
      begin
        PByte( Ptr( pData ) + ( i + X ) * 4 + ( j + Y ) * fntPageSize * 4 + 0 )^ := 255;//PByte( Ptr( buff ) + i + j * fw )^;
        PByte( Ptr( pData ) + ( i + X ) * 4 + ( j + Y ) * fntPageSize * 4 + 1 )^ := 255;//PByte( Ptr( buff ) + i + j * fw )^;
        PByte( Ptr( pData ) + ( i + X ) * 4 + ( j + Y ) * fntPageSize * 4 + 2 )^ := 255;//PByte( Ptr( buff ) + i + j * fw )^;
        PByte( Ptr( pData ) + ( i + X ) * 4 + ( j + Y ) * fntPageSize * 4 + 3 )^ := PByte( Ptr( buff ) + i + j * fw )^;
      end;
end;

procedure fnt_GenPages;
  var
    pData  : Pointer;
    i, j   : Integer;
    cid    : Integer;
    cx, cy : Integer;
    cs     : Integer;
    u, v   : Single;
begin
  cs := fntPageSize div fntCCount;

  Font.Count.Pages := Font.Count.Chars div sqr( fntCCount ) + 1;
  SetLength( Font.Pages, Font.Count.Pages );
  Font.MaxHeight := 0;
  Font.MaxShiftY := 0;
  for i := 0 to Font.Count.Pages - 1 do
    begin
      Font.Pages[ i ]        := tex_Add;
      Font.Pages[ i ].Width  := fntPageSize;
      Font.Pages[ i ].Height := fntPageSize;
      Font.Pages[ i ].U      := 1;
      Font.Pages[ i ].V      := 1;
      Font.Pages[ i ].Flags  := TEX_CLAMP or TEX_FILTER_LINEAR;

      u := 1 / Font.Pages[ i ].Width;
      v := 1 / Font.Pages[ i ].Height;

      zgl_GetMem( pData, sqr( fntPageSize ) * 4 );
      for j := 0 to sqr( fntCCount ) - 1 do
        begin
          cid := j + i * sqr( fntCCount );
          if cid > Font.Count.Chars - 1 Then break;
          cy  := j div fntCCount;
          cx  := j - cy * fntCCount;
          fnt_PutChar( pData, cx * cs + ( cs - fntBitmaps[ cid ].width ) div 2,
                              cy * cs + ( cs - fntBitmaps[ cid ].rows  ) div 2, cid );

          zgl_GetMem( Pointer( Font.CharDesc[ fntCharsID[ cid ] ] ), SizeOf( zglTCharDesc ) );
          Font.CharDesc[ fntCharsID[ cid ] ].Page   := i;
          Font.CharDesc[ fntCharsID[ cid ] ].Width  := fntBitmaps[ cid ].width;
          Font.CharDesc[ fntCharsID[ cid ] ].Height := fntBitmaps[ cid ].rows;
          Font.CharDesc[ fntCharsID[ cid ] ].ShiftX := FT_BitmapGlyph( fntGlyphs[ cid ] ).left;
          Font.CharDesc[ fntCharsID[ cid ] ].ShiftY := fntBitmaps[ cid ].rows - FT_BitmapGlyph( fntGlyphs[ cid ] ).top;
          Font.CharDesc[ fntCharsID[ cid ] ].ShiftP := fntCharsP[ cid ];

          Font.CharDesc[ fntCharsID[ cid ] ].TexCoords[ 0 ].X := ( cx * cs + ( cs - fntBitmaps[ cid ].width ) div 2 - fntPadding[ 0 ] ) * u;
          Font.CharDesc[ fntCharsID[ cid ] ].TexCoords[ 0 ].Y := 1 - ( cy * cs + ( cs - fntBitmaps[ cid ].rows ) div 2 - fntPadding[ 1 ] ) * v;
          Font.CharDesc[ fntCharsID[ cid ] ].TexCoords[ 1 ].X := ( cx * cs + ( cs - fntBitmaps[ cid ].width ) div 2 + fntBitmaps[ cid ].width + fntPadding[ 2 ] ) * u;
          Font.CharDesc[ fntCharsID[ cid ] ].TexCoords[ 1 ].Y := 1 - ( cy * cs + ( cs - fntBitmaps[ cid ].rows ) div 2 - fntPadding[ 1 ] ) * v;
          Font.CharDesc[ fntCharsID[ cid ] ].TexCoords[ 2 ].X := ( cx * cs + ( cs - fntBitmaps[ cid ].width ) div 2 + fntBitmaps[ cid ].width + fntPadding[ 2 ] ) * u;
          Font.CharDesc[ fntCharsID[ cid ] ].TexCoords[ 2 ].Y := 1 - ( cy * cs + ( cs - fntBitmaps[ cid ].rows ) div 2 + fntBitmaps[ cid ].rows + fntPadding[ 3 ] ) * v;
          Font.CharDesc[ fntCharsID[ cid ] ].TexCoords[ 3 ].X := ( cx * cs + ( cs - fntBitmaps[ cid ].width ) div 2 - fntPadding[ 0 ] ) * u;
          Font.CharDesc[ fntCharsID[ cid ] ].TexCoords[ 3 ].Y := 1 - ( cy * cs + ( cs - fntBitmaps[ cid ].rows ) div 2 + fntBitmaps[ cid ].rows + fntPadding[ 3 ] ) * v;

          Font.MaxHeight := Max( Font.MaxHeight, fntBitmaps[ cid ].rows );
          Font.MaxShiftY := Max( Font.MaxShiftY, Font.CharDesc[ fntCharsID[ cid ] ].ShiftY );
        end;
      Font.Padding[ 0 ] := fntPadding[ 0 ];
      Font.Padding[ 1 ] := fntPadding[ 1 ];
      Font.Padding[ 2 ] := fntPadding[ 2 ];
      Font.Padding[ 3 ] := fntPadding[ 3 ];
      tga_FlipVertically( PByteArray( pData )^, fntPageSize, fntPageSize, 4 );
      tex_Create( Font.Pages[ i ]^, pData );
      FreeMemory( pData );
    end;
end;

procedure fnt_Save;
  var
    TGA  : zglTTGAHeader;
    F    : zglTFile;
    i, c : Integer;
    Data : Pointer;
    size : Integer;
begin
  file_Open( F, FileName + '.zfi', FOM_CREATE );
  file_Write( F, ZGL_FONT_INFO, 13 );
  file_Write( F, Font.Count.Pages, 2 );
  file_Write( F, Font.Count.Chars, 2 );
  file_Write( F, Font.MaxHeight,   4 );
  file_Write( F, Font.MaxShiftY,   4 );
  file_Write( F, fntPadding[ 0 ],  4 );
  for i := 0 to Font.Count.Chars - 1 do
    begin
      c := fntCharsID[ i ];
      file_Write( F, c, 4 );
      file_Write( F, Font.CharDesc[ c ].Page, 4 );
      file_Write( F, Font.CharDesc[ c ].Width, 1 );
      file_Write( F, Font.CharDesc[ c ].Height, 1 );
      file_Write( F, Font.CharDesc[ c ].ShiftX, 4 );
      file_Write( F, Font.CharDesc[ c ].ShiftY, 4 );
      file_Write( F, Font.CharDesc[ c ].ShiftP, 4 );
      file_Write( F, Font.CharDesc[ c ].TexCoords[ 0 ], SizeOf( zglTPoint2D ) * 4 );
    end;
  file_Close( F );
  for i := 0 to Font.Count.Pages - 1 do
    begin
      FillChar( TGA, SizeOf( zglTTGAHeader ), 0 );
      TGA.ImageType      := 2;
      TGA.ImgSpec.Width  := fntPageSize;
      TGA.ImgSpec.Height := fntPageSize;
      TGA.ImgSpec.Depth  := 32;
      TGA.ImgSpec.Desc   := 8;

      tex_GetData( Font.Pages[ i ], Data, size );

      file_Open( F, FileName + '_' + u_IntToStr( i ) + '.tga', FOM_CREATE );
      file_Write( F, TGA, SizeOf( zglTTGAHeader ) );
      file_Write( F, Data^, sqr( fntPageSize ) * size );
      file_Close( F );
      FreeMemory( Data );
    end;
end;

end.
