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
unit zgl_text;

{$I define.inc}

interface

uses
  GL,
  zgl_types,
  zgl_const,
  zgl_global_var,
  zgl_opengl,
  zgl_log,
  zgl_textures,
  zgl_file,
  zgl_memory,
  zgl_sprite_2d,
  zgl_math,
  Utils;

function  font_Add : zglPFont;
procedure font_Del( const Font : zglPFont );

function font_LoadFromFile( const Texture, FontInfo : String ) : zglPFont;

procedure text_Draw( const Font : zglPFont; X, Y : Single; const _Text : String; const Alpha : Byte; const Color : DWORD; const Step, Scale : Single );
function  text_GetWidth( const Font : zglPFont; const _Text : String; const Step, Scale : Single ) : Single;

const
  ZGL_FONT_INFO = 'ZGL_FONT_INFO';

implementation
uses
  zgl_main;

function font_Add;
begin
  Result := @managerFont.First;
  while Assigned( Result.Next ) do
    Result := Result.Next;

  Result.Next := AllocMem( SizeOf( zglTFont ) );
  FillChar( Result.Next^, SizeOf( zglTFont ), 0 );
  Result.Next.Prev := Result;
  Result := Result.Next;
  INC( managerFont.Count );
end;

procedure font_Del;
begin
  tex_Del( Font.Texture );
  if Assigned( Font.Prev ) Then
    Font.Prev.Next := Font.Next;
  Freememory( Font );
  DEC( managerFont.Count );
end;

function font_LoadFromFile;
  var
    M      : zglTMemory;
    ID     : array[ 0..12 ] of Char;
    i, j   : Byte;
    tX, tY, tW, tH : Single;
begin
  Result := font_Add;

  Result.Texture := tex_LoadFromFile( Texture, $FF000000, TEX_CLAMP or TEX_FILTER_LINEAR or TEX_CONVERT_TO_POT );
  Result.Texture.FramesX := 13;
  Result.Texture.FramesY := 13;
  if not file_Exists( FontInfo ) Then
    begin
      log_Add( 'Cannot read ' + FontInfo );
      zgl_Destroy;
      exit;
    end;

  mem_LoadFromFile( M, FontInfo );
  mem_Read( M, ID, 13 );
  if ID <> ZGL_FONT_INFO Then
    begin
      log_Add( FontInfo + ' - it''s not a ZenGL font info file' );
      zgl_Destroy;
      exit;
    end;

  mem_Read( M, Result.Height, 1 );
  for i := 0 to 255 do
    mem_Read( M, Result.Width[ i ], 1 );
  for i := 0 to 255 do
    begin
      mem_Read( M, j, 1 );

      tX := j;
      tY := 1;
      while tX > 13 do
        begin
          tX := tX - 13;
          tY := tY + 1;
        end;
      if tY < 1 Then tY := tY + 13;
      tW := Round( Result.Texture.Width  / 13 );
      tH := Round( Result.Texture.Height / 13 );
      tX := Round( tX * tW );
      tY := Round( tY * tH );

      tX := ( 1 / ( Result.Texture.Width  / Result.Texture.U ) ) * ( tX - tW );
      tY := ( 1 / ( Result.Texture.Height / Result.Texture.V ) ) * ( Result.Texture.Height - ( tY - tH ) );
      tW := tX + ( 1 / ( Result.Texture.Width  / Result.Texture.U ) ) * tW;
      tH := tY + ( 1 / ( Result.Texture.Height / Result.Texture.V ) ) * ( - tH );

      Result.TexCoords[ i ][ 0 ].X := tX;
      Result.TexCoords[ i ][ 0 ].Y := tY;
      Result.TexCoords[ i ][ 1 ].X := tW;
      Result.TexCoords[ i ][ 1 ].Y := tY;
      Result.TexCoords[ i ][ 2 ].X := tW;
      Result.TexCoords[ i ][ 2 ].Y := tH;
      Result.TexCoords[ i ][ 3 ].X := tX;
      Result.TexCoords[ i ][ 3 ].Y := tH;
    end;

  mem_Free( M );
end;

// TODO: переработать код в более человеческий :)
procedure text_Draw;
  var
    i     : DWORD;
    c, c2 : Byte;
    w, h  : Single;
    xt    : Single;
    Text  : String;
begin
  w := Round( Font.Texture.Width  / 13 ) * Scale;
  h := Round( Font.Texture.Height / 13 ) * Scale;
  Y := Y - Round( ( h - Font.Height ) / 2 * Scale );
  xt := X;

  Text := _Text + ' ';

  glColor4ub( Color and $FF, ( Color and $FF00 ) shr 8, Color shr 16, Alpha );

  glEnable( GL_BLEND );
  glEnable( GL_TEXTURE_2D );
  glBindTexture( GL_TEXTURE_2D, Font.Texture.ID );

  glBegin( GL_QUADS );
  c := Getchar( Text[ 1 ], Text[ 2 ] );
  c2 := c;
  X := Round( X - ( w - Font.Width[ c2 ] * Scale ) / 2 );
  for i := 1 to length( Text ) - 1 do
    begin
      if Text[ i ] = Char( 13 ) Then
        begin
          X := xt - ( w - Font.Width[ Getchar( Text[ i + 2 ], Text[ i + 3 ] ) ] * Scale ) / 2;
          Y := Y + h;
          continue;
        end;

      c := Getchar( Text[ i ], Text[ i + 1 ] );
      if ( i <> 1 ) and ( Text[ i - 1 ] <> Char( 13 ) ) Then
        X := X + Round( Font.Width[ c2 ] * Scale + ( w - Font.Width[ c2 ] * Scale ) / 2 - ( w - Font.Width[ c ] * Scale ) / ( 2 / ( Byte( i = 1 ) + 1 ) ) + Step * Scale );

      glTexCoord2fv( @Font.TexCoords[ c ][ 0 ] );
      gl_Vertex2f( X, Y );

      glTexCoord2fv( @Font.TexCoords[ c ][ 1 ] );
      gl_Vertex2f( X + w, Y );

      glTexCoord2fv( @Font.TexCoords[ c ][ 2 ] );
      gl_Vertex2f( X + w, Y + h );

      glTexCoord2fv( @Font.TexCoords[ c ][ 3 ] );
      gl_Vertex2f( X, Y + h );

      c2 := c;
    end;
  glEnd;

  glDisable( GL_TEXTURE_2D );
  glDisable( GL_BLEND );
end;

function text_GetWidth;
  var
    i : DWORD;
    Text : String;
begin
  Text := _Text + ' ';
  Result := 0;
  for i := 1 to length( Text ) - 1 do
    Result := Result + Font.Width[ Getchar( Text[ i ], Text[ i + 1 ] ) ] * Scale + Step * Scale;
end;

end.
