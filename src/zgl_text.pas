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
unit zgl_text;

interface
uses
  zgl_const,
  zgl_main,
  zgl_opengl_all,
  zgl_font,
  zgl_math_2d;

const
  TEXT_ALIGN_LEFT    = $000001;
  TEXT_ALIGN_CENTER  = $000002;
  TEXT_ALIGN_RIGHT   = $000004;
  TEXT_ALIGN_JUSTIFY = $000008;

procedure text_Draw( const Font : zglPFont; X, Y : Single; const Text : String; const Flags : DWORD = 0 );
procedure text_DrawEx( const Font : zglPFont; X, Y, Scale, Step : Single; const Text : String; const Alpha : Byte = 255; const Color : DWORD = $FFFFFF; const Flags : DWORD = 0 );
procedure text_DrawInRect( const Font : zglPFont; const Rect : zglTRect; const Text : String; const Alpha : Byte = 255; const Flags : DWORD = 0 );
function  text_GetWidth( const Font : zglPFont; const Text : String; const Step : Single = 0.0 ) : Single;

implementation
uses
  zgl_opengl_simple,
  zgl_fx;

var
  textRGBA  : array[ 0..3 ] of Byte = ( 255, 255, 255, 255 );
  textScale : Single = 1.0;
  textStep  : Single = 0.0;

procedure text_Draw;
  var
    i : Integer;
    c : Integer;
    lastPage : Integer;
begin
  if Text = '' Then exit;

  glEnable( GL_BLEND );
  glEnable( GL_TEXTURE_2D );

  glColor4ub( textRGBA[ 0 ], textRGBA[ 1 ], textRGBA[ 2 ], textRGBA[ 3 ] );

  {X := Round( X );
  Y := Round( Y );}
  Y := Y - Font.MaxShiftY;

  lastPage := -1;
  c := font_GetUID( Text, 1, @i );
  i := 1;
  if Assigned( Font.CharDesc[ c ] ) Then
    begin
      lastPage := Font.CharDesc[ c ].Page;
      glBindTexture( GL_TEXTURE_2D, Font.Pages[ Font.CharDesc[ c ].Page ].ID );
    end;
  glBegin( GL_QUADS );
  while i <= length( Text ) do
    begin
      c := font_GetUID( Text, i, @i );

      if not Assigned( Font.CharDesc[ c ] ) Then continue;

      if lastPage <> Font.CharDesc[ c ].Page Then
        begin
          lastPage := Font.CharDesc[ c ].Page;

          glEnd;

          glBindTexture( GL_TEXTURE_2D, Font.Pages[ Font.CharDesc[ c ].Page ].ID );
          glBegin( GL_QUADS );
        end;

      glTexCoord2fv( @Font.CharDesc[ c ].TexCoords[ 0 ] );
      gl_Vertex2f( X + ( Font.CharDesc[ c ].ShiftX - Font.Padding[ 0 ] ) * textScale,
                   Y + ( Font.CharDesc[ c ].ShiftY + ( Font.MaxHeight - Font.CharDesc[ c ].Height ) - Font.Padding[ 1 ] ) * textScale );

      glTexCoord2fv( @Font.CharDesc[ c ].TexCoords[ 1 ] );
      gl_Vertex2f( X + ( Font.CharDesc[ c ].ShiftX + Font.CharDesc[ c ].Width + Font.Padding[ 2 ] ) * textScale,
                   Y + ( Font.CharDesc[ c ].ShiftY + ( Font.MaxHeight - Font.CharDesc[ c ].Height ) - Font.Padding[ 1 ] ) * textScale );

      glTexCoord2fv( @Font.CharDesc[ c ].TexCoords[ 2 ] );
      gl_Vertex2f( X + ( Font.CharDesc[ c ].ShiftX + Font.CharDesc[ c ].Width ) * textScale + Font.Padding[ 2 ],
                   Y + ( Font.CharDesc[ c ].ShiftY + Font.CharDesc[ c ].Height + ( Font.MaxHeight - Font.CharDesc[ c ].Height ) + Font.Padding[ 3 ] ) * textScale );

      glTexCoord2fv( @Font.CharDesc[ c ].TexCoords[ 3 ] );
      gl_Vertex2f( X + ( Font.CharDesc[ c ].ShiftX - Font.Padding[ 0 ] ) * textScale,
                   Y + ( Font.CharDesc[ c ].ShiftY + Font.CharDesc[ c ].Height + ( Font.MaxHeight - Font.CharDesc[ c ].Height ) + Font.Padding[ 3 ] ) * textScale );

      X := X + ( Font.CharDesc[ c ].ShiftP + textStep ) * textScale;
    end;
  glEnd;

  glDisable( GL_BLEND );
  glDisable( GL_TEXTURE_2D );
end;

procedure text_DrawEx;
begin
  textRGBA[ 0 ] :=   Color and $FF;
  textRGBA[ 1 ] := ( Color and $FF00 ) shr 8;
  textRGBA[ 2 ] :=   Color             shr 16;
  textRGBA[ 3 ] := Alpha;
  textScale     := Scale;
  textStep      := Step;
  text_Draw( Font, X, Y, Text, Flags );
  textRGBA[ 0 ] := 255;
  textRGBA[ 1 ] := 255;
  textRGBA[ 2 ] := 255;
  textRGBA[ 3 ] := 255;
  textScale     := 1;
  textStep      := 0;
end;

procedure text_DrawInRect;
  var
    X, Y, W, H : Integer;
begin
  if Text = '' Then exit;

  X := Round( Rect.X );
  Y := Round( Rect.Y );
  W := Round( Rect.W );
  H := Round( Rect.H );
  scissor_Begin( X, Y, W, H );

  glEnable( GL_BLEND );
  glEnable( GL_TEXTURE_2D );

  glDisable( GL_BLEND );
  glDisable( GL_TEXTURE_2D );

  scissor_End;
end;

function text_GetWidth;
  var
    i : Integer;
    c : DWORD;
begin
  if Text = '' Then exit;
  i      := 1;
  c      := font_GetUID( Text, i, @i );
  Result := Font.CharDesc[ c ].ShiftX;
  i      := 1;
  while i <= length( Text ) do
    begin
      c := font_GetUID( Text, i, @i );
      if Assigned( Font.CharDesc[ c ] ) Then
        Result := Result + Font.CharDesc[ c ].ShiftP;
    end;
end;

end.
