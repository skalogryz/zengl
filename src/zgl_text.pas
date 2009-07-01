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
  zgl_types,
  zgl_font,
  zgl_math_2d;

const
  TEXT_HALIGN_LEFT    = $000001;
  TEXT_HALIGN_CENTER  = $000002;
  TEXT_HALIGN_RIGHT   = $000004;
  TEXT_HALIGN_JUSTIFY = $000008;
  TEXT_VALIGN_TOP     = $000010;
  TEXT_VALIGN_CENTER  = $000020;
  TEXT_VALIGN_BOTTOM  = $000040;

type
  zglTTextWord = record
    X, Y, W : Integer;
    ShiftX  : Integer;
    str     : String;
end;

procedure text_Draw( const Font : zglPFont; X, Y : Single; const Text : String; const Flags : DWORD = 0 );
procedure text_DrawEx( const Font : zglPFont; X, Y, Scale, Step : Single; const Text : String; const Alpha : Byte = 255; const Color : DWORD = $FFFFFF; const Flags : DWORD = 0 );
procedure text_DrawInRect( const Font : zglPFont; const Rect : zglTRect; const Text : String; const Flags : DWORD = 0 );
procedure text_DrawInRectEx( const Font : zglPFont; const Rect : zglTRect; const Scale, Step : Single; const Text : String; const Alpha : Byte = 0; const Color : DWORD = $FFFFFF; const Flags : DWORD = 0 );
function  text_GetWidth( const Font : zglPFont; const Text : String; const Step : Single = 0.0 ) : Single;

implementation
uses
  zgl_main,
  zgl_opengl,
  zgl_opengl_all,
  zgl_opengl_simple,
  zgl_fx,
  zgl_utils;

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
  if ( Text = '' ) or ( not Assigned( Font ) ) Then exit;

  glEnable( GL_BLEND );
  glEnable( GL_TEXTURE_2D );

  glColor4ub( textRGBA[ 0 ], textRGBA[ 1 ], textRGBA[ 2 ], textRGBA[ 3 ] );

  Y := Y - Font.MaxShiftY;
  if Flags and TEXT_HALIGN_CENTER > 0 Then
    X := X - Round( text_GetWidth( Font, Text, textStep ) / 2 ) * textScale
  else
    if Flags and TEXT_HALIGN_RIGHT > 0 Then
      X := X - Round( text_GetWidth( Font, Text, textStep ) ) * textScale;

  if Flags and TEXT_VALIGN_CENTER > 0 Then
    Y := Y - ( Font.MaxHeight div 2 ) * textScale
  else
    if Flags and TEXT_VALIGN_BOTTOM > 0 Then
      Y := Y - Font.MaxHeight * textScale;

  lastPage := -1;
  c := font_GetCID( Text, 1, @i );
  i := 1;
  if Assigned( Font.CharDesc[ c ] ) Then
    begin
      lastPage := Font.CharDesc[ c ].Page;
      glBindTexture( GL_TEXTURE_2D, Font.Pages[ Font.CharDesc[ c ].Page ].ID );
    end;
  glBegin( GL_QUADS );
  while i <= length( Text ) do
    begin
      c := font_GetCID( Text, i, @i );

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

// TODO:
// - оптимизировать количество DIP'ов
// - добавить переносы строк по LF символу
procedure text_DrawInRect;
  var
    i, j, b, l : Integer;
    X, Y, W, H : Integer;
    SpaceShift : Integer;
    WordsArray : array of zglTTextWord;
    WordsCount : Integer;
    LineFeed   : Boolean;
begin
  if ( Text = '' ) or ( not Assigned( Font ) ) Then exit;
  if ( Rect.W <= ogl_CropX ) or ( Rect.H <= ogl_CropY ) or
     ( Rect.X + Rect.W <= ogl_CropX ) or ( Rect.Y + Rect.H <= ogl_CropY ) or
     ( Rect.X > ogl_CropW ) or ( Rect.Y > ogl_CropH ) Then exit;

  SpaceShift := Round( ( text_GetWidth( Font, ' ' ) + textStep ) * textScale );

  X := Round( Rect.X );
  Y := Round( Rect.Y );
  W := Round( Rect.W );
  H := Round( Rect.H );
  scissor_Begin( X, Y, W, H );

  WordsCount := u_Words( Text ) + u_Words( Text, #10 ) - 1;
  SetLength( WordsArray, WordsCount + 1 );
  WordsArray[ WordsCount ].str := ' ';
  WordsArray[ WordsCount ].W   := Round( Rect.W + 1 );

  LineFeed := FALSE;
  l := length( Text );
  b := 1;
  for i := 0 to WordsCount - 1 do
    for j := b to l do
      begin
        LineFeed := Text[ j ] = #10;
        if ( ( Text[ j ] = ' ' ) and ( j <> 1 ) ) or ( j = l ) or LineFeed Then
          begin
            if b = 1 Then
              WordsArray[ i ].str := Copy( Text, b, j - b )
            else
              WordsArray[ i ].str := Copy( Text, b - 1, j - b + 1 + Byte( j = j ) );
            WordsArray[ i ].W      := Round( text_GetWidth( Font, WordsArray[ i ].str, textStep ) * textScale );
            WordsArray[ i ].ShiftX := Font.CharDesc[ font_GetCID( WordsArray[ i ].str, 1, @H ) ].ShiftX;
            if LineFeed Then
              b := j + 2
            else
              b := j + 1;
            LineFeed := FALSE;
            break;
          end;
      end;

  l := 0;
  if Flags and TEXT_HALIGN_JUSTIFY = 0 Then
    INC( WordsCount );
  for i := 0 to WordsCount - 1 do
    begin
      WordsArray[ i ].X := X;
      WordsArray[ i ].Y := Y;
      X := X + WordsArray[ i ].W - WordsArray[ i ].ShiftX;
      if ( i > 0 ) and ( WordsArray[ i - 1 ].str[ length( WordsArray[ i - 1 ].str ) ] = #10 ) Then
        LineFeed := TRUE;
      if ( ( X >= Rect.X + Rect.W ) and ( i - l > 0 ) ) or LineFeed Then
        begin
          X := Round( Rect.X );
          Y := Y + Round( Font.MaxHeight * textScale );
          WordsArray[ i ].X := X - SpaceShift * Byte( not LineFeed );
          WordsArray[ i ].Y := Y;
          X := X + WordsArray[ i ].W - SpaceShift;

          if ( Flags and TEXT_HALIGN_JUSTIFY > 0 ) and ( i - l > 1 ) and ( not LineFeed ) Then
            begin
              W := Round( Rect.X + Rect.W ) - ( WordsArray[ i - 1 ].X + WordsArray[ i - 1 ].W - SpaceShift );
              while W > ( i - 1 ) - l do
                begin
                  for b := l + 1 to i - 1 do
                    INC( WordsArray[ b ].X, 1 + ( b - ( l + 1 ) ) );
                  W := Round( Rect.X + Rect.W ) - ( WordsArray[ i - 1 ].X + WordsArray[ i - 1 ].W - SpaceShift );
                end;
              WordsArray[ i - 1 ].X := WordsArray[ i - 1 ].X + W;
            end else
              if Flags and TEXT_HALIGN_CENTER > 0 Then
                begin
                  W := ( Round( Rect.X + Rect.W ) - ( WordsArray[ i - 1 ].X + WordsArray[ i - 1 ].W - SpaceShift ) ) div 2;
                  for b := l to i - 1 do
                    INC( WordsArray[ b ].X, W );
                end else
                  if Flags and TEXT_HALIGN_RIGHT > 0 Then
                    begin
                      W := Round( Rect.X + Rect.W ) - ( WordsArray[ i - 1 ].X + WordsArray[ i - 1 ].W - SpaceShift );
                      for b := l to i - 1 do
                        INC( WordsArray[ b ].X, W );
                    end;
          LineFeed := FALSE;
          l := i;
        end;
    end;
  if Flags and TEXT_HALIGN_JUSTIFY = 0 Then
    DEC( WordsCount );

  if Flags and TEXT_VALIGN_CENTER > 0 Then
    begin
      H := ( Round( Rect.Y + Rect.H ) - ( WordsArray[ WordsCount - 1 ].Y + Font.MaxHeight ) ) div 2;
      for i := 0 to WordsCount - 1 do
        INC( WordsArray[ i ].Y, H );
    end else
      if Flags and TEXT_VALIGN_BOTTOM > 0 Then
        begin
          H := Round( Rect.Y + Rect.H ) - ( WordsArray[ WordsCount - 1 ].Y + Font.MaxHeight );
          for i := 0 to WordsCount - 1 do
            INC( WordsArray[ i ].Y, H );
        end;

  for i := 0 to WordsCount - 1 do
    text_Draw( Font, WordsArray[ i ].X, WordsArray[ i ].Y, WordsArray[ i ].str );

  SetLength( WordsArray, 0 );
  scissor_End;
end;

procedure text_DrawInRectEx;
begin
  textRGBA[ 0 ] :=   Color and $FF;
  textRGBA[ 1 ] := ( Color and $FF00 ) shr 8;
  textRGBA[ 2 ] :=   Color             shr 16;
  textRGBA[ 3 ] := Alpha;
  textScale     := Scale;
  textStep      := Step;
  text_DrawInRect( Font, Rect, Text, Flags );
  textRGBA[ 0 ] := 255;
  textRGBA[ 1 ] := 255;
  textRGBA[ 2 ] := 255;
  textRGBA[ 3 ] := 255;
  textScale     := 1;
  textStep      := 0;
end;

function text_GetWidth;
  var
    i : Integer;
    c : DWORD;
begin
  Result := 0;
  if ( Text = '' ) or ( not Assigned( Font ) ) Then exit;
  i  := 1;
  c  := font_GetCID( Text, i, @i );
  if Assigned( Font.CharDesc[ c ] ) Then
    Result := Font.CharDesc[ c ].ShiftX;
  i := 1;
  while i <= length( Text ) do
    begin
      c := font_GetCID( Text, i, @i );
      if Assigned( Font.CharDesc[ c ] ) Then
        Result := Result + Font.CharDesc[ c ].ShiftP + Step;
    end;
end;

end.
