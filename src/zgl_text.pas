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
unit zgl_text;

{$I zgl_config.cfg}

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
  TEXT_FX_VCA         = $000080;
  TEXT_FX_LENGTH      = $000100;

type
  zglTTextWord = record
    X, Y, W : Integer;
    ShiftX  : Integer;
    LF      : Boolean;
    LFShift : Integer;
    str     : String;
end;

procedure text_Draw( const Font : zglPFont; X, Y : Single; const Text : String; const Flags : DWORD = 0 );
procedure text_DrawEx( const Font : zglPFont; X, Y, Scale, Step : Single; const Text : String; const Alpha : Byte = 255; const Color : DWORD = $FFFFFF; const Flags : DWORD = 0 );
procedure text_DrawInRect( const Font : zglPFont; const Rect : zglTRect; const Text : String; const Flags : DWORD = 0 );
procedure text_DrawInRectEx( const Font : zglPFont; const Rect : zglTRect; const Scale, Step : Single; const Text : String; const Alpha : Byte = 0; const Color : DWORD = $FFFFFF; const Flags : DWORD = 0 );
function  text_GetWidth( const Font : zglPFont; const Text : String; const Step : Single = 0.0 ) : Single;
procedure textFx_SetLength( const Length : Integer; const LastCoord : zglPPoint2D = nil; const LastCharDesc : zglPCharDesc = nil );

implementation
uses
  zgl_main,
  zgl_opengl,
  zgl_opengl_all,
  zgl_opengl_simple,
  zgl_render_2d,
  zgl_fx,
  zgl_utils;

var
  textRGBA      : array[ 0..3 ] of Byte = ( 255, 255, 255, 255 );
  textScale     : Single = 1.0;
  textStep      : Single = 0.0;
  textLength    : Integer;
  textLCoord    : zglPPoint2D;
  textLCharDesc : zglPCharDesc;

procedure text_Draw;
  var
    i, c, s : Integer;
    CharDesc : zglPCharDesc;
    Quad     : array[ 0..3 ] of zglTPoint2D;
    sx : Single;
    lastPage : Integer;
begin
  if ( Text = '' ) or ( not Assigned( Font ) ) Then exit;

  glColor4ubv( @textRGBA[ 0 ] );

  Y := Y - Font.MaxShiftY * textScale;
  if Flags and TEXT_HALIGN_CENTER > 0 Then
    X := X - Round( text_GetWidth( Font, Text, textStep ) / 2 ) * textScale
  else
    if Flags and TEXT_HALIGN_RIGHT > 0 Then
      X := X - Round( text_GetWidth( Font, Text, textStep ) ) * textScale;
  sx := X;

  if Flags and TEXT_VALIGN_CENTER > 0 Then
    Y := Y - ( Font.MaxHeight div 2 ) * textScale
  else
    if Flags and TEXT_VALIGN_BOTTOM > 0 Then
      Y := Y - Font.MaxHeight * textScale;

  FillChar( Quad[ 0 ], SizeOf( Quad[ 0 ] ) * 3, 0 );
  CharDesc := nil;
  lastPage := -1;
  c := font_GetCID( Text, 1, @i );
  s := 1;
  i := 1;
  if not b2d_Started Then
    begin
      if Assigned( Font.CharDesc[ c ] ) Then
        begin
          lastPage := Font.CharDesc[ c ].Page;
          batch2d_Check( GL_TRIANGLES, FX_BLEND, Font.Pages[ Font.CharDesc[ c ].Page ] );

          glEnable( GL_BLEND );
          glEnable( GL_TEXTURE_2D );
          glBindTexture( GL_TEXTURE_2D, Font.Pages[ Font.CharDesc[ c ].Page ].ID );
          glBegin( GL_TRIANGLES );
        end else
          begin
            glEnable( GL_BLEND );
            glEnable( GL_TEXTURE_2D );
          end;
    end;
  while i <= length( Text ) do
    begin
      if Text[ i ] = #10 Then
        begin
          X := sx;
          Y := Y + Font.MaxHeight;
        end;
      c := font_GetCID( Text, i, @i );

      if ( Flags and TEXT_FX_LENGTH > 0 ) and ( s > textLength ) Then
        begin
          if s > 1 Then
            begin
              if Assigned( textLCoord ) Then
                begin
                  textLCoord.X := Quad[ 0 ].X + Font.Padding[ 0 ] * textScale;
                  textLCoord.Y := Quad[ 0 ].Y + Font.Padding[ 1 ] * textScale;
                end;
              if Assigned( textLCharDesc ) Then
                textLCharDesc^ := CharDesc^;
            end;
          break;
        end;
      INC( s );

      CharDesc := Font.CharDesc[ c ];
      if not Assigned( CharDesc ) Then continue;

      if lastPage <> CharDesc.Page Then
        begin
          lastPage := Font.CharDesc[ c ].Page;

          if ( not b2d_Started ) Then
            begin
              glEnd;

              glBindTexture( GL_TEXTURE_2D, Font.Pages[ CharDesc.Page ].ID );
              glBegin( GL_TRIANGLES );
            end else
              if batch2d_Check( GL_TRIANGLES, FX_BLEND, Font.Pages[ CharDesc.Page ] ) Then
                begin
                  glEnable( GL_BLEND );

                  glEnable( GL_TEXTURE_2D );
                  glBindTexture( GL_TEXTURE_2D, Font.Pages[ CharDesc.Page ].ID );
                  glBegin( GL_TRIANGLES );
                end;
        end;

      Quad[ 0 ].X := X + ( CharDesc.ShiftX - Font.Padding[ 0 ] ) * textScale;
      Quad[ 0 ].Y := Y + ( CharDesc.ShiftY + ( Font.MaxHeight - CharDesc.Height ) - Font.Padding[ 1 ] ) * textScale;
      Quad[ 1 ].X := X + ( CharDesc.ShiftX + Font.CharDesc[ c ].Width + Font.Padding[ 2 ] ) * textScale;
      Quad[ 1 ].Y := Y + ( CharDesc.ShiftY + ( Font.MaxHeight - CharDesc.Height ) - Font.Padding[ 1 ] ) * textScale;
      Quad[ 2 ].X := X + ( CharDesc.ShiftX + CharDesc.Width ) * textScale + Font.Padding[ 2 ];
      Quad[ 2 ].Y := Y + ( CharDesc.ShiftY + CharDesc.Height + ( Font.MaxHeight - CharDesc.Height ) + Font.Padding[ 3 ] ) * textScale;
      Quad[ 3 ].X := X + ( CharDesc.ShiftX - Font.Padding[ 0 ] ) * textScale;
      Quad[ 3 ].Y := Y + ( CharDesc.ShiftY + CharDesc.Height + ( Font.MaxHeight - CharDesc.Height ) + Font.Padding[ 3 ] ) * textScale;

      if Flags and TEXT_FX_VCA > 0 Then
        begin
          glColor4ubv( @FX2D_VCA1[ 0 ] );
          glTexCoord2fv( @CharDesc.TexCoords[ 0 ] );
          gl_Vertex2fv( @Quad[ 0 ] );

          glColor4ubv( @FX2D_VCA2[ 0 ] );
          glTexCoord2fv( @CharDesc.TexCoords[ 1 ] );
          gl_Vertex2fv( @Quad[ 1 ] );

          glColor4ubv( @FX2D_VCA3[ 0 ] );
          glTexCoord2fv( @CharDesc.TexCoords[ 2 ] );
          gl_Vertex2fv( @Quad[ 2 ] );

          glColor4ubv( @FX2D_VCA3[ 0 ] );
          glTexCoord2fv( @CharDesc.TexCoords[ 2 ] );
          gl_Vertex2fv( @Quad[ 2 ] );

          glColor4ubv( @FX2D_VCA4[ 0 ] );
          glTexCoord2fv( @CharDesc.TexCoords[ 3 ] );
          gl_Vertex2fv( @Quad[ 3 ] );

          glColor4ubv( @FX2D_VCA1[ 0 ] );
          glTexCoord2fv( @CharDesc.TexCoords[ 0 ] );
          gl_Vertex2fv( @Quad[ 0 ] );
        end else
          begin
            glTexCoord2fv( @CharDesc.TexCoords[ 0 ] );
            gl_Vertex2fv( @Quad[ 0 ] );

            glTexCoord2fv( @CharDesc.TexCoords[ 1 ] );
            gl_Vertex2fv( @Quad[ 1 ] );

            glTexCoord2fv( @CharDesc.TexCoords[ 2 ] );
            gl_Vertex2fv( @Quad[ 2 ] );

            glTexCoord2fv( @CharDesc.TexCoords[ 2 ] );
            gl_Vertex2fv( @Quad[ 2 ] );

            glTexCoord2fv( @CharDesc.TexCoords[ 3 ] );
            gl_Vertex2fv( @Quad[ 3 ] );

            glTexCoord2fv( @CharDesc.TexCoords[ 0 ] );
            gl_Vertex2fv( @Quad[ 0 ] );
          end;

      X := X + ( Font.CharDesc[ c ].ShiftP + textStep ) * textScale;
    end;

  if not b2d_Started Then
    begin
      glEnd;

      glDisable( GL_TEXTURE_2D );
      glDisable( GL_BLEND );
    end;
end;

procedure text_DrawEx;
begin
  textRGBA[ 0 ] :=   Color             shr 16;
  textRGBA[ 1 ] := ( Color and $FF00 ) shr 8;
  textRGBA[ 2 ] :=   Color and $FF;
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
// - Переписать весь этот говнокод
procedure text_DrawInRect;
  var
    i, j, b, l : Integer;
    X, Y, W, H : Integer;
    SpaceShift : Integer;
    WordsArray : array of zglTTextWord;
    WordsCount : Integer;
    LineFeed   : Boolean;
    NewFlags   : Integer;
begin
  if ( Text = '' ) or ( not Assigned( Font ) ) Then exit;
  if ( Rect.W <= ogl_CropX ) or ( Rect.H <= ogl_CropY ) or
     ( Rect.X + Rect.W <= ogl_CropX ) or ( Rect.Y + Rect.H <= ogl_CropY ) or
     ( Rect.X > ogl_CropW ) or ( Rect.Y > ogl_CropH ) Then exit;

  SpaceShift := Round( ( text_GetWidth( Font, ' ' ) + textStep ) * textScale );

  X := Round( Rect.X ) + 1;
  Y := Round( Rect.Y ) + 1;
  W := Round( Rect.W );
  H := Round( Rect.H );

  WordsCount := 0;
  for i := 1 to length( Text ) do
    if Text[ i ] = #10 Then
      INC( WordsCount );
  WordsCount := WordsCount + u_Words( Text );
  if WordsCount = 0 Then
    begin
      scissor_End;
      exit;
    end;
  SetLength( WordsArray, WordsCount + 1 );
  WordsArray[ WordsCount ].str := ' ';
  WordsArray[ WordsCount ].W   := Round( Rect.W + 1 );

  LineFeed := FALSE;
  l := length( Text );
  b := 1;
  W := 0;
  H := 1;
  for i := 0 to WordsCount - 1 do
    for j := b to l do
      begin
        LineFeed := Text[ j ] = #10;
        if ( ( Text[ j ] = ' ' ) and ( j <> 1 ) ) or ( j = l ) or LineFeed Then
          begin
            if ( j < l ) and ( Text[ j + 1 ] = #10 ) Then
              begin
                INC( W );
                if length( WordsArray[ i ].str ) > H Then
                  font_GetCID( WordsArray[ i ].str, H, @H );
                continue;
              end;
            if ( j > 1 ) and ( ( Text[ j - 1 ] = ' ' ) and ( j <> l ) ) Then
              begin
                INC( H );
                continue;
              end;
            if ( b = 1 ) and ( WordsCount > 1 ) Then
              WordsArray[ i ].str := Copy( Text, b, j - b )
            else
              WordsArray[ i ].str := Copy( Text, b - 1, j - b + 1 + 1 * Byte( not LineFeed ) - W );
            font_GetCID( WordsArray[ i ].str, H, @H );
            WordsArray[ i ].LF      := LineFeed;
            WordsArray[ i ].LFShift := W + 1;
            WordsArray[ i ].W       := Round( text_GetWidth( Font, WordsArray[ i ].str, textStep ) * textScale );
            if length( WordsArray[ i ].str ) > H Then
              begin
                W := font_GetCID( WordsArray[ i ].str, H, @H );
                while not Assigned( Font.CharDesc[ W ] ) do
                  W := font_GetCID( WordsArray[ i ].str, H, @H );
                WordsArray[ i ].ShiftX := Font.CharDesc[ W ].ShiftX;
              end;
            if LineFeed Then
              b := j + 2
            else
              b := j + 1;
            LineFeed := FALSE;
            W := 0;
            H := 1;
            break;
          end;
      end;
  WordsArray[ WordsCount - 1 ].LF := TRUE;
  WordsArray[ 0 ].W := WordsArray[ 0 ].W + SpaceShift;

  l := 0;
  if Flags and TEXT_HALIGN_JUSTIFY = 0 Then
    INC( WordsCount );
  for i := 0 to WordsCount - 1 do
    begin
      WordsArray[ i ].X := X;
      WordsArray[ i ].Y := Y;
      X := X + WordsArray[ i ].W - SpaceShift;
      if i > 0 Then
        LineFeed := WordsArray[ i - 1 ].LF;
      if ( ( X + SpaceShift >= Rect.X + Rect.W - 1 ) and ( i - l > 0 ) ) or LineFeed Then
        begin
          X := Round( Rect.X ) - WordsArray[ i ].ShiftX - SpaceShift * Byte( not LineFeed );
          if i > 0 Then
            Y := Y + Round( Font.MaxHeight * textScale ) * WordsArray[ i - 1 ].LFShift;
          WordsArray[ i ].X := X;
          WordsArray[ i ].Y := Y;
          X := X + WordsArray[ i ].W - SpaceShift;

          if ( Flags and TEXT_HALIGN_JUSTIFY > 0 ) and ( i - l > 1 ) and ( not LineFeed ) Then
            begin
              W := Round( Rect.X + Rect.W - 1 ) - ( WordsArray[ i - 1 ].X + WordsArray[ i - 1 ].W - SpaceShift );
              while W > ( i - 1 ) - l do
                begin
                  for b := l + 1 to i - 1 do
                    INC( WordsArray[ b ].X, 1 + ( b - ( l + 1 ) ) );
                  W := Round( Rect.X + Rect.W - 1 ) - ( WordsArray[ i - 1 ].X + WordsArray[ i - 1 ].W - SpaceShift );
                end;
              WordsArray[ i - 1 ].X := WordsArray[ i - 1 ].X + W;
            end else
              if Flags and TEXT_HALIGN_CENTER > 0 Then
                begin
                  W := ( Round( Rect.X + Rect.W - 1 ) - ( WordsArray[ i - 1 ].X + WordsArray[ i - 1 ].W - SpaceShift ) ) div 2;
                  if i = WordsCount - 1 Then
                    begin
                      for b := l to i - 1 do
                        INC( WordsArray[ b ].X, W - SpaceShift div 2 );
                    end else
                      for b := l to i - 1 do
                        INC( WordsArray[ b ].X, W );
                end else
                  if Flags and TEXT_HALIGN_RIGHT > 0 Then
                    begin
                      W := Round( Rect.X + Rect.W - 1 ) - ( WordsArray[ i - 1 ].X + WordsArray[ i - 1 ].W - SpaceShift * Byte( not WordsArray[ i - 1 ].LF ) );
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
      H := ( Round( Rect.Y + Rect.H - 1 ) - ( WordsArray[ WordsCount - 1 ].Y + Font.MaxHeight ) ) div 2;
      for i := 0 to WordsCount - 1 do
        INC( WordsArray[ i ].Y, H );
    end else
      if Flags and TEXT_VALIGN_BOTTOM > 0 Then
        begin
          H := Round( Rect.Y + Rect.H - 1 ) - ( WordsArray[ WordsCount - 1 ].Y + Font.MaxHeight );
          for i := 0 to WordsCount - 1 do
            INC( WordsArray[ i ].Y, H );
        end;

  NewFlags := 0;
  if Flags and TEXT_FX_VCA > 0 Then
    NewFlags := NewFlags or TEXT_FX_VCA;
  if Flags and TEXT_FX_LENGTH > 0 Then
    NewFlags := NewFlags or TEXT_FX_LENGTH;

  l := 0;
  b := textLength;
  for i := 0 to WordsCount - 1 do
    begin
      if Flags and TEXT_FX_LENGTH > 0 Then
        begin
          LineFeed := ( i > 0 ) and ( i < WordsCount - 2 ) and ( WordsArray[ i ].Y <> WordsArray[ i - 1 ].Y );
          textFx_SetLength( b - l, textLCoord, textLCharDesc );
          if l > b Then continue;
          l := l + u_Length( WordsArray[ i ].str ) - Byte( not LineFeed );
        end;
      text_Draw( Font, WordsArray[ i ].X, WordsArray[ i ].Y, WordsArray[ i ].str, NewFlags );
    end;

  SetLength( WordsArray, 0 );
end;

procedure text_DrawInRectEx;
begin
  textRGBA[ 0 ] :=   Color             shr 16;
  textRGBA[ 1 ] := ( Color and $FF00 ) shr 8;
  textRGBA[ 2 ] :=   Color and $FF;
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
    lResult : Single;
begin
  lResult := 0;
  Result  := 0;
  if ( Text = '' ) or ( not Assigned( Font ) ) Then exit;
  i  := 1;
  while i <= length( Text ) do
    begin
      c := font_GetCID( Text, i, @i );
      if c = 10 Then
        begin
          lResult := Result;
          Result  := 0;
        end else
          if Assigned( Font.CharDesc[ c ] ) Then
            Result := Result + Font.CharDesc[ c ].ShiftP + Step;
    end;
  if lResult > Result Then
    Result := lResult;
end;

procedure textFx_SetLength;
begin
  textLength    := Length;
  textLCoord    := LastCoord;
  textLCharDesc := LastCharDesc;
end;

end.
