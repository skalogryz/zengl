{
 *  Copyright © Kemka Andrey aka Andru
 *  mail: dr.andru@gmail.com
 *  site: http://andru-kun.inf.ua
 *
 *  This file is part of ZenGL.
 *
 *  ZenGL is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public Licens as
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
unit zgl_fx;

{$I zgl_config.cfg}

interface

const
  FX_BLEND_NORMAL = $00;
  FX_BLEND_ADD    = $01;
  FX_BLEND_MULT   = $02;
  FX_BLEND_BLACK  = $03;
  FX_BLEND_WHITE  = $04;
  FX_BLEND_MASK   = $05;

  FX_COLOR_MIX    = $00;
  FX_COLOR_SET    = $01;

  FX2D_FLIPX      = $000001;
  FX2D_FLIPY      = $000002;
  FX2D_VCA        = $000004;
  FX2D_VCHANGE    = $000008;
  FX2D_SCALE      = $000010;

  FX_BLEND        = $100000;
  FX_COLOR        = $200000;

procedure fx_SetBlendMode( const Mode : Byte );
procedure fx_SetColorMode( const Mode : Byte );

procedure fx2d_SetColor( const Color : LongWord );
procedure fx2d_SetVCA( const c1, c2, c3, c4 : LongWord; const a1, a2, a3, a4 : Byte );
procedure fx2d_SetVertexes( const x1, y1, x2, y2, x3, y3, x4, y4 : Single );
procedure fx2d_SetScale( const scaleX, scaleY : Single );

var
  // FX2D_COLORMIX
  FX2D_R : Byte = 255;
  FX2D_G : Byte = 255;
  FX2D_B : Byte = 255;

  // FX2D_VCA
  FX2D_VCA1 : array[ 0..3 ] of Byte = ( 255, 255, 255, 255 );
  FX2D_VCA2 : array[ 0..3 ] of Byte = ( 255, 255, 255, 255 );
  FX2D_VCA3 : array[ 0..3 ] of Byte = ( 255, 255, 255, 255 );
  FX2D_VCA4 : array[ 0..3 ] of Byte = ( 255, 255, 255, 255 );

  // FX2D_VCHANGE
  FX2D_VX1, FX2D_VX2, FX2D_VX3, FX2D_VX4 : Single;
  FX2D_VY1, FX2D_VY2, FX2D_VY3, FX2D_VY4 : Single;

  // FX2D_SCALE
  FX2D_SX, FX2D_SY : Single;

implementation
uses
  zgl_opengl,
  zgl_opengl_all,
  zgl_render_2d;

procedure fx_SetBlendMode;
  var
    srcBlend : LongWord;
    dstBlend : LongWord;
begin
  if b2d_Started and ( Mode <> b2dcur_Blend ) Then
    begin
      batch2d_Flush();
      b2d_New := TRUE;
    end;
  b2dcur_Blend := Mode;
  case Mode of
    FX_BLEND_NORMAL:
      begin
        srcBlend := GL_SRC_ALPHA;
        dstBlend := GL_ONE_MINUS_SRC_ALPHA;
      end;
    FX_BLEND_ADD:
      begin
        srcBlend := GL_SRC_ALPHA;
        dstBlend := GL_ONE;
      end;
    FX_BLEND_MULT:
      begin
        srcBlend := GL_ZERO;
        dstBlend := GL_SRC_COLOR;
      end;
    FX_BLEND_BLACK:
      begin
        srcBlend := GL_SRC_COLOR;
        dstBlend := GL_ONE_MINUS_SRC_COLOR;
      end;
    FX_BLEND_WHITE:
      begin
        srcBlend := GL_ONE_MINUS_SRC_COLOR;
        dstBlend := GL_SRC_COLOR;
      end;
    FX_BLEND_MASK:
      begin
        srcBlend := GL_ZERO;
        dstBlend := GL_SRC_COLOR;
      end;
  end;
  if ogl_Separate Then
    glBlendFuncSeparateEXT( srcBlend, dstBlend, GL_ONE, GL_ONE_MINUS_SRC_ALPHA )
  else
    glBlendFunc( srcBlend, dstBlend );
end;

procedure fx_SetColorMode;
begin
  if b2d_Started and ( Mode <> b2dcur_Color ) Then
    begin
      batch2d_Flush();
      b2d_New := TRUE;
    end;
  b2dcur_Color := Mode;
  case Mode of
    FX_COLOR_MIX:
      begin
        glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE );
      end;
    FX_COLOR_SET:
      begin
        glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_ARB );
        glTexEnvi( GL_TEXTURE_ENV, GL_COMBINE_RGB_ARB,  GL_REPLACE );
        glTexEnvi( GL_TEXTURE_ENV, GL_SOURCE0_RGB_ARB,  GL_PRIMARY_COLOR_ARB );
      end;
  end;
end;

procedure fx2d_SetColor;
begin
  FX2D_R :=   Color             shr 16;
  FX2D_G := ( Color and $FF00 ) shr 8;
  FX2D_B :=   Color and $FF;
end;

procedure fx2d_SetVCA;
begin
  FX2D_VCA1[ 0 ] :=   C1             shr 16;
  FX2D_VCA1[ 1 ] := ( C1 and $FF00 ) shr 8;
  FX2D_VCA1[ 2 ] :=   C1 and $FF;
  FX2D_VCA1[ 3 ] := A1;

  FX2D_VCA2[ 0 ] :=   C2             shr 16;
  FX2D_VCA2[ 1 ] := ( C2 and $FF00 ) shr 8;
  FX2D_VCA2[ 2 ] :=   C2 and $FF;
  FX2D_VCA2[ 3 ] := A2;

  FX2D_VCA3[ 0 ] :=   C3             shr 16;
  FX2D_VCA3[ 1 ] := ( C3 and $FF00 ) shr 8;
  FX2D_VCA3[ 2 ] :=   C3 and $FF;
  FX2D_VCA3[ 3 ] := A3;

  FX2D_VCA4[ 0 ] :=   C4             shr 16;
  FX2D_VCA4[ 1 ] := ( C4 and $FF00 ) shr 8;
  FX2D_VCA4[ 2 ] :=   C4 and $FF;
  FX2D_VCA4[ 3 ] := A4;
end;

procedure fx2d_SetVertexes;
begin
  FX2D_VX1 := x1;
  FX2D_VY1 := y1;
  FX2D_VX2 := x2;
  FX2D_VY2 := y2;
  FX2D_VX3 := x3;
  FX2D_VY3 := y3;
  FX2D_VX4 := x4;
  FX2D_VY4 := y4;
end;

procedure fx2d_SetScale;
begin
  FX2D_SX := scaleX;
  FX2D_SY := scaleY;
end;

end.
