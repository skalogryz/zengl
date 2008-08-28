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
unit zgl_fx_2d;

{$I define.inc}

interface

const
  FX2D_FLIPX    = $000001;
  FX2D_FLIPY    = $000002;
  FX2D_COLORMIX = $000004;
  FX2D_VCA      = $000008;
  FX2D_VCHANGE  = $000010;
  FX2D_SCALE    = $000020;
  
  FX_BLEND      = $000040;
  
procedure fx2d_SetColorMix( Color : DWORD ); extdecl;
procedure fx2d_SetVCA( c1, c2, c3, c4 : DWORD; a1, a2, a3, a4 : Byte ); extdecl;
procedure fx2d_SetVertexes( x1, y1, x2, y2, x3, y3, x4, y4 : Single ); extdecl;
procedure fx2d_SetScale( scaleX, scaleY : Single ); extdecl;
  
var
  // FX2D_COLORMIX
  FX2D_R : Byte = 255;
  FX2D_G : Byte = 255;
  FX2D_B : Byte = 255;
  
  // FX2D_VCA
  FX2D_VR1, FX2D_VG1, FX2D_VB1, FX2D_VA1 : Byte;
  FX2D_VR2, FX2D_VG2, FX2D_VB2, FX2D_VA2 : Byte;
  FX2D_VR3, FX2D_VG3, FX2D_VB3, FX2D_VA3 : Byte;
  FX2D_VR4, FX2D_VG4, FX2D_VB4, FX2D_VA4 : Byte;
  
  // FX2D_VCHANGE
  FX2D_VX1, FX2D_VX2, FX2D_VX3, FX2D_VX4 : Single;
  FX2D_VY1, FX2D_VY2, FX2D_VY3, FX2D_VY4 : Single;
  
  // FX2D_SCALE
  FX2D_SX, FX2D_SY : Single;

implementation

procedure fx2d_SetColorMix;
begin
  FX2D_R :=   Color and $FF;
  FX2D_G := ( Color and $FF00 ) shr 8;
  FX2D_B :=   Color             shr 16;
end;

procedure fx2d_SetVCA;
begin
  FX2D_VR1 :=   C1 and $FF;
  FX2D_VG1 := ( C1 and $FF00 ) shr 8;
  FX2D_VB1 :=   C1             shr 16;
  FX2D_VA1 := A1;

  FX2D_VR2 :=   C2 and $FF;
  FX2D_VG2 := ( C2 and $FF00 ) shr 8;
  FX2D_VB2 :=   C2             shr 16;
  FX2D_VA2 := A2;

  FX2D_VR3 :=   C3 and $FF;
  FX2D_VG3 := ( C3 and $FF00 ) shr 8;
  FX2D_VB3 :=   C3             shr 16;
  FX2D_VA3 := A3;

  FX2D_VR4 :=   C4 and $FF;
  FX2D_VG4 := ( C4 and $FF00 ) shr 8;
  FX2D_VB4 :=   C4             shr 16;
  FX2D_VA4 := A4;
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