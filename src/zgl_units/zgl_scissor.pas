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
unit zgl_scissor;

{$I define.inc}

interface

uses
  GL,
  zgl_global_var;
  
procedure scissor_Begin( const X, Y, Width, Height : WORD );
procedure scissor_End;

implementation

var
  tSCount  : WORD;
  tScissor : array of array[ 0..3 ] of WORD;

procedure scissor_Begin;
begin
  glEnable( GL_SCISSOR_TEST );
  glScissor( X, ogl_Height - Y - Height, Width, Height );

  INC( tSCount );
  SetLength( tScissor, tSCount );
  tScissor[ tSCount - 1 ][ 0 ] := ogl_CropX;
  tScissor[ tSCount - 1 ][ 1 ] := ogl_CropY;
  tScissor[ tSCount - 1 ][ 2 ] := ogl_CropW;
  tScissor[ tSCount - 1 ][ 3 ] := ogl_CropH;

  ogl_CropX := X;
  ogl_CropY := Y;
  ogl_CropW := Width;
  ogl_CropH := Height;
end;

procedure scissor_End;
begin
  glDisable( GL_SCISSOR_TEST );
  DEC( tSCount );
  ogl_CropX := tScissor[ tSCount ][ 0 ];
  ogl_CropY := tScissor[ tSCount ][ 1 ];
  ogl_CropW := tScissor[ tSCount ][ 2 ];
  ogl_CropH := tScissor[ tSCount ][ 3 ];
  SetLength( tScissor, tSCount );
end;

end.
