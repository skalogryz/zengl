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
  
procedure scissor_Begin( X, Y, Width, Height : WORD ); extdecl;
procedure scissor_End; extdecl;

implementation

var
  tScissorX : WORD;
  tScissorY : WORD;
  tScissorW : WORD;
  tScissorH : WORD;

procedure scissor_Begin;
begin
  glEnable( GL_SCISSOR_TEST );
  glScissor( X, ogl_Height - Y - Height, Width, Height );

  tScissorX := ScissorX;
  tScissorY := ScissorY;
  tScissorW := ScissorW;
  tScissorH := ScissorH;

  ScissorX := X;
  ScissorY := Y;
  ScissorW := Width  + X;
  ScissorH := Height + Y;
end;

procedure scissor_End;
begin
  glDisable( GL_SCISSOR_TEST );
  ScissorX := tScissorX;
  ScissorY := tScissorY;
  ScissorW := tScissorW;
  ScissorH := tScissorH;
end;

end.
