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
unit zgl_fx;

{$I define.inc}

interface

uses
  GL;
  
const
  FX_BLEND_NORMAL = $00;
  FX_BLEND_ADD    = $01;
  FX_BLEND_MULT   = $02;
  FX_BLEND_BLACK  = $03;
  FX_BLEND_WHITE  = $04;
  FX_BLEND_MASK   = $05;
  
procedure fx_SetBlendMode( Mode : Byte ); extdecl;

implementation

procedure fx_SetBlendMode;
begin
  case Mode of
    FX_BLEND_NORMAL : glBlendFunc( GL_SRC_ALPHA,           GL_ONE_MINUS_SRC_ALPHA );
    FX_BLEND_ADD    : glBlendFunc( GL_SRC_ALPHA,           GL_ONE                 );
    FX_BLEND_MULT   : glBlendFunc( GL_ZERO,                GL_SRC_COLOR           );
    FX_BLEND_BLACK  : glBlendFunc( GL_SRC_COLOR,           GL_ONE_MINUS_SRC_COLOR );
    FX_BLEND_WHITE  : glBlendFunc( GL_ONE_MINUS_SRC_COLOR, GL_SRC_COLOR           );
    FX_BLEND_MASK   : glBlendFunc( GL_ZERO,                GL_SRC_COLOR           );
  end;
end;

end.
