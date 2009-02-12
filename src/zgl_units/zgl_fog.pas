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
unit zgl_fog;

{$I define.inc}

interface
uses
  GL, GLExt;

procedure fog_Enable;
procedure fog_Disable;

const
  FOG_MODE_EXP    = 0;
  FOG_MODE_EXP2   = 1;
  FOG_MODE_LINEAR = 2;
procedure fog_SetMode( const Mode : Byte );
procedure fog_SetColor( const Color : DWORD );
procedure fog_SetDensity( const Density : Single );
procedure fog_SetBeginEnd( const fBegin, fEnd : Single );

var
  fog_Color : array[ 0..3 ] of Single = ( 0.9, 0.9, 1.0, 0.0 );

implementation

procedure fog_Enable;
begin
  glEnable( GL_FOG                    );
  glFogfv ( GL_FOG_COLOR,  @fog_Color );
end;

procedure fog_Disable;
begin
  glDisable( GL_FOG );
end;

procedure fog_SetMode;
begin
  case Mode of
    FOG_MODE_EXP:    glFogi( GL_FOG_MODE, GL_EXP    );
    FOG_MODE_EXP2:   glFogi( GL_FOG_MODE, GL_EXP2   );
    FOG_MODE_LINEAR: glFogi( GL_FOG_MODE, GL_LINEAR );
  end;
end;

procedure fog_SetColor;
begin
  fog_Color[ 0 ] := (   Color and $FF              ) * 1 / 255;
  fog_Color[ 1 ] := ( ( Color and $FF00   ) shr 8  ) * 1 / 255;
  fog_Color[ 2 ] := (   Color               shr 16 ) * 1 / 255;

  glFogfv( GL_FOG_COLOR, @fog_Color );
end;

procedure fog_SetDensity;
begin
  glFogf( GL_FOG_DENSITY, Density );
end;

procedure fog_SetBeginEnd;
begin
  glFogf( GL_FOG_START, fBegin );
  glFogf( GL_FOG_END,   fEnd   );
end;

end.
