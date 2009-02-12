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
unit zgl_light;

{$I define.inc}

interface

uses
  GL,
  zgl_global_var,
  zgl_object_3d;

procedure light_Enable( const ID : Byte );
procedure light_Disable( const ID : Byte );
procedure light_SetPosition( const ID : Byte; const X, Y, Z, W : Single );

procedure light_SetMaterial( const ID, Material : Byte; const Color : DWORD; const Alpha : Byte );

implementation

procedure light_Enable;
  var
    i : Byte;
begin
  glEnable( GL_LIGHT0 + ID - 1 );
  if ID = 0 Then
    for i := 0 to ogl_MaxLights do
      glEnable( GL_LIGHT0 + i );
end;

procedure light_Disable;
  var
    i : Byte;
begin
  glDisable( GL_LIGHT0 + ID - 1 );
  if ID = 0 Then
    for i := 0 to ogl_MaxLights do
      glDisable( GL_LIGHT0 + i );
end;

procedure light_SetPosition;
  var
    pos : array[ 0..3 ] of Single;
begin
  pos[ 0 ] := X;
  pos[ 1 ] := Y;
  pos[ 2 ] := Z;
  pos[ 3 ] := W;
  glLightfv( GL_LIGHT0 + ID - 1, GL_POSITION, @pos );
end;

procedure light_SetMaterial;
  var
    clr : array[ 0..3 ] of Single;
begin
  clr[ 0 ] := (   Color and $FF   )         / 255;
  clr[ 1 ] := ( ( Color and $FF00 ) shr 8 ) / 255;
  clr[ 2 ] := (   Color shr 16    )         / 255;
  clr[ 3 ] := ( 1 / 255 ) * Alpha;
  glLightfv( GL_LIGHT0 + ID - 1,
             GL_AMBIENT  * Byte( Material = MAT_AMBIENT ) or
             GL_DIFFUSE  * Byte( Material = MAT_DIFFUSE ) or
             GL_SPECULAR * Byte( Material = MAT_SPECULAR ),
             @clr );
end;

end.
