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
unit zgl_camera_2d;

{$I define.inc}

interface

uses
  GL,
  zgl_opengl,
  zgl_const,
  zgl_types,
  zgl_global_var;
  
procedure cam2d_Set( const Camera : zglPCamera2D );

procedure cam2d_Vertex2f( X, Y : Single );
procedure cam2d_Vertex2fv( v : Pointer );

var
  cam2dGlobal   : zglPCamera2D = nil;
  constCamera2D : zglTCamera2D = ( X: 0; Y: 0 );

implementation

procedure cam2d_Set;
begin
  cam2dGlobal := Camera;
  if Camera = nil Then
    begin
      cam2dGlobal  := @constCamera2D;
      gl_Vertex2f  := @glVertex2f;
      gl_Vertex2fv := @glVertex2fv;
    end else
      begin
        gl_Vertex2f  := @cam2d_Vertex2f;
        gl_Vertex2fv := @cam2d_Vertex2fv;
      end;
end;

procedure cam2d_Vertex2f;
begin
  X := X - cam2dGlobal.X;
  Y := Y - cam2dGlobal.Y;
  glVertex2f( X, Y );
end;

procedure cam2d_Vertex2fv;
  var
    v2 : array[ 0..1 ] of Single;
begin
  v2[ 0 ] := PSingle( v + 0 )^ - cam2dGlobal.X;
  v2[ 1 ] := PSingle( v + 4 )^ - cam2dGlobal.Y;
  glVertex2fv( @v2[ 0 ] );
end;

initialization
  cam2dGlobal := @constCamera2D;

end.
