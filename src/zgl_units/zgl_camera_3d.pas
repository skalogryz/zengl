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
unit zgl_camera_3d;

{$I define.inc}

interface

uses
  GL,
  zgl_opengl,
  zgl_const,
  zgl_types,
  zgl_math;

procedure cam3d_Set( Camera : zglPCamera3D ); extdecl;
procedure cam3d_Fly( Camera : zglPCamera3D; Speed : Single ); extdecl;
procedure cam3d_Strafe( Camera : zglPCamera3D; Speed : Single ); extdecl;

implementation

procedure cam3d_Set;
  var
    A, B, C, D, E, F : Single;
    cx, cy, cz       : zglTPoint3D;
begin
  m_SinCos( Camera.Rotation.X, B, A );
  m_SinCos( Camera.Rotation.Y, C, D );
  m_SinCos( Camera.Rotation.Z, F, E );

  cx := vector_Get( C * E + B * D * F, A * F, B * C * F - E * D );
  cy := vector_Get( B * D * E - C * F, A * E, D * F + B * C * E );
  cz := vector_Get(             A * D,    -B,             A * C );

  Camera.Matrix[ 0 ][ 0 ] := cx.X;
  Camera.Matrix[ 0 ][ 1 ] := cy.X;
  Camera.Matrix[ 0 ][ 2 ] := cz.X;
  Camera.Matrix[ 0 ][ 3 ] := 0;

  Camera.Matrix[ 1 ][ 0 ] := cx.Y;
  Camera.Matrix[ 1 ][ 1 ] := cy.Y;
  Camera.Matrix[ 1 ][ 2 ] := cz.Y;
  Camera.Matrix[ 1 ][ 3 ] := 0;

  Camera.Matrix[ 2 ][ 0 ] := cx.Z;
  Camera.Matrix[ 2 ][ 1 ] := cy.Z;
  Camera.Matrix[ 2 ][ 2 ] := cz.Z;
  Camera.Matrix[ 2 ][ 3 ] := 0;

  Camera.Matrix[ 3 ][ 0 ] := -vector_Dot( cx, Camera.Position );
  Camera.Matrix[ 3 ][ 1 ] := -vector_Dot( cy, Camera.Position );
  Camera.Matrix[ 3 ][ 2 ] := -vector_Dot( cz, Camera.Position );
  Camera.Matrix[ 3 ][ 3 ] := 1;
  
  glLoadMatrixf( @Camera.Matrix );
end;

procedure cam3d_Fly;
  var
    Vector : zglTPoint3D;
begin
  Vector.X :=  Cos( Camera.Rotation.Y ) * Cos( Camera.Rotation.X );
  Vector.Y := -Sin( Camera.Rotation.X ) * Cos( Camera.Rotation.Z );
  Vector.Z :=  Sin( Camera.Rotation.Y ) * Cos( Camera.Rotation.X );
  Camera.Position := vector_Add( Camera.Position, vector_MulV( Vector, Speed ) );
end;

procedure cam3d_Strafe;
  var
    Cross,Vector : zglTPoint3D;
    Up    : zglTPoint3D;
begin
  Up.X := 0;
  Up.Y := 1;
  Up.Z := 0;
  Vector.X := Cos( Camera.Rotation.Y ) * Cos( Camera.Rotation.X );
  Vector.Y := Sin( Camera.Rotation.X );
  Vector.Z := Sin( Camera.Rotation.Y ) * Cos( Camera.Rotation.X );
  Cross := vector_Cross( Up, Vector );
  Camera.Position := vector_Add( Camera.Position, vector_MulV( Cross, Speed ) );
end;

end.
