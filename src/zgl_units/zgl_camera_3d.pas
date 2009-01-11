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

procedure cam3d_Set( var Camera : zglTCamera3D );
procedure cam3d_Fly( var Camera : zglTCamera3D; const Speed : Single );
procedure cam3d_Strafe( var Camera : zglTCamera3D; const Speed : Single );

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

  Camera.Matrix.a11 := cx.X;
  Camera.Matrix.a12 := cy.X;
  Camera.Matrix.a13 := cz.X;
  Camera.Matrix.a14 := 0;

  Camera.Matrix.a21 := cx.Y;
  Camera.Matrix.a22 := cy.Y;
  Camera.Matrix.a23 := cz.Y;
  Camera.Matrix.a24 := 0;

  Camera.Matrix.a31 := cx.Z;
  Camera.Matrix.a32 := cy.Z;
  Camera.Matrix.a33 := cz.Z;
  Camera.Matrix.a34 := 0;

  Camera.Matrix.a41 := -vector_Dot( cx, Camera.Position );
  Camera.Matrix.a42 := -vector_Dot( cy, Camera.Position );
  Camera.Matrix.a43 := -vector_Dot( cz, Camera.Position );
  Camera.Matrix.a44 := 1;
  
  glLoadMatrixf( @Camera.Matrix );
end;

procedure cam3d_Fly;
  var
    Vector : zglTPoint3D;
begin
  Vector.X :=  Cos( Camera.Rotation.Y ) * Cos( Camera.Rotation.X );
  Vector.Y := -Sin( Camera.Rotation.X );
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
