{
 * Copyright © Kemka Andrey aka Andru
 * mail: dr.andru@gmail.com
 * site: http://andru-kun.ru
 *
 * This file is part of ZenGL
 *
 * ZenGL is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * ZenGL is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
}
unit zgl_camera_3d;

{$I zgl_config.cfg}

interface
uses
  zgl_math_3d;

type
  zglPCamera3D = ^zglTCamera3D;
  zglTCamera3D = record
    Position : zglTPoint3D;
    Rotation : zglTPoint3D;
    Matrix   : zglTMatrix4f;
end;

procedure cam3d_Set( const Camera : zglTCamera3D );
procedure cam3d_CalcView( var Camera : zglTCamera3D );
procedure cam3d_LookAt( var Camera : zglTCamera3D; const Eyes, Up : zglTPoint3D );
procedure cam3d_Fly( var Camera : zglTCamera3D; const Speed : Single );
procedure cam3d_Strafe( var Camera : zglTCamera3D; const Speed : Single );

implementation
uses
  zgl_opengl_all;

procedure cam3d_Set;
begin
  glLoadMatrixf( @Camera.Matrix );
end;

procedure cam3d_CalcView;
begin
  with Camera, Camera.Matrix do
    begin
      Matrix := matrix4f_Identity;
      matrix4f_SetRot( Matrix, Rotation.X, Rotation.Y, Rotation.Z );
      Matrix := matrix4f_Translate( Matrix, -Position.X, -Position.Y, -Position.Z );
    end;
end;

procedure cam3d_LookAt;
  var
    x, y, z : zglTPoint3D;
begin
  with Camera, Camera.Matrix do
    begin
      z := vector_Sub( Eyes, Position );
      z := vector_Negate( z );
      z := vector_Normalize( z );

      x := vector_Cross( Up, z );
      x := vector_Normalize( x );

      y := vector_Cross( z, x );
      y := vector_Normalize( y );

      a11 := x.X;
      a12 := x.Y;
      a13 := x.Z;
      a14 := 0;

      a21 := y.X;
      a22 := y.Y;
      a23 := y.Z;
      a24 := 0;

      a31 := z.X;
      a32 := z.Y;
      a33 := z.Z;
      a34 := 0;

      a41 := Position.X;
      a42 := Position.Y;
      a43 := Position.Z;
      a44 := 1;

      Matrix := matrix4f_Inverse( Matrix );
    end;
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
    Cross, Vector : zglTPoint3D;
    Up : zglTPoint3D;
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
