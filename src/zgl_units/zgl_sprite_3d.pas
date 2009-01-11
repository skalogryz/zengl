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
unit zgl_sprite_3d;

{$I define.inc}

interface
uses
  GL,
  zgl_opengl,
  zgl_types,
  zgl_global_var,
  zgl_math;

procedure ssprite3d_Draw( const X, Y, Z, sX, sY, sZ : Single; const Matrix : zglTMatrix4f );
procedure asprite3d_Draw( const X, Y, Z, sX, sY, sZ : Single; const Frame : Integer; const Matrix : zglTMatrix4f );

implementation

procedure ssprite3d_Draw;
  var
    Cross1, Cross2 : zglTPoint3D;
    Position       : zglTPoint3D;
    Size           : zglTPoint3D;
    A, B, C, D     : zglTPoint3D;
begin
  Position.X := X;
  Position.Y := Y;
  Position.Z := Z;
  Size.X     := sX;
  Size.Y     := sY;
  Size.Z     := sZ;

  Cross1.X := Matrix.a11;
  Cross1.Y := Matrix.a21;
  Cross1.Z := Matrix.a31;
  Cross1   := vector_Mul( Cross1, Size );

  Cross2.X := Matrix.a12;
  Cross2.Y := Matrix.a22;
  Cross2.Z := Matrix.a32;
  Cross2   := Vector_Mul( Cross2, Size );

  A := vector_Add( Position, vector_Get( -Cross1.X - Cross2.X, -Cross1.Y - Cross2.Y, -Cross1.Z - Cross2.Z ) );
  B := vector_Add( Position, vector_Get(  Cross1.X - Cross2.X,  Cross1.Y - Cross2.Y,  Cross1.Z - Cross2.Z ) );
  C := vector_Add( Position, vector_Get(  Cross1.X + Cross2.X,  Cross1.Y + Cross2.Y,  Cross1.Z + Cross2.Z ) );
  D := vector_Add( Position, vector_Get( -Cross1.X + Cross2.X, -Cross1.Y + Cross2.Y, -Cross1.Z + Cross2.Z ) );

  glBegin( GL_QUADS );
    gl_TexCoord2f( 0, 0 ); glVertex3fv( @A );
    gl_TexCoord2f( 1, 0 ); glVertex3fv( @B );
    gl_TexCoord2f( 1, 1 ); glVertex3fv( @C );
    gl_TexCoord2f( 0, 1 ); glVertex3fv( @D );
  glEnd;
end;

procedure asprite3d_Draw;
  var
    Cross1, Cross2 : zglTPoint3D;
    Position       : zglTPoint3D;
    Size           : zglTPoint3D;
    A, B, C, D     : zglTPoint3D;
    FX, FY, SU, SV : Single;
begin
  Position.X := X;
  Position.Y := Y;
  Position.Z := Z;
  Size.X     := sX;
  Size.Y     := sY;
  Size.Z     := sZ;

  Cross1.X := Matrix.a11;
  Cross1.Y := Matrix.a21;
  Cross1.Z := Matrix.a31;
  Cross1   := vector_Mul( Cross1, Size );

  Cross2.X := Matrix.a12;
  Cross2.Y := Matrix.a22;
  Cross2.Z := Matrix.a32;
  Cross2   := Vector_Mul( Cross2, Size );

  A := vector_Add( Position, vector_Get( -Cross1.X - Cross2.X, -Cross1.Y - Cross2.Y, -Cross1.Z - Cross2.Z ) );
  B := vector_Add( Position, vector_Get(  Cross1.X - Cross2.X,  Cross1.Y - Cross2.Y,  Cross1.Z - Cross2.Z ) );
  C := vector_Add( Position, vector_Get(  Cross1.X + Cross2.X,  Cross1.Y + Cross2.Y,  Cross1.Z + Cross2.Z ) );
  D := vector_Add( Position, vector_Get( -Cross1.X + Cross2.X, -Cross1.Y + Cross2.Y, -Cross1.Z + Cross2.Z ) );

  SU := ogl_LastTexture.U / ogl_LastTexture.FramesX;
  SV := ogl_LastTexture.V / ogl_LastTexture.FramesY;

  FX := abs( Frame );
  FY := ogl_LastTexture.FramesY;
  while FX > ogl_LastTexture.FramesX do
    begin
      FX := FX - ogl_LastTexture.FramesX;
      FY := FY - 1;
    end;
  while FY < 1 do
    FY := FY + ogl_LastTexture.FramesY;
  FX := FX * SU;
  FY := FY * SV;

  glBegin( GL_QUADS );
    gl_TexCoord2f( FX - SU, FY - SV ); glVertex3fv( @A );
    gl_TexCoord2f(      FX, FY - SV ); glVertex3fv( @B );
    gl_TexCoord2f(      FX,      FY ); glVertex3fv( @C );
    gl_TexCoord2f( FX - SU,      FY ); glVertex3fv( @D );
  glEnd;
end;

end.
