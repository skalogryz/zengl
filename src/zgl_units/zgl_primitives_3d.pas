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
unit zgl_primitives_3d;

{$I define.inc}

interface
uses
  GL,
  zgl_types,
  zgl_opengl,
  zgl_math;

procedure pr3d_Point( X, Y, Z : Single ); extdecl;
procedure pr3d_Line( X1, Y1, Z1, X2, Y2, Z2 : Single ); extdecl;
procedure pr3d_Plane( Width, Height : Single ); extdecl;
procedure pr3d_AABB( Width, Height, ZDepth : Single ); extdecl;
procedure pr3d_Sphere( Radius : Single; Quality : Integer ); extdecl;

implementation

procedure pr3d_Point;
begin
  glBegin( GL_POINTS );
    glVertex3f( X, Y, Z );
  glEnd;
end;

procedure pr3d_Line;
begin
  glBegin( GL_LINES );
    glVertex3f( X1, Y1, Z1 );
    glVertex3f( X2, Y2, Z2 );
  glEnd;
end;

procedure pr3d_Plane;
begin
  glBegin( GL_QUADS );
    glNormal3f( 0, 0, 1 );
    gl_TexCoord2f( 0, 0 ); glVertex2f( -Width, -Height );
    gl_TexCoord2f( 1, 0 ); glVertex2f(  Width, -Height );
    gl_TexCoord2f( 1, 1 ); glVertex2f(  Width,  Height );
    gl_TexCoord2f( 0, 1 ); glVertex2f( -Width,  Height );
  glEnd;
end;

procedure pr3d_AABB;
begin
glBegin( GL_QUADS );
  // bottom
  glNormal3f( 0, -1, 0 );
  gl_TexCoord2f( 1, 1 ); glVertex3f( -Width, -Height, -ZDepth );
  gl_TexCoord2f( 0, 1 ); glVertex3f(  Width, -Height, -ZDepth );
  gl_TexCoord2f( 0, 0 ); glVertex3f(  Width, -Height,  ZDepth );
  gl_TexCoord2f( 1, 0 ); glVertex3f( -Width, -Height,  ZDepth );

  // top
  glNormal3f( 0, 1, 0 );
  gl_TexCoord2f( 0, 1 ); glVertex3f( -Width,  Height, -ZDepth );
  gl_TexCoord2f( 0, 0 ); glVertex3f( -Width,  Height,  ZDepth );
  gl_TexCoord2f( 1, 0 ); glVertex3f(  Width,  Height,  ZDepth );
  gl_TexCoord2f( 1, 1 ); glVertex3f(  Width,  Height, -ZDepth );

  // back
  glNormal3f( 0, 0, -1 );
  gl_TexCoord2f( 1, 0 ); glVertex3f( -Width, -Height, -ZDepth );
  gl_TexCoord2f( 1, 1 ); glVertex3f( -Width,  Height, -ZDepth );
  gl_TexCoord2f( 0, 1 ); glVertex3f(  Width,  Height, -ZDepth );
  gl_TexCoord2f( 0, 0 ); glVertex3f(  Width, -Height, -ZDepth );

  // front
  glNormal3f( 0, 0, 1 );
  gl_TexCoord2f( 0, 0 ); glVertex3f( -Width, -Height,  ZDepth );
  gl_TexCoord2f( 1, 0 ); glVertex3f(  Width, -Height,  ZDepth );
  gl_TexCoord2f( 1, 1 ); glVertex3f(  Width,  Height,  ZDepth );
  gl_TexCoord2f( 0, 1 ); glVertex3f( -Width,  Height,  ZDepth );

  // left
  glNormal3f( -1, 0, 0 );
  gl_TexCoord2f( 0, 0 ); glVertex3f( -Width, -Height, -ZDepth );
  gl_TexCoord2f( 1, 0 ); glVertex3f( -Width, -Height,  ZDepth );
  gl_TexCoord2f( 1, 1 ); glVertex3f( -Width,  Height,  ZDepth );
  gl_TexCoord2f( 0, 1 ); glVertex3f( -Width,  Height, -ZDepth );

  // right
  glNormal3f( 1, 0, 0 );
  gl_TexCoord2f( 1, 0 ); glVertex3f( Width, -Height, -ZDepth );
  gl_TexCoord2f( 1, 1 ); glVertex3f( Width,  Height, -ZDepth );
  gl_TexCoord2f( 0, 1 ); glVertex3f( Width,  Height,  ZDepth );
  gl_TexCoord2f( 0, 0 ); glVertex3f( Width, -Height,  ZDepth );
glEnd;
end;

procedure pr3d_Sphere;
  var
    i, j, tmpN : Integer;
    t1, t2, t3 : Integer;
    x, y, z, q : Single;
begin
  if Radius  < 0 Then Radius  := -Radius;
  if Quality < 0 Then Quality := -Quality;

  tmpN := 1;
  while tmpN < Quality do tmpN := tmpN * 2;
  Quality := tmpN;
  q := 1 / tmpN;

  for j := 0 to Quality shr 1 - 1 do
    begin
      t1 := m_Round( j * 360 * q - 90 );
      t2 := m_Round( ( j + 1 ) * 360 * q - 90 );
      glBegin( GL_TRIANGLE_STRIP );
      for i := 0 to Quality do
        begin
          t3 := M_Round( i * 360 * q );

          x := m_Cos( t2 ) * m_Cos( t3 );
          y := m_Sin( t2 );
          z := m_Cos( t2 ) * m_Sin( t3 );

          glNormal3f( x, -y, z );
          gl_TexCoord2f( 1 - i * q, 2 * ( j + 1 ) * q );
          glVertex3f( Radius * x, -Radius * y, Radius * z );

          x := m_Cos( t1 ) * m_Cos( t3 );
          y := m_Sin( t1 );
          z := m_Cos( t1 ) * m_Sin( t3 );

          glNormal3f( x, -y, z );
          gl_TexCoord2f( 1 - i * q, 2 * j * q );
          glVertex3f( Radius * x, -Radius * y, Radius * z );
        end;
      glEnd;
    end;
end;

end.
