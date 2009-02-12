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
unit zgl_frustum;

{$I define.inc}

interface

uses
  GL,

  zgl_types;

procedure frustum_Calc( var f : zglTFrustum );

function frustum_PointIn   ( const f : zglTFrustum; const x, y, z : Single )             : Boolean;
function frustum_PPointIn  ( const f : zglTFrustum; const Vertex : zglTPoint3D )         : Boolean;
function frustum_TriangleIn( const f : zglTFrustum; const v1, v2, v3 : zglTPoint3D )     : Boolean;
function frustum_SphereIn  ( const f : zglTFrustum; const x, y, z, r : Single )          : Boolean;
function frustum_BoxIn     ( const f : zglTFrustum; const x, y, z, bx, by, bz : Single ) : Boolean;
function frustum_CubeIn    ( const f : zglTFrustum; const x, y, z, size : Single )       : Boolean;

implementation

procedure frustum_Calc;
  var
    p, m, clip : array [ 0..15 ] of Single;
    t          : Single;
begin
  glGetFloatv( GL_PROJECTION_MATRIX, @p );
  glGetFloatv( GL_MODELVIEW_MATRIX,  @m );

  clip[ 0 ] := m[ 0 ] * p[ 0 ] + m[ 1 ] * p[ 4 ] + m[ 2 ] * p[ 8 ]  + m[ 3 ] * p[ 12 ];
  clip[ 1 ] := m[ 0 ] * p[ 1 ] + m[ 1 ] * p[ 5 ] + m[ 2 ] * p[ 9 ]  + m[ 3 ] * p[ 13 ];
  clip[ 2 ] := m[ 0 ] * p[ 2 ] + m[ 1 ] * p[ 6 ] + m[ 2 ] * p[ 10 ] + m[ 3 ] * p[ 14 ];
  clip[ 3 ] := m[ 0 ] * p[ 3 ] + m[ 1 ] * p[ 7 ] + m[ 2 ] * p[ 11 ] + m[ 3 ] * p[ 15 ];

  clip[ 4 ] := m[ 4 ] * p[ 0 ] + m[ 5 ] * p[ 4 ] + m[ 6 ] * p[  8 ] + m[ 7 ] * p[ 12 ];
  clip[ 5 ] := m[ 4 ] * p[ 1 ] + m[ 5 ] * p[ 5 ] + m[ 6 ] * p[  9 ] + m[ 7 ] * p[ 13 ];
  clip[ 6 ] := m[ 4 ] * p[ 2 ] + m[ 5 ] * p[ 6 ] + m[ 6 ] * p[ 10 ] + m[ 7 ] * p[ 14 ];
  clip[ 7 ] := m[ 4 ] * p[ 3 ] + m[ 5 ] * p[ 7 ] + m[ 6 ] * p[ 11 ] + m[ 7 ] * p[ 15 ];

  clip[ 8  ] := m[ 8 ] * p[ 0 ] + m[ 9 ] * p[ 4 ] + m[ 10 ] * p[  8 ] + m[ 11 ] * p[ 12 ];
  clip[ 9  ] := m[ 8 ] * p[ 1 ] + m[ 9 ] * p[ 5 ] + m[ 10 ] * p[  9 ] + m[ 11 ] * p[ 13 ];
  clip[ 10 ] := m[ 8 ] * p[ 2 ] + m[ 9 ] * p[ 6 ] + m[ 10 ] * p[ 10 ] + m[ 11 ] * p[ 14 ];
  clip[ 11 ] := m[ 8 ] * p[ 3 ] + m[ 9 ] * p[ 7 ] + m[ 10 ] * p[ 11 ] + m[ 11 ] * p[ 15 ];

  clip[ 12 ] := m[ 12 ] * p[ 0 ] + m[ 13 ] * p[ 4 ] + m[ 14 ] * p[ 8  ] + m[ 15 ] * p[ 12 ];
  clip[ 13 ] := m[ 12 ] * p[ 1 ] + m[ 13 ] * p[ 5 ] + m[ 14 ] * p[ 9  ] + m[ 15 ] * p[ 13 ];
  clip[ 14 ] := m[ 12 ] * p[ 2 ] + m[ 13 ] * p[ 6 ] + m[ 14 ] * p[ 10 ] + m[ 15 ] * p[ 14 ];
  clip[ 15 ] := m[ 12 ] * p[ 3 ] + m[ 13 ] * p[ 7 ] + m[ 14 ] * p[ 11 ] + m[ 15 ] * p[ 15 ];

  //Правая отсекающая плоскость
  f[ 0 ][ 0 ] := clip[  3 ] - clip[  0 ];
  f[ 0 ][ 1 ] := clip[  7 ] - clip[  4 ];
  f[ 0 ][ 2 ] := clip[ 11 ] - clip[  8 ];
  f[ 0 ][ 3 ] := clip[ 15 ] - clip[ 12 ];
  t := 1 / Sqrt( f[ 0 ][ 0 ] * f[ 0 ][ 0 ] + f[ 0 ][ 1 ] * f[ 0 ][ 1 ] + f[ 0 ][ 2 ] * f[ 0 ][ 2 ] );
  f[ 0 ][ 0 ] := f[ 0 ][ 0 ] * t;
  f[ 0 ][ 1 ] := f[ 0 ][ 1 ] * t;
  f[ 0 ][ 2 ] := f[ 0 ][ 2 ] * t;
  f[ 0 ][ 3 ] := f[ 0 ][ 3 ] * t;

  //Левая отсекающая плоскость
  f[ 1 ][ 0 ] := clip[  3 ] + clip[  0 ];
  f[ 1 ][ 1 ] := clip[  7 ] + clip[  4 ];
  f[ 1 ][ 2 ] := clip[ 11 ] + clip[  8 ];
  f[ 1 ][ 3 ] := clip[ 15 ] + clip[ 12 ];
  t := 1 / Sqrt( f[ 1 ][ 0 ] * f[ 1 ][ 0 ] + f[ 1 ][ 1 ] * f[ 1 ][ 1 ] + f[ 1 ][ 2 ] * f[ 1 ][ 2 ] );
  f[ 1 ][ 0 ] := f[ 1 ][ 0 ] * t;
  f[ 1 ][ 1 ] := f[ 1 ][ 1 ] * t;
  f[ 1 ][ 2 ] := f[ 1 ][ 2 ] * t;
  f[ 1 ][ 3 ] := f[ 1 ][ 3 ] * t;

  //Нижняя отсекающая плоскость
  f[ 2 ][ 0 ] := clip[  3] + clip[  1 ];
  f[ 2 ][ 1 ] := clip[  7] + clip[  5 ];
  f[ 2 ][ 2 ] := clip[ 11] + clip[  9 ];
  f[ 2 ][ 3 ] := clip[ 15] + clip[ 13 ];
  t := 1 / Sqrt( f[ 2 ][ 0 ] * f[ 2 ][ 0 ] + f[ 2 ][ 1 ] * f[ 2 ][ 1 ] + f[ 2 ][ 2 ] * f[ 2 ][ 2 ] );
  f[ 2 ][ 0 ] := f[ 2 ][ 0 ] * t;
  f[ 2 ][ 1 ] := f[ 2 ][ 1 ] * t;
  f[ 2 ][ 2 ] := f[ 2 ][ 2 ] * t;
  f[ 2 ][ 3 ] := f[ 2 ][ 3 ] * t;

  //Верхняя отсекающая плоскость
  f[ 3 ][ 0 ] := clip[  3 ] - clip[  1 ];
  f[ 3 ][ 1 ] := clip[  7 ] - clip[  5 ];
  f[ 3 ][ 2 ] := clip[ 11 ] - clip[  9 ];
  f[ 3 ][ 3 ] := clip[ 15 ] - clip[ 13 ];
  t := 1 / Sqrt( f[ 3 ][ 0 ] * f[ 3 ][ 0 ] + f[ 3 ][ 1 ] * f[ 3 ][ 1 ] + f[ 3 ][ 2 ] * f[ 3 ][ 2 ] );
  f[ 3 ][ 0 ] := f[ 3 ][ 0 ] * t;
  f[ 3 ][ 1 ] := f[ 3 ][ 1 ] * t;
  f[ 3 ][ 2 ] := f[ 3 ][ 2 ] * t;
  f[ 3 ][ 3 ] := f[ 3 ][ 3 ] * t;

  //Задняя отсекающая плоскость
  f[ 4 ][ 0 ] := clip[  3 ] - clip[  2 ];
  f[ 4 ][ 1 ] := clip[  7 ] - clip[  6 ];
  f[ 4 ][ 2 ] := clip[ 11 ] - clip[ 10 ];
  f[ 4 ][ 3 ] := clip[ 15 ] - clip[ 14 ];
  t := 1 / Sqrt( f[ 4 ][ 0 ] * f[ 4 ][ 0 ] + f[ 4 ][ 1 ] * f[ 4 ][ 1 ] + f[ 4 ][ 2 ] * f[ 4 ][ 2 ] );
  f[ 4 ][ 0 ] := f[ 4 ][ 0 ] * t;
  f[ 4 ][ 1 ] := f[ 4 ][ 1 ] * t;
  f[ 4 ][ 2 ] := f[ 4 ][ 2 ] * t;
  f[ 4 ][ 3 ] := f[ 4 ][ 3 ] * t;

  //Передняя отсекающая плоскость
  f[ 5 ][ 0 ] := clip[  3 ] + clip[  2 ];
  f[ 5 ][ 1 ] := clip[  7 ] + clip[  6 ];
  f[ 5 ][ 2 ] := clip[ 11 ] + clip[ 10 ];
  f[ 5 ][ 3 ] := clip[ 15 ] + clip[ 14 ];
  t := 1 / Sqrt( f[ 5 ][ 0 ] * f[ 5 ][ 0 ] + f[ 5 ][ 1 ] * f[ 5 ][ 1 ] + f[ 5 ][ 2 ] * f[ 5 ][ 2 ] );
  f[ 5 ][ 0 ] := f[ 5 ][ 0 ] * t;
  f[ 5 ][ 1 ] := f[ 5 ][ 1 ] * t;
  f[ 5 ][ 2 ] := f[ 5 ][ 2 ] * t;
  f[ 5 ][ 3 ] := f[ 5 ][ 3 ] * t;
end;

function frustum_PointIn;
  var
    i : Integer;
begin
  for i := 0 to 5 do
    if f[ i ][ 0 ] * x + f[ i ][ 1 ] * y + f[ i ][ 2 ] * z + f[ i ][ 3 ] < 0 Then
      begin
        Result := FALSE;
        exit;
      end;
  Result := TRUE;
end;

function frustum_PPointIn;
  var
    i : Integer;
begin
  for i := 0 to 5 do
    if f[ i ][ 0 ] * Vertex.X + f[ i ][ 1 ] * Vertex.Y + f[ i ][ 2 ] * Vertex.Z + f[ i ][ 3 ] < 0 Then
      begin
        Result := FALSE;
        exit;
      end;
  Result := TRUE;
end;

function frustum_TriangleIn;
  var
    i : Integer;
begin
  for i := 0 to 5 do
    begin
      if f[ i ][ 0 ] * v1.X + f[ i ][ 1 ] * v1.Y + f[ i ][ 2 ] * v1.Z + f[ i ][ 3 ] >= 0 Then continue;
      if f[ i ][ 0 ] * v2.X + f[ i ][ 1 ] * v2.Y + f[ i ][ 2 ] * v2.Z + f[ i ][ 3 ] >= 0 Then continue;
      if f[ i ][ 0 ] * v3.X + f[ i ][ 1 ] * v3.Y + f[ i ][ 2 ] * v3.Z + f[ i ][ 3 ] >= 0 Then continue;
      Result := FALSE;
      exit;
    end;
  Result := TRUE;
end;

function frustum_SphereIn;
  var
    i : Integer;
begin
  for i := 0 to 5 do
    if ( f[ i ][ 0 ] * x + f[ i ][ 1 ] * y + f[ i ][ 2 ] * z + f[ i ][ 3 ] ) <= -r Then
      begin
        Result := FALSE;
        exit;
      end;
  Result := TRUE;
end;

function frustum_BoxIn;
  var
    i : Integer;
begin
  for i := 0 to 5 do
    begin
      if ( f[ i ][ 0 ] * ( x - bx ) + f[ i ][ 1 ] * ( y - by ) + f[ i ][ 2 ] * ( z - bz ) + f[ i ][ 3 ] > 0 ) Then continue;
      if ( f[ i ][ 0 ] * ( x + bx ) + f[ i ][ 1 ] * ( y - by ) + f[ i ][ 2 ] * ( z - bz ) + f[ i ][ 3 ] > 0 ) Then continue;
      if ( f[ i ][ 0 ] * ( x - bx ) + f[ i ][ 1 ] * ( y + by ) + f[ i ][ 2 ] * ( z - bz ) + f[ i ][ 3 ] > 0 ) Then continue;
      if ( f[ i ][ 0 ] * ( x - bx ) + f[ i ][ 1 ] * ( y - by ) + f[ i ][ 2 ] * ( z + bz ) + f[ i ][ 3 ] > 0 ) Then continue;
      if ( f[ i ][ 0 ] * ( x + bx ) + f[ i ][ 1 ] * ( y + by ) + f[ i ][ 2 ] * ( z - bz ) + f[ i ][ 3 ] > 0 ) Then continue;
      if ( f[ i ][ 0 ] * ( x + bx ) + f[ i ][ 1 ] * ( y - by ) + f[ i ][ 2 ] * ( z + bz ) + f[ i ][ 3 ] > 0 ) Then continue;
      if ( f[ i ][ 0 ] * ( x - bx ) + f[ i ][ 1 ] * ( y + by ) + f[ i ][ 2 ] * ( z + bz ) + f[ i ][ 3 ] > 0 ) Then continue;
      if ( f[ i ][ 0 ] * ( x + bx ) + f[ i ][ 1 ] * ( y + by ) + f[ i ][ 2 ] * ( z + bz ) + f[ i ][ 3 ] > 0 ) Then continue;
      Result := FALSE;
      exit;
   end;
  Result := TRUE;
end;

function frustum_CubeIn;
  var
    i : Integer;
begin
  for i := 0 to 5 do
    begin
      if ( f[ i ][ 0 ] * ( x - size ) + f[ i ][ 1 ] * ( y - size ) + f[ i ][ 2 ] * ( z - size ) + f[ i ][ 3 ] > 0 ) Then continue;
      if ( f[ i ][ 0 ] * ( x + size ) + f[ i ][ 1 ] * ( y - size ) + f[ i ][ 2 ] * ( z - size ) + f[ i ][ 3 ] > 0 ) Then continue;
      if ( f[ i ][ 0 ] * ( x - size ) + f[ i ][ 1 ] * ( y + size ) + f[ i ][ 2 ] * ( z - size ) + f[ i ][ 3 ] > 0 ) Then continue;
      if ( f[ i ][ 0 ] * ( x - size ) + f[ i ][ 1 ] * ( y - size ) + f[ i ][ 2 ] * ( z + size ) + f[ i ][ 3 ] > 0 ) Then continue;
      if ( f[ i ][ 0 ] * ( x + size ) + f[ i ][ 1 ] * ( y + size ) + f[ i ][ 2 ] * ( z - size ) + f[ i ][ 3 ] > 0 ) Then continue;
      if ( f[ i ][ 0 ] * ( x + size ) + f[ i ][ 1 ] * ( y - size ) + f[ i ][ 2 ] * ( z + size ) + f[ i ][ 3 ] > 0 ) Then continue;
      if ( f[ i ][ 0 ] * ( x - size ) + f[ i ][ 1 ] * ( y + size ) + f[ i ][ 2 ] * ( z + size ) + f[ i ][ 3 ] > 0 ) Then continue;
      if ( f[ i ][ 0 ] * ( x + size ) + f[ i ][ 1 ] * ( y + size ) + f[ i ][ 2 ] * ( z + size ) + f[ i ][ 3 ] > 0 ) Then continue;
      Result := FALSE;
      exit;
   end;
  Result := TRUE;
end;

end.
