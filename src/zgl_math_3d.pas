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
unit zgl_math_3d;

{$I zgl_config.cfg}

interface

type
  zglPPoint3D = ^zglTPoint3D;
  zglTPoint3D = record
    case Byte of
    1: ( x, y, z : Single );
    2: ( point : array[ 0..2 ] of Single );
end;

type
  zglPMatrix4f = ^zglTMatrix4f;
  zglTMatrix4f = record
    a11, a12, a13, a14 : Single;
    a21, a22, a23, a24 : Single;
    a31, a32, a33, a34 : Single;
    a41, a42, a43, a44 : Single;
end;

const
  matrix4f_Identity : zglTMatrix4f = ( a11: 1; a12: 0; a13: 0; a14: 0;
                                       a21: 0; a22: 1; a23: 0; a24: 0;
                                       a31: 0; a32: 0; a33: 1; a34: 0;
                                       a41: 0; a42: 0; a43: 0; a44: 1 );

// Vector
function vector_Get( const x, y, z : Single ) : zglTPoint3D;

function vector_Add( const Vector1, Vector2 : zglTPoint3D ) : zglTPoint3D;
function vector_Sub( const Vector1, Vector2 : zglTPoint3D ) : zglTPoint3D;
function vector_Mul( const Vector1, Vector2 : zglTPoint3D ) : zglTPoint3D;
function vector_Div( const Vector1, Vector2 : zglTPoint3D ) : zglTPoint3D;

function vector_AddV( const Vector : zglTPoint3D; const Value : Single ) : zglTPoint3D;
function vector_SubV( const Vector : zglTPoint3D; const Value : Single ) : zglTPoint3D;
function vector_MulV( const Vector : zglTPoint3D; const Value : Single ) : zglTPoint3D;
function vector_DivV( const Vector : zglTPoint3D; const Value : Single ) : zglTPoint3D;

function vector_MulM4f( const Vector : zglTPoint3D; const Matrix : zglTMatrix4f ) : zglTPoint3D;

function vector_Negate( const Vector : zglTPoint3D ) : zglTPoint3D;
function vector_Normalize( const Vector : zglTPoint3D ) : zglTPoint3D;
function vector_Cross( const Vector1, Vector2 : zglTPoint3D ) : zglTPoint3D;
function vector_Dot( const Vector1, Vector2 : zglTPoint3D ) : Single;
function vector_Lerp( const Vector1, Vector2 : zglTPoint3D; const Value : Single ) : zglTPoint3D;

// Matrix
function matrix4f_Frustum( const Left, Right, Bottom, Top, zNear, zFar : Single ) : zglTMatrix4f;
function matrix4f_Perspective( const FOVY, Aspect, zNear, zFar : Single ) : zglTMatrix4f;
function matrix4f_FromVectorAngle( const Vector : zglTPoint3D; const Angle : Single ) : zglTMatrix4f;

procedure matrix4f_SetPos( var Matrix : zglTMatrix4f; const X, Y, Z : Single );
procedure matrix4f_SetRot( var Matrix : zglTMatrix4f; const aX, aY, aZ : Single  );

function matrix4f_Determinant( const Matrix : zglTMatrix4f ) : Single;
function matrix4f_Inverse( const Matrix : zglTMatrix4f ) : zglTMatrix4f;
function matrix4f_Transpose( const Matrix : zglTMatrix4f ) : zglTMatrix4f;
function matrix4f_Translate( const Matrix : zglTMatrix4f; const tX, tY, tZ : Single ) : zglTMatrix4f;
function matrix4f_Rotate( const Matrix : zglTMatrix4f; const Vector : zglTPoint3D; const Angle : Single ) : zglTMatrix4f;

function matrix4f_Mul( const Matrix1, Matrix2 : zglTMatrix4f ) : zglTMatrix4f;

implementation
uses
  zgl_math_2d,
  math;

// Vectors
function vector_Get;
begin
  Result.X := x;
  Result.Y := y;
  Result.Z := z;
end;

function vector_Add;
begin
  Result.X := Vector1.X + Vector2.X;
  Result.Y := Vector1.Y + Vector2.Y;
  Result.Z := Vector1.Z + Vector2.Z;
end;


function vector_Sub;
begin
  Result.X := Vector1.X - Vector2.X;
  Result.Y := Vector1.Y - Vector2.Y;
  Result.Z := Vector1.Z - Vector2.Z;
end;

function vector_Mul;
begin
  Result.X := Vector1.X * Vector2.X;
  Result.Y := Vector1.Y * Vector2.Y;
  Result.Z := Vector1.Z * Vector2.Z;
end;

function vector_Div;
begin
  Result.X := Vector1.X / Vector2.X;
  Result.Y := Vector1.Y / Vector2.Y;
  Result.Z := Vector1.Z / Vector2.Z;
end;

function vector_AddV;
begin
  Result.X := Vector.X + Value;
  Result.Y := Vector.Y + Value;
  Result.Z := Vector.Z + Value;
end;

function vector_SubV;
begin
  Result.X := Vector.X - Value;
  Result.Y := Vector.Y - Value;
  Result.Z := Vector.Z - Value;
end;

function vector_MulV;
begin
  Result.X := Vector.X * Value;
  Result.Y := Vector.Y * Value;
  Result.Z := Vector.Z * Value;
end;

function vector_DivV;
  var
    v : Single;
begin
  if Value <> 0 Then
    v := 1 / Value
  else
    begin
      Result := vector_Get( 0, 0, 0 );
      exit;
    end;
  Result.X := Vector.X * v;
  Result.Y := Vector.Y * v;
  Result.Z := Vector.Z * v;
end;

function vector_MulM4f;
begin
  with Matrix, Vector do
    begin
      Result.X := a11 * X + a12 * Y + a13 * Z + a14;
      Result.Y := a21 * X + a22 * Y + a23 * Z + a24;
      Result.Z := a31 * X + a32 * Y + a33 * Z + a34;
    end;
end;

function vector_Negate;
begin
  Result.X := -Vector.X;
  Result.Y := -Vector.Y;
  Result.Z := -Vector.Z;
end;

function vector_Normalize;
  var
    len : Single;
begin
  len := sqrt( sqr( Vector.X ) + sqr( Vector.Y ) + sqr( Vector.Z ) );
  if len <> 0 Then
    len := 1 / len
  else
    begin
      Result := vector_Get( 0, 0, 0 );
      exit;
    end;
  Result.X := Vector.X * len;
  Result.Y := Vector.Y * len;
  Result.Z := Vector.Z * len;
end;

function vector_Cross;
begin
  Result.X := Vector1.Y * Vector2.Z - Vector1.Z * Vector2.Y;
  Result.Y := Vector1.Z * Vector2.X - Vector1.X * Vector2.Z;
  Result.Z := Vector1.X * Vector2.Y - Vector1.Y * Vector2.X;
end;

function vector_Dot;
begin
  Result := Vector1.X * Vector2.X + Vector1.Y * Vector2.Y + Vector1.Z * Vector2.Z;
end;

function vector_Lerp;
begin
  Result.X := Vector1.X + ( Vector2.X - Vector1.X ) * Value;
  Result.Y := Vector1.Y + ( Vector2.Y - Vector1.Y ) * Value;
  Result.Z := Vector1.Z + ( Vector2.Z - Vector1.Z ) * Value;
end;

// Matrices
function matrix4f_Frustum;
begin
  with Result do
    begin
      a11 := ( zNear * 2 ) / ( Right - Left );
      a12 := 0;
      a13 := 0;
      a14 := 0;

      a21 := 0;
      a22 := ( zNear * 2 ) / ( Top - Bottom );
      a23 := 0;
      a24 := 0;

      a31 := ( Right + Left ) / ( Right - Left );
      a32 := ( Top + Bottom ) / ( Top - Bottom );
      a33 := -( zFar + zNear ) / ( zFar - zNear );
      a34 := -1;

      a41 := 0;
      a42 := 0;
      a43 := -( zFar * zNear * 2 ) / ( zFar - zNear );
      a44 := 0;
    end;
end;

function matrix4f_Perspective;
  var
    xmax, ymax : Single;
begin
  ymax := zNear * tan( FOVY * pi / 360 );
  xmax := ymax * aspect;

  Result := matrix4f_Frustum( -xmax, xmax, -ymax, ymax, zNear, zFar );
end;

procedure matrix4f_SetPos;
begin
  Matrix.a41 := X;
  Matrix.a42 := Y;
  Matrix.a43 := Z;
  Matrix.a44 := 1;
end;

procedure matrix4f_SetRot;
  var
    A, B, C, D, E, F : Single;
begin
  A := Sin( aX );
  D := Cos( aX );
  B := Sin( aY );
  E := Cos( aY );
  C := Sin( aZ );
  F := Cos( aZ );

  Matrix.a11 := E * F;
  Matrix.a12 := E * C;
  Matrix.a13 := -B;
  Matrix.a14 := 0;

  Matrix.a21 := A * B * F - D * C;
  Matrix.a22 := A * B * C + D * F;
  Matrix.a23 := A * E;
  Matrix.a24 := 0;

  Matrix.a31 := D * B * F + A * C;
  Matrix.a32 := D * B * C - A * F;
  Matrix.a33 := D * E;
  Matrix.a34 := 0;
end;

function matrix4f_Determinant;
begin
  with Matrix do
    Result := a11 * a22 * a33 + a21 * a32 * a13 + a31 * a12 * a23 - a31 * a22 * a13 - a21 * a12 * a33 - a11 * a32 * a23;
end;

function matrix4f_Inverse;
  var
    det : Single;
begin
  det := 1 / matrix4f_Determinant( Matrix );

  with Matrix do
    begin
      Result.a11 :=  ( a22 * ( a33 * a44 - a43 * a34 ) - a32 * ( a23 * a44 - a43 * a24 ) + a42 * ( a23 * a34 - a33 * a24 ) ) * det;
      Result.a12 := -( a12 * ( a33 * a44 - a43 * a34 ) - a32 * ( a13 * a44 - a43 * a14 ) + a42 * ( a13 * a34 - a33 * a14 ) ) * det;
      Result.a13 :=  ( a12 * ( a23 * a44 - a43 * a24 ) - a22 * ( a13 * a44 - a43 * a14 ) + a42 * ( a13 * a24 - a23 * a14 ) ) * det;
      Result.a14 := -( a12 * ( a23 * a34 - a33 * a24 ) - a22 * ( a13 * a34 - a33 * a14 ) + a32 * ( a13 * a24 - a23 * a14 ) ) * det;
      Result.a21 := -( a21 * ( a33 * a44 - a43 * a34 ) - a31 * ( a23 * a44 - a43 * a24 ) + a41 * ( a23 * a34 - a33 * a24 ) ) * det;
      Result.a22 :=  ( a11 * ( a33 * a44 - a43 * a34 ) - a31 * ( a13 * a44 - a43 * a14 ) + a41 * ( a13 * a34 - a33 * a14 ) ) * det;
      Result.a23 := -( a11 * ( a23 * a44 - a43 * a24 ) - a21 * ( a13 * a44 - a43 * a14 ) + a41 * ( a13 * a24 - a23 * a14 ) ) * det;
      Result.a24 :=  ( a11 * ( a23 * a34 - a33 * a24 ) - a21 * ( a13 * a34 - a33 * a14 ) + a31 * ( a13 * a24 - a23 * a14 ) ) * det;
      Result.a31 :=  ( a21 * ( a32 * a44 - a42 * a34 ) - a31 * ( a22 * a44 - a42 * a24 ) + a41 * ( a22 * a34 - a32 * a24 ) ) * det;
      Result.a32 := -( a11 * ( a32 * a44 - a42 * a34 ) - a31 * ( a12 * a44 - a42 * a14 ) + a41 * ( a12 * a34 - a32 * a14 ) ) * det;
      Result.a33 :=  ( a11 * ( a22 * a44 - a42 * a24 ) - a21 * ( a12 * a44 - a42 * a14 ) + a41 * ( a12 * a24 - a22 * a14 ) ) * det;
      Result.a34 := -( a11 * ( a22 * a34 - a32 * a24 ) - a21 * ( a12 * a34 - a32 * a14 ) + a31 * ( a12 * a24 - a22 * a14 ) ) * det;
      Result.a41 := -( a21 * ( a32 * a43 - a42 * a33 ) - a31 * ( a22 * a43 - a42 * a23 ) + a41 * ( a22 * a33 - a32 * a23 ) ) * det;
      Result.a42 :=  ( a11 * ( a32 * a43 - a42 * a33 ) - a31 * ( a12 * a43 - a42 * a13 ) + a41 * ( a12 * a33 - a32 * a13 ) ) * det;
      Result.a43 := -( a11 * ( a22 * a43 - a42 * a23 ) - a21 * ( a12 * a43 - a42 * a13 ) + a41 * ( a12 * a23 - a22 * a13 ) ) * det;
      Result.a44 :=  ( a11 * ( a22 * a33 - a32 * a23 ) - a21 * ( a12 * a33 - a32 * a13 ) + a31 * ( a12 * a23 - a22 * a13 ) ) * det;
    end;
end;

function matrix4f_FromVectorAngle;
  var
    s, c, _c : Single;
begin
  s := Sin( Angle );
  c := Cos( Angle );
  _c := 1 - c;

  with Result do
    begin
      a11 := ( _c * Vector.X * Vector.X ) + c;
      a12 := ( _c * Vector.X * Vector.Y ) - ( Vector.Z * s );
      a13 := ( _c * Vector.Z * Vector.X ) + ( Vector.Y * s );
      a14 := 0;

      a21 := ( _c * Vector.X * Vector.Y ) + ( Vector.Z * s );
      a22 := ( _c * Vector.Y * Vector.Y ) + c;
      a23 := ( _c * Vector.Y * Vector.Z ) - ( Vector.X * s );
      a24 := 0;

      a31 := ( _c * Vector.Z * Vector.X ) - ( Vector.Y * s );
      a32 := ( _c * Vector.Y * Vector.Z ) + ( Vector.X * s );
      a33 := ( _c * Vector.Z * Vector.Z ) + c;
      a34 := 0;

      a41 := 0;
      a42 := 0;
      a43 := 0;
      a44 := 1;
    end;
end;

function matrix4f_Transpose;
begin
  with Result do
    begin
      a11 := Matrix.a11;
      a12 := Matrix.a21;
      a21 := Matrix.a12;

      a22 := Matrix.a22;
      a13 := Matrix.a31;
      a31 := Matrix.a13;

      a14 := Matrix.a41;
      a41 := Matrix.a14;
      a23 := Matrix.a32;
      a32 := Matrix.a23;

      a33 := Matrix.a33;
      a24 := Matrix.a42;
      a42 := Matrix.a24;

      a34 := Matrix.a43;
      a43 := Matrix.a34;
      a44 := Matrix.a44;
    end;
end;

function matrix4f_Translate;
begin
  Move( Matrix, Result, 48 );
  with Matrix do
    begin
      Result.a41 := a11 * tX + a21 * tY + a31 * tZ + a41;
      Result.a42 := a12 * tX + a22 * tY + a32 * tZ + a42;
      Result.a43 := a13 * tX + a23 * tY + a33 * tZ + a43;
      Result.a44 := a14 * tX + a24 * tY + a34 * tZ + a44;
    end;
end;

function matrix4f_Rotate;
begin
  Result := matrix4f_Mul( Matrix, matrix4f_FromVectorAngle( Vector, Angle ) );
end;

function matrix4f_Mul;
begin
  with Result do
    begin
      a11 := Matrix1.a11 * Matrix2.a11 + Matrix1.a12 * Matrix2.a21 + Matrix1.a13 * Matrix2.a31 + Matrix1.a14 * Matrix2.a41;
      a12 := Matrix1.a11 * Matrix2.a12 + Matrix1.a12 * Matrix2.a22 + Matrix1.a13 * Matrix2.a32 + Matrix1.a14 * Matrix2.a42;
      a13 := Matrix1.a11 * Matrix2.a13 + Matrix1.a12 * Matrix2.a23 + Matrix1.a13 * Matrix2.a33 + Matrix1.a14 * Matrix2.a43;
      a14 := Matrix1.a11 * Matrix2.a14 + Matrix1.a12 * Matrix2.a24 + Matrix1.a13 * Matrix2.a34 + Matrix1.a14 * Matrix2.a44;

      a21 := Matrix1.a21 * Matrix2.a11 + Matrix1.a22 * Matrix2.a21 + Matrix1.a23 * Matrix2.a31 + Matrix1.a24 * Matrix2.a41;
      a22 := Matrix1.a21 * Matrix2.a12 + Matrix1.a22 * Matrix2.a22 + Matrix1.a23 * Matrix2.a32 + Matrix1.a24 * Matrix2.a42;
      a23 := Matrix1.a21 * Matrix2.a13 + Matrix1.a22 * Matrix2.a23 + Matrix1.a23 * Matrix2.a33 + Matrix1.a24 * Matrix2.a43;
      a24 := Matrix1.a21 * Matrix2.a14 + Matrix1.a22 * Matrix2.a24 + Matrix1.a23 * Matrix2.a34 + Matrix1.a24 * Matrix2.a44;

      a31 := Matrix1.a31 * Matrix2.a11 + Matrix1.a32 * Matrix2.a21 + Matrix1.a33 * Matrix2.a31 + Matrix1.a34 * Matrix2.a41;
      a32 := Matrix1.a31 * Matrix2.a12 + Matrix1.a32 * Matrix2.a22 + Matrix1.a33 * Matrix2.a32 + Matrix1.a34 * Matrix2.a42;
      a33 := Matrix1.a31 * Matrix2.a13 + Matrix1.a32 * Matrix2.a23 + Matrix1.a33 * Matrix2.a33 + Matrix1.a34 * Matrix2.a43;
      a34 := Matrix1.a31 * Matrix2.a14 + Matrix1.a32 * Matrix2.a24 + Matrix1.a33 * Matrix2.a34 + Matrix1.a34 * Matrix2.a44;

      a41 := Matrix1.a41 * Matrix2.a11 + Matrix1.a42 * Matrix2.a21 + Matrix1.a43 * Matrix2.a31 + Matrix1.a44 * Matrix2.a41;
      a42 := Matrix1.a41 * Matrix2.a12 + Matrix1.a42 * Matrix2.a22 + Matrix1.a43 * Matrix2.a32 + Matrix1.a44 * Matrix2.a42;
      a43 := Matrix1.a41 * Matrix2.a13 + Matrix1.a42 * Matrix2.a23 + Matrix1.a43 * Matrix2.a33 + Matrix1.a44 * Matrix2.a43;
      a44 := Matrix1.a41 * Matrix2.a14 + Matrix1.a42 * Matrix2.a24 + Matrix1.a43 * Matrix2.a34 + Matrix1.a44 * Matrix2.a44;
    end;
end;

end.
