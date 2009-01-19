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
unit zgl_math;

{$I define.inc}

interface
uses
  zgl_const,
  zgl_types;

procedure InitCosSinTables;
function  m_Cos( Angle : Integer ) : Single;
function  m_Sin( Angle : Integer ) : Single;
procedure m_SinCos( const Angle : Single; var S, C : Single );

function m_Distance( const x1, y1, x2, y2 : Single ) : Single;
function m_FDistance( const x1, y1, x2, y2 : Single ) : Single;
function m_Angle( const x1, y1, x2, y2 : Single ) : Single;

{------------------------------------------------------------------------------}
{--------------------------------- Vectors ------------------------------------}
{------------------------------------------------------------------------------}
function vector_Get( const x, y, z : Single ) : zglTPoint3D;

function vector_Add( const Vector1, Vector2 : zglTPoint3D ) : zglTPoint3D;
function vector_Sub( const Vector1, Vector2 : zglTPoint3D ) : zglTPoint3D;
function vector_Mul( const Vector1, Vector2 : zglTPoint3D ) : zglTPoint3D;
function vector_Div( const Vector1, Vector2 : zglTPoint3D ) : zglTPoint3D;

function vector_AddV( const Vector : zglTPoint3D; const Value : Single ) : zglTPoint3D;
function vector_SubV( const Vector : zglTPoint3D; const Value : Single ) : zglTPoint3D;
function vector_MulV( const Vector : zglTPoint3D; const Value : Single ) : zglTPoint3D;
function vector_DivV( const Vector : zglTPoint3D; Value : Single ) : zglTPoint3D;

function vector_MulM3f( const Vector : zglTPoint3D; const Matrix : zglTMatrix3f ) : zglTPoint3D;
function vector_MulM4f( const Vector : zglTPoint3D; const Matrix : zglTMatrix4f ) : zglTPoint3D;
function vector_MulInvM4f( const Vector : zglTPoint3D; const Matrix : zglTMatrix4f ) : zglTPoint3D;

function vector_RotateX( const Vector : zglTPoint3D; const Value : Single ) : zglTPoint3D;
function vector_RotateY( const Vector : zglTPoint3D; const Value : Single ) : zglTPoint3D;
function vector_RotateZ( const Vector : zglTPoint3D; const Value : Single ) : zglTPoint3D;
function vector_RotateQ( const Vector : zglTPoint3D; const Quaternion : zglTQuaternion ) : zglTPoint3D;

function vector_Negate( const Vector : zglTPoint3D ) : zglTPoint3D;
function vector_Normalize( const Vector : zglTPoint3D ) : zglTPoint3D;
function vector_Angle( const Vector1, Vector2 : zglTPoint3D ) : Single;
function vector_Cross( const Vector1, Vector2 : zglTPoint3D ) : zglTPoint3D;
function vector_Dot( const Vector1, Vector2 : zglTPoint3D ) : Single;
function vector_Distance( const Vector1, Vector2 : zglTPoint3D ) : Single;
function vector_FDistance( const  Vector1, Vector2 : zglTPoint3D ) : Single;
function vector_Length( const Vector : zglTPoint3D ) : Single;
function vector_Lerp( const Vector1, Vector2 : zglTPoint3D; const Value : Single ) : zglTPoint3D;

{------------------------------------------------------------------------------}
{--------------------------------- Matrix3f -----------------------------------}
{------------------------------------------------------------------------------}
function  matrix3f_Get( const v1, v2, v3 : zglTPoint3D ) : zglTMatrix3f;

procedure matrix3f_OrthoNormalize( var Matrix : zglTMatrix3f );
procedure matrix3f_Transpose( var Matrix : zglTMatrix3f );
procedure matrix3f_SetRot( var Matrix : zglTMatrix3f; const aX, aY, aZ : Single );
function  matrix3f_Add( const Matrix1, Matrix2 : zglTMatrix3f ) : zglTMatrix3f;
function  matrix3f_Mul( const Matrix1, Matrix2 : zglTMatrix3f ) : zglTMatrix3f;

procedure matrix4f_Transpose( var Matrix : zglTMatrix4f );
function  matrix4f_Determinant( const Matrix : zglTMatrix4f ): Single;
function  matrix4f_Inverse( const Matrix : zglTMatrix4f ) : zglTMatrix4f;
procedure matrix4f_Translate( var Matrix : zglTMatrix4f; const tX, tY, tZ : Single );
procedure matrix4f_SetPos( var Matrix : zglTMatrix4f; const X, Y, Z : Single );
procedure matrix4f_SetRot( var Matrix : zglTMatrix4f; const aX, aY, aZ : Single );
procedure matrix4f_Scale( var Matrix : zglTMatrix4f; const sX, sY, sZ : Single );
function  matrix4f_Mul( const Matrix1, Matrix2 : zglTMatrix4f ) : zglTMatrix4f;

{------------------------------------------------------------------------------}
{-------------------------------- Quaternion ----------------------------------}
{------------------------------------------------------------------------------}
function quater_Get( const X, Y, Z, W : Single ) : zglTQuaternion;
function quater_Add( const q1, q2 : zglTQuaternion ) : zglTQuaternion;
function quater_Sub( const q1, q2 : zglTQuaternion ) : zglTQuaternion;
function quater_Mul( const q1, q2 : zglTQuaternion ) : zglTQuaternion;
function quater_Negate( const Quaternion : zglTQuaternion ) : zglTQuaternion;
function quater_Normalize( const Quaternion : zglTQuaternion ) : zglTQuaternion;
function quater_Dot( const q1, q2 : zglTQuaternion ) : Single;
function quater_Lerp( const q1, q2 : zglTQuaternion; Value : Single ) : zglTQuaternion;
function quater_FromRotation( const Rotation : zglTPoint3D ) : zglTQuaternion;
function quater_GetM4f( const Quaternion : zglTQuaternion ) : zglTMatrix4f;

function line3d_ClosestPoint( const A, B, Point : zglTPoint3D ) : zglTPoint3D;

function plane_Get( const A, B, C : zglTPoint3D ) : zglTPlane;
function plane_Distance( const Plane : zglTPlane; const Point : zglTPoint3D ) : Single;

function tri_GetNormal( const A, B, C : zglTPoint3D ) : zglTPoint3D;

function ArcTan2( X, Y : Single ) : Single;
function ArcCos( Value : Single ) : Single;

const
  matrix3f_Identity: zglTMatrix3f = ( a11: 1; a12: 0; a13: 0; a21: 0; a22: 1; a23: 0; a31: 0; a32: 0; a33: 1 );
  matrix4f_Identity: zglTMatrix4f = ( a11: 1; a12: 0; a13: 0; a14: 0; a21: 0; a22: 1; a23: 0; a24: 0; a31: 0; a32: 0; a33: 1; a34: 0; a41: 0; a42: 0; a43: 0; a44: 1 );
  quater_Zero      : zglTQuaternion = ( X: 0; Y: 0; Z: 0; W: 0 );

var
  CosTable : array[ 0..360 ] of Single;
  SinTable : array[ 0..360 ] of Single;

implementation

procedure InitCosSinTables;
  var
    i         : Integer;
    rad_angle : Single;
begin
  for i := 0 to 360 do
    begin
      rad_angle := i * ( cv_pi / 180 );
      CosTable[ i ] := cos( rad_angle );
      SinTable[ i ] := sin( rad_angle );
    end;
end;

function m_Cos;
begin
  while Angle > 360 do Angle := Angle - 360;
  while Angle < 0   do Angle := Angle + 360;
  if Angle > 0 Then
    Result := CosTable[ Angle ]
  else
    Result := CosTable[ 360 - Angle ]
end;

function m_Sin;
begin
  while Angle > 360 do Angle := Angle - 360;
  while Angle < 0   do Angle := Angle + 360;
  if Angle > 0 Then
    Result := SinTable[ Angle ]
  else
    Result := SinTable[ 360 - Angle ]
end;

procedure m_SinCos;
asm
  FLD Angle
  FSINCOS
  FSTP [ EDX ]
  FSTP [ EAX ]
end;

function m_Distance;
begin
  Result := Sqrt( ( X1 - X2 ) * ( X1 - X2 ) + ( Y1 - Y2 ) * ( Y1 - Y2 ) );
end;

function m_FDistance;
begin
  Result := ( X1 - X2 ) * ( X1 - X2 ) + ( Y1 - Y2 ) * ( Y1 - Y2 );
end;

function m_Angle;
begin
  Result := ArcTan2( x2 - x1, y2 - y1 );
end;

function ArcTan2;
asm
  FLD    X
  FLD    Y
  FPATAN
  FWAIT
end;

function ArcCos;
begin
  if 1 - sqr( Value ) <= 0 Then
    Result := -1
  else
    Result := ArcTan2( sqrt( 1 - sqr( Value ) ), Value );
end;

{------------------------------------------------------------------------------}
{--------------------------------- Vectors ------------------------------------}
{------------------------------------------------------------------------------}
function vector_Get;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
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
begin
  Value := 1 / Value;
  Result.X := Vector.X * Value;
  Result.Y := Vector.Y * Value;
  Result.Z := Vector.Z * Value;
end;

function vector_MulM3f;
begin
  Result.X := Matrix.a11 * Vector.X + Matrix.a21 * Vector.Y + Matrix.a31 * Vector.Z;
  Result.Y := Matrix.a12 * Vector.X + Matrix.a22 * Vector.Y + Matrix.a32 * Vector.Z;
  Result.Z := Matrix.a13 * Vector.X + Matrix.a23 * Vector.Y + Matrix.a33 * Vector.Z;
end;

function vector_MulM4f;
begin
  Result.X := Matrix.a11 * Vector.X + Matrix.a21 * Vector.Y + Matrix.a31 * Vector.Z + Matrix.a41;
  Result.Y := Matrix.a12 * Vector.X + Matrix.a22 * Vector.Y + Matrix.a32 * Vector.Z + Matrix.a42;
  Result.Z := Matrix.a13 * Vector.X + Matrix.a23 * Vector.Y + Matrix.a33 * Vector.Z + Matrix.a43;
end;

function vector_MulInvM4f;
begin
  Result.x := Matrix.a11 * Vector.X + Matrix.a12 * Vector.Y + Matrix.a13 * Vector.Z + Matrix.a14;
  Result.y := Matrix.a21 * Vector.X + Matrix.a22 * Vector.Y + Matrix.a23 * Vector.Z + Matrix.a24;
  Result.z := Matrix.a31 * Vector.X + Matrix.a32 * Vector.Y + Matrix.a33 * Vector.Z + Matrix.a34;
end;

function vector_RotateX;
  var
    sina, cosa : Single;
begin
  m_SinCos( Value, sina, cosa );
  Result.X := Vector.X;
  Result.Y := ( Vector.Y * cosa ) + ( Vector.Z * ( -sina ) );
  Result.Z := ( Vector.Y * sina ) + ( Vector.Z * cosa );
end;

function vector_RotateY;
  var
    sina, cosa : Single;
begin
  m_SinCos( Value, sina, cosa );
  Result.X := ( Vector.X * cosa ) + ( Vector.Z * sina );
  Result.Y := Vector.Y;
  Result.Z := ( Vector.X * ( -sina ) ) + ( Vector.Z * cosa );
end;

function vector_RotateZ;
  var
    sina, cosa : Single;
begin
  m_SinCos( Value, sina, cosa );
  Result.X := ( Vector.X * cosa ) + ( Vector.Y * ( -sina ) );
  Result.Y := ( Vector.X * sina ) + ( Vector.Y * cosa );
  Result.Z := Vector.Z;
end;

function vector_RotateQ;
  var
    vecQuat, resQuat : zglTQuaternion;
begin
	vecQuat.x := Vector.x;
	vecQuat.y := Vector.y;
	vecQuat.z := Vector.z;
	vecQuat.w := 0;
 
	resQuat := quater_Mul( vecQuat, quater_Negate( Quaternion ) );
	resQuat := quater_Mul( Quaternion, resQuat );
 
	Result.X := resQuat.x;
  Result.Y := resQuat.y;
  Result.Z := resQuat.z;
end;


function vector_Negate;
begin
  Result.X := -Result.X;
  Result.Y := -Result.Y;
  Result.Z := -Result.Z
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

function vector_Angle;
begin
  Result := ArcCos( vector_Dot( vector_Normalize( Vector1 ), vector_Normalize( Vector2 ) ) );
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

function vector_Distance;
begin
  Result := sqrt( sqr( Vector2.X - Vector1.X ) +
                  sqr( Vector2.Y - Vector1.Y ) +
                  sqr( Vector2.Z - Vector1.Z ) );
end;

function vector_FDistance;
begin
  Result := sqr( Vector2.X - Vector1.X ) +
            sqr( Vector2.Y - Vector1.Y ) +
            sqr( Vector2.Z - Vector1.Z );
end;

function vector_Length;
begin
  Result := sqrt( sqr( Vector.X ) + sqr( Vector.Y ) + sqr( Vector.Z ) );
end;

function vector_Lerp;
begin
  Result.X := Vector1.X + ( Vector2.X - Vector1.X ) * Value;
  Result.Y := Vector1.Y + ( Vector2.Y - Vector1.Y ) * Value;
  Result.Z := Vector1.Z + ( Vector2.Z - Vector1.Z ) * Value;
end;

{------------------------------------------------------------------------------}
{--------------------------------- Matrix3f -----------------------------------}
{------------------------------------------------------------------------------}
function matrix3f_Get;
begin
  Result.a11 := v1.X;
  Result.a12 := v1.Y;
  Result.a13 := v1.Z;

  Result.a21 := v2.X;
  Result.a22 := v2.Y;
  Result.a23 := v2.Z;

  Result.a31 := v3.X;
  Result.a32 := v3.Y;
  Result.a33 := v3.Z;
end;

procedure matrix3f_OrthoNormalize;
begin
  Matrix.row[ 0 ] := vector_Normalize( Matrix.row[ 0 ] );
  Matrix.row[ 2 ] := vector_Normalize( vector_Cross( Matrix.row[ 0 ], Matrix.row[ 1 ] ) );
  Matrix.row[ 1 ] := vector_Normalize( vector_Cross( Matrix.row[ 2 ], Matrix.row[ 0 ] ) );
end;

procedure matrix3f_Transpose;
  var
    t : Single;
begin
  t          := Matrix.a12;
  Matrix.a12 := Matrix.a21;
  Matrix.a21 := t;

  t          := Matrix.a13;
  Matrix.a13 := Matrix.a31;
  Matrix.a31 := t;

  t          := Matrix.a23;
  Matrix.a23 := Matrix.a32;
  Matrix.a32 := t;
end;

procedure matrix3f_SetRot;
  var
    tMatrix : zglTMatrix3f;
begin
  tMatrix.row[ 0 ] := vector_Get(   0, -aZ,  aY );
  tMatrix.row[ 1 ] := vector_Get(  aZ,   0, -aX );
  tMatrix.row[ 2 ] := vector_Get( -aY,  aX,   0 );
  tMatrix := matrix3f_Mul( tMatrix, Matrix );
  Matrix  := matrix3f_Add( Matrix, tMatrix );
  matrix3f_OrthoNormalize( Matrix );
end;

function matrix3f_Add;
begin
  Result.row[ 0 ] := vector_Add( Matrix1.row[ 0 ], Matrix2.row[ 0 ] );
  Result.row[ 1 ] := vector_Add( Matrix1.row[ 1 ], Matrix2.row[ 1 ] );
  Result.row[ 2 ] := vector_Add( Matrix1.row[ 2 ], Matrix2.row[ 2 ] );
end;

function matrix3f_Mul;
begin
  Result.a11 := Matrix1.a11 * Matrix2.a11 + Matrix1.a12 * Matrix2.a21 + Matrix1.a13 * Matrix2.a31;
  Result.a12 := Matrix1.a11 * Matrix2.a12 + Matrix1.a12 * Matrix2.a22 + Matrix1.a13 * Matrix2.a32;
  Result.a13 := Matrix1.a11 * Matrix2.a13 + Matrix1.a12 * Matrix2.a23 + Matrix1.a13 * Matrix2.a33;

  Result.a21 := Matrix1.a21 * Matrix2.a11 + Matrix1.a22 * Matrix2.a21 + Matrix1.a23 * Matrix2.a31;
  Result.a22 := Matrix1.a21 * Matrix2.a12 + Matrix1.a22 * Matrix2.a22 + Matrix1.a23 * Matrix2.a32;
  Result.a23 := Matrix1.a21 * Matrix2.a13 + Matrix1.a22 * Matrix2.a23 + Matrix1.a23 * Matrix2.a33;

  Result.a31 := Matrix1.a31 * Matrix2.a11 + Matrix1.a32 * Matrix2.a21 + Matrix1.a33 * Matrix2.a31;
  Result.a32 := Matrix1.a31 * Matrix2.a12 + Matrix1.a32 * Matrix2.a22 + Matrix1.a33 * Matrix2.a32;
  Result.a33 := Matrix1.a31 * Matrix2.a13 + Matrix1.a32 * Matrix2.a23 + Matrix1.a33 * Matrix2.a33;
end;

procedure matrix4f_Transpose;
  var
    t : Single;
begin
  t          := Matrix.a12;
  Matrix.a12 := Matrix.a21;
  Matrix.a21 := t;

  t          := Matrix.a13;
  Matrix.a13 := Matrix.a31;
  Matrix.a31 := t;

  t          := Matrix.a14;
  Matrix.a14 := Matrix.a41;
  Matrix.a41 := t;

  t          := Matrix.a23;
  Matrix.a23 := Matrix.a32;
  Matrix.a32 := t;

  t          := Matrix.a24;
  Matrix.a24 := Matrix.a42;
  Matrix.a42 := t;

  t          := Matrix.a34;
  Matrix.a34 := Matrix.a43;
  Matrix.a43 := t;
end;

function matrix4f_Determinant; 
begin
  Result := Matrix.a11 * Matrix.a22 * Matrix.a33 +
            Matrix.a21 * Matrix.a32 * Matrix.a13 +
            Matrix.a31 * Matrix.a12 * Matrix.a23 -
            Matrix.a31 * Matrix.a22 * Matrix.a13 -
            Matrix.a21 * Matrix.a12 * Matrix.a33 -
            Matrix.a11 * Matrix.a32 * Matrix.a23;
end;

function matrix4f_Inverse;
  var
    det : Single;
begin
  det := 1 / matrix4f_Determinant( Matrix );

  Result.a11 :=  ( Matrix.a22 * Matrix.a33 - Matrix.a32 * Matrix.a23 ) * det;
  Result.a12 := -( Matrix.a12 * Matrix.a33 - Matrix.a32 * Matrix.a13 ) * det;
  Result.a13 :=  ( Matrix.a12 * Matrix.a23 - Matrix.a22 * Matrix.a13 ) * det;
  Result.a14 := 0;

  Result.a21 := -( Matrix.a21 * Matrix.a33 - Matrix.a31 * Matrix.a23 ) * det;
  Result.a22 :=  ( Matrix.a11 * Matrix.a33 - Matrix.a31 * Matrix.a13 ) * det;
  Result.a23 := -( Matrix.a11 * Matrix.a23 - Matrix.a21 * Matrix.a13 ) * det;
  Result.a24 := 0;

  Result.a31 :=  ( Matrix.a21 * Matrix.a32 - Matrix.a31 * Matrix.a22 ) * det;
  Result.a32 := -( Matrix.a11 * Matrix.a32 - Matrix.a31 * Matrix.a12 ) * det;
  Result.a33 :=  ( Matrix.a11 * Matrix.a22 - Matrix.a21 * Matrix.a12 ) * det;
  Result.a34 := 0;

  Result.a41 := -( Matrix.a41 * Result.a11 + Matrix.a42 * Result.a21 + Matrix.a43 * Result.a31 );
  Result.a42 := -( Matrix.a41 * Result.a12 + Matrix.a42 * Result.a22 + Matrix.a43 * Result.a32 );
  Result.a43 := -( Matrix.a41 * Result.a13 + Matrix.a42 * Result.a23 + Matrix.a43 * Result.a33 );
  Result.a44 := 1;
end;

procedure matrix4f_Translate;
begin
  Matrix.a41 := Matrix.a11 * tX + Matrix.a21 * tY + Matrix.a31 * tZ + Matrix.a41;
  Matrix.a42 := Matrix.a12 * tX + Matrix.a22 * tY + Matrix.a32 * tZ + Matrix.a42;
  Matrix.a43 := Matrix.a13 * tX + Matrix.a23 * tY + Matrix.a33 * tZ + Matrix.a43;
  Matrix.a44 := Matrix.a14 * tX + Matrix.a24 * tY + Matrix.a34 * tZ + Matrix.a44;end;

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
  m_SinCos( aX, A, D );
  m_SinCos( aY, B, E );
  m_SinCos( aZ, C, F );

  Matrix.a11 := E * F;
  Matrix.a12 := E * C;
  Matrix.a13 := - B;
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

procedure matrix4f_Scale;
  var
    sMatrix : zglTMatrix4f;
begin
  sMatrix := matrix4f_Identity;
  sMatrix.a11 := sX;
  sMatrix.a22 := sY;
  sMatrix.a33 := sZ;
  Matrix := matrix4f_Mul( Matrix, sMatrix );
end;

function matrix4f_Mul;
begin
  Result.a11 := Matrix1.a11 * Matrix2.a11 + Matrix1.a12 * Matrix2.a21 + Matrix1.a13 * Matrix2.a31 + Matrix1.a14 * Matrix2.a41;
  Result.a21 := Matrix1.a21 * Matrix2.a11 + Matrix1.a22 * Matrix2.a21 + Matrix1.a23 * Matrix2.a31 + Matrix1.a24 * Matrix2.a41;
  Result.a31 := Matrix1.a31 * Matrix2.a11 + Matrix1.a32 * Matrix2.a21 + Matrix1.a33 * Matrix2.a31 + Matrix1.a34 * Matrix2.a41;
  Result.a41 := Matrix1.a41 * Matrix2.a11 + Matrix1.a42 * Matrix2.a21 + Matrix1.a43 * Matrix2.a31 + Matrix1.a44 * Matrix2.a41;
  Result.a12 := Matrix1.a11 * Matrix2.a12 + Matrix1.a12 * Matrix2.a22 + Matrix1.a13 * Matrix2.a32 + Matrix1.a14 * Matrix2.a42;
  Result.a22 := Matrix1.a21 * Matrix2.a12 + Matrix1.a22 * Matrix2.a22 + Matrix1.a23 * Matrix2.a32 + Matrix1.a24 * Matrix2.a42;
  Result.a32 := Matrix1.a31 * Matrix2.a12 + Matrix1.a32 * Matrix2.a22 + Matrix1.a33 * Matrix2.a32 + Matrix1.a34 * Matrix2.a42;
  Result.a42 := Matrix1.a41 * Matrix2.a12 + Matrix1.a42 * Matrix2.a22 + Matrix1.a43 * Matrix2.a32 + Matrix1.a44 * Matrix2.a42;
  Result.a13 := Matrix1.a11 * Matrix2.a13 + Matrix1.a12 * Matrix2.a23 + Matrix1.a13 * Matrix2.a33 + Matrix1.a14 * Matrix2.a43;
  Result.a23 := Matrix1.a21 * Matrix2.a13 + Matrix1.a22 * Matrix2.a23 + Matrix1.a23 * Matrix2.a33 + Matrix1.a24 * Matrix2.a43;
  Result.a33 := Matrix1.a31 * Matrix2.a13 + Matrix1.a32 * Matrix2.a23 + Matrix1.a33 * Matrix2.a33 + Matrix1.a34 * Matrix2.a43;
  Result.a43 := Matrix1.a41 * Matrix2.a13 + Matrix1.a42 * Matrix2.a23 + Matrix1.a43 * Matrix2.a33 + Matrix1.a44 * Matrix2.a43;
  Result.a14 := Matrix1.a11 * Matrix2.a14 + Matrix1.a12 * Matrix2.a24 + Matrix1.a13 * Matrix2.a34 + Matrix1.a14 * Matrix2.a44;
  Result.a24 := Matrix1.a21 * Matrix2.a14 + Matrix1.a22 * Matrix2.a24 + Matrix1.a23 * Matrix2.a34 + Matrix1.a24 * Matrix2.a44;
  Result.a34 := Matrix1.a31 * Matrix2.a14 + Matrix1.a32 * Matrix2.a24 + Matrix1.a33 * Matrix2.a34 + Matrix1.a34 * Matrix2.a44;
  Result.a44 := Matrix1.a41 * Matrix2.a14 + Matrix1.a42 * Matrix2.a24 + Matrix1.a43 * Matrix2.a34 + Matrix1.a44 * Matrix2.a44;
end;

{------------------------------------------------------------------------------}
{-------------------------------- Quaternion ----------------------------------}
{------------------------------------------------------------------------------}
function quater_Get;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
  Result.W := W;
end;

function quater_Add;
begin
  Result.X := q1.X + q2.X;
  Result.Y := q1.Y + q2.Y;
  Result.Z := q1.Z + q2.Z;
  Result.W := q1.W + q2.W;
end;

function quater_Sub;
begin
  Result.X := q1.X - q2.X;
  Result.Y := q1.Y - q2.Y;
  Result.Z := q1.Z - q2.Z;
  Result.W := q1.W - q2.W;
end;

function quater_Mul;
begin
  Result.X := q1.W * q2.X + q1.X * q2.W + q1.Y * q2.Z - q1.Z * q2.Y;
  Result.Y := q1.W * q2.Y + q1.Y * q2.W + q1.Z * q2.X - q1.X * q2.Z;
  Result.Z := q1.W * q2.Z + q1.Z * q2.W + q1.X * q2.Y - q1.Y * q2.X;
  Result.W := q1.W * q2.W - q1.X * q2.X - q1.Y * q2.Y - q1.Z * q2.Z;
end;

function quater_Negate;
begin
  Result.X := -Quaternion.X;
  Result.Y := -Quaternion.Y;
  Result.Z := -Quaternion.Z;
  Result.W :=  Quaternion.W;
end;

function quater_Normalize;
  var
    t : Single;
begin
  t := ( sqr( Quaternion.X ) + sqr( Quaternion.Y ) + sqr( Quaternion.Z ) + sqr( Quaternion.W ) );
  if t <> 0 Then
    begin
      t        := 1 / t;
      Result.X := Quaternion.X * t;
      Result.Y := Quaternion.Y * t;
      Result.Z := Quaternion.Z * t;
      Result.W := Quaternion.W * t;
    end else
      Result := quater_Zero;
end;

function quater_Dot;
begin
  Result := q1.X * q2.X + q1.Y * q2.Y + q1.Z * q2.Z + q1.W * q2.W;
end;

function quater_Lerp;
  var
    p : zglTQuaternion;
    omega, cosom, scale0, scale1 : Single;
begin
  cosom := q1.X * q2.X + q1.Y * q2.Y + q1.Z * q2.Z + q1.W * q2.W;

  if cosom < 0 then
    begin 
      cosom := -cosom;
      p.X := -q2.X;
      p.Y := -q2.Y;
      p.Z := -q2.Z;
      p.W := -q2.W;
    end else
      begin
        p.X := q2.X;
        p.Y := q2.Y;
        p.Z := q2.Z;
        p.W := q2.W;
      end;

  scale0 := 1 - Value;
  scale1 := Value;

  Result.X := scale0 * q1.X + scale1 * p.X;
  Result.Y := scale0 * q1.Y + scale1 * p.Y;
  Result.Z := scale0 * q1.Z + scale1 * p.Z;
  Result.W := scale0 * q1.W + scale1 * p.W;
end;

function quater_FromRotation;
  var
    sr, sp, sy : Single;
    cr, cp, cy : Single;
    crcp, srsp : Single;
begin
  m_SinCos( Rotation.X * 0.5, sr, cr );
  m_SinCos( Rotation.Y * 0.5, sp, cp );
  m_SinCos( Rotation.Z * 0.5, sy, cy );
  crcp := cr * cp;
  srsp := sr * sp;

  Result.X := sr * cp * cy - cr * sp * sy;
  Result.Y := cr * sp * cy + sr * cp * sy;
  Result.Z := crcp * sy - srsp * cy;
  Result.W := crcp * cy + srsp * sy;
end;

function quater_GetM4f;
begin
  with Quaternion do
    begin
      Result.a11 := 1 - 2 * Y * Y - 2 * Z * Z;
      Result.a21 := 2 * X * Y + 2 * W * Z;
      Result.a31 := 2 * X * Z - 2 * W * Y;
      Result.a12 := 2 * X * Y - 2 * W * Z;
      Result.a22 := 1 - 2 * X * X - 2 * Z * Z;
      Result.a32 := 2 * Y * Z + 2 * W * X;
      Result.a13 := 2 * X * Z + 2 * W * Y;
      Result.a23 := 2 * Y * Z - 2 * W * X;
      Result.a33 := 1 - 2 * X * X - 2 * Y * Y;
    end;

  Result.a41 := 0;
  Result.a42 := 0;
  Result.a43 := 0;
  Result.a14 := 0;
  Result.a24 := 0;
  Result.a34 := 0;
  Result.a44 := 1;
end;

{------------------------------------------------------------------------------}
{------------------------------------ Line ------------------------------------}
{------------------------------------------------------------------------------}

function line3d_ClosestPoint;
  var
    v1, v2 : zglTPoint3D;
    d, t   : Single;
begin
	v1 := vector_Sub( Point, A );
  v2 := vector_Normalize( vector_Sub( B, A ) );
  d  := vector_FDistance( A, B );
  t  := vector_Dot( v2, v1 );

  if  t <= 0 Then
    begin
      Result := A;
		  exit;
  	end;

  if sqr( t ) >= d Then
    begin
	  	Result := B;
		  exit;
  	end;

  Result := vector_Add( A, vector_MulV( v2, t ) );
end;

{------------------------------------------------------------------------------}
{----------------------------------- Plane ------------------------------------}
{------------------------------------------------------------------------------}
function plane_Get;
begin
  Result.Points[ 0 ] := A;
  Result.Points[ 1 ] := B;
  Result.Points[ 2 ] := C;
  Result.Normal      := tri_GetNormal( A, B, C );
  Result.D           := -vector_Dot( Result.Normal, A );
end;

function plane_Distance;
begin
	Result := Plane.Normal.X * Point.X + Plane.Normal.Y * Point.Y + Plane.Normal.Z * Point.Z + Plane.D;
end;

function tri_GetNormal;
  var
    s1, s2, p : zglTPoint3D;
    uvector   : Single;
begin
  s1 := vector_Sub( A, B );
  s2 := vector_Sub( B, C );
  // вектор перпендикулярен центру треугольника
  p := vector_Cross( s1, s2 );
  // получаем унитарный вектор единичной длины
  uvector := sqrt( sqr( p.X ) + sqr( p.Y ) + sqr( p.Z ) );
  if uvector <> 0 Then
    Result := vector_MulV( p, 1 / uvector )
  else
    Result := vector_Get( 0, 0, 0 );
end;

initialization
  InitCosSinTables;

end.
