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
unit zgl_collision_3d;

{$I define.inc}

interface

uses
  zgl_const,
  zgl_types,
  zgl_math;

type
  zglPCol3DCallback = ^zglTCol3DCallback;
  zglTCol3DCallback = procedure( const Offset : zglTPoint3D; const Data : Pointer );

type
  zglPCollision3D = ^zglTCollision3D;
  zglTCollision3D = record
    Result : Boolean;
    Offset : zglTPoint3D;
end;
  
// point 3d
function col3d_PointInTri( const Point, A, B, C : zglTPoint3D  ) : Boolean;
function col3d_PointInAABB( const Point : zglTPoint3D; const AABB : zglTAABB ) : Boolean;
function col3d_PointInOBB( const Point : zglTPoint3D; const OBB : zglTOBB ) : Boolean;
function col3d_PointInSphere( const Point : zglTPoint3D; const Sphere : zglTSphere ) : Boolean;
// line 3d
function col3d_LineVsAABB( const Line : zglTLine3D; const AABB : zglTAABB ) : Boolean;
function col3d_LineVsOBB( const Line : zglTLine3D; const OBB : zglTOBB ) : Boolean;
function col3d_LineVsSphere( const Line : zglTLine3D; const Sphere : zglTSphere ) : Boolean;
// plane
function col3d_PlaneVsSphere( const Plane : zglTPlane; const Sphere : zglTSphere ) : zglTCollision3D;
// aabb
function col3d_AABBVsAABB( const AABB1, AABB2 : zglTAABB ) : Boolean;
function col3d_AABBVsOBB( const AABB : zglTAABB; OBB : zglTOBB ) : Boolean;
function col3d_AABBVsSphere( const AABB : zglTAABB; Sphere : zglTSphere ) : Boolean;
function col3d_AABBInAABB( const AABB1, AABB2 : zglTAABB ) : Boolean;
// obb
function col3d_OBBVsOBB( const OBB1, OBB2 : zglTOBB ) : Boolean;
function col3d_OBBVsSphere( const OBB : zglTOBB; const Sphere : zglTSphere ) : Boolean;
// sphere
function col3d_SphereVsSphere( const Sphere1, Sphere2 : zglTSphere ) : Boolean;
function col3d_SphereVsNode( const Sphere : zglTSphere; const Octree : zglTOctree; const Node : zglTNode; const Callback : zglTCol3DCallback; const CData : Pointer ) : Boolean;

implementation
  var
    tResult : Boolean = FALSE;

{------------------------------------------------------------------------------}
{---------------------------------- Point 3D ----------------------------------}
{------------------------------------------------------------------------------}
function col3d_PointInTri;
  var
    Angle : Single;
begin
  Result := FALSE;
 
  Angle := vector_Angle( vector_Sub( A, Point ), vector_Sub( B, Point ) ) +
           vector_Angle( vector_Sub( B, Point ), vector_Sub( C, Point ) ) +
           vector_Angle( vector_Sub( C, Point ), vector_Sub( A, Point ) );

	if Angle >= cv_pi * 2 * 0.999 Then Result := TRUE;
end;

function col3d_PointInAABB;
begin
  Result := ( abs( Point.X - AABB.Position.X ) < AABB.Size.X ) and
            ( abs( Point.Y - AABB.Position.Y ) < AABB.Size.Y ) and
            ( abs( Point.Z - AABB.Position.Z ) < AABB.Size.Z );
end;

function col3d_PointInOBB;
  var
    tAABB  : zglTAABB;
    tPoint : zglTPoint3D;
begin
  // "создаем" AABB идентичный текущему OBB
  tAABB.Position := OBB.Position;
  tAABB.Size     := OBB.Size;

  // трансформируем точку в систему координат OBB
  tPoint := vector_MulM3f( tPoint, OBB.Matrix );

	Result := col3d_PointInAABB( tPoint, tAABB );
end;

function col3d_PointInSphere;
begin
  Result := abs( vector_FDistance( Sphere.Position, Point ) ) < Sphere.Radius * Sphere.Radius;
end;

{------------------------------------------------------------------------------}
{----------------------------------- Line 3D ----------------------------------}
{------------------------------------------------------------------------------}
function col3d_LineVsAABB;
  var
    T : zglTPoint3D;
    hl, r : Single;
begin
  T := vector_Sub( AABB.Position, Line.p1 );
  hl := vector_Distance( Line.p1, Line.p2 );

  // проверяем, является ли одна из осей X,Y,Z разделяющей
  if ( abs( T.X ) > AABB.Size.X + hl * abs( Line.p2.X ) ) or
     ( abs( T.Y ) > AABB.Size.Y + hl * abs( Line.p2.Y ) ) or
     ( abs( T.Z ) > AABB.Size.Z + hl * abs( Line.p2.Z ) ) Then
    begin
      Result := FALSE;
      exit;
    end;

  // проверяем X ^ dir
  r := AABB.Size.Y * abs( Line.p2.Z ) + AABB.Size.Z * abs( Line.p2.Y );
  if abs( T.Y * Line.p2.Z - T.Z * Line.p2.Y ) > r Then
    begin
      Result := FALSE;
      exit;
    end;

  // проверяем Y ^ dir
  r := AABB.Size.X * abs( Line.p2.Z ) + AABB.Size.Z * abs( Line.p2.X );
  if abs( T.Z * Line.p2.X - T.X * Line.p2.Z ) > r Then
    begin
      Result := FALSE;
      exit;
    end;

  // проверяем Z ^ dir
  r := AABB.Size.X * abs( Line.p2.Y ) + AABB.Size.Y * abs( Line.p2.X );
  if abs( T.X * Line.p2.Y - T.Y * Line.p2.X ) > r Then
    begin
      Result := FALSE;
      exit;
    end;

   Result := TRUE;
end;

function col3d_LineVsOBB;
  var
    tAABB : zglTAABB;
    tLine : zglTLine3D;
begin
  // "создаем" AABB идентичный текущему OBB
  tAABB.Position := OBB.Position;
  tAABB.Size     := OBB.Size;

  // трансформируем линию в систему координат OBB
  tLine.p1 := vector_Add( OBB.Position, vector_MulM3f( vector_Sub( OBB.Position, Line.p1 ), OBB.Matrix ) );
  tLine.p2 := vector_MulM3f( Line.p2, OBB.Matrix );

	Result := col3d_LinevsAABB( tLine, tAABB );
end;

function col3d_LineVsSphere;
  var
    p1, p2     : zglTPoint3D;
    dx, dy, dz : Single;
    a, b, c    : Single;
begin
  p1 := vector_Sub( Line.p1, Sphere.Position );
  p2 := vector_Sub( Line.p2, Sphere.Position );
  
  dx := p2.X - p1.X;
  dy := p2.Y - p1.Y;
  dz := p2.Z - p1.Z;

  a := dx * dx + dy * dy + dz * dz;
  b := 2.0 * ( p1.X * dx + p1.Y * dy + p1.Z * dz );
  c := p1.X * p1.X + p1.Y * p1.Y + p1.Z * p1.Z - Sphere.Radius * Sphere.Radius;

  if -b < 0 Then
    Result := c < 0
  else
    if -b < ( 2.0 * a ) Then
      Result := 4.0 * a * c - b * b  < 0
    else
      Result := a + b + c < 0;
end;

{------------------------------------------------------------------------------}
{---------------------------------- Plane 3D ----------------------------------}
{------------------------------------------------------------------------------}
function col3d_PlaneVsSphere;
  var
    point    : zglTPoint3D;
    distance : Single;
  function EdgeSphereCollision( const Plane : zglTPlane; const Sphere : zglTSphere; var dis :Single ) : Boolean;
    var
      i : Byte;
      t : Single;
      c : zglTPoint3D;
  begin
    Result := FALSE;

    t := sqr( Sphere.Radius * 0.5 );
	  for i := 0 to 2 do
    	begin
		    c := line3d_ClosestPoint( Plane.Points[ i ], Plane.Points[ ( i + 1 ) mod 3 ], Sphere.Position );

    		dis := vector_FDistance( c, Sphere.Position );

    		if dis < t Then
          begin
  			    Result := TRUE;
            t      := dis;
            Point  := c;
          end;
    	end;

    if Result Then dis := sqrt( t );
  end;
  function GetCollisionOffset( const Normal : zglTPoint3D; const Radius, Distance : Single ) : zglTPoint3D;
    var
      distanceOver : Single;
  begin
    if distance > 0 Then
      begin
        distanceOver := Radius - Distance;
        Result       := vector_MulV( Normal, distanceOver );
      end else
        begin
          distanceOver := radius + distance;
          Result       := vector_MulV( Normal, -distanceOver );
        end;
  end;
begin
	Result.Result := FALSE;

  distance := plane_Distance( Plane, Sphere.Position );

	if abs( distance ) < Sphere.Radius Then
  	begin
		  Result.Offset := vector_MulV( Plane.Normal, distance );
      point         := vector_Sub( Sphere.Position, Result.Offset );

  		if col3d_PointInTri( point, Plane.Points[ 0 ], Plane.Points[ 1 ], Plane.Points[ 2 ] ) Then
        begin
          Result.Offset := GetCollisionOffset( Plane.Normal, Sphere.Radius, distance );
          Result.Result := TRUE;
        end else
          if EdgeSphereCollision( Plane, Sphere, distance ) Then
            begin
              Result.Offset := GetCollisionOffset( Plane.Normal, Sphere.Radius, distance );
              Result.Result := TRUE;
            end;
    end;
end;

{------------------------------------------------------------------------------}
{------------------------------------ AABB ------------------------------------}
{------------------------------------------------------------------------------}
function col3d_AABBVsAABB;
  var
    T : zglTPoint3D;
begin
  T := vector_Sub( AABB1.Position, AABB2.Position );
  Result := ( abs( T.X ) <= ( AABB1.Size.X + AABB2.Size.X ) ) and
            ( abs( T.Y ) <= ( AABB1.Size.Y + AABB2.Size.Y ) ) and
            ( abs( T.Z ) <= ( AABB1.Size.Z + AABB2.Size.Z ) );
end;

function col3d_AABBVsOBB;
  var
    tOBB : zglTOBB;
begin
	// "создаем" OBB идентичный AABB
  tOBB.Position := AABB.Position;
  tOBB.Size     := AABB.Size;

  Result := col3d_OBBvsOBB( OBB, tOBB );
end;

function col3d_AABBVsSphere;
  var
    d, a : Single;
    i : Integer;
    vMin, vMax : zglTPoint3D;
begin
  d := 0;
  a := 0;
  vMin := vector_Sub( AABB.Position, AABB.Size );
  vMax := vector_Add( AABB.Position, AABB.Size );

  // проходим по осям X,Y,Z
  for i := 0 to 2 do
    begin
      // если центр сферы лежит перед AABB,
      if Sphere.Position.point[ i ] < vMin.point[ i ] Then
        begin
          // то вычисляем квадрат расстояния по этой оси
          a := Sphere.Position.point[ i ] - vMin.point[ i ];
          d := d + a * a;
        end;

      // если центр сферы лежит после AABB,
      if Sphere.Position.point[ i ] > vMax.point[ i ] Then
        begin
          // то вычисляем квадрат расстояния по этой оси
          a := Sphere.Position.point[ i ] - vMax.point[ i ];
          d := d + a * a;
        end;
    end;

  Result := d <= ( Sphere.Radius * Sphere.Radius );
end;

function col3d_AABBInAABB;
begin
end;

{------------------------------------------------------------------------------}
{------------------------------------- OBB ------------------------------------}
{------------------------------------------------------------------------------}
function col3d_OBBVsOBB;
  var
    v, t : zglTPoint3D;
    mR : zglTMatrix3f;
    ra, rb, ts : Single;
    i, k : WORD;
begin
  //смещение в мировой системе координат
  v := vector_Sub( OBB1.Position, OBB2.Position );

  //смещение в системе координат А
  T := vector_MulM3f( v, OBB1.Matrix );

  //"создаем" матрицу поворота B относительно А
  mR := OBB2.Matrix;
  matrix3f_Transpose( mR );
  mR := matrix3f_Mul( mR, OBB1.Matrix );

  //система координат А
  for i := 0 to 2 do
    begin
      ra := OBB1.Size.point[ i ];
      rb := OBB2.Size.X * abs( mR.row[ i ].X ) + OBB2.Size.Y * abs( mR.row[ i ].Y ) + OBB2.Size.Z * abs( mR.row[ i ].Z );
      ts := abs( T.point[ i ] );
      if ts > ra + rb Then
        begin
          Result := FALSE;
          exit;
        end;
    end;

  //система координат B
  for k := 0 to 2 do
    begin
      ra := OBB1.Size.X * abs( mR.row[ 0 ].point[ k ] ) + OBB1.Size.Y * abs( mR.row[ 1 ].point[ k ] ) + OBB1.Size.Z * abs( mR.row[ 2 ].point[ k ] );
      rb := OBB2.Size.point[ k ];
      ts := abs( T.X * mR.row[ 0 ].point[ k ] + T.Y * mR.row[ 1 ].point[ k ] + T.Z * mR.row[ 2 ].point[ k ] );
      if ts > ra + rb Then
        begin
          Result := FALSE;
          exit;
        end;
    end;

  //9 векторных произведений
  //L = A0 x B0
  ra := OBB1.Size.Y * abs( mR.row[ 2 ].X ) + OBB1.Size.Z * abs( mR.row[ 1 ].X );
  rb := OBB2.Size.Y * abs( mR.row[ 0 ].Z ) + OBB2.Size.Z * abs( mR.row[ 0 ].Y );
  ts := abs( T.Z * mR.row[ 1 ].X - T.Y * mR.row[ 2 ].X );
  if ts > ra + rb Then
    begin
      Result := FALSE;
      exit;
    end;

  //L = A0 x B1
  ra := OBB1.Size.Y * abs( mR.row[ 2 ].Y ) + OBB1.Size.Z * abs( mR.row[ 1 ].Y );
  rb := OBB2.Size.X * abs( mR.row[ 0 ].Z ) + OBB2.Size.Z * abs( mR.row[ 0 ].X );
  ts := abs( T.Z * mR.row[ 1 ].Y - T.Y * mR.row[ 2 ].Y );
  if ts > ra + rb Then
    begin
      Result := FALSE;
      exit;
    end;

  //L = A0 x B2
  ra := OBB1.Size.Y * abs( mR.row[ 2 ].Z ) + OBB1.Size.Z * abs( mR.row[ 1 ].Z );
  rb := OBB2.Size.X * abs( mR.row[ 0 ].Y ) + OBB2.Size.Y * abs( mR.row[ 0 ].X );
  ts := abs( T.Z * mR.row[ 1 ].Z - T.Y * mR.row[ 2 ].Z );
  if ts > ra + rb Then
    begin
      Result := FALSE;
      exit;
    end;

  //L = A1 x B0
  ra := OBB1.Size.X * abs( mR.row[ 2 ].X ) + OBB1.Size.Z * abs( mR.row[ 0 ].X );
  rb := OBB2.Size.Y * abs( mR.row[ 1 ].Z ) + OBB2.Size.Z * abs( mR.row[ 1 ].Y );
  ts := abs( T.X * mR.row[ 2 ].X - T.Z * mR.row[ 0 ].X );
  if ts > ra + rb Then
    begin
      Result := FALSE;
      exit;
    end;

  //L = A1 x B1
  ra := OBB1.Size.X * abs( mR.row[ 2 ].Y ) + OBB1.Size.Z * abs( mR.row[ 0 ].Y );
  rb := OBB2.Size.X * abs( mR.row[ 1 ].Z ) + OBB2.Size.Z * abs( mR.row[ 1 ].X );
  ts := abs( T.X * mR.row[ 2 ].Y - T.Z * mR.row[ 0 ].Y );
  if ts > ra + rb Then
    begin
      Result := FALSE;
      exit;
    end;

  //L = A1 x B2
  ra := OBB1.Size.X * abs( mR.row[ 2 ].Z ) + OBB1.Size.Z * abs( mR.row[ 0 ].Z );
  rb := OBB2.Size.X * abs( mR.row[ 1 ].Y ) + OBB2.Size.Y * abs( mR.row[ 1 ].X );
  ts := abs( T.X * mR.row[ 2 ].Z - T.Z * mR.row[ 0 ].Z );
  if ts > ra + rb Then
    begin
      Result := FALSE;
      exit;
    end;

  //L = A2 x B0
  ra := OBB1.Size.X * abs( mR.row[ 1 ].X ) + OBB1.Size.Y * abs( mR.row[ 0 ].X );
  rb := OBB2.Size.Y * abs( mR.row[ 2 ].Z ) + OBB2.Size.Z * abs( mR.row[ 2 ].Y );
  ts := abs( T.Y * mR.row[ 0 ].X - T.X * mR.row[ 1 ].X );
  if ts > ra + rb Then
    begin
      Result := FALSE;
      exit;
    end;

  //L = A2 x B1
  ra := OBB1.Size.X * abs( mR.row[ 1 ].Y ) + OBB1.Size.Y * abs( mR.row[ 0 ].Y );
  rb := OBB2.Size.X * abs( mR.row[ 2 ].Z ) + OBB2.Size.Z * abs( mR.row[ 2 ].X );
  ts := abs( T.Y * mR.row[ 0 ].Y - T.X * mR.row[ 1 ].Y );
  if ts > ra + rb Then
    begin
      Result := FALSE;
      exit;
    end;

  //L = A2 x B2
  ra := OBB1.Size.X * abs( mR.row[ 1 ].Z ) + OBB1.Size.Y * abs( mR.row[ 0 ].Z );
  rb := OBB2.Size.X * abs( mR.row[ 2 ].Y ) + OBB2.Size.Y * abs( mR.row[ 2 ].X );
  ts := abs( T.Y * mR.row[ 0 ].Z - T.X * mR.row[ 1 ].Z );
  if ts > ra + rb Then
    begin
      Result := FALSE;
      exit;
    end;

  Result := TRUE;
end;

function col3d_OBBVsSphere;
  var
    tAABB   : zglTAABB;
    tSphere : zglTSphere;
begin
  // "создаем" AABB идентичный текущему OBB
  tAABB.Position := OBB.Position;
  tAABB.Size     := OBB.Size;

  // трансформируем сферу в систему координат OBB
  tSphere.Position := vector_Add( OBB.Position, vector_MulM3f( vector_Sub( OBB.Position, Sphere.Position ), OBB.Matrix ) );
  tSphere.Radius   := Sphere.Radius;

  Result := col3d_AABBvsSphere( tAABB, tSphere );
end;

{------------------------------------------------------------------------------}
{----------------------------------- Sphere -----------------------------------}
{------------------------------------------------------------------------------}
function col3d_SphereVsSphere;
begin
  Result := sqr( Sphere1.Position.X - Sphere2.Position.X ) +
            sqr( Sphere1.Position.Y - Sphere2.Position.Y ) +
            sqr( Sphere1.Position.Z - Sphere2.Position.Z ) < sqr( Sphere1.Radius + Sphere2.Radius );
end;

function col3d_SphereVsNode;
  var
    i : DWORD;
    t : zglTCollision3D;
begin
  Result := FALSE;
  if not col3d_AABBVsSphere( Node.Cube, Sphere ) Then exit;
  
  if Node.NInside Then
    for i := 0 to 7 do
      if Assigned( Node.SubNodes[ i ] ) Then
        if col3d_SphereVsNode( Sphere, Octree, Node.SubNodes[ i ]^, Callback, CData ) Then tResult := TRUE;

  if Node.PCount > 0 Then
    for i := 0 to Node.PCount - 1 do
      begin
        t := col3d_PlaneVsSphere( Octree.Planes[ Node.Planes[ i ] ], Sphere );
        if t.Result Then
          begin
            if Assigned( Callback ) Then Callback( t.Offset, CData );
            tResult := TRUE;
          end;
      end;
      
  if tResult Then
    begin
      Result  := tResult;
      tResult := FALSE;
    end;
end;

{------------------------------------------------------------------------------}
{---------------------------------- Extended ----------------------------------}
{------------------------------------------------------------------------------}

end.
